//! Flash-style jump navigation for Vim mode.
//!
//! This module implements a flash.nvim-like feature that allows users to:
//! 1. Enter a search pattern
//! 2. See jump labels at all matching positions
//! 3. Press a label key to jump directly to that position

use crate::Vim;
use editor::{
    display_map::{DisplayRow, DisplaySnapshot, ToDisplayPoint},
    inlays::Inlay,
    Anchor, DisplayPoint, Editor, SelectionEffects, ToOffset, ToPoint,
};
use gpui::{Context, Entity, WeakEntity, Window};
use language::{Point, SelectionGoal};
use project::InlayId;
use std::ops::Range;
use workspace::Pane;

/// Labels for flash jumps - prioritizing home row keys for easy typing
/// These will be filtered dynamically to exclude characters in the search pattern
const FLASH_LABELS: &str = "asdfghjklqwertyuiopzxcvbnm";
/// Maximum number of matches to find (for performance)
const MAX_MATCHES: usize = 100;

struct FlashHighlight;

#[derive(Clone, Debug)]
pub struct FlashMatch {
    /// Buffer position (row, column in bytes) - stable across display changes
    pub buffer_point: Point,
    /// Display position for rendering
    pub display_point: DisplayPoint,
    /// Jump label (can be 1 or more characters)
    pub label: String,
    pub anchor: Option<Anchor>,
    /// The editor this match belongs to (None means current editor)
    pub editor: Option<WeakEntity<Editor>>,
    /// The pane containing this editor (for activation)
    pub pane: Option<WeakEntity<Pane>>,
}

#[derive(Clone, Debug, Default)]
pub struct FlashState {
    pub active: bool,
    pub search_chars: String,
    pub matches: Vec<FlashMatch>,
    pub backwards: bool,
    pub inlay_ids: Vec<InlayId>,
    /// Inlay IDs per editor (for multi-pane support)
    pub editor_inlay_ids: Vec<(WeakEntity<Editor>, Vec<InlayId>)>,
    /// Buffer for multi-character label input
    pub label_input: String,
}

impl FlashState {
    pub fn new(backwards: bool) -> Self {
        Self {
            active: true,
            search_chars: String::new(),
            matches: Vec::new(),
            backwards,
            inlay_ids: Vec::new(),
            editor_inlay_ids: Vec::new(),
            label_input: String::new(),
        }
    }

    pub fn add_char(&mut self, c: char) {
        self.search_chars.push(c);
    }
    
    /// Check if any label starts with the given prefix
    pub fn has_label_prefix(&self, prefix: &str) -> bool {
        self.matches.iter().any(|m| m.label.starts_with(prefix))
    }
    
    /// Find exact label match
    pub fn find_label_match(&self, label: &str) -> Option<FlashMatch> {
        self.matches.iter().find(|m| m.label == label).cloned()
    }
}

impl Vim {
    pub fn flash_search(
        &mut self,
        backwards: bool,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        self.flash_state = Some(FlashState::new(backwards));
        self.sync_vim_settings(window, cx);
    }

    pub fn flash_input(
        &mut self,
        text: &str,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> bool {
        let Some(flash_state) = &mut self.flash_state else {
            return false;
        };

        let first_char = text.chars().next();
        let Some(c) = first_char else {
            return false;
        };

        // If we have matches displayed, try to match labels
        if !flash_state.matches.is_empty() {
            // Build up the label input
            let mut test_label = flash_state.label_input.clone();
            test_label.push(c);
            
            // Check for exact label match
            if let Some(flash_match) = flash_state.find_label_match(&test_label) {
                // Jump to the matching label
                self.jump_to_flash_match(flash_match, window, cx);
                self.clear_flash(window, cx);
                return true;
            }
            
            // Check if this could be a prefix of a longer label
            if flash_state.has_label_prefix(&test_label) {
                // Store the partial label input and wait for more
                flash_state.label_input = test_label;
                // Refresh inlays to show the typed prefix as dimmed
                self.refresh_flash_inlays(window, cx);
                self.sync_vim_settings(window, cx);
                return true;
            }
            
            // Not a label - clear label input and continue searching
            flash_state.label_input.clear();
        }

        // Add character to search pattern
        flash_state.add_char(c);

        // Start searching after at least 2 characters
        if flash_state.search_chars.len() >= 2 {
            self.update_flash_matches(window, cx);
        }
        self.sync_vim_settings(window, cx);
        true
    }

    fn update_flash_matches(&mut self, window: &mut Window, cx: &mut Context<Self>) {
        let Some(flash_state) = &mut self.flash_state else {
            return;
        };

        let search_pattern = flash_state.search_chars.clone();
        if search_pattern.is_empty() {
            return;
        }

        let backwards = flash_state.backwards;
        
        // Get old inlay IDs to remove from all editors
        let old_inlay_ids = flash_state.inlay_ids.clone();
        let old_editor_inlay_ids = flash_state.editor_inlay_ids.clone();

        // Clear old inlays from other editors first
        for (weak_editor, inlay_ids) in old_editor_inlay_ids {
            if let Some(editor) = weak_editor.upgrade() {
                editor.update(cx, |editor, cx| {
                    editor.splice_inlays(&inlay_ids, vec![], cx);
                    editor.clear_background_highlights::<FlashHighlight>(cx);
                });
            }
        }

        // Collect all editors from workspace panes
        let workspace = self.workspace(window);
        let mut all_editors: Vec<(Entity<Editor>, Option<WeakEntity<Pane>>)> = Vec::new();
        
        if let Some(workspace) = workspace {
            let workspace_read = workspace.read(cx);
            for pane in workspace_read.panes() {
                let pane_weak = pane.downgrade();
                for item in pane.read(cx).items() {
                    if let Some(editor) = item.downcast::<Editor>() {
                        all_editors.push((editor, Some(pane_weak.clone())));
                    }
                }
            }
        }

        // First, collect all matches from all visible editors
        struct EditorMatches {
            editor: WeakEntity<Editor>,
            pane: Option<WeakEntity<Pane>>,
            matches: Vec<MatchResult>,
            display_snapshot: DisplaySnapshot,
        }
        
        let mut all_editor_matches: Vec<EditorMatches> = Vec::new();
        let pattern_byte_len = search_pattern.len() as u32;
        
        // Process current editor first
        self.update_editor(cx, |vim, editor, cx| {
            // Remove old inlays first
            if !old_inlay_ids.is_empty() {
                editor.splice_inlays(&old_inlay_ids, vec![], cx);
            }
            editor.clear_background_highlights::<FlashHighlight>(cx);

            let snapshot = editor.snapshot(window, cx);
            let display_snapshot = snapshot.display_snapshot.clone();

            // Get cursor position for context-aware searching
            let cursor_point = editor
                .selections
                .newest::<Point>(&display_snapshot)
                .head()
                .to_display_point(&display_snapshot);

            // Search visible range in current editor
            let search_range_lines = 100u32;
            let max_row = display_snapshot.max_point().row();

            let start_row = cursor_point.row().0.saturating_sub(search_range_lines);
            let end_row = (cursor_point.row().0 + search_range_lines).min(max_row.0);

            let start_point = DisplayPoint::new(DisplayRow(start_row), 0);
            let end_point = DisplayPoint::new(
                DisplayRow(end_row),
                display_snapshot.line_len(DisplayRow(end_row)),
            );

            let matches = find_all_matches_multi_char(
                &display_snapshot,
                start_point,
                end_point,
                &search_pattern,
            );

            if let Some(state) = vim.flash_state.as_mut() {
                state.inlay_ids.clear();
                state.editor_inlay_ids.clear();
            }
            
            all_editor_matches.push(EditorMatches {
                editor: cx.entity().downgrade(),
                pane: None,
                matches,
                display_snapshot,
            });
        });

        // Process other editors in split panes
        let current_editor_id = self.editor().map(|e| e.entity_id());
        for (editor, pane) in all_editors {
            if Some(editor.entity_id()) == current_editor_id {
                continue;
            }
            
            editor.update(cx, |editor, cx| {
                let snapshot = editor.snapshot(window, cx);
                let display_snapshot = snapshot.display_snapshot.clone();
                let max_row = display_snapshot.max_point().row();

                let start_point = DisplayPoint::new(DisplayRow(0), 0);
                let end_point = DisplayPoint::new(
                    max_row,
                    display_snapshot.line_len(max_row),
                );

                let matches = find_all_matches_multi_char(
                    &display_snapshot,
                    start_point,
                    end_point,
                    &search_pattern,
                );
                
                if !matches.is_empty() {
                    all_editor_matches.push(EditorMatches {
                        editor: cx.entity().downgrade(),
                        pane,
                        matches,
                        display_snapshot,
                    });
                }
            });
        }

        // Collect all matches and build skip chars
        let mut all_matches: Vec<(MatchResult, WeakEntity<Editor>, Option<WeakEntity<Pane>>, DisplaySnapshot)> = Vec::new();
        let mut skip_chars: std::collections::HashSet<char> = std::collections::HashSet::new();

        for editor_match in &all_editor_matches {
            let buffer_snapshot = editor_match.display_snapshot.buffer_snapshot();
            
            for match_result in &editor_match.matches {
                let match_point = match_result.buffer_point;
                let line_len = buffer_snapshot.line_len(multi_buffer::MultiBufferRow(match_point.row));
                let after_col = match_point.column + match_result.pattern_byte_len;
                
                if after_col < line_len {
                    let line_start = Point::new(match_point.row, 0);
                    let line_start_offset = line_start.to_offset(&buffer_snapshot);
                    let line_end = Point::new(match_point.row, line_len);
                    let line_end_offset = line_end.to_offset(&buffer_snapshot);
                    
                    let line_text: String = buffer_snapshot
                        .text_for_range(line_start_offset..line_end_offset)
                        .collect();
                    
                    let mut byte_count: u32 = 0;
                    for c in line_text.chars() {
                        if byte_count == after_col {
                            skip_chars.insert(c.to_ascii_lowercase());
                            break;
                        }
                        byte_count += c.len_utf8() as u32;
                        if byte_count > after_col {
                            break;
                        }
                    }
                }
            }
        }

        for editor_match in all_editor_matches {
            for match_result in editor_match.matches {
                all_matches.push((
                    match_result,
                    editor_match.editor.clone(),
                    editor_match.pane.clone(),
                    editor_match.display_snapshot.clone(),
                ));
            }
        }

        if backwards {
            all_matches.reverse();
        }

        // Filter labels
        let mut label_chars: Vec<char> = FLASH_LABELS
            .chars()
            .filter(|c| !skip_chars.contains(&c.to_ascii_lowercase()))
            .collect();
        
        if label_chars.is_empty() {
            label_chars = FLASH_LABELS.chars().collect();
        }
        
        let labels = generate_labels(&label_chars, all_matches.len());
        
        // Create flash matches with editor references
        // Only create matches for which we have labels (safety check)
        let flash_matches: Vec<FlashMatch> = all_matches
            .iter()
            .enumerate()
            .take(labels.len()) // Ensure we don't exceed available labels
            .map(|(i, (match_result, editor, pane, display_snapshot))| {
                let buffer_snapshot = display_snapshot.buffer_snapshot();
                let anchor = buffer_snapshot.anchor_before(match_result.buffer_point);
                FlashMatch {
                    buffer_point: match_result.buffer_point,
                    display_point: match_result.display_point,
                    label: labels[i].clone(),
                    anchor: Some(anchor),
                    editor: Some(editor.clone()),
                    pane: pane.clone(),
                }
            })
            .collect();

        // Group matches by editor and create inlays/highlights
        let mut editor_inlay_ids: Vec<(WeakEntity<Editor>, Vec<InlayId>)> = Vec::new();
        let mut inlay_counter = 0usize;
        // Get label input length for prefix highlighting (0 during initial match display)
        let label_input_len = self.flash_state.as_ref().map(|s| s.label_input.len()).unwrap_or(0);

        // Group matches by editor entity id along with their snapshots
        let mut matches_by_editor: std::collections::HashMap<gpui::EntityId, (WeakEntity<Editor>, DisplaySnapshot, Vec<&FlashMatch>)> = 
            std::collections::HashMap::new();
        
        for (i, flash_match) in flash_matches.iter().enumerate() {
            if let Some(ref editor) = flash_match.editor {
                if let Some(editor_entity) = editor.upgrade() {
                    let editor_id = editor_entity.entity_id();
                    let entry = matches_by_editor
                        .entry(editor_id)
                        .or_insert_with(|| {
                            let (_, _, _, ds) = &all_matches[i];
                            (editor.clone(), ds.clone(), Vec::new())
                        });
                    entry.2.push(flash_match);
                }
            }
        }

        // Apply highlights and inlays to each editor
        for (_editor_id, (editor_weak, display_snapshot, matches)) in matches_by_editor {
            if let Some(editor) = editor_weak.upgrade() {
                editor.update(cx, |editor, cx| {
                    let buffer_snapshot = display_snapshot.buffer_snapshot();
                    
                    // Create highlight ranges
                    let ranges: Vec<Range<Anchor>> = matches
                        .iter()
                        .map(|m| {
                            let start_point = m.buffer_point;
                            let end_point = Point::new(start_point.row, start_point.column + pattern_byte_len);
                            let start = buffer_snapshot.anchor_before(start_point);
                            let end = buffer_snapshot.anchor_after(end_point);
                            start..end
                        })
                        .collect();

                    editor.highlight_background::<FlashHighlight>(
                        &ranges,
                        |_, colors| colors.colors().editor_document_highlight_write_background,
                        cx,
                    );

                    // Create inlays with prefix highlighting support
                    let inlays: Vec<Inlay> = matches
                        .iter()
                        .filter_map(|m| {
                            m.anchor.map(|anchor| {
                                let inlay = Inlay::flash(inlay_counter, anchor, m.label.clone(), label_input_len);
                                inlay_counter += 1;
                                inlay
                            })
                        })
                        .collect();

                    let inlay_ids: Vec<InlayId> = inlays.iter().map(|inlay| inlay.id).collect();
                    editor_inlay_ids.push((editor_weak.clone(), inlay_ids));
                    
                    editor.splice_inlays(&[], inlays, cx);
                });
            }
        }

        // Update flash state
        if let Some(flash_state) = &mut self.flash_state {
            flash_state.matches = flash_matches;
            flash_state.editor_inlay_ids = editor_inlay_ids;
        }

        self.show_flash_labels(window, cx);
    }

    fn show_flash_labels(&mut self, _window: &mut Window, cx: &mut Context<Self>) {
        // Labels are shown inline via inlays, no need for status bar display
        cx.notify();
    }

    /// Refresh flash inlays to update prefix highlighting when user types partial labels
    fn refresh_flash_inlays(&mut self, _window: &mut Window, cx: &mut Context<Self>) {
        let Some(flash_state) = &self.flash_state else {
            return;
        };

        let label_input_len = flash_state.label_input.len();
        let matches = flash_state.matches.clone();
        let old_editor_inlay_ids = flash_state.editor_inlay_ids.clone();

        // Remove old inlays and create new ones with updated prefix_len
        let mut new_editor_inlay_ids: Vec<(WeakEntity<Editor>, Vec<InlayId>)> = Vec::new();
        let mut inlay_counter = 0usize;

        for (editor_weak, old_inlay_ids) in old_editor_inlay_ids {
            if let Some(editor) = editor_weak.upgrade() {
                // Get matches for this editor
                let editor_id = editor.entity_id();
                let editor_matches: Vec<&FlashMatch> = matches
                    .iter()
                    .filter(|m| {
                        m.editor
                            .as_ref()
                            .and_then(|e| e.upgrade())
                            .map(|e| e.entity_id() == editor_id)
                            .unwrap_or(false)
                    })
                    .collect();

                editor.update(cx, |editor, cx| {
                    // Remove old inlays
                    editor.splice_inlays(&old_inlay_ids, vec![], cx);

                    // Create new inlays with updated prefix_len
                    let inlays: Vec<Inlay> = editor_matches
                        .iter()
                        .filter_map(|m| {
                            m.anchor.map(|anchor| {
                                let inlay = Inlay::flash(inlay_counter, anchor, m.label.clone(), label_input_len);
                                inlay_counter += 1;
                                inlay
                            })
                        })
                        .collect();

                    let inlay_ids: Vec<InlayId> = inlays.iter().map(|inlay| inlay.id).collect();
                    new_editor_inlay_ids.push((editor_weak.clone(), inlay_ids));

                    editor.splice_inlays(&[], inlays, cx);
                });
            }
        }

        // Update flash state with new inlay IDs
        if let Some(flash_state) = &mut self.flash_state {
            flash_state.editor_inlay_ids = new_editor_inlay_ids;
        }

        cx.notify();
    }

    fn jump_to_flash_match(
        &mut self,
        flash_match: FlashMatch,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        let anchor = flash_match.anchor.clone();
        let target_editor = flash_match.editor.clone();
        let target_pane = flash_match.pane.clone();
        let is_visual = self.mode.is_visual();

        // Clear all inlays from all editors first
        if let Some(flash_state) = &self.flash_state {
            for (weak_editor, inlay_ids) in &flash_state.editor_inlay_ids {
                if let Some(editor) = weak_editor.upgrade() {
                    editor.update(cx, |editor, cx| {
                        editor.splice_inlays(inlay_ids, Vec::new(), cx);
                    });
                }
            }
        }

        if let Some(state) = self.flash_state.as_mut() {
            state.inlay_ids.clear();
            state.editor_inlay_ids.clear();
        }

        // If jumping to another editor, activate its pane and item first
        if let Some(ref target_editor_weak) = target_editor {
            if let Some(target_editor) = target_editor_weak.upgrade() {
                let current_editor_id = self.editor().map(|e| e.entity_id());
                
                if Some(target_editor.entity_id()) != current_editor_id {
                    // Activate the pane containing the target editor
                    if let Some(ref pane_weak) = target_pane {
                        if let Some(pane) = pane_weak.upgrade() {
                            let target_id = target_editor.entity_id();
                            let item_index = pane.read(cx).items().position(|item| {
                                item.downcast::<Editor>()
                                    .map(|e| e.entity_id() == target_id)
                                    .unwrap_or(false)
                            });
                            if let Some(index) = item_index {
                                pane.update(cx, |pane, cx| {
                                    pane.activate_item(index, true, true, window, cx);
                                });
                            }
                        }
                    }

                    // Jump in the target editor directly
                    target_editor.update(cx, |editor, cx| {
                        let snapshot = editor.snapshot(window, cx);
                        let display_snapshot = &snapshot.display_snapshot;
                        
                        let point = if let Some(ref anchor) = anchor {
                            let buffer_point = anchor.to_point(&display_snapshot.buffer_snapshot());
                            buffer_point.to_display_point(display_snapshot)
                        } else {
                            flash_match.display_point
                        };

                        editor.change_selections(SelectionEffects::no_scroll(), window, cx, |s| {
                            s.move_with(|_, selection| {
                                if is_visual {
                                    selection.set_head(point, SelectionGoal::None);
                                } else {
                                    selection.collapse_to(point, SelectionGoal::None);
                                }
                            });
                        });
                    });
                    return;
                }
            }
        }

        // Jump in current editor
        self.update_editor(cx, |_, editor, cx| {
            let snapshot = editor.snapshot(window, cx);
            let display_snapshot = &snapshot.display_snapshot;
            
            let point = if let Some(ref anchor) = anchor {
                let buffer_point = anchor.to_point(&display_snapshot.buffer_snapshot());
                buffer_point.to_display_point(display_snapshot)
            } else {
                flash_match.display_point
            };

            editor.change_selections(SelectionEffects::no_scroll(), window, cx, |s| {
                s.move_with(|_, selection| {
                    if is_visual {
                        selection.set_head(point, SelectionGoal::None);
                    } else {
                        selection.collapse_to(point, SelectionGoal::None);
                    }
                });
            });
        });
    }

    pub fn clear_flash(&mut self, window: &mut Window, cx: &mut Context<Self>) {
        // Get inlay IDs to remove before clearing state
        let inlay_ids: Vec<InlayId> = self
            .flash_state
            .as_ref()
            .map(|s| s.inlay_ids.clone())
            .unwrap_or_default();
        let editor_inlay_ids: Vec<(WeakEntity<Editor>, Vec<InlayId>)> = self
            .flash_state
            .as_ref()
            .map(|s| s.editor_inlay_ids.clone())
            .unwrap_or_default();

        self.flash_state = None;
        self.status_label = None;
        
        // Clear inlays from all editors
        for (weak_editor, inlay_ids) in editor_inlay_ids {
            if let Some(editor) = weak_editor.upgrade() {
                editor.update(cx, |editor, cx| {
                    editor.clear_background_highlights::<FlashHighlight>(cx);
                    if !inlay_ids.is_empty() {
                        editor.splice_inlays(&inlay_ids, vec![], cx);
                    }
                });
            }
        }
        
        // Also clear from current editor
        self.update_editor(cx, |_, editor, cx| {
            editor.clear_background_highlights::<FlashHighlight>(cx);
            if !inlay_ids.is_empty() {
                editor.splice_inlays(&inlay_ids, vec![], cx);
            }
        });
        self.sync_vim_settings(window, cx);
    }

    pub fn is_flash_active(&self) -> bool {
        self.flash_state.as_ref().is_some_and(|s| s.active)
    }
}

/// Generate unique labels for matches.
/// Ensures no label is a prefix of another label.
/// If we have enough single chars, use them. Otherwise, use only 2-char labels.
/// If still not enough, use 3-char labels.
fn generate_labels(chars: &[char], count: usize) -> Vec<String> {
    if chars.is_empty() || count == 0 {
        return Vec::new();
    }
    
    let mut labels = Vec::with_capacity(count);
    
    // If we can fit all matches with single character labels, use them
    if count <= chars.len() {
        for &c in chars.iter().take(count) {
            labels.push(c.to_string());
        }
    } else if count <= chars.len() * chars.len() {
        // Need more labels than single chars available
        // Use only 2-character combinations to avoid prefix conflicts
        'outer: for &c1 in chars {
            for &c2 in chars {
                if labels.len() >= count {
                    break 'outer;
                }
                labels.push(format!("{}{}", c1, c2));
            }
        }
    } else {
        // Need even more labels - use 3-character combinations
        'outer: for &c1 in chars {
            for &c2 in chars {
                for &c3 in chars {
                    if labels.len() >= count {
                        break 'outer;
                    }
                    labels.push(format!("{}{}{}", c1, c2, c3));
                }
            }
        }
    }
    
    labels
}

/// A match result containing both buffer and display positions
struct MatchResult {
    buffer_point: Point,
    display_point: DisplayPoint,
    /// Byte length of the matched pattern
    pattern_byte_len: u32,
}

/// Find all matches for a multi-character pattern with case-insensitive search.
/// Returns buffer Points (stable) along with DisplayPoints (for rendering).
fn find_all_matches_multi_char(
    map: &DisplaySnapshot,
    start: DisplayPoint,
    end: DisplayPoint,
    pattern: &str,
) -> Vec<MatchResult> {
    let mut matches = Vec::new();
    let pattern_lower: Vec<char> = pattern.to_lowercase().chars().collect();
    let pattern_char_len = pattern_lower.len();
    let pattern_byte_len = pattern.len() as u32;
    
    if pattern_char_len == 0 {
        return matches;
    }

    let buffer_snapshot = map.buffer_snapshot();
    
    // Get start and end points for line-by-line searching
    let start_point = start.to_point(map);
    let end_point = end.to_point(map);
    
    // Search line by line to handle multi-byte characters correctly
    for row in start_point.row..=end_point.row {
        if matches.len() >= MAX_MATCHES {
            break;
        }
        
        let line_len_bytes = buffer_snapshot.line_len(multi_buffer::MultiBufferRow(row));
        if line_len_bytes == 0 {
            continue;
        }
        
        // Determine search bounds within this line (in bytes)
        let search_start_col = if row == start_point.row { start_point.column } else { 0 };
        let search_end_col = if row == end_point.row { end_point.column } else { line_len_bytes };
        
        // Get line text
        let line_start = Point::new(row, 0);
        let line_start_offset = line_start.to_offset(&buffer_snapshot);
        let line_end = Point::new(row, line_len_bytes);
        let line_end_offset = line_end.to_offset(&buffer_snapshot);
        
        if line_start_offset >= line_end_offset {
            continue;
        }
        
        let line_text: String = buffer_snapshot
            .text_for_range(line_start_offset..line_end_offset)
            .collect();
        let line_lower: Vec<char> = line_text.to_lowercase().chars().collect();
        
        // Build a mapping from character index to byte offset within the line
        let mut char_to_byte: Vec<u32> = Vec::with_capacity(line_lower.len() + 1);
        let mut byte_pos: u32 = 0;
        for c in line_text.chars() {
            char_to_byte.push(byte_pos);
            byte_pos += c.len_utf8() as u32;
        }
        char_to_byte.push(byte_pos); // End position
        
        // Find matches in this line
        for char_idx in 0..line_lower.len() {
            if matches.len() >= MAX_MATCHES {
                break;
            }
            
            let col_bytes = char_to_byte[char_idx];
            
            // Skip if before search start (in bytes)
            if col_bytes < search_start_col {
                continue;
            }
            // Stop if past search end (in bytes)
            if col_bytes >= search_end_col {
                break;
            }
            
            // Check for pattern match
            if char_idx + pattern_char_len <= line_lower.len() {
                let mut found = true;
                for (i, &pc) in pattern_lower.iter().enumerate() {
                    if line_lower[char_idx + i] != pc {
                        found = false;
                        break;
                    }
                }
                
                if found {
                    // Create buffer Point with correct byte column
                    let buffer_point = Point::new(row, col_bytes);
                    let display_point = buffer_point.to_display_point(map);
                    
                    if display_point >= start && display_point < end {
                        matches.push(MatchResult {
                            buffer_point,
                            display_point,
                            pattern_byte_len,
                        });
                    }
                }
            }
        }
    }

    matches
}
