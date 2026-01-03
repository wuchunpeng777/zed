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
    Anchor, DisplayPoint, SelectionEffects, ToOffset, ToPoint,
};
use gpui::{Context, Window};
use language::{Point, SelectionGoal};
use project::InlayId;
use std::ops::Range;

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
}

#[derive(Clone, Debug, Default)]
pub struct FlashState {
    pub active: bool,
    pub search_chars: String,
    pub matches: Vec<FlashMatch>,
    pub backwards: bool,
    pub inlay_ids: Vec<InlayId>,
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
        // Get old inlay IDs to remove before creating new ones
        let old_inlay_ids = flash_state.inlay_ids.clone();

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

            // Search a reasonable range around cursor (about 100 lines in each direction)
            // This is much faster than searching the entire document
            let search_range_lines = 100u32;
            let max_row = display_snapshot.max_point().row();

            let start_row = cursor_point.row().0.saturating_sub(search_range_lines);
            let end_row = (cursor_point.row().0 + search_range_lines).min(max_row.0);

            let start_point = DisplayPoint::new(DisplayRow(start_row), 0);
            let end_point = DisplayPoint::new(
                DisplayRow(end_row),
                display_snapshot.line_len(DisplayRow(end_row)),
            );

            // Find matches with case-insensitive multi-character search
            let mut matches = find_all_matches_multi_char(
                &display_snapshot,
                start_point,
                end_point,
                &search_pattern,
            );

            if backwards {
                matches.reverse();
            }

            let buffer_snapshot = display_snapshot.buffer_snapshot();
            
            // flash.nvim style: exclude characters that would continue the search
            // For each match, get the character immediately following it
            // These chars would conflict with labels (typing them continues search)
            let mut skip_chars: std::collections::HashSet<char> = std::collections::HashSet::new();
            
            for match_result in &matches {
                // Use buffer_point directly - no DisplayPoint conversion needed
                let match_point = match_result.buffer_point;
                let line_len = buffer_snapshot.line_len(multi_buffer::MultiBufferRow(match_point.row));
                
                // Calculate byte position after the pattern
                let after_col = match_point.column + match_result.pattern_byte_len;
                
                // Only proceed if we're still within the line
                if after_col < line_len {
                    // Get the text after the match to find the next character
                    let line_start = Point::new(match_point.row, 0);
                    let line_start_offset = line_start.to_offset(&buffer_snapshot);
                    let line_end = Point::new(match_point.row, line_len);
                    let line_end_offset = line_end.to_offset(&buffer_snapshot);
                    
                    // Get the whole line text and find char after match by character iteration
                    let line_text: String = buffer_snapshot
                        .text_for_range(line_start_offset..line_end_offset)
                        .collect();
                    
                    // Find the character at the byte position after the match
                    let mut byte_count: u32 = 0;
                    for c in line_text.chars() {
                        if byte_count == after_col {
                            skip_chars.insert(c.to_ascii_lowercase());
                            break;
                        }
                        byte_count += c.len_utf8() as u32;
                        if byte_count > after_col {
                            // We're inside a multi-byte character - skip
                            break;
                        }
                    }
                }
            }
            
            // Filter labels: exclude chars that would continue a match
            let mut label_chars: Vec<char> = FLASH_LABELS
                .chars()
                .filter(|c| !skip_chars.contains(&c.to_ascii_lowercase()))
                .collect();
            
            // If no labels available after filtering, use all labels as fallback
            if label_chars.is_empty() {
                label_chars = FLASH_LABELS.chars().collect();
            }
            
            // Generate labels: single chars first, then 2-char combinations
            let labels = generate_labels(&label_chars, matches.len());
            
            let flash_matches: Vec<FlashMatch> = matches
                .into_iter()
                .enumerate()
                .map(|(i, match_result)| {
                    let anchor = buffer_snapshot.anchor_before(match_result.buffer_point);
                    FlashMatch {
                        buffer_point: match_result.buffer_point,
                        display_point: match_result.display_point,
                        label: labels[i].clone(),
                        anchor: Some(anchor),
                    }
                })
                .collect();

            let flash_state = vim.flash_state.as_mut().expect("flash state should exist");
            let pattern_byte_len = search_pattern.len() as u32;
            flash_state.matches = flash_matches.clone();

            // Create highlight ranges for background using buffer points
            let ranges: Vec<Range<Anchor>> = flash_matches
                .iter()
                .map(|m| {
                    let start_point = m.buffer_point;
                    // Use byte length for column offset since Point.column is in bytes
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

            // Create inlays for labels
            let inlays: Vec<Inlay> = flash_matches
                .iter()
                .enumerate()
                .filter_map(|(i, m)| {
                    m.anchor.map(|anchor| {
                        Inlay::flash(i, anchor, format!("[{}]", m.label))
                    })
                })
                .collect();

            let inlay_ids: Vec<InlayId> = inlays.iter().map(|inlay| inlay.id).collect();
            flash_state.inlay_ids = inlay_ids;
            
            editor.splice_inlays(&[], inlays, cx);
        });

        self.show_flash_labels(window, cx);
    }

    fn show_flash_labels(&mut self, _window: &mut Window, cx: &mut Context<Self>) {
        // Labels are shown inline via inlays, no need for status bar display
        cx.notify();
    }

    fn jump_to_flash_match(
        &mut self,
        flash_match: FlashMatch,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        // Use anchor-based position to avoid offset issues when inlays are removed
        // The anchor is buffer-relative and won't shift when inlays disappear
        let anchor = flash_match.anchor.clone();
        
        self.update_editor(cx, |vim, editor, cx| {
            // First, remove inlays to get accurate display positions
            let inlay_ids: Vec<InlayId> = vim
                .flash_state
                .as_ref()
                .map(|s| s.inlay_ids.clone())
                .unwrap_or_default();
            if !inlay_ids.is_empty() {
                editor.splice_inlays(&inlay_ids, Vec::new(), cx);
            }
            if let Some(state) = vim.flash_state.as_mut() {
                state.inlay_ids.clear();
            }
            
            // Now calculate the correct display point from the anchor
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
                    if vim.mode.is_visual() {
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

        self.flash_state = None;
        self.status_label = None;
        self.update_editor(cx, |_, editor, cx| {
            editor.clear_background_highlights::<FlashHighlight>(cx);
            // Remove flash inlays
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
fn generate_labels(chars: &[char], count: usize) -> Vec<String> {
    let mut labels = Vec::with_capacity(count);
    
    // If we can fit all matches with single character labels, use them
    if count <= chars.len() {
        for &c in chars.iter().take(count) {
            labels.push(c.to_string());
        }
    } else {
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
