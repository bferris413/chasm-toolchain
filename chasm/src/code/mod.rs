use std::{
    fmt::{Debug, Write}, ops::{Not, Range}, sync::{Arc, RwLock}
};

use crate::{CodeArgs, project::{ChasmProject, ModulePath}};

use anyhow::Result;
use crossterm::{cursor, event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers}};
use ratatui::{
    DefaultTerminal,
    Frame,
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style, Stylize},
    text::{Line, Span, Text},
    widgets::{Block, Borders, FrameExt, List, ListState, Paragraph, StatefulWidget, Widget, WidgetRef}
};

#[derive(Copy, Clone, Debug, Default)]
struct Metadata {
    status_area: StatusArea,
    view_area: ViewArea,
}

trait ChasmWidget: Debug {
    fn handle_event(&mut self, event: &Event, meta: &Metadata) -> AppCommand;
    fn render(&self, area: Rect, buf: &mut Buffer);
}
impl Widget for &dyn ChasmWidget {
    fn render(self, area: Rect, buf: &mut Buffer) {
        self.render(area, buf);
    }
}
impl WidgetRef for &dyn ChasmWidget {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        self.render(area, buf);
    }
}

pub fn code(args: CodeArgs) -> Result<()> {
    let project = ChasmProject::load(args.project_dir.0)?;
    ratatui::run(|terminal| App::new(project).run(terminal))
}

#[derive(Debug)]
enum AppCommand {
    None,
    Quit,
    PopView,
    PushView(Box<dyn ChasmWidget>),
    Yes,
    No,
}

#[derive(Copy, Clone, Debug, Default)]
struct StatusArea(Rect);
#[derive(Copy, Clone, Debug, Default)]
struct ViewArea(Rect);

#[derive(Debug)]
pub struct App {
    project: Arc<RwLock<ChasmProject>>,
    status: StatusBar,
    view_stack: Vec<Box<dyn ChasmWidget>>,

    should_exit: bool,
    exit_modal_active: bool,
    metadata: Metadata,
}
impl App {
    pub fn new(project: ChasmProject) -> Self {
        let project = Arc::new(RwLock::new(project));
        let mut select_list_state = ListState::default();
        select_list_state.select(Some(0));

        let args = ModuleSelectViewArgs {
            project: project.clone(),
            select_list_state,
            module_paths: project.read().unwrap().module_names(),
        };
        Self {
            project: project.clone(),
            status: StatusBar::new(),
            view_stack: vec![Box::new(ModuleSelectView::new(args))],

            should_exit: false,
            exit_modal_active: false,

            // populated in first draw call, but needs to be initialized to something
            metadata: Metadata {
                status_area: StatusArea(Rect::default()),
                view_area: ViewArea(Rect::default()),
            },
        }
    }

    pub fn run(&mut self, terminal: &mut DefaultTerminal) -> Result<()> {
        while !self.should_exit {
            let mut metadata = Metadata::default();

            terminal.draw(|frame| {
                let (status_area, view_area) = Self::top_level_layout(&frame);
                metadata = Metadata { status_area, view_area };
                self.draw(frame, status_area, view_area);
            })?;

            self.handle_events(metadata)?;
        }
        Ok(())
    }

    fn top_level_layout(f: &Frame) -> (StatusArea, ViewArea) {
        let layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1),
                Constraint::Fill(1),
            ])
            .split(f.area());

        (StatusArea(layout[0]), ViewArea(layout[1]))
    }

    fn draw(&self, frame: &mut Frame, status_area: StatusArea, view_area: ViewArea) {
        frame.render_widget(&self.status, status_area.0);
        frame.render_widget_ref(self.view_stack.last().unwrap().as_ref(), view_area.0);
    }

    fn handle_events(&mut self, meta: Metadata) -> Result<()> {
        match event::read()? {
            e @ Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                self.status.handle_event(&e, &meta);

                match key_event.code {
                    KeyCode::Char('q') => {
                        if !self.exit_modal_active {
                            let modal = YesNoModal::new("Exit", "Are you sure you want to exit?");
                            self.view_stack.push(Box::new(modal));
                            self.exit_modal_active = true;
                        }
                    }
                    _other => {
                        let cmd = self.view_stack.last_mut().unwrap().handle_event(&e, &meta);
                        match cmd {
                            AppCommand::Yes => {
                                if self.exit_modal_active {
                                    self.view_stack.pop();
                                    self.exit_modal_active = false;
                                    self.should_exit = true;
                                }
                            }
                            AppCommand::No => {
                                if self.exit_modal_active {
                                    self.view_stack.pop();
                                    self.exit_modal_active = false;
                                }
                            }
                            AppCommand::None => {}
                            AppCommand::PushView(widget) => self.view_stack.push(widget),
                            other => { 
                                panic!("Unexpected command from exit modal: {other:?}");
                            }
                        }
                    }
                }
            }
            e => {
                self.status.handle_event(&e, &meta);
            }
        };

        Ok(())
    }
}

#[derive(Debug)]
struct StatusBar {
    event_text: String,
}
impl StatusBar {
    pub fn new() -> Self {
        Self {
            event_text: String::new(),
         }
     }
}
impl ChasmWidget for StatusBar {
    fn handle_event(&mut self, event: &Event, _meta: &Metadata) -> AppCommand {
        match event {
            Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                self.event_text.clear();
                let modifier = if key_event.modifiers.is_empty() {
                    String::new()
                } else {
                    format!("{}+", key_event.modifiers)
                };
                write!(&mut self.event_text, "<pressed {modifier}{}>", key_event.code).unwrap();
            }
            other => {
                self.event_text.clear();
                write!(&mut self.event_text, "<event {:?}>", other).unwrap();
            }
        };

        AppCommand::None
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let title = "Chasm Editor";
        let block = Block::default().style(Style::new().bg(Color::LightGreen).fg(Color::Black));
        let text = Line::from(vec![
            Span::styled(title, Style::new().bold()),
            Span::raw(" - "),
            Span::raw(&self.event_text),
        ]);

        Paragraph::new(text)
            .block(block)
            .render(area, buf);
    }
}
impl WidgetRef for StatusBar {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        ChasmWidget::render(self, area, buf);
    }
}
impl Widget for &StatusBar {
    fn render(self, area: Rect, buf: &mut Buffer) {
        ChasmWidget::render(self, area, buf);
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum EditorMode {
    Normal,
    Insert,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct VisualSelection {
    anchor: Position,
    cursor: Position,
}
impl VisualSelection {
    fn is_empty(&self) -> bool {
        self.anchor == self.cursor
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Position {
    line: usize,
    column: usize,
}

#[derive(Debug)]
struct Editor {
    module: ModulePath,
    code: Vec<String>,
    scroll_y: usize,
    mode: EditorMode,

    // cursor 0-based y position in the code (not screen)
    cursor_y: usize,
    // cursor 0-based x position in the code (not screen)
    cursor_x: usize,
    // last x position explicitly moved to by the user
    last_requested_x: usize,
    active_selection: Option<VisualSelection>,
}
impl Editor {
    pub fn new(module: ModulePath) -> Self {

        let code = std::fs::read_to_string(&module.path).unwrap_or_else(|e| {
            format!("Error loading module {}: {}\n\nReload or something...", module.name, e)
        });

        let code = code.lines().map(|line| line.to_string()).collect();

        Self {
            module,
            code,
            scroll_y: 0,
            mode: EditorMode::Normal,
            cursor_y: 0,
            cursor_x: 0,
            last_requested_x: 0,
            active_selection: None,
        }
    }

    fn snap_view_to_cursor(&mut self, meta: &Metadata) {
        let view_height = meta.view_area.0.height as usize;
        if self.cursor_y > self.scroll_y + view_height - 1 {
            self.scroll_y = self.cursor_y.saturating_sub(view_height - 1);
        } else if self.cursor_y < self.scroll_y {
            self.scroll_y = self.cursor_y;
        }
    }

    fn max_cursor_x(&self) -> usize {
        let clamp_at = 1;
        self.code[self.cursor_y].len().saturating_sub(clamp_at)
    }

    fn move_cursor_up(&mut self, move_size: usize, meta: &Metadata) {
        self.cursor_y = self.cursor_y.saturating_sub(move_size);
        self.cursor_x = self.last_requested_x.min(self.max_cursor_x());
        self.snap_view_to_cursor(meta);
    }

    fn move_cursor_down(&mut self, move_size: usize, meta: &Metadata) {
        self.cursor_y = self.cursor_y.saturating_add(move_size).min(self.code.len().saturating_sub(1));
        self.cursor_x = self.last_requested_x.min(self.max_cursor_x());
        self.snap_view_to_cursor(meta);
    }

    fn move_cursor_left(&mut self) {
        self.cursor_x = self.cursor_x.saturating_sub(1);
        self.last_requested_x = self.cursor_x;
    }

    fn move_cursor_right(&mut self) {
        let max_cursor_x = self.max_cursor_x();
        self.cursor_x = (self.cursor_x + 1).min(max_cursor_x);
        self.last_requested_x = self.cursor_x;
    }

    fn delete_forward(&mut self) {
        if self.cursor_x < self.code[self.cursor_y].len() {
            self.code[self.cursor_y].remove(self.cursor_x);
        } else if self.cursor_x == self.code[self.cursor_y].len() && self.cursor_y < self.code.len() - 1 {
            // delete the logical newline by merging the current line with the next line
            let line_to_append = self.code.remove(self.cursor_y + 1);
            self.code[self.cursor_y].push_str(&line_to_append);
        }
    }

    fn delete_backward(&mut self) {
        if self.cursor_x > 0 {
            self.code[self.cursor_y].remove(self.cursor_x - 1);
            self.cursor_x -= 1;
        } else if self.cursor_y > 0 {
            assert!(self.cursor_x == 0);

            let prev_line_len = self.code[self.cursor_y - 1].len();
            let current_line = self.code.remove(self.cursor_y);
            self.cursor_y -= 1;
            self.code[self.cursor_y].push_str(&current_line);
            self.cursor_x = prev_line_len;
        }
    }

    fn skip_x_whitespace_forward(&mut self) {
        let mut line_chars = self.code[self.cursor_y].chars();
        while let Some(c) = line_chars.next() {
            if c.is_ascii_whitespace() {
                self.cursor_x += 1;
            } else {
                break;
            }
        }
        self.last_requested_x = self.cursor_x;
    }

    fn toggle_selection(&mut self) {
        if self.active_selection.is_none() {
            let anchor = Position { line: self.cursor_y, column: self.cursor_x };
            self.active_selection = Some(VisualSelection { anchor, cursor: anchor });
        } else {
            self.active_selection = None;
        }
    }

    fn should_update_visual_selection(&self) -> bool {
        self.active_selection.is_some()
    }

    fn handle_normal_mode_event(&mut self, event: &Event, meta: &Metadata) -> AppCommand {
        fn get_vertical_move_distance(kev: &KeyEvent, meta: &Metadata) -> usize {
            if kev.modifiers.contains(KeyModifiers::CONTROL) {
                (meta.view_area.0.height as usize).saturating_sub(1)
            } else {
                1
            }
        }

        if ! event.is_key_press() {
            return AppCommand::None;
        }

        let Event::Key(key_event) = event else { unreachable!() };

        let app_cmd = match key_event.code {
            KeyCode::Esc => {
                if self.active_selection.is_some() {
                    self.active_selection = None;
                }
                AppCommand::None
            }
            KeyCode::Char('G') => {
                let target_line = self.code.len().saturating_sub(1);
                if target_line != self.cursor_y {
                    let move_len = target_line.saturating_sub(self.cursor_y);
                    self.move_cursor_down(move_len, meta);
                }
                AppCommand::None
            }
            KeyCode::Char('g') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                let target_line = 0;
                if target_line != self.cursor_y {
                    let move_len = self.cursor_y.saturating_sub(target_line);
                    self.move_cursor_up(move_len, meta);
                }

                AppCommand::None
            }
            KeyCode::Char('I') => {
                self.cursor_x = 0;
                self.skip_x_whitespace_forward();
                self.mode = EditorMode::Insert;
                self.active_selection = None;
                AppCommand::None
            }
            KeyCode::Char('x') => {
                self.delete_forward();
                AppCommand::None
            }
            KeyCode::Char('d') => {
                if key_event.modifiers.contains(KeyModifiers::CONTROL) {
                    let move_size = (meta.view_area.0.height as usize).saturating_sub(1);
                    self.move_cursor_down(move_size, meta);
                } else if let Some(selection) = self.active_selection.take() {
                    if selection.is_empty() {
                        self.delete_forward();
                    }
                }
                AppCommand::None
            }
            KeyCode::Char('a') => {
                let is_max_normal_len = self.cursor_x == self.max_cursor_x();
                let is_empty = self.code[self.cursor_y].is_empty();

                if is_empty {
                    // we have an empty line, no need to move cursor
                    assert!(is_max_normal_len);
                } else if is_max_normal_len {
                    // user wants to append
                    self.cursor_x += 1;
                } else {
                    self.move_cursor_right();
                }

                self.mode = EditorMode::Insert;
                self.active_selection = None;
                AppCommand::None
            }
            KeyCode::Char('s') => {
                let is_empty = self.code[self.cursor_y].is_empty();

                if is_empty {
                    // do nothing
                } else {
                    self.code[self.cursor_y].remove(self.cursor_x);
                }

                self.mode = EditorMode::Insert;
                self.active_selection = None;
                AppCommand::None
            }
            KeyCode::Char('O') => {
                let has_no_lines = self.code.is_empty();

                if has_no_lines {
                    self.code.push(String::new());
                } else {
                    self.code.insert(self.cursor_y, String::new());
                    self.cursor_x = 0;
                }

                self.mode = EditorMode::Insert;
                self.active_selection = None;
                AppCommand::None
            }
            KeyCode::Char('o') => {
                self.code.insert(self.cursor_y + 1, String::new());
                self.cursor_y += 1;
                self.cursor_x = 0;
                self.mode = EditorMode::Insert;
                self.active_selection = None;
                AppCommand::None
            }
            KeyCode::Char('J') | KeyCode::Down if key_event.modifiers.contains(KeyModifiers::SHIFT) => {
                self.scroll_y = self.scroll_y.saturating_add(1);
                AppCommand::None
            }
            KeyCode::Char('K') | KeyCode::Up if key_event.modifiers.contains(KeyModifiers::SHIFT) => {
                self.scroll_y = self.scroll_y.saturating_sub(1);
                AppCommand::None
            }
            KeyCode::Char('u') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                let move_size = (meta.view_area.0.height as usize).saturating_sub(1);
                self.move_cursor_up(move_size, meta);
                AppCommand::None
            }
            KeyCode::Char('j') | KeyCode::Down => {
                let move_size = get_vertical_move_distance(key_event, meta);
                self.move_cursor_down(move_size, meta);
                AppCommand::None
            }
            KeyCode::Char('k') | KeyCode::Up => {
                let move_size = get_vertical_move_distance(key_event, meta);
                self.move_cursor_up(move_size, meta);
                AppCommand::None
            }
            KeyCode::Char('h') | KeyCode::Left => {
                self.move_cursor_left();
                AppCommand::None
            }
            KeyCode::Char('l') | KeyCode::Right => {
                self.move_cursor_right();
                AppCommand::None
            }
            KeyCode::Char('0') => {
                self.cursor_x = 0;
                self.last_requested_x = self.cursor_x;
                AppCommand::None
            }
            KeyCode::Char('$') => {
                let max_cursor_x = self.max_cursor_x();
                self.cursor_x = max_cursor_x;
                self.last_requested_x = self.cursor_x;
                AppCommand::None
            }
            KeyCode::Char('i') => {
                // insert mode
                self.mode = EditorMode::Insert;
                self.active_selection = None;
                AppCommand::None
            }
            KeyCode::Char('v') => {
                self.toggle_selection();
                AppCommand::None
            }
            _other => {
                AppCommand::None
            }
        };

        let new_cursor_pos = Position { line: self.cursor_y, column: self.cursor_x };

        if self.should_update_visual_selection() {
            let selection = self.active_selection.as_mut().unwrap();
            selection.cursor = new_cursor_pos;
        }

        app_cmd
    }

    fn handle_insert_mode_event(&mut self, event: &Event, meta: &Metadata) -> AppCommand {
        if let Event::Key(key_event) = event && key_event.kind == KeyEventKind::Press {
            match key_event.code {
                KeyCode::Esc => {
                    self.mode = EditorMode::Normal;
                    let is_empty = self.code[self.cursor_y].is_empty();
                    let is_off_line = self.cursor_x == self.code[self.cursor_y].len();

                    if is_empty && is_off_line {
                        // nothing to do, cursor is in valid position
                    } else if is_off_line {
                        assert!(!is_empty);
                        // we are on a non-empty line but past the end (e.g. via 'a'), move back to last char
                        self.move_cursor_left();
                    }

                    self.last_requested_x = self.cursor_x;
                    AppCommand::None
                }
                KeyCode::Char('j') | KeyCode::Char('k') | KeyCode::Char('h') | KeyCode::Char('l') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                    match key_event.code {
                        KeyCode::Char('j') => {
                            self.move_cursor_down(1, meta);
                        }
                        KeyCode::Char('k') => {
                            self.move_cursor_up(1, meta);
                        }
                        KeyCode::Char('h') => {
                            self.move_cursor_left();
                        }
                        KeyCode::Char('l') => {
                            self.move_cursor_right();
                        }
                        _ => unreachable!(),
                    }

                    AppCommand::None
                }
                KeyCode::Left => {
                    self.move_cursor_left();
                    AppCommand::None
                }
                KeyCode::Right => {
                    self.move_cursor_right();
                    AppCommand::None
                }
                KeyCode::Up => {
                    self.move_cursor_up(1, meta);
                    AppCommand::None
                }
                KeyCode::Down => {
                    self.move_cursor_down(1, meta);
                    AppCommand::None
                }
                KeyCode::Char(c) => {
                    self.code[self.cursor_y].insert(self.cursor_x, c);
                    self.cursor_x += 1;
                    AppCommand::None
                }
                KeyCode::Backspace => {
                    self.delete_backward();
                    AppCommand::None
                }
                KeyCode::Delete => {
                    self.delete_forward();
                    AppCommand::None
                }
                _other => {
                    AppCommand::None
                }
            }
        } else {
            AppCommand::None
        }
    }

}
impl ChasmWidget for Editor {
    
    fn handle_event(&mut self, event: &Event, meta: &Metadata) -> AppCommand {
        match self.mode {
            EditorMode::Normal => self.handle_normal_mode_event(event, meta),
            EditorMode::Insert => self.handle_insert_mode_event(event, meta),
        }
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let height = area.height as usize;
        let digits_for_line_numbers = digits_in(self.code.len() as u64);
        let total_lines = self.code.len();

        let layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(digits_for_line_numbers as u16 + 1), // line numbers + padding
                Constraint::Fill(1), // code
            ])
            .split(area);

        let gutter = layout[0];
        let code_area = layout[1];

        let start = self.scroll_y;
        let end = (start + height).min(total_lines);

        // render line numbers manually in gutter
        for (screen_row, line_idx) in (start..end).enumerate() {
            let line_number = line_idx + 1;

            let text = format!("{:<width$} ", line_number, width = digits_for_line_numbers as usize);

            let target_y = gutter.y + screen_row as u16;
            buf.set_string(
                gutter.x,
                target_y,
                text,
                Style::default().fg(Color::Gray),
            );
        }

        let code_slice = &self.code[start..end];
        let code_as_lines = code_slice.iter().map(|line| Line::from(line.as_str())).collect::<Vec<_>>();

        Paragraph::new(code_as_lines).block(Block::default()).render(code_area, buf);  

        // draw the line highlight (must be before selection)
        let mut cursor_line_is_visible = false;
        if (start..end).contains(&self.cursor_y) {
            cursor_line_is_visible = true; 
            let cursor_screen_y = self.cursor_y - start;
            let cursor_line_rect = Rect { y: code_area.y + cursor_screen_y as u16, x: gutter.x, width: gutter.width + code_area.width, height: 1 };
            buf.set_style(cursor_line_rect, Style::default().bg(Color::DarkGray));
        }


        // draw the selection
        let visible_range_y = start..end;

        if let Some(selection) = &self.active_selection {
            if selection.is_empty() {
                // nothing to render
            } else if selection_has_no_overlap(selection, &visible_range_y) {
                // nothing to render
            } else {
                // This is admittedly a bit of a mess
                let (selection_start,selection_end) = selection_visible_range(selection, &visible_range_y);
                for line_no in selection_start.line..=selection_end.line {
                    let render_line = (line_no - start) as u16;

                    let (x_start, x_width) = if line_no == selection_start.line && line_no == selection_end.line {
                        // start at selection start, width is selection length
                        (selection_start.column as u16, (selection_end.column - selection_start.column) as u16)
                    } else if line_no == selection_start.line {
                        // start at selection start, width is entire line
                        (selection_start.column as u16, (self.code[line_no].len() - selection_start.column + 1) as u16)
                    } else if line_no == selection_end.line {
                        // start at selection start, width is selection end column
                        (0, selection_end.column as u16) 
                    } else {
                        // entire line is selected
                        (0, self.code[line_no].len() as u16 + 1)
                    };

                    let line_rect = Rect { y: code_area.y + render_line, x: code_area.x + x_start, width: x_width, height: 1 };
                    buf.set_style(line_rect, Style::default().bg(Color::Gray));
                }
            }
        }

        // draw cursor (after all highlights/selections)
        if cursor_line_is_visible {
            // TODO: we do nothing we x scroll
            if self.cursor_x < code_area.width as usize {
                // cursor is within visible code area, draw normally
                let cursor_screen_y = self.cursor_y - start;
                let cursor_line_rect_y = code_area.y + cursor_screen_y as u16;

                let cursor_screen_x = self.cursor_x as u16 + gutter.width;
                let cell = buf.cell_mut((cursor_screen_x, cursor_line_rect_y)).unwrap();

                match self.mode {
                    EditorMode::Normal => {
                        cell.set_style(Style::default().bg(Color::LightGreen).fg(Color::Black));
                    }
                    EditorMode::Insert => {
                        cell.set_style(Style::default().bg(Color::Red).fg(Color::Black));
                    }
                }
            }
        }
    }
}

fn selection_visible_range(selection: &VisualSelection, visible_range_y: &Range<usize>) -> (Position, Position) {
    assert!(!selection.is_empty(), "empty selection!");
    let (start, end) = selection_absolute_order(selection);
    clamp_selection_to_visible_range(start, end, visible_range_y)
}

fn clamp_selection_to_visible_range(start: Position, end: Position, visible_range_y: &Range<usize>) -> (Position, Position) {
    let clamped_start = if start.line < visible_range_y.start {
        Position { line: visible_range_y.start, column: 0 }
    } else {
        start
    };

    let clamped_end = if end.line >= visible_range_y.end {
        Position { line: visible_range_y.end - 1, column: usize::MAX }
    } else {
        end
    };

    (clamped_start, clamped_end)
}

/// Returns the (start, end) positions of a given selection by comparing their line/col positions.
fn selection_absolute_order(selection: &VisualSelection) -> (Position, Position) {
    if selection.anchor.line < selection.cursor.line {
        // selected downwards
        (selection.anchor, selection.cursor)
    } else if selection.cursor.line < selection.anchor.line {
        // selected upwards
        (selection.cursor, selection.anchor)
    } else if selection.anchor.column <= selection.cursor.column {
        // selection is across the same line
        (selection.anchor, selection.cursor)
    } else {
        assert!(selection.cursor.column < selection.anchor.column);
        (selection.cursor, selection.anchor)
    }
}

fn selection_has_no_overlap(selection: &VisualSelection, visible_range_y: &Range<usize>) -> bool {
    let anchor_is_above = selection.anchor.line < visible_range_y.start;
    let cursor_is_above = selection.cursor.line < visible_range_y.start;

    let anchor_is_below = selection.anchor.line >= visible_range_y.end;
    let cursor_is_below = selection.cursor.line >= visible_range_y.end;

    (anchor_is_above && cursor_is_above) || (anchor_is_below && cursor_is_below)
}

#[derive(Debug)]
struct ModuleSelectViewArgs {
    pub project: Arc<RwLock<ChasmProject>>,
    pub select_list_state: ListState,
    pub module_paths: Vec<ModulePath>,
}

#[derive(Debug)]
struct ModuleSelectView {
    project: Arc<RwLock<ChasmProject>>,
    select_list_state: ListState,
    list: List<'static>,
    module_paths: Vec<ModulePath>,
}
impl ModuleSelectView {
    pub fn new(args:ModuleSelectViewArgs) -> Self {
        let base_item = ["Create new module".to_string()].into_iter();
        let modules = args.module_paths.iter().map(|m| m.name.clone());
        let list = List::new(base_item.chain(modules))
            .highlight_style(Style::new().reversed())
            .highlight_symbol(">> ".bold())
            .repeat_highlight_symbol(true);

        Self { project: args.project, select_list_state: args.select_list_state, list, module_paths: args.module_paths }
    }

    fn update_selection(&mut self, next_fn: fn(isize) -> isize) {
        let len = self.list.len() as isize;
        let cur_selected = self.select_list_state.selected().unwrap_or(0) as isize;

        let mut next = next_fn(cur_selected);

        if next < 0 {
            next = len - 1;
        } else if next >= len {
            next = 0;
        }

        self.select_list_state.select(Some(next as usize));
    }
}

impl ChasmWidget for ModuleSelectView {
    fn handle_event(&mut self, event: &Event, _meta: &Metadata) -> AppCommand {
        if let Event::Key(key_event) = event && key_event.kind == KeyEventKind::Press {
            match key_event.code {
                KeyCode::Char('j') | KeyCode::Down => {
                    self.update_selection(|n| n + 1);
                }
                KeyCode::Char('k') | KeyCode::Up => {
                    self.update_selection(|n| n - 1);
                }
                KeyCode::Enter => {
                    // Open selected module or create new module if "Create new module" is selected
                    let selected = self.select_list_state.selected().unwrap();
                    if selected == 0 {
                        // TODO create new module
                    } else {
                        let selected = selected - 1; // offset to account for "Create new"
                        let module_path = self.module_paths[selected].clone();
                        let editor = Editor::new(module_path);
                        return AppCommand::PushView(Box::new(editor));
                    }

                }
                _other => {
                    // No-op for now
                }
            }
        }

        AppCommand::None
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let mut state = self.select_list_state;
        StatefulWidget::render(&self.list, area, buf, &mut state);
    }
}

#[derive(Copy, Clone, Debug)]
enum ModalSelection {
    Yes,
    No,
}
impl Not for ModalSelection {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            ModalSelection::Yes => ModalSelection::No,
            ModalSelection::No => ModalSelection::Yes
        }
    }
}
struct YesNoModal {
    title: &'static str,
    prompt: Span<'static>,
    selection: ModalSelection,
}
impl YesNoModal {
    pub fn new(title: &'static str, prompt: &'static str) -> Self {
        let prompt = Span::raw(prompt);
        Self { title, prompt, selection: ModalSelection::No }
    }
}
impl ChasmWidget for YesNoModal {
    fn handle_event(&mut self, event: &Event, _meta: &Metadata) -> AppCommand {
        if let Event::Key(key_event) = event && key_event.kind == KeyEventKind::Press {
            match key_event.code {
                KeyCode::Char('h') | KeyCode::Left => {
                    // left, no wrap
                    self.selection = ModalSelection::Yes;
                    AppCommand::None
                }
                KeyCode::Char('l') | KeyCode::Right => {
                    // right, no wrap
                    self.selection = ModalSelection::No;
                    AppCommand::None
                }
                KeyCode::Char('Y') => {
                    if matches!(self.selection, ModalSelection::Yes) {
                        AppCommand::Yes
                    } else {
                        self.selection = ModalSelection::Yes;
                        AppCommand::None
                    }
                }
                KeyCode::Char('N') => {
                    if matches!(self.selection, ModalSelection::No) {
                        AppCommand::No
                    } else {
                        self.selection = ModalSelection::No;
                        AppCommand::None
                    }
                }
                KeyCode::Tab => {
                    // toggle
                    self.selection = !self.selection;
                    AppCommand::None
                }
                KeyCode::Enter => {
                    match self.selection {
                        ModalSelection::Yes => AppCommand::Yes,
                        ModalSelection::No => AppCommand::No,
                    }
                }
                KeyCode::Esc => {
                    AppCommand::No
                }
                _other => {
                    AppCommand::None
                }
            }
        } else {
            AppCommand::None
        }
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let modal_block = Block::default().title(self.title).borders(Borders::ALL);
        let modal_area = centered_rect(50, 12, area);
        let inner_area = modal_block.inner(modal_area);
        let inner_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Min(0),     // top spacer
                Constraint::Length(3),  // prompt
                Constraint::Length(1),  // gap
                Constraint::Length(1),  // buttons
                Constraint::Min(0),     // bottom spacer
            ])
            .split(inner_area);

        let prompt_area = inner_layout[1];
        let button_row_area = inner_layout[3];

        let button_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Min(0),
                Constraint::Length(7), // yes
                Constraint::Length(4), // gap between buttons
                Constraint::Length(6), // no
                Constraint::Min(0),
            ])
            .split(button_row_area);

        let yes_area = button_layout[1];
        let no_area = button_layout[3];

        let prompt_block = Paragraph::new(self.prompt.clone()).centered();
        let yes_style = if matches!(self.selection, ModalSelection::Yes) {
            Style::default().reversed().bold()
        } else {
            Style::default()
        };

        let no_style = if matches!(self.selection, ModalSelection::No) {
            Style::default().reversed().bold()
        } else {
            Style::default()
        };

        let yes_button = Paragraph::new(Line::from(vec!["[ ".into(), "Y".underlined(), "es ]".into()])).centered().style(yes_style).block(Block::default());
        let no_button = Paragraph::new(Line::from(vec!["[ ".into(), "N".underlined(), "o ]".into()])).centered().style(no_style).block(Block::default());

        modal_block.render(modal_area, buf);
        prompt_block.render(prompt_area, buf);
        yes_button.render(yes_area, buf);
        no_button.render(no_area, buf)
    }
}
impl std::fmt::Debug for YesNoModal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("YesNoModal")
            .field("title", &self.title)
            .field("prompt", &self.prompt)
            .field("selection", &self.selection)
            .finish()
    }
}

fn centered_rect(width: u16, height: u16, area: Rect) -> Rect {
    let x = area.x + (area.width.saturating_sub(width)) / 2;
    let y = area.y + (area.height.saturating_sub(height)) / 2;

    Rect::new(x, y, width, height)
}

fn digits_in(n: u64) -> u32 {
    if n == 0 {
        1
    } else {
        n.ilog10() + 1
    }
}