//! Module select widget.

use std::sync::{Arc, RwLock};

use crossterm::event::{Event, KeyCode, KeyEventKind};
use ratatui::{buffer::Buffer, layout::Rect, style::{Style, Stylize}, widgets::{List, ListState, StatefulWidget}};

use crate::{code::{AppCommand, ChasmWidget, Editor, WidgetContext, editor::ModuleOrFile}, project::{ChasmProject, ModulePath}};

#[derive(Debug)]
pub (super) struct ModuleSelectViewArgs {
    pub project: Arc<RwLock<ChasmProject>>,
    pub select_list_state: ListState,
    pub module_paths: Vec<ModulePath>,
}

#[derive(Debug)]
pub (super) struct ModuleSelectView {
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
    fn handle_event(&mut self, event: &Event, ctx: &mut WidgetContext) {
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
                        let mod_or_file = ModuleOrFile::Module(module_path);
                        let editor = Editor::new(mod_or_file);
                        ctx.command_queue_tx.push(AppCommand::PushView(Box::new(editor)));
                    }

                }
                _other => {
                    // No-op for now
                }
            }
        }
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let mut state = self.select_list_state;
        StatefulWidget::render(&self.list, area, buf, &mut state);
    }
}