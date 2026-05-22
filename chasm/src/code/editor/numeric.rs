//! Numeric input functionality.

pub (super) struct NumericBuffer {
    /// The current numeric input.
    input: String,

    /// Whether we're active or not
    is_active: bool,
}

impl NumericBuffer {
    pub fn new() -> Self {
        Self {
            input: String::new(),
            is_active: false,
        }
    }

    pub fn is_active(&self) -> bool {
        self.is_active
    }

    pub fn activate(&mut self) {
        self.is_active = true;
    }

    pub fn deactivate(&mut self) {
        self.is_active = false;
        self.input.clear();
        self.input.shrink_to(10);
    }

    pub fn push(&mut self, c: char) {
        self.input.push(c);
    }
    
    #[allow(unused)]
    pub fn pop(&mut self) -> Option<char> {
        self.input.pop()
    }

    /// Returns the current numeric input as a usize.
    /// 
    /// Panics if numeric input is not active.
    pub fn current_input(&self) -> usize {
        assert!(self.is_active, "Numeric input is not active");
        self.input.parse().expect("Failed to parse numeric input")
    }
}
