use fumarole::*;

pub trait MenuItemClone {
    fn clone_box(&self) -> Box<dyn MenuItem>;
}

pub trait MenuItem: Send + MenuItemClone {
    fn draw(&self, frame: &mut Frame, parent: Transform);
    
    fn draw_selected(&self, frame: &mut Frame, parent: Transform);
}

impl<T> MenuItemClone for T 
    where T: MenuItem + Clone + 'static
{
    fn clone_box(&self) -> Box<dyn MenuItem> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn MenuItem> {
    fn clone(&self) -> Self {
        self.clone_box() 
    }
}

#[derive(Clone)]
pub struct Menu {
    pub transform: Transform,
    pub selected_item: Option<usize>,
    pub items: Vec<Box<dyn MenuItem>>,
}

impl Menu {
    pub fn new(items: Vec<Box<dyn MenuItem>>, transform: Transform) -> Self {
        Menu {
            transform,
            selected_item: None,
            items,
        }
    }
    
    pub fn draw(&self, frame: &mut Frame, parent: Transform) {
        frame.rect()
            .transform(self.transform)
            .parent(parent)
            .color(color::rgb(0.7, 0.3, 0.1))
            .depth(5.0)
            .draw();

        for (index, item) in self.items.iter().enumerate() {
            if Some(index) == self.selected_item {
                item.draw_selected(frame, self.transform);
            } else {
                item.draw(frame, self.transform);
            }
        }
    }
}

impl MenuItem for Menu {
    fn draw(&self, frame: &mut Frame, parent: Transform) {
        self.draw(frame, parent);
    }

    fn draw_selected(&self, frame: &mut Frame, parent: Transform) {
        self.draw(frame, parent);
    }
}

#[derive(Clone)]
pub struct Button {
    pub transform: Transform,
    pub text: String,
    pub menu: Menu,
}

impl MenuItem for Button {
    fn draw(&self, frame: &mut Frame, parent: Transform) {
        frame.rect()
            .parent(parent)
            .transform(self.transform)
            .depth(6.0)
            .draw();
    }

    fn draw_selected(&self, frame: &mut Frame, parent: Transform) {
        self.draw(frame, parent);
    }
}

pub struct List {
    pub items: Vec<String>
}
