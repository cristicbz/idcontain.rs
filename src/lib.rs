extern crate rand;

mod id;
mod slab;
mod option;

pub use id::{IdTag, Id};
pub use slab::{MAXIMUM_CAPACITY, IdSlab, Iter, IterMut};
pub use option::OptionId;
