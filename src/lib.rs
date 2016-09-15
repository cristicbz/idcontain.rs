extern crate rand;

mod id;
mod vec;
mod option;

pub use id::{IdTag, Id};
pub use vec::{MAXIMUM_CAPACITY, IdVec, Iter, IterMut};
pub use option::OptionId;
