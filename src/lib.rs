extern crate rand;

mod flat;
mod id;
mod map;
mod option;
mod slab;

pub use flat::{FlatAccess, FlatAccessMut, Flat, FlatGet, FlatGetMut};
pub use id::{IdTag, Id, MAXIMUM_CAPACITY};
pub use map::IdMap;
pub use option::OptionId;
pub use slab::{IdSlab, Iter, IterMut};
