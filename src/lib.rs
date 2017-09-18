extern crate rand;

mod flat;
mod id;
mod map;
mod vec;
mod option;
mod slab;

pub use flat::{FlatAccess, FlatAccessMut, Flat, FlatGet, FlatGetMut};
pub use id::{IdTag, Id, MAXIMUM_CAPACITY};
pub use map::{IdMap, IdMapVec};
pub use vec::IdVec;
pub use option::OptionId;
pub use slab::{IdSlab, Iter, IterMut};
