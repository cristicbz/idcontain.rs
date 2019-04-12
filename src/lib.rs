extern crate rand;

mod flat;
mod id;
mod map;
mod option;
mod slab;
mod vec;

pub use flat::{Flat, FlatAccess, FlatAccessMut, FlatGet, FlatGetMut};
pub use id::{Id, IdTag, MAXIMUM_CAPACITY};
pub use map::{IdMap, IdMapVec};
pub use option::OptionId;
pub use slab::{IdSlab, Iter, IterMut};
pub use vec::IdVec;
