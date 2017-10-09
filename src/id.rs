use std::u32;
use std::marker::PhantomData;
use std::fmt::{Result as FmtResult, Formatter, Debug};
use std::hash::{Hash, Hasher};
use std::cmp::Ordering;

/// The maximum size of id containers.
pub const MAXIMUM_CAPACITY: usize = u32::MAX as usize - 1;

/// The type used internally to store a tag.
///
/// This type is almost internal and you should only care about it if calling
/// `with_capacity_and_seed_tag`.
pub type IdTag = u32;

/// The type used internally to store a index.
///
/// This type is almost internal and you should only care about it if building data structures on
/// top of the existing ones.
pub type IdIndex = u32;

/// The `Id` of an element of type `T` as returned by `IdSlab` insertions.
///
/// `Id`-s are opaque, but `Copy`, comparable and hashable. Think of an `Id` as a safe `*const T`
/// which you can dereference if you have a reference to the originating `IdSlab`.
///
/// Implementation
/// ---
/// Internally an `Id` is implemented as an `(index, tag)` pair. The `index` points to a slot in
/// the `IdSlab`, while the `tag` allows disambiguating between values when a slot gets reused (a
/// matching tag is stored in the slot and is incremented every time a value is removed from that
/// slot).
pub struct Id<T> {
    #[doc(hidden)]
    pub index: IdIndex,

    #[doc(hidden)]
    pub tag: IdTag,

    #[doc(hidden)]
    pub _data: PhantomData<T>,
}

impl<T> Id<T> {
    /// Returns an invalid `Id` (which will never be returned by a container).
    ///
    /// Panics
    /// ---
    /// None.
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::{IdSlab, Id};
    /// let id_slab: IdSlab<&'static str> = IdSlab::new();
    /// assert!(!id_slab.contains(Id::invalid()));
    /// ```
    pub fn invalid() -> Self {
        Id {
            index: u32::MAX,
            tag: u32::MAX,
            _data: PhantomData,
        }
    }

    /// Casts the `Id` to an `Id` of a different type.
    ///
    /// This is handy when using `IdSlab` to generate proxy `Id`-s for an `IdMap`.
    ///
    /// Panics
    /// ---
    /// None.
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::{IdSlab, Id};
    /// let mut id_slab: IdSlab<()> = IdSlab::new();
    /// let id: Id<u32> = id_slab.insert(()).cast();
    /// ```
    pub fn cast<U>(self) -> Id<U> {
        Id {
            index: self.index,
            tag: self.tag,
            _data: PhantomData,
        }
    }
}

impl<T> Debug for Id<T> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        write!(formatter, "{}.{:x}", self.index, self.tag)
    }
}

impl<T> Hash for Id<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.index, self.tag).hash(state)
    }
}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.tag == other.tag && self.index == other.index
    }
}

impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.tag, self.index).cmp(&(other.tag, other.index))
    }
}

#[cfg_attr(feature = "cargo-clippy", allow(expl_impl_clone_on_copy))]
impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Id {
            index: self.index,
            tag: self.tag,
            _data: PhantomData,
        }
    }
}

impl<T> Copy for Id<T> {}
impl<T> Eq for Id<T> {}

impl<T> Default for Id<T> {
    fn default() -> Self {
        Self::invalid()
    }
}
