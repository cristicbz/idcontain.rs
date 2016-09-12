extern crate rand;

use rand::Rng;
use std::mem;
use std::u32;
use std::slice::{Iter as SliceIter, IterMut as SliceIterMut};
use std::ops::{Index, IndexMut};
use std::marker::PhantomData;

pub type IdIndex = u32;
pub type IdTag = u32;

pub const MAXIMUM_CAPACITY: usize = u32::MAX as usize - 1;

/// The `Id` of an element of type `T` as returned by `IdVec` insertions.
///
/// `Id`-s are opaque, but `Copy`, comparable and hashable. Think of them as safe `*const T` which
/// you can dereference if you have reference to the originating `IdVec`.
///
/// Internally an `Id` is implemented as an `(index, tag)` pair. The `index` points to a slot in the
/// `IdVec`, while the `tag` allows disambiguating between values when a slot gets reused (a
/// matching tag is stored in the slot and is incremented every time a value is removed from that
/// slot).
#[derive(Debug, Hash, PartialOrd, Ord, PartialEq, Eq)]
pub struct Id<T> {
    index: IdIndex,
    tag: IdTag,
    _data: PhantomData<T>,
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Id {
            index: self.index,
            tag: self.index,
            _data: PhantomData,
        }
    }
}

impl<T> Copy for Id<T> {}

#[derive(Debug)]
enum Slot<T> {
    Free { next_free: IdIndex },
    Occupied { value: T },
}

#[derive(Debug)]
struct TaggedSlot<T> {
    tag: IdTag,
    slot: Slot<T>,
}


/// An `IdVec` stores an unordered collection of elements with fast access by opaque `Id`-s.
///
/// Inserting an element returns an `Id` which may later be used for lookup and deletion. It
/// supports O(1) insertion, deletion and id-lookup. Ordering is unstable when elements are
/// added and removed.
///
/// The maximum number of elements which can be stored in an `IdVec` is `MAXIMUM_CAPACITY`
/// (currently 2^32). This keeps `Id`-s at 64bits. A future version may support custom types for
/// the `Id`-s making the capacity-id-size trade-off customisable.
///
/// Example
/// ---
/// ```
/// use idcontain::{IdVec, Id};
///
/// let mut id_vec: IdVec<&'static str> = IdVec::new();
///
/// // The `Id` type encodes the type of the value, to statically prevent errors caused by mixing
/// // id-s.
/// let hello_id: Id<&'static str> = id_vec.insert("hello");
/// let world_id = id_vec.insert("world");
///
/// assert_eq!(id_vec[hello_id], "hello");
/// assert_eq!(id_vec[world_id], "world");
///
/// assert_eq!(id_vec.remove(world_id), Some("world"));  // The value is returned on deletion.
/// assert!(!id_vec.contains(world_id));
///
/// // New id-s are different from previous ones, even though the memory is reused.
/// let new_world_id = id_vec.insert("new world");
/// assert!(new_world_id != world_id);
/// ```
///
/// Id Reuse
/// ---
/// Removing an `Id` will cause future lookups with that `Id` to fail (either returning `None` for
/// `get` and `remove`, or panicking for indexing), but this feature is 'best-effort' and should
/// not be relied on for memory safety or security.
///
/// In particular removing and adding an element 2^32 times will cause that `Id` to be reused. This
/// is, for most workloads unlikely and is made even less likely when operations are more mixed
/// (adding more elements and removing them in between).
///
///
/// Mixing Id-s
/// ---
/// Using an `Id` from a different `IdVec` will fail at compile time, unless both `IdVec`-s are of
/// the same type. You are encouraged to newtype liberally to make leverage this as much as
/// possible.
///
/// When using `Id`-s of the same type, lookups are still most likely going to fail at runtime:
///
/// ```
/// # use idcontain::IdVec;
/// let mut id_vec_1 = IdVec::new();
/// let id1 = id_vec_1.insert(1);
///
/// let mut id_vec_2 = IdVec::new();
/// let id2 = id_vec_2.insert(1);
///
/// assert!(id1 != id2);
/// assert!(!id_vec_1.contains(id2));
/// assert!(!id_vec_2.contains(id1));
/// ```
///
/// The mechanism behind this is built on the same tagging mechanism used for preventing `Id` reuse
/// of the same `IdVec`. As such, this feature is also best-effort and should not be used for
/// memory safety or security.
///
/// For all other situations, it's probably fine, the probability curve follows the birthday
/// paradox equation with m=2^32 / avg_num_elements_per_id_vec and n=num_id_vecs. So, for instance,
/// 10 `IdVec`-s with 1000 elements each, gives a probability of about `(n^2/2m) = 0.001%` or
/// 1 in 100,000.
///
/// To reiterate, though, try to rely on the compile time checks as much as possible anyway.
pub struct IdVec<T> {
    slots: Vec<TaggedSlot<T>>,
    first_free: IdIndex,
    seed_tag: IdTag,
    len: usize,
}

impl<T> IdVec<T> {
    /// Creates a new `IdVec` with zero capacity.
    ///
    /// The first insertion will cause an allocation.
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    /// Creates a new `IdVec` with a given capacity.
    ///
    /// As long as number of elements stays under this capacity, no re-allocation will be
    /// triggered.
    pub fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_seed_tag(capacity, default_seed_tag())
    }

    /// Creates a new `IdVec` with a given capacity and seed tag.
    ///
    /// This is an advanced feature which may cause more `Id` collisions between your `IdVec`-s if
    /// not used correctly.
    ///
    /// The `seed_tag` of an `IdVec` is the tag assigned to new slots. Removing elements increments
    /// this tag and wraps around, providing the mechanism for `Id` reuse prevention and `Id`
    /// mixing detection.
    ///
    /// The `new` and `with_capacity` constructors set the seed tag to a random number, but better
    /// strategies exist for preventing collisions if you know your workload.
    pub fn with_capacity_and_seed_tag(capacity: usize, seed_tag: IdTag) -> Self {
        assert!(capacity <= MAXIMUM_CAPACITY);

        IdVec {
            slots: Vec::with_capacity(capacity),
            first_free: 0,
            seed_tag: seed_tag,
            len: 0,
        }
    }

    /// Returns the number of elements in the `IdVec`.
    ///
    /// Panics
    /// ---
    /// None.
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::IdVec;
    /// let mut id_vec = IdVec::new();
    /// assert_eq!(id_vec.len(), 0);
    ///
    /// let id = id_vec.insert(1);
    /// assert_eq!(id_vec.len(), 1);
    ///
    /// id_vec.remove(id);
    /// assert_eq!(id_vec.len(), 0);
    /// ```
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns `true` if there exists an element with the given `Id`.
    ///
    /// See struct-level docs for caveats about `Id` reuse and mixing (almost always fine, but
    /// `contains` may give you false positives in pathological cases so don't rely on it for
    /// memory safety or security).
    ///
    /// Panics
    /// ---
    /// None.
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::IdVec;
    /// let mut id_vec = IdVec::new();
    /// assert_eq!(id_vec.len(), 0);
    ///
    /// let id = id_vec.insert(1);
    /// assert!(id_vec.contains(id));
    ///
    /// id_vec.remove(id);
    /// assert!(!id_vec.contains(id));
    /// ```
    pub fn contains(&self, id: Id<T>) -> bool {
        self.slots
            .get(id.index as usize)
            .map(|tagged_slot| {
                if tagged_slot.tag == id.tag {
                    match tagged_slot.slot {
                        Slot::Occupied { .. } => true,
                        Slot::Free { .. } => false,
                    }
                } else {
                    false
                }
            })
            .unwrap_or(false)
    }

    /// Returns a reference to an element by `Id` or `None` if it doesn't exist.
    ///
    /// Use indexing for a panicking version of this operation.
    ///
    /// Panics
    /// ---
    /// None.
    ///
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::IdVec;
    /// let mut id_vec = IdVec::new();
    /// let id = id_vec.insert(1);
    ///
    /// assert_eq!(id_vec.get(id), Some(&1));
    ///
    /// id_vec.remove(id);
    /// assert_eq!(id_vec.get(id), None);
    /// ```
    pub fn get(&self, id: Id<T>) -> Option<&T> {
        self.get_or_tagged_slot(id).ok()
    }

    /// Returns a mutable reference to an element by `Id` or `None` if it doesn't exist.
    ///
    /// Use indexing for a panicking version of this operation.
    ///
    /// Panics
    /// ---
    /// None.
    ///
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::IdVec;
    /// let mut id_vec = IdVec::new();
    /// let id = id_vec.insert(1);
    ///
    /// *id_vec.get_mut(id).unwrap() = 10;
    /// assert_eq!(id_vec[id], 10);
    ///
    /// id_vec.remove(id);
    /// assert_eq!(id_vec.get_mut(id), None);
    /// ```
    pub fn get_mut(&mut self, id: Id<T>) -> Option<&mut T> {
        self.get_mut_or_tagged_slot(id).ok()
    }

    /// Inserts a new element into the `IdVec`, returning its `Id<T>`.
    ///
    /// In general the `Id`-s do not get reused and should not collide with other `IdVec`-s, but
    /// caveats apply, see the struct-level doc for more details.
    ///
    /// Panics
    /// ---
    /// If `self.len() == MAXIMUM_CAPACITY`.
    ///
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::IdVec;
    /// let mut id_vec = IdVec::new();
    /// let id1 = id_vec.insert(1);
    /// let id2 = id_vec.insert(2);
    ///
    /// assert_eq!(id_vec[id1], 1);
    /// assert_eq!(id_vec[id2], 2);
    ///
    /// // Id-s are not (caveats apply) shared between `IdVec`-s.
    /// let mut new_id_vec = IdVec::new();
    /// let new_id1 = new_id_vec.insert(1);
    /// assert!(new_id1 != id1);
    ///
    /// // Id-s are not (caveats apply) reused.
    /// id_vec.remove(id1);
    /// let id3 = id_vec.insert(3);
    /// assert!(id3 != id1);
    /// ```
    pub fn insert(&mut self, value: T) -> Id<T> {
        assert!(self.len() < MAXIMUM_CAPACITY);

        self.len += 1;
        if self.first_free < self.slots.len() as IdIndex {
            let index = self.first_free;
            let tagged_slot = &mut self.slots[self.first_free as usize];
            match mem::replace(&mut tagged_slot.slot, Slot::Occupied { value: value }) {
                Slot::Free { next_free } => self.first_free = next_free,
                Slot::Occupied { .. } => panic!("Occupied slot in free list."),
            }
            Id {
                tag: tagged_slot.tag,
                index: index,
                _data: PhantomData,
            }
        } else {
            self.slots.push(TaggedSlot {
                tag: self.seed_tag,
                slot: Slot::Occupied { value: value },
            });
            self.first_free = self.slots.len() as IdIndex;
            Id {
                index: self.first_free - 1,
                tag: self.seed_tag,
                _data: PhantomData,
            }
        }
    }

    /// Removes an element by `Id` returning its value.
    ///
    /// Returns `None` if no element with the given `Id` exists. See the struct level docs for the
    /// pathological cases where this may not be the case.
    ///
    /// Panics
    /// ---
    /// None.
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::IdVec;
    /// let mut id_vec = IdVec::new();
    /// let id1 = id_vec.insert(1);
    ///
    /// assert_eq!(id_vec[id1], 1);
    /// assert_eq!(id_vec.remove(id1), Some(1));
    ///
    /// assert!(!id_vec.contains(id1));
    /// assert_eq!(id_vec.remove(id1), None);
    /// ```
    pub fn remove(&mut self, id: Id<T>) -> Option<T> {
        let IdVec { ref mut slots, ref mut len, ref mut first_free, .. } = *self;
        slots.get_mut(id.index as usize).and_then(|tagged_slot| if tagged_slot.tag == id.tag {
            match mem::replace(&mut tagged_slot.slot, Slot::Free { next_free: *first_free }) {
                Slot::Occupied { value } => {
                    *len = len.checked_sub(1).expect("invalid len in remove()");
                    tagged_slot.tag = tagged_slot.tag.wrapping_add(1);
                    *first_free = id.index;
                    Some(value)
                }
                rollback @ Slot::Free { .. } => {
                    tagged_slot.slot = rollback;
                    None
                }
            }
        } else {
            None
        })
    }

    /// Iterates over references to the elements in the `IdVec`.
    ///
    /// While this operation has good cache locality, its worst-case complexity is
    /// `O(max_number_of_elements_ever)`. I.e.  there are pathological cases where adding and
    /// removing 1 million elements, then adding one element back will cause iteration to be `O(1
    /// million)`.
    ///
    /// The iteration order is unspecified, but it doesn't change unless the `IdVec` is mutated.
    ///
    /// Panics
    /// ---
    /// None.
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::IdVec;
    /// let mut id_vec = IdVec::new();
    /// id_vec.insert(1);
    /// id_vec.insert(2);
    /// id_vec.insert(3);
    ///
    /// for i in id_vec.iter() {
    ///     println!("{}", i);  // Prints 1, 2, 3.
    /// }
    ///
    /// // Can use `&id_vec` instead:
    /// for i in &id_vec {
    ///     println!("{}", i);  // Prints 1, 2, 3.
    /// }
    /// ```
    pub fn iter(&self) -> Iter<T> {
        Iter {
            num_left: self.len(),
            iter: self.slots.iter(),
        }
    }

    /// Iterates over mutable references to the elements in the `IdVec`.
    ///
    /// See `iter` for notes on complexity and iteration order.
    ///
    /// Panics
    /// ---
    /// None.
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::IdVec;
    /// let mut id_vec = IdVec::new();
    /// id_vec.insert(1);
    /// id_vec.insert(2);
    /// id_vec.insert(3);
    ///
    /// for value in id_vec.iter_mut() {  // Or `&mut id_vec`.
    ///     *value += 1;
    /// }
    ///
    /// for i in &id_vec {
    ///     println!("{}", i);  // Prints 2, 3, 4.
    /// }
    /// ```
    pub fn iter_mut(&mut self) -> IterMut<T> {
        IterMut {
            num_left: self.len(),
            iter: self.slots.iter_mut(),
        }
    }

    fn get_or_tagged_slot(&self, id: Id<T>) -> Result<&T, Option<&TaggedSlot<T>>> {
        match self.slots.get(id.index as usize) {
            Some(&TaggedSlot { slot: Slot::Occupied { ref value }, tag }) if tag == id.tag => {
                Ok(value)
            }
            tagged_slot @ _ => Err(tagged_slot),
        }
    }

    fn get_mut_or_tagged_slot(&mut self, id: Id<T>) -> Result<&mut T, Option<&mut TaggedSlot<T>>> {
        match self.slots.get_mut(id.index as usize) {
            Some(&mut TaggedSlot { slot: Slot::Occupied { ref mut value }, tag }) if tag ==
                                                                                     id.tag => {
                Ok(value)
            }
            tagged_slot @ _ => Err(tagged_slot),
        }
    }
}

#[cold]
#[inline(never)]
fn panic_for_bad_id<'a, T: 'a>(num_slots: usize,
                               seed_tag: IdTag,
                               len: usize,
                               tagged_slot: Option<&'a TaggedSlot<T>>,
                               id: Id<T>)
                               -> ! {
    let reason = if id.index as usize > num_slots {
        format!("index `{}` larger than number of slots `{}` (wrong `IdVec`?)",
                id.index,
                num_slots)
    } else if let Some(&TaggedSlot { tag, ref slot }) = tagged_slot {
        if tag > id.tag {
            if (tag - id.tag) < 100 {
                format!("tag `{}` larger than slot tag `{}` (deleted?)", id.tag, tag)
            } else {
                format!("tag `{}` much larger than slot tag `{}` (wrong `IdVec` or deleted?)",
                        id.tag,
                        tag)
            }
        } else if tag < id.tag {
            format!("tag `{}` smaller than slot tag `{}` (wrong `IdVec`?)",
                    id.tag,
                    tag)
        } else {
            match *slot {
                Slot::Free { .. } => {
                    format!("tag `{}` matches, but the slot is free (wrong `IdVec` with same \
                             seed_tag `{}`?)",
                            id.tag,
                            seed_tag)
                }
                Slot::Occupied { .. } => format!("<IdVec bug [occupied], please report!>"),
            }
        }
    } else {
        format!("<IdVec bug [no TaggedSlot], please report!>")
    };
    panic!("Invalid id (index=`{}`, tag=`{}`) for IdVec (num_slots=`{}`, seed_tag=`{}`, \
            len=`{}`): {}",
           id.index,
           id.tag,
           num_slots,
           seed_tag,
           len,
           reason);
}

impl<T> Index<Id<T>> for IdVec<T> {
    type Output = T;

    fn index<'a>(&'a self, id: Id<T>) -> &'a Self::Output {
        self.get_or_tagged_slot(id).unwrap_or_else(|tagged_slot| {
            panic_for_bad_id(self.slots.len(), self.seed_tag, self.len, tagged_slot, id)
        })
    }
}

impl<T> IndexMut<Id<T>> for IdVec<T> {
    fn index_mut(&mut self, id: Id<T>) -> &mut Self::Output {
        let num_slots = self.slots.len();
        let &mut IdVec { seed_tag, len, .. } = self;
        self.get_mut_or_tagged_slot(id).unwrap_or_else(|tagged_slot| {
            panic_for_bad_id(num_slots,
                             seed_tag,
                             len,
                             tagged_slot.map(|tagged_slot| &*tagged_slot),
                             id)
        })
    }
}

#[derive(Clone, Debug)]
pub struct Iter<'a, T: 'a> {
    num_left: usize,
    iter: SliceIter<'a, TaggedSlot<T>>,
}

impl<'a, T: 'a> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        while self.num_left > 0 {
            let tagged_slot = self.iter.next().expect("Too few elements in Iter");
            if let TaggedSlot { slot: Slot::Occupied { ref value }, .. } = *tagged_slot {
                self.num_left -= 1;
                return Some(value);
            }
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.num_left, Some(self.num_left))
    }
}

impl<'a, T: 'a> DoubleEndedIterator for Iter<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        while self.num_left > 0 {
            let tagged_slot = self.iter.next_back().expect("Too few elements in Iter");
            if let TaggedSlot { slot: Slot::Occupied { ref value }, .. } = *tagged_slot {
                self.num_left -= 1;
                return Some(value);
            }
        }
        None
    }
}

impl<'a, T: 'a> ExactSizeIterator for Iter<'a, T> {
    fn len(&self) -> usize {
        self.num_left
    }
}

fn default_seed_tag() -> IdTag {
    rand::thread_rng().gen()
}

#[derive(Debug)]
pub struct IterMut<'a, T: 'a> {
    iter: SliceIterMut<'a, TaggedSlot<T>>,
    num_left: usize,
}

impl<'a, T: 'a> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        while self.num_left > 0 {
            let tagged_slot = self.iter.next().expect("Too few elements in IterMut");
            if let TaggedSlot { slot: Slot::Occupied { ref mut value }, .. } = *tagged_slot {
                self.num_left -= 1;
                return Some(value);
            }
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.num_left, Some(self.num_left))
    }
}

impl<'a, T: 'a> DoubleEndedIterator for IterMut<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        while self.num_left > 0 {
            let tagged_slot = self.iter.next_back().expect("Too few elements in IterMut");
            if let TaggedSlot { slot: Slot::Occupied { ref mut value }, .. } = *tagged_slot {
                self.num_left -= 1;
                return Some(value);
            }
        }
        None
    }
}

impl<'a, T: 'a> ExactSizeIterator for IterMut<'a, T> {
    fn len(&self) -> usize {
        self.num_left
    }
}

impl<'a, T: 'a> IntoIterator for &'a IdVec<T> {
    type IntoIter = Iter<'a, T>;
    type Item = <Self::IntoIter as Iterator>::Item;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T: 'a> IntoIterator for &'a mut IdVec<T> {
    type IntoIter = IterMut<'a, T>;
    type Item = <Self::IntoIter as Iterator>::Item;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}


#[cfg(test)]
mod tests {
    use super::IdVec;
    use std::marker::PhantomData;

    #[test]
    fn len_iter_contains_on_empty() {
        let id_vec = IdVec::<i32>::new();
        assert_eq!(id_vec.len(), 0);
        assert_eq!(id_vec.iter().next(), None);
        assert!(!id_vec.contains(super::Id {
            index: 0,
            tag: 0,
            _data: PhantomData,
        }));
    }

    #[test]
    fn iter_mut_on_empty() {
        let mut id_vec = IdVec::<String>::new();
        assert_eq!(id_vec.iter_mut().next(), None);
    }

    #[test]
    fn id_vec_insert_then_get_and_index() {
        let mut id_vec = IdVec::new();

        let id_a = id_vec.insert(1);
        let id_b = id_vec.insert(2);
        let id_c = id_vec.insert(3);

        // Id-s all differ from each other.
        assert!(id_a != id_b);
        assert!(id_b != id_c);
        assert!(id_c != id_a);

        // `get` returns the correct values.
        assert_eq!(id_vec.get(id_a).map(|&x| x), Some(1));
        assert_eq!(id_vec.get(id_b).map(|&x| x), Some(2));
        assert_eq!(id_vec.get(id_c).map(|&x| x), Some(3));

        // `get_mut` returns the correct values.
        assert_eq!(id_vec.get_mut(id_a).map(|&mut x| x), Some(1));
        assert_eq!(id_vec.get_mut(id_b).map(|&mut x| x), Some(2));
        assert_eq!(id_vec.get_mut(id_c).map(|&mut x| x), Some(3));

        // `index` returns the correct values.
        assert_eq!(*(&id_vec[id_a]), 1);
        assert_eq!(*(&id_vec[id_b]), 2);
        assert_eq!(*(&id_vec[id_c]), 3);

        // `index` returns the correct values.
        assert_eq!(*(&mut id_vec[id_a]), 1);
        assert_eq!(*(&mut id_vec[id_b]), 2);
        assert_eq!(*(&mut id_vec[id_c]), 3);

        // Mutating through id_b then `get`-ing again works.
        id_vec[id_b] = 10;
        assert_eq!(id_vec[id_b], 10);
    }

    #[test]
    #[should_panic]
    fn id_vec_insert_then_remove_index_panics() {
        let mut id_vec = IdVec::new();
        let id = id_vec.insert(1);
        id_vec.remove(id);
        id_vec[id];
    }

    #[test]
    #[should_panic]
    fn id_vec_insert_then_remove_index_mut_panics() {
        let mut id_vec = IdVec::new();
        let id = id_vec.insert(1);
        id_vec.remove(id);
        id_vec[id] = 10;
    }

    #[test]
    fn id_vec_insert_then_remove_get() {
        let mut id_vec = IdVec::with_capacity(3);

        let id_a = id_vec.insert(1);
        let id_b = id_vec.insert(2);
        let id_c = id_vec.insert(3);

        assert_eq!(id_vec.remove(id_b), Some(2));
        assert_eq!(id_vec.get(id_b), None);
        assert!(!id_vec.contains(id_b));

        assert_eq!(id_vec[id_a], 1);
        assert_eq!(id_vec[id_c], 3);

        let id_d = id_vec.insert(5);

        assert_eq!(id_vec.remove(id_a), Some(1));
        assert_eq!(id_vec.remove(id_a), None);
        assert_eq!(id_vec.remove(id_c), Some(3));
        assert_eq!(id_vec.get(id_a), None);
        assert_eq!(id_vec.get(id_c), None);
        assert!(!id_vec.contains(id_a));
        assert!(!id_vec.contains(id_b));
        assert!(!id_vec.contains(id_c));
        assert!(id_vec.contains(id_d));

        let id_e = id_vec.insert(6);
        let id_f = id_vec.insert(7);

        assert!(id_d.tag == id_b.tag.wrapping_add(1));
        assert!(id_d.index == id_b.index);

        assert!(id_e.tag == id_c.tag.wrapping_add(1));
        assert!(id_e.index == id_c.index);

        assert!(id_f.tag == id_a.tag.wrapping_add(1));
        assert!(id_f.index == id_a.index);
    }

    #[test]
    fn id_vec_insert_then_remove_iter() {
        let mut id_vec = IdVec::with_capacity(3);

        let id_a = id_vec.insert(1);
        let id_b = id_vec.insert(2);
        id_vec.insert(3);
        id_vec.remove(id_b);
        id_vec.insert(4);
        let id_e = id_vec.insert(5);
        id_vec.remove(id_e);
        id_vec.remove(id_a);

        assert_eq!(&id_vec.iter().cloned().collect::<Vec<_>>()[..], &[4, 3]);
        assert_eq!(&id_vec.iter_mut().map(|&mut x| x).collect::<Vec<_>>()[..],
                   &[4, 3]);

        assert_eq!(&(&id_vec).into_iter().cloned().collect::<Vec<_>>()[..],
                   &[4, 3]);
        assert_eq!(&(&mut id_vec).into_iter().map(|&mut x| x).collect::<Vec<_>>()[..],
                   &[4, 3]);
    }

    #[test]
    fn id_vec_ids_from_different_id_vecs() {
        let mut id_vec_1 = IdVec::with_capacity(3);
        let mut id_vec_2 = IdVec::with_capacity(4);

        let id_a_1 = id_vec_1.insert(1);
        let id_b_1 = id_vec_1.insert(2);
        let id_c_1 = id_vec_1.insert(3);

        let id_a_2 = id_vec_2.insert(1);
        let id_b_2 = id_vec_2.insert(2);
        let id_c_2 = id_vec_2.insert(3);
        let id_d_2 = id_vec_2.insert(4);

        assert_eq!(id_vec_1.get(id_a_2), None);
        assert_eq!(id_vec_1.get(id_b_2), None);
        assert_eq!(id_vec_1.get(id_c_2), None);
        assert_eq!(id_vec_1.get(id_d_2), None);
        assert_eq!(id_vec_1.get_mut(id_a_2), None);
        assert_eq!(id_vec_1.get_mut(id_b_2), None);
        assert_eq!(id_vec_1.get_mut(id_c_2), None);
        assert_eq!(id_vec_1.get_mut(id_d_2), None);
        assert!(!id_vec_1.contains(id_a_2));
        assert!(!id_vec_1.contains(id_b_2));
        assert!(!id_vec_1.contains(id_c_2));
        assert!(!id_vec_1.contains(id_d_2));
        assert_eq!(id_vec_1.remove(id_a_2), None);
        assert_eq!(id_vec_1.remove(id_b_2), None);
        assert_eq!(id_vec_1.remove(id_c_2), None);
        assert_eq!(id_vec_1.remove(id_d_2), None);
        assert_eq!(&id_vec_1.iter().cloned().collect::<Vec<_>>(), &[1, 2, 3]);

        assert_eq!(id_vec_2.get(id_a_1), None);
        assert_eq!(id_vec_2.get(id_b_1), None);
        assert_eq!(id_vec_2.get(id_c_1), None);
        assert_eq!(id_vec_2.get_mut(id_a_1), None);
        assert_eq!(id_vec_2.get_mut(id_b_1), None);
        assert_eq!(id_vec_2.get_mut(id_c_1), None);
        assert!(!id_vec_2.contains(id_a_1));
        assert!(!id_vec_2.contains(id_b_1));
        assert!(!id_vec_2.contains(id_c_1));
        assert_eq!(id_vec_2.remove(id_a_1), None);
        assert_eq!(id_vec_2.remove(id_b_1), None);
        assert_eq!(id_vec_2.remove(id_c_1), None);
        assert_eq!(&id_vec_2.iter().cloned().collect::<Vec<_>>(), &[1, 2, 3, 4]);
    }
}
