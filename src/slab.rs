use rand::{self, Rng};
use std::mem;
use std::u32;
use std::slice::{Iter as SliceIter, IterMut as SliceIterMut};
use std::ops::{Index, IndexMut};
use std::marker::PhantomData;
use super::id::{Id, IdIndex, IdTag, MAXIMUM_CAPACITY};

/// An `IdSlab` stores an unordered collection of elements with fast access by opaque `Id`-s.
///
/// Inserting an element returns an `Id` which may later be used for lookup and deletion. It
/// supports O(1) insertion, deletion and id-lookup. Ordering is unstable when elements are
/// added and removed.
///
/// The maximum number of elements which can be stored in an `IdSlab` is `MAXIMUM_CAPACITY`
/// (currently 2^32). This keeps `Id`-s at 64bits. A future version may support custom types for
/// the `Id`-s making the capacity-id-size trade-off customisable.
///
/// Example
/// ---
/// ```
/// use idcontain::{IdSlab, Id};
///
/// let mut id_slab: IdSlab<&'static str> = IdSlab::new();
///
/// // The `Id` type encodes the type of the value, to statically prevent errors caused by mixing
/// // id-s.
/// let hello_id: Id<&'static str> = id_slab.insert("hello");
/// let world_id = id_slab.insert("world");
///
/// assert_eq!(id_slab[hello_id], "hello");
/// assert_eq!(id_slab[world_id], "world");
///
/// assert_eq!(id_slab.remove(world_id), Some("world"));  // The value is returned on deletion.
/// assert!(!id_slab.contains(world_id));
///
/// // New id-s are different from previous ones, even though the memory is reused.
/// let new_world_id = id_slab.insert("new world");
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
/// Id Mixing
/// ---
/// Using an `Id` from a different `IdSlab` will fail at compile time, unless both `IdSlab`-s are of
/// the same type. You are encouraged to newtype liberally to make leverage this as much as
/// possible.
///
/// When using `Id`-s of the same type, lookups are still most likely going to fail at runtime:
///
/// ```
/// # use idcontain::IdSlab;
/// let mut id_slab_1 = IdSlab::new();
/// let id1 = id_slab_1.insert(1);
///
/// let mut id_slab_2 = IdSlab::new();
/// let id2 = id_slab_2.insert(1);
///
/// assert!(id1 != id2);
/// assert!(!id_slab_1.contains(id2));
/// assert!(!id_slab_2.contains(id1));
/// ```
///
/// The mechanism behind this is built on the same tagging mechanism used for preventing `Id` reuse
/// of the same `IdSlab`. As such, this feature is also best-effort and should not be used for
/// memory safety or security.
///
/// For all other situations, it's probably fine. The probability curve follows the birthday
/// paradox equation with `m=2^32 / avg_num_elements_per_id_slab` and `n=num_id_slabs`. So, for
/// instance, 10 `IdSlab`-s with 1000 elements each, gives a collision probability of about
/// `(n^2/2m) = 0.001%` or 1 in 100,000.
#[derive(Clone, Debug)]
pub struct IdSlab<T> {
    slots: Vec<TaggedSlot<T>>,
    first_free: IdIndex,
    seed_tag: IdTag,
    len: usize,
}

impl<T> IdSlab<T> {
    /// Creates a new `IdSlab` with zero capacity.
    ///
    /// The first insertion will cause an allocation.
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    /// Creates a new `IdSlab` with a given capacity.
    ///
    /// As long as number of elements stays under this capacity, no re-allocation will be
    /// triggered.
    pub fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_seed_tag(capacity, default_seed_tag())
    }

    /// Creates a new `IdSlab` with a given capacity and seed tag.
    ///
    /// This is an advanced feature which may cause more `Id` collisions between your `IdSlab`-s if
    /// used incorrectly.
    ///
    /// The `seed_tag` of an `IdSlab` is the tag assigned to new slots. Removing elements increments
    /// this tag and wraps around, providing the mechanism for `Id` reuse prevention and `Id`
    /// mixing detection.
    ///
    /// The `new` and `with_capacity` constructors set the seed tag to a random number, but better
    /// strategies exist for preventing collisions if you know your workload.
    pub fn with_capacity_and_seed_tag(capacity: usize, seed_tag: IdTag) -> Self {
        assert!(capacity <= MAXIMUM_CAPACITY);

        IdSlab {
            slots: Vec::with_capacity(capacity),
            first_free: 0,
            seed_tag: seed_tag,
            len: 0,
        }
    }

    /// Returns the number of elements in the `IdSlab`.
    ///
    /// Panics
    /// ---
    /// None.
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::IdSlab;
    /// let mut id_slab = IdSlab::new();
    /// assert_eq!(id_slab.len(), 0);
    ///
    /// let id = id_slab.insert(1);
    /// assert_eq!(id_slab.len(), 1);
    ///
    /// id_slab.remove(id);
    /// assert_eq!(id_slab.len(), 0);
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
    /// # use idcontain::IdSlab;
    /// let mut id_slab = IdSlab::new();
    /// assert_eq!(id_slab.len(), 0);
    ///
    /// let id = id_slab.insert(1);
    /// assert!(id_slab.contains(id));
    ///
    /// id_slab.remove(id);
    /// assert!(!id_slab.contains(id));
    /// ```
    pub fn contains(&self, id: Id<T>) -> bool {
        match self.slots.get(id.index as usize) {
            Some(&TaggedSlot { slot: Slot::Occupied { .. }, tag }) if tag == id.tag => true,
            _ => false,
        }
    }

    /// Returns a reference to an element by `Id` or `None` if it doesn't exist.
    ///
    /// Use indexing for a panicking version of this operation.
    ///
    /// Panics
    /// ---
    /// None.
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::IdSlab;
    /// let mut id_slab = IdSlab::new();
    /// let id = id_slab.insert(1);
    ///
    /// assert_eq!(id_slab.get(id), Some(&1));
    ///
    /// id_slab.remove(id);
    /// assert_eq!(id_slab.get(id), None);
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
    /// Example
    /// ---
    /// ```
    /// # use idcontain::IdSlab;
    /// let mut id_slab = IdSlab::new();
    /// let id = id_slab.insert(1);
    ///
    /// *id_slab.get_mut(id).unwrap() = 10;
    /// assert_eq!(id_slab[id], 10);
    ///
    /// id_slab.remove(id);
    /// assert_eq!(id_slab.get_mut(id), None);
    /// ```
    pub fn get_mut(&mut self, id: Id<T>) -> Option<&mut T> {
        self.get_mut_or_tagged_slot(id).ok()
    }

    /// Inserts a new element into the `IdSlab`, returning its `Id<T>`.
    ///
    /// In general the `Id`-s do not get reused and should not collide with other `IdSlab`-s, but
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
    /// # use idcontain::IdSlab;
    /// let mut id_slab = IdSlab::new();
    /// let id1 = id_slab.insert(1);
    /// let id2 = id_slab.insert(2);
    ///
    /// assert_eq!(id_slab[id1], 1);
    /// assert_eq!(id_slab[id2], 2);
    ///
    /// // Id-s are not (caveats apply) shared between `IdSlab`-s.
    /// let mut new_id_slab = IdSlab::new();
    /// let new_id1 = new_id_slab.insert(1);
    /// assert!(new_id1 != id1);
    ///
    /// // Id-s are not (caveats apply) reused.
    /// id_slab.remove(id1);
    /// let id3 = id_slab.insert(3);
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
    /// # use idcontain::IdSlab;
    /// let mut id_slab = IdSlab::new();
    /// let id1 = id_slab.insert(1);
    ///
    /// assert_eq!(id_slab[id1], 1);
    /// assert_eq!(id_slab.remove(id1), Some(1));
    ///
    /// assert!(!id_slab.contains(id1));
    /// assert_eq!(id_slab.remove(id1), None);
    /// ```
    pub fn remove(&mut self, id: Id<T>) -> Option<T> {
        let IdSlab { ref mut slots, ref mut len, ref mut first_free, .. } = *self;
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

    /// Iterates over references to the elements in the `IdSlab`.
    ///
    /// While this operation has good cache locality, its worst-case complexity is
    /// `O(max_number_of_elements_ever)`. I.e.  there are pathological cases where adding and
    /// removing 1 million elements, then adding one element back will cause iteration to be `O(1
    /// million)`.
    ///
    /// The iteration order is unspecified, but it doesn't change unless the `IdSlab` is mutated.
    ///
    /// Panics
    /// ---
    /// None.
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::IdSlab;
    /// let mut id_slab = IdSlab::new();
    /// id_slab.insert(1);
    /// id_slab.insert(2);
    /// id_slab.insert(3);
    ///
    /// for i in id_slab.iter() {
    ///     println!("{}", i);  // Prints 1, 2, 3.
    /// }
    ///
    /// // Can use `&id_slab` instead:
    /// for i in &id_slab {
    ///     println!("{}", i);  // Prints 1, 2, 3.
    /// }
    /// ```
    pub fn iter<'a>(&'a self) -> Iter<'a, T> {
        Iter {
            num_left: self.len(),
            iter: self.slots.iter(),
        }
    }

    /// Iterates over mutable references to the elements in the `IdSlab`.
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
    /// # use idcontain::IdSlab;
    /// let mut id_slab = IdSlab::new();
    /// id_slab.insert(1);
    /// id_slab.insert(2);
    /// id_slab.insert(3);
    ///
    /// for value in id_slab.iter_mut() {  // Or `&mut id_slab`.
    ///     *value += 1;
    /// }
    ///
    /// for i in &id_slab {
    ///     println!("{}", i);  // Prints 2, 3, 4.
    /// }
    /// ```
    pub fn iter_mut<'a>(&'a mut self) -> IterMut<'a, T> {
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

    /// Returns a reference to an element by index or `None` if it doesn't exist.
    ///
    /// This is a low-level operation that bypasses the tag check. Useful for building other
    /// containers on top.
    ///
    /// Panics
    /// ---
    /// None.
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::IdSlab;
    /// let mut id_slab = IdSlab::new();
    /// let id = id_slab.insert(1);
    ///
    /// assert_eq!(id_slab.by_index(0), Some(&1));
    /// ```
    pub fn by_index(&self, index: IdIndex) -> Option<&T> {
        match self.slots.get(index as usize) {
            Some(&TaggedSlot { slot: Slot::Occupied { ref value }, .. }) => Some(value),
            _ => None,
        }
    }

    /// Returns a mutable reference to an element by index or `None` if it doesn't exist.
    ///
    /// This is a low-level operation that bypasses the tag check. Useful for building other
    /// containers on top.
    ///
    /// Panics
    /// ---
    /// None.
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::IdSlab;
    /// let mut id_slab = IdSlab::new();
    /// let id = id_slab.insert(1);
    ///
    /// *id_slab.by_index_mut(0).unwrap() = 10;
    /// assert_eq!(id_slab[id], 10);
    /// ```
    pub fn by_index_mut(&mut self, index: IdIndex) -> Option<&mut T> {
        match self.slots.get_mut(index as usize) {
            Some(&mut TaggedSlot { slot: Slot::Occupied { ref mut value }, .. }) => Some(value),
            _ => None,
        }
    }

    /// Returns the `Id` for a given occupied index.
    ///
    /// Returns `None` if the index is invalid.
    ///
    /// This is a low-level operation that bypasses the tag check. Useful for building other
    /// containers on top.
    ///
    /// Panics
    /// ---
    /// None.
    ///
    /// Example
    /// ---
    /// ```
    /// # use idcontain::IdSlab;
    /// let mut id_slab = IdSlab::new();
    /// let id = id_slab.insert(1);
    ///
    /// assert_eq!(id_slab.index_to_id(0), Some(id));
    /// ```
    pub fn index_to_id(&self, index: IdIndex) -> Option<Id<T>> {
        match self.slots.get(index as usize) {
            Some(&TaggedSlot { slot: Slot::Occupied { .. }, tag }) => {
                Some(Id {
                    index: index,
                    tag: tag,
                    _data: PhantomData,
                })
            }
            _ => None,
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
        format!("index `{}` larger than number of slots `{}` (wrong `IdSlab`?)",
                id.index,
                num_slots)
    } else if let Some(&TaggedSlot { tag, ref slot }) = tagged_slot {
        if tag > id.tag {
            if (tag - id.tag) < 100 {
                format!("tag `{}` older than slot tag `{}`, deleted?", id.tag, tag)
            } else {
                format!("tag `{}` much older than slot tag `{}`, wrong `IdSlab` or deleted?",
                        id.tag,
                        tag)
            }
        } else if tag < id.tag {
            format!("tag `{}` newer than slot tag `{}`, wrong `IdSlab`?",
                    id.tag,
                    tag)
        } else {
            match *slot {
                Slot::Free { .. } => {
                    format!("tag `{}` matches, but the slot is free, wrong `IdSlab` with same \
                             seed_tag `{}`?",
                            id.tag,
                            seed_tag)
                }
                Slot::Occupied { .. } => format!("<IdSlab bug [occupied], please report!>"),
            }
        }
    } else {
        format!("<IdSlab bug [no TaggedSlot], please report!>")
    };
    panic!("Invalid id: {} (id={{ index=`{}`, tag=`{}` }}, id_slab={{ num_slots=`{}`, \
            seed_tag=`{}`, len=`{}` }})",
           reason,
           id.index,
           id.tag,
           num_slots,
           seed_tag,
           len)
}

impl<T> Index<Id<T>> for IdSlab<T> {
    type Output = T;

    fn index<'a>(&'a self, id: Id<T>) -> &'a Self::Output {
        self.get_or_tagged_slot(id).unwrap_or_else(|tagged_slot| {
            panic_for_bad_id(self.slots.len(), self.seed_tag, self.len, tagged_slot, id)
        })
    }
}

impl<T> IndexMut<Id<T>> for IdSlab<T> {
    fn index_mut(&mut self, id: Id<T>) -> &mut Self::Output {
        let num_slots = self.slots.len();
        let &mut IdSlab { seed_tag, len, .. } = self;
        self.get_mut_or_tagged_slot(id).unwrap_or_else(|tagged_slot| {
            panic_for_bad_id(num_slots,
                             seed_tag,
                             len,
                             tagged_slot.map(|tagged_slot| &*tagged_slot),
                             id)
        })
    }
}

/// The type returned by `IdSlab.iter()`.
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

/// The type returned by `IdSlab.iter_mut()`.
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

impl<'a, T: 'a> IntoIterator for &'a IdSlab<T> {
    type IntoIter = Iter<'a, T>;
    type Item = <Self::IntoIter as Iterator>::Item;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T: 'a> IntoIterator for &'a mut IdSlab<T> {
    type IntoIter = IterMut<'a, T>;
    type Item = <Self::IntoIter as Iterator>::Item;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}


#[derive(Debug, Clone)]
enum Slot<T> {
    Free { next_free: IdIndex },
    Occupied { value: T },
}

#[derive(Debug, Clone)]
struct TaggedSlot<T> {
    tag: IdTag,
    slot: Slot<T>,
}



#[cfg(test)]
mod tests {
    use super::IdSlab;
    use super::super::id::Id;
    use std::marker::PhantomData;

    #[test]
    fn len_iter_contains_on_empty() {
        let id_slab = IdSlab::<i32>::new();
        assert_eq!(id_slab.len(), 0);
        assert_eq!(id_slab.iter().next(), None);
        assert!(!id_slab.contains(Id {
            index: 0,
            tag: 0,
            _data: PhantomData,
        }));
    }

    #[test]
    fn iter_mut_on_empty() {
        let mut id_slab = IdSlab::<String>::new();
        assert_eq!(id_slab.iter_mut().next(), None);
    }

    #[test]
    fn id_slab_insert_then_get_and_index() {
        let mut id_slab = IdSlab::new();

        let id_a = id_slab.insert(1);
        let id_b = id_slab.insert(2);
        let id_c = id_slab.insert(3);

        // Id-s all differ from each other.
        assert!(id_a != id_b);
        assert!(id_b != id_c);
        assert!(id_c != id_a);

        // `get` returns the correct values.
        assert_eq!(id_slab.get(id_a).map(|&x| x), Some(1));
        assert_eq!(id_slab.get(id_b).map(|&x| x), Some(2));
        assert_eq!(id_slab.get(id_c).map(|&x| x), Some(3));

        // `get_mut` returns the correct values.
        assert_eq!(id_slab.get_mut(id_a).map(|&mut x| x), Some(1));
        assert_eq!(id_slab.get_mut(id_b).map(|&mut x| x), Some(2));
        assert_eq!(id_slab.get_mut(id_c).map(|&mut x| x), Some(3));

        // `index` returns the correct values.
        assert_eq!(*(&id_slab[id_a]), 1);
        assert_eq!(*(&id_slab[id_b]), 2);
        assert_eq!(*(&id_slab[id_c]), 3);

        // `index` returns the correct values.
        assert_eq!(*(&mut id_slab[id_a]), 1);
        assert_eq!(*(&mut id_slab[id_b]), 2);
        assert_eq!(*(&mut id_slab[id_c]), 3);

        // Mutating through id_b then `get`-ing again works.
        id_slab[id_b] = 10;
        assert_eq!(id_slab[id_b], 10);
    }

    #[test]
    #[should_panic]
    fn id_slab_insert_then_remove_index_panics() {
        let mut id_slab = IdSlab::new();
        let id = id_slab.insert(1);
        id_slab.remove(id);
        id_slab[id];
    }

    #[test]
    #[should_panic]
    fn id_slab_insert_then_remove_index_mut_panics() {
        let mut id_slab = IdSlab::new();
        let id = id_slab.insert(1);
        id_slab.remove(id);
        id_slab[id] = 10;
    }

    #[test]
    fn id_slab_insert_then_remove_get() {
        let mut id_slab = IdSlab::with_capacity(3);

        let id_a = id_slab.insert(1);
        let id_b = id_slab.insert(2);
        let id_c = id_slab.insert(3);

        assert_eq!(id_slab.remove(id_b), Some(2));
        assert_eq!(id_slab.get(id_b), None);
        assert!(!id_slab.contains(id_b));

        assert_eq!(id_slab[id_a], 1);
        assert_eq!(id_slab[id_c], 3);

        let id_d = id_slab.insert(5);

        assert_eq!(id_slab.remove(id_a), Some(1));
        assert_eq!(id_slab.remove(id_a), None);
        assert_eq!(id_slab.remove(id_c), Some(3));
        assert_eq!(id_slab.get(id_a), None);
        assert_eq!(id_slab.get(id_c), None);
        assert!(!id_slab.contains(id_a));
        assert!(!id_slab.contains(id_b));
        assert!(!id_slab.contains(id_c));
        assert!(id_slab.contains(id_d));

        let id_e = id_slab.insert(6);
        let id_f = id_slab.insert(7);

        assert!(id_d.tag == id_b.tag.wrapping_add(1));
        assert!(id_d.index == id_b.index);

        assert!(id_e.tag == id_c.tag.wrapping_add(1));
        assert!(id_e.index == id_c.index);

        assert!(id_f.tag == id_a.tag.wrapping_add(1));
        assert!(id_f.index == id_a.index);
    }

    #[test]
    fn id_slab_insert_then_remove_iter() {
        let mut id_slab = IdSlab::with_capacity(3);

        let id_a = id_slab.insert(1);
        let id_b = id_slab.insert(2);
        id_slab.insert(3);
        id_slab.remove(id_b);
        id_slab.insert(4);
        let id_e = id_slab.insert(5);
        id_slab.remove(id_e);
        id_slab.remove(id_a);

        assert_eq!(&id_slab.iter().cloned().collect::<Vec<_>>()[..], &[4, 3]);
        assert_eq!(&id_slab.iter_mut().map(|&mut x| x).collect::<Vec<_>>()[..],
                   &[4, 3]);

        assert_eq!(&(&id_slab).into_iter().cloned().collect::<Vec<_>>()[..],
                   &[4, 3]);
        assert_eq!(&(&mut id_slab).into_iter().map(|&mut x| x).collect::<Vec<_>>()[..],
                   &[4, 3]);
    }

    #[test]
    fn id_slab_ids_from_different_id_slabs() {
        let mut id_slab_1 = IdSlab::with_capacity(3);
        let mut id_slab_2 = IdSlab::with_capacity(4);

        let id_a_1 = id_slab_1.insert(1);
        let id_b_1 = id_slab_1.insert(2);
        let id_c_1 = id_slab_1.insert(3);

        let id_a_2 = id_slab_2.insert(1);
        let id_b_2 = id_slab_2.insert(2);
        let id_c_2 = id_slab_2.insert(3);
        let id_d_2 = id_slab_2.insert(4);

        assert_eq!(id_slab_1.get(id_a_2), None);
        assert_eq!(id_slab_1.get(id_b_2), None);
        assert_eq!(id_slab_1.get(id_c_2), None);
        assert_eq!(id_slab_1.get(id_d_2), None);
        assert_eq!(id_slab_1.get_mut(id_a_2), None);
        assert_eq!(id_slab_1.get_mut(id_b_2), None);
        assert_eq!(id_slab_1.get_mut(id_c_2), None);
        assert_eq!(id_slab_1.get_mut(id_d_2), None);
        assert!(!id_slab_1.contains(id_a_2));
        assert!(!id_slab_1.contains(id_b_2));
        assert!(!id_slab_1.contains(id_c_2));
        assert!(!id_slab_1.contains(id_d_2));
        assert_eq!(id_slab_1.remove(id_a_2), None);
        assert_eq!(id_slab_1.remove(id_b_2), None);
        assert_eq!(id_slab_1.remove(id_c_2), None);
        assert_eq!(id_slab_1.remove(id_d_2), None);
        assert_eq!(&id_slab_1.iter().cloned().collect::<Vec<_>>(), &[1, 2, 3]);

        assert_eq!(id_slab_2.get(id_a_1), None);
        assert_eq!(id_slab_2.get(id_b_1), None);
        assert_eq!(id_slab_2.get(id_c_1), None);
        assert_eq!(id_slab_2.get_mut(id_a_1), None);
        assert_eq!(id_slab_2.get_mut(id_b_1), None);
        assert_eq!(id_slab_2.get_mut(id_c_1), None);
        assert!(!id_slab_2.contains(id_a_1));
        assert!(!id_slab_2.contains(id_b_1));
        assert!(!id_slab_2.contains(id_c_1));
        assert_eq!(id_slab_2.remove(id_a_1), None);
        assert_eq!(id_slab_2.remove(id_b_1), None);
        assert_eq!(id_slab_2.remove(id_c_1), None);
        assert_eq!(&id_slab_2.iter().cloned().collect::<Vec<_>>(),
                   &[1, 2, 3, 4]);
    }
}
