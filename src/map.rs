use id::{Id, IdIndex, MAXIMUM_CAPACITY};
use flat::{FlatAccess, FlatAccessMut, Flat, FlatGet, FlatGetMut};

pub type IdMapVec<E, T> = IdMap<E, Vec<T>>;

#[derive(Debug)]
pub struct IdMap<E, F: Flat> {
    lookup: Vec<Id<E>>,
    reverse_lookup: Vec<IdIndex>,
    flat: F,
}

impl<E, F: Flat> IdMap<E, F> {
    pub fn new() -> Self {
        IdMap {
            lookup: Vec::new(),
            reverse_lookup: Vec::new(),
            flat: F::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        assert!(capacity <= MAXIMUM_CAPACITY);
        IdMap {
            lookup: Vec::with_capacity(capacity),
            reverse_lookup: Vec::with_capacity(capacity),
            flat: F::with_capacity(capacity),
        }
    }

    #[inline]
    pub fn insert(&mut self, id: Id<E>, mut element: F::Element) -> Option<F::Element> {
        let IdMap { ref mut lookup, ref mut reverse_lookup, ref mut flat } = *self;
        let new_index = {
            let len = reverse_lookup.len();
            assert!(len <= MAXIMUM_CAPACITY);
            len as IdIndex
        };

        let usize_index = id.index as usize;
        let lookup_id = if usize_index < lookup.len() {
            let lookup_id = &mut lookup[usize_index];
            element = match flat.replace(lookup_id.index as usize, element) {
                Ok(value) => {
                    return if id.tag == lookup_id.tag {
                        Some(value)
                    } else {
                        lookup_id.tag = id.tag;
                        None
                    }
                }
                Err(element) => element,
            };
            lookup_id
        } else {
            lookup.resize(usize_index + 1, Id::invalid());
            &mut lookup[usize_index]
        };
        *lookup_id = Id { index: new_index, ..id };
        reverse_lookup.push(id.index);
        flat.push(element);
        None
    }

    #[inline]
    pub fn remove(&mut self, id: Id<E>) -> Option<F::Element> {
        let IdMap { ref mut lookup, ref mut reverse_lookup, ref mut flat } = *self;

        let usize_index = id.index as usize;
        let lookup_index = match lookup.get_mut(usize_index) {
            Some(lookup_id) => {
                if lookup_id.tag != id.tag {
                    return None;
                }
                let index = lookup_id.index;
                *lookup_id = Id::invalid();
                index
            }
            None => return None,
        };
        let usize_lookup_index = lookup_index as usize;
        let old_value = match flat.swap_remove(usize_lookup_index) {
            Some(old_value) => old_value,
            None => return None,
        };
        reverse_lookup.swap_remove(usize_lookup_index);
        reverse_lookup.get(usize_lookup_index)
            .map(|&reverse_index| lookup[reverse_index as usize].index = lookup_index);
        Some(old_value)
    }

    #[inline]
    pub fn get<'a>(&'a self, id: Id<E>) -> Option<<&'a F as FlatGet>::ElementRef>
        where &'a F: FlatGet
    {
        match self.lookup.get(id.index as usize) {
            Some(lookup_id) if lookup_id.tag == id.tag => {
                self.flat.flat_get(lookup_id.index as usize)
            }
            _ => None,
        }
    }

    #[inline]
    pub fn get_mut<'a>(&'a mut self, id: Id<E>) -> Option<<&'a mut F as FlatGetMut>::ElementRefMut>
        where &'a mut F: FlatGetMut
    {
        match self.lookup.get(id.index as usize) {
            Some(lookup_id) if lookup_id.tag == id.tag => {
                self.flat.flat_get_mut(lookup_id.index as usize)
            }
            _ => None,
        }
    }

    #[inline]
    pub fn id_to_index(&self, id: Id<E>) -> Option<usize> {
        match self.lookup.get(id.index as usize) {
            Some(lookup_id) if lookup_id.tag == id.tag => Some(lookup_id.index as usize),
            _ => None,
        }
    }

    #[inline]
    pub fn index_to_id(&self, index: usize) -> Option<Id<E>> {
        match self.reverse_lookup.get(index) {
            Some(&lookup_index) => {
                Some(Id { index: lookup_index, ..self.lookup[lookup_index as usize] })
            }
            _ => None,
        }
    }

    #[inline]
    pub fn access<'a>(&'a self) -> <&'a F as FlatAccess>::Access
        where &'a F: FlatAccess
    {
        self.flat.flat_access()
    }

    #[inline]
    pub fn access_mut<'a>(&'a mut self) -> <&'a mut F as FlatAccessMut>::AccessMut
        where &'a mut F: FlatAccessMut
    {
        self.flat.flat_access_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use slab::IdSlab;

    fn setup() -> (IdSlab<()>, IdMapVec<(), u32>) {
        (IdSlab::new(), IdMapVec::new())
    }

    #[test]
    fn insert_twice_returns_first_and_respects_tag() {
        let (mut slab, mut map) = setup();
        let id1 = slab.insert(());
        assert_eq!(map.insert(id1, 1), None);
        assert_eq!(map.insert(id1, 2), Some(1));
        slab.remove(id1);
        let id2 = slab.insert(());
        assert_eq!(map.insert(id2, 3), None);
        assert_eq!(map.insert(id2, 4), Some(3));
        assert_eq!(map.insert(id1, 4), None);
    }

    #[test]
    fn insert_get_returns_correct() {
        let (mut slab, mut map) = setup();
        let id1 = slab.insert(());
        let id2 = slab.insert(());
        assert_eq!(map.get(id1), None);
        assert_eq!(map.get(id2), None);

        assert_eq!(map.insert(id1, 1), None);
        assert_eq!(map.insert(id2, 2), None);

        assert_eq!(map.get(id1), Some(&1));
        assert_eq!(map.get(id2), Some(&2));

        assert_eq!(map.get_mut(id1).map(|x| *x), Some(1));
        assert_eq!(map.get_mut(id2).map(|x| *x), Some(2));

        slab.remove(id1);
        slab.remove(id2);
        let id3 = slab.insert(());
        let id4 = slab.insert(());
        assert_eq!(map.get(id3), None);
        assert_eq!(map.get(id4), None);

        assert_eq!(map.insert(id3, 3), None);
        assert_eq!(map.insert(id4, 4), None);

        assert_eq!(map.get(id1), None);
        assert_eq!(map.get(id2), None);

        assert_eq!(map.get(id3), Some(&3));
        assert_eq!(map.get(id4), Some(&4));
        assert_eq!(map.get_mut(id3).map(|x| *x), Some(3));
        assert_eq!(map.get_mut(id4).map(|x| *x), Some(4));
    }

    #[test]
    fn modify_through_get_mut() {
        let (mut slab, mut map) = setup();
        let id1 = slab.insert(());
        assert_eq!(map.get(id1), None);
        assert_eq!(map.insert(id1, 1), None);
        *map.get_mut(id1).unwrap() = 10;
        assert_eq!(map.get(id1), Some(&10));
        assert_eq!(map.insert(id1, 20), Some(10));
    }

    #[test]
    fn insert_remove_get() {
        let (mut slab, mut map) = setup();
        let id1 = slab.insert(());
        let id2 = slab.insert(());

        assert_eq!(map.insert(id1, 1), None);
        assert_eq!(map.remove(id1), Some(1));
        assert_eq!(map.remove(id1), None);
        assert_eq!(map.get(id1), None);

        assert_eq!(map.insert(id2, 20), None);
        assert_eq!(map.insert(id1, 10), None);
        assert_eq!(map.remove(id1), Some(10));
        assert_eq!(map.get(id2), Some(&20));
        assert_eq!(map.remove(id2), Some(20));
        assert_eq!(map.get(id2), None);
    }
}
