use super::map::IdMap;
use super::slab::IdSlab;
use super::id::{Id, IdIndex, MAXIMUM_CAPACITY};
use flat::{FlatAccess, FlatAccessMut, Flat, FlatGet, FlatGetMut};

pub struct IdVec<F: Flat> {
    ids: IdSlab<u32>,
    reverse: Vec<IdIndex>,
    flat: F,
}

impl<F: Flat> IdVec<F> {
    pub fn new() -> Self {
        IdVec {
            ids: IdSlab::new(),
            reverse: Vec::new(),
            flat: F::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        assert!(capacity <= MAXIMUM_CAPACITY);
        IdVec {
            ids: IdSlab::with_capacity(capacity),
            reverse: Vec::with_capacity(capacity),
            flat: F::with_capacity(capacity),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.ids.len()
    }

    #[inline]
    pub fn contains(&self, id: Id<F::Element>) -> bool {
        self.ids.contains(id.cast())
    }

    #[inline]
    pub fn get<'a>(&'a self, id: Id<F::Element>) -> Option<<&'a F as FlatGet>::ElementRef>
        where &'a F: FlatGet
    {
        self.ids.get(id.cast()).and_then(|&index| self.flat.flat_get(index as usize))
    }

    #[inline]
    pub fn get_mut<'a>(&'a mut self,
                       id: Id<F::Element>)
                       -> Option<<&'a mut F as FlatGetMut>::ElementRefMut>
        where &'a mut F: FlatGetMut
    {
        // Not using `.and_then` to work around borrowck.
        match self.ids.get(id.cast()) {
            Some(&index) => self.flat.flat_get_mut(index as usize),
            None => None,
        }
    }

    #[inline]
    pub fn insert(&mut self, element: F::Element) -> Id<F::Element> {
        assert!(self.ids.len() <= MAXIMUM_CAPACITY);
        let id = self.ids.insert(self.reverse.len() as u32);
        self.reverse.push(id.index);
        self.flat.push(element);
        id.cast()
    }

    #[inline]
    pub fn remove(&mut self, id: Id<F::Element>) -> Option<F::Element> {
        let IdVec { ref mut flat, ref mut ids, ref mut reverse } = *self;
        ids.remove(id.cast()).and_then(|index| {
            *ids.by_index_mut(reverse.swap_remove(index as usize))
                .expect("reverse out of sync with ids in `remove`") = index;
            flat.swap_remove(index as usize)
        })
    }

    #[inline]
    pub fn id_to_index(&self, id: Id<F::Element>) -> Option<usize> {
        self.ids.get(id.cast()).map(|&index| index as usize)
    }

    #[inline]
    pub fn index_to_id(&self, index: usize) -> Option<Id<F::Element>> {
        match self.reverse.get(index) {
            Some(&index) => {
                Some(self.ids
                    .index_to_id(index)
                    .expect("reverse out of sync with ids in `index_to_id`")
                    .cast())
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
