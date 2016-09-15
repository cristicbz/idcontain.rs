use std::mem;
use std::option::IntoIter as OptionIntoIter;
use super::Id;

/// An `Option`-like container of `Id<T>` that takes up no additional space.
///
/// Internally, `None` is represented as `Id::invalid()`. An `OptionId<T>` implements conversion to
/// and from an `Option<Id<T>>`, but it also forwards all the methods that `Option<T>` does, for
/// convenience.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct OptionId<T>(Id<T>);

impl<T> Into<Option<Id<T>>> for OptionId<T> {
    fn into(self) -> Option<Id<T>> {
        self.into_option()
    }
}

impl<T> From<Option<Id<T>>> for OptionId<T> {
    fn from(option: Option<Id<T>>) -> Self {
        match option {
            Some(id) => OptionId(id),
            None => OptionId(Id::invalid()),
        }
    }
}

impl<T> OptionId<T> {
    #[inline]
    pub fn into_option(self) -> Option<Id<T>> {
        if self.is_some() { Some(self.0) } else { None }
    }

    #[inline]
    pub fn is_none(&self) -> bool {
        self.0 == Id::invalid()
    }

    #[inline]
    pub fn is_some(&self) -> bool {
        !self.is_none()
    }

    #[inline]
    pub fn as_ref(&self) -> Option<&Id<T>> {
        if self.is_some() { Some(&self.0) } else { None }
    }

    #[inline]
    pub fn as_mut(&mut self) -> Option<&mut Id<T>> {
        if self.is_some() {
            Some(&mut self.0)
        } else {
            None
        }
    }

    #[inline]
    pub fn expect(self, msg: &str) -> Id<T> {
        self.into_option().expect(msg)
    }

    #[inline]
    pub fn unwrap(self) -> Id<T> {
        self.into_option().unwrap()
    }

    #[inline]
    pub fn unwrap_or(self, def: Id<T>) -> Id<T> {
        self.into_option().unwrap_or(def)
    }

    #[inline]
    pub fn unwrap_or_else<F>(self, f: F) -> Id<T>
        where F: FnOnce() -> Id<T>
    {
        self.into_option().unwrap_or_else(f)
    }


    #[inline]
    pub fn map<U, F>(self, f: F) -> Option<U>
        where F: FnOnce(Id<T>) -> U
    {
        self.into_option().map(f)
    }


    #[inline]
    pub fn map_or<U, F>(self, default: U, f: F) -> U
        where F: FnOnce(Id<T>) -> U
    {
        self.into_option().map_or(default, f)
    }


    #[inline]
    pub fn map_or_else<U, D, F>(self, default: D, f: F) -> U
        where D: FnOnce() -> U,
              F: FnOnce(Id<T>) -> U
    {
        self.into_option().map_or_else(default, f)
    }


    #[inline]
    pub fn ok_or<E>(self, err: E) -> Result<Id<T>, E> {
        self.into_option().ok_or(err)
    }

    #[inline]
    pub fn ok_or_else<E, F>(self, err: F) -> Result<Id<T>, E>
        where F: FnOnce() -> E
    {
        self.into_option().ok_or_else(err)
    }


    #[inline]
    pub fn iter(&self) -> OptionIntoIter<&Id<T>> {
        self.as_ref().into_iter()
    }

    #[inline]
    pub fn iter_mut(&mut self) -> OptionIntoIter<&mut Id<T>> {
        self.as_mut().into_iter()
    }

    #[inline]
    pub fn and<U, O>(self, optb: O) -> Option<U>
        where O: Into<Option<U>>
    {
        self.into_option().and(optb.into())
    }

    #[inline]
    pub fn and_then<U, O, F>(self, f: F) -> Option<U>
        where F: FnOnce(Id<T>) -> O,
              O: Into<Option<U>>
    {
        self.into_option().and_then(|id| f(id).into())
    }

    #[inline]
    pub fn or<O>(self, optb: O) -> Option<Id<T>>
        where O: Into<Option<Id<T>>>
    {
        self.into_option().or(optb.into())
    }

    #[inline]
    pub fn or_else<F, O>(self, f: F) -> Option<Id<T>>
        where F: FnOnce() -> O,
              O: Into<Option<Id<T>>>
    {
        self.into_option().or_else(|| f().into())
    }

    #[inline]
    pub fn take(&mut self) -> Self {
        OptionId(mem::replace(&mut self.0, Id::invalid()))
    }
}
