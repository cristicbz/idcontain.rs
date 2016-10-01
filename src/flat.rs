use std::mem;

pub trait Flat: Sized {
    type Element;

    fn new() -> Self;
    fn with_capacity(capacity: usize) -> Self;

    fn swap_remove(&mut self, index: usize) -> Option<Self::Element>;
    fn replace(&mut self,
               index: usize,
               element: Self::Element)
               -> Result<Self::Element, Self::Element>;

    fn push(&mut self, element: Self::Element);
}


pub trait FlatGet {
    type ElementRef;
    fn flat_get(self, index: usize) -> Option<Self::ElementRef>;
}

pub trait FlatGetMut {
    type ElementRefMut;
    fn flat_get_mut(self, index: usize) -> Option<Self::ElementRefMut>;
}

pub trait FlatAccess {
    type Access;
    fn flat_access(self) -> Self::Access;
}

pub trait FlatAccessMut {
    type AccessMut;
    fn flat_access_mut(self) -> Self::AccessMut;
}

impl<T> Flat for Vec<T> {
    type Element = T;

    #[inline]
    fn new() -> Self {
        Vec::new()
    }

    #[inline]
    fn with_capacity(capacity: usize) -> Self {
        Vec::with_capacity(capacity)
    }

    #[inline]
    fn replace(&mut self,
               index: usize,
               element: Self::Element)
               -> Result<Self::Element, Self::Element> {
        match self.get_mut(index) {
            Some(old) => Ok(mem::replace(old, element)),
            None => Err(element),
        }
    }

    #[inline]
    fn swap_remove(&mut self, index: usize) -> Option<Self::Element> {
        let index = index;
        if index < self.len() {
            Some(self.swap_remove(index))
        } else {
            None
        }
    }

    #[inline]
    fn push(&mut self, element: Self::Element) {
        self.push(element);
    }
}


impl<'a, T> FlatGet for &'a Vec<T> {
    type ElementRef = &'a T;

    fn flat_get(self, index: usize) -> Option<Self::ElementRef> {
        self.get(index)
    }
}

impl<'a, T> FlatGetMut for &'a mut Vec<T> {
    type ElementRefMut = &'a mut T;

    fn flat_get_mut(self, index: usize) -> Option<Self::ElementRefMut> {
        self.get_mut(index)
    }
}

impl<'a, T> FlatAccess for &'a Vec<T> {
    type Access = &'a [T];

    fn flat_access(self) -> Self::Access {
        &self[..]
    }
}

impl<'a, T> FlatAccessMut for &'a mut Vec<T> {
    type AccessMut = &'a mut [T];

    fn flat_access_mut(self) -> Self::AccessMut {
        &mut self[..]
    }
}

#[macro_export]
macro_rules! derive_flat {
    (
        #[element($element:ident, & $element_ref:ident, &mut $element_ref_mut:ident)]
        #[access(& $access:ident, &mut $access_mut:ident)]
        pub struct $struct_name:ident {
            $(#[element($element_field:ident)]
              pub $field:ident : $field_type:ty,)+
        }
    ) => {
        pub struct $struct_name {
            $(pub $field : $field_type,)+
        }

        #[allow(dead_code)]
        #[allow(unused_variables)]
        pub struct $element {
            $(pub $element_field : <$field_type as $crate::Flat>::Element,)+
        }

        #[allow(dead_code)]
        #[allow(unused_variables)]
        pub struct $element_ref <'a> {
            $(pub $element_field : <&'a $field_type as $crate::FlatGet>::ElementRef,)+
        }

        #[allow(dead_code)]
        #[allow(unused_variables)]
        pub struct $element_ref_mut <'a> {
            $(pub $element_field : <&'a mut $field_type as $crate::FlatGetMut>::ElementRefMut,)+
        }

        #[allow(dead_code)]
        #[allow(unused_variables)]
        pub struct $access <'a> {
            $(pub $field : <&'a $field_type as $crate::FlatAccess>::Access,)+
        }

        #[allow(dead_code)]
        #[allow(unused_variables)]
        pub struct $access_mut <'a> {
            $(pub $field : <&'a mut $field_type as $crate::FlatAccessMut>::AccessMut,)+
        }

        impl $crate::Flat for $struct_name {
            type Element = $element;

            #[inline]
            fn new() -> Self {
                $struct_name {
                    $($field : $crate::Flat::new()),+
                }
            }

            #[inline]
            fn with_capacity(capacity: usize) -> Self {
                $struct_name {
                    $($field : $crate::Flat::with_capacity(capacity)),+
                }
            }

            #[inline]
            fn replace(&mut self,
                       index: usize,
                       element: Self::Element)
                       -> Result<Self::Element, Self::Element> {
                match ($($crate::Flat::replace(&mut self.$field,
                                               index,
                                               element.$element_field),)+) {
                    ($(Ok($element_field),)+) => Ok($element {
                        $($element_field: $element_field,)+
                    }),
                    ($(Err($element_field),)+) => Err($element {
                        $($element_field: $element_field,)+
                    }),
                    _ => panic!("out of lockstep derived flat in `replace`"),
                }
            }

            #[inline]
            #[allow(dead_code)]
            #[allow(unused_variables)]
            fn swap_remove(&mut self, index: usize) -> Option<Self::Element> {
                match ($($crate::Flat::swap_remove(&mut self.$field, index),)+) {
                    ($(Some($element_field),)+) => Some($element {
                        $($element_field: $element_field,)+
                    }),
                    ($($element_field@None,)+) => None,
                    _ => panic!("out of lockstep derived flat in `swap_remove`"),
                }
            }

            #[inline]
            fn push(&mut self, element: Self::Element) {
                $($crate::Flat::push(&mut self.$field, element.$element_field);)+
            }
        }


        impl<'a> $crate::FlatGet for &'a $struct_name {
            type ElementRef = $element_ref <'a>;

            #[allow(dead_code)]
            #[allow(unused_variables)]
            fn flat_get(self, index: usize) -> Option<Self::ElementRef> {
                match ($($crate::FlatGet::flat_get(&self.$field, index),)+) {
                    ($(Some($element_field),)+) => Some($element_ref {
                        $($element_field: $element_field,)+
                    }),
                    ($($element_field@None,)+) => None,
                    _ => panic!("out of lockstep derived flat in `flat_get`"),
                }
            }
        }

        impl<'a> $crate::FlatGetMut for &'a mut $struct_name {
            type ElementRefMut = $element_ref_mut <'a>;

            #[allow(dead_code)]
            #[allow(unused_variables)]
            fn flat_get_mut(self, index: usize) -> Option<Self::ElementRefMut> {
                match ($($crate::FlatGetMut::flat_get_mut(&mut self.$field, index),)+) {
                    ($(Some($element_field),)+) => Some($element_ref_mut {
                        $($element_field: $element_field,)+
                    }),
                    ($($element_field@None,)+) => None,
                    _ => panic!("out of lockstep derived flat in `flat_get_mut`"),
                }
            }
        }

        impl<'a> $crate::FlatAccess for &'a $struct_name {
            type Access = $access <'a>;

            fn flat_access(self) -> Self::Access {
                $access {
                    $($field: $crate::FlatAccess::flat_access(&self.$field),)+
                }
            }
        }

        impl<'a> $crate::FlatAccessMut for &'a mut $struct_name {
            type AccessMut = $access_mut <'a>;

            fn flat_access_mut(self) -> Self::AccessMut {
                $access_mut {
                    $($field: $crate::FlatAccessMut::flat_access_mut(&mut self.$field),)+
                }
            }
        }
    }
}
