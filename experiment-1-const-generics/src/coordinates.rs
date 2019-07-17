use std::mem;

macro_rules! coordinates {
    ($len:expr, $name:ident, $($coord:ident),+) => {
        #[repr(C)]
        pub struct $name<T> {
            $(pub $coord: T),+
        }

        impl<T> $name<T> {
            pub fn from_array_ref(a: &[T; $len]) -> &$name<T> {
                assert_eq!(mem::size_of::<$name<T>>(), mem::size_of::<[T; $len]>());
                unsafe { &*(a as *const _ as *const $name<T>) }
            }

            pub fn from_array_ref_mut(a: &mut [T; $len]) -> &mut $name<T> {
                assert_eq!(mem::size_of::<$name<T>>(), mem::size_of::<[T; $len]>());
                unsafe { &mut*(a as *mut _ as *mut $name<T>) }
            }
        }
    };
}

coordinates!(1, X, x);
coordinates!(2, XY, x, y);
coordinates!(3, XYZ, x, y, z);
coordinates!(4, XYZW, x, y, z, w);
