#![feature(const_generics)]
//! This crate represents a rough sketch of how a 3D math library could be structured with
//! const generics in mind. The intent of this work isn't to design something that's remotely
//! functional at the time of writing (spoiler: it very much isn't), but to look at what the
//! state-of-the-art could look like in a few years, see what sort of designs that opens
//! up, and figure out how to pull some of those advances back into the present day.
//!
//! Notably, these are the two big advantages this approach brings:
//! 1. Maximum compaction of the documentation. *Every possible vector operation is expressed in a
//!    single place, and any possible effect on the vector's dimensionality is clearly and concisely
//!    documented*. You'll also notice that there are no `new` functions, which ties into the second
//!    point...
//! 2. Arbitrary construction and deconstruction of vectors through swizzling and the `vector`
//!    macro. This (in theory) lets you handle vectors in a way that matches the expressiveness of
//!    shader languages.
//!
//! I think the second point is the most interesting, since we can potentially pull the advantages
//! back into an API design that works today. I'll let it speak for itself, since I think the
//! ergonomic benefits are clear:
//!
//! ```rust
//! // type annotations are unnecessary, but shown for clarity.
//! let a: Vector<f32, 3> = vector![0.0, 1.0, 2.0];
//! let b: Vector<f32, 4> = vector![3.0, a.yz(), 4.0];
//! let c: Vector<f32, 2> = b.xw();
//! let d: Vector<f32, 6> = vector![b, c];
//! ```
//!
//! I'm not going to opine on whether or not having a single API for creating vectors of every
//! dimension is better than separate macros for each, as I see no reason we couldn't expose both.
//! We can also attain similar expressiveness, but that's an exercise left for future experiments.
//!
//! -----
//!
//! The main problem is that this design doesn't even work on nightly, and I don't expect it to work
//! for at least a year (if I'm being generous with the pace of Rust feature stabilization). That's
//! for a few reasons:
//! 1. `const_generics` is currently buggy, and several useful designs straight up crash the
//!    compiler.
//! 2. `const_generics` intentionally does not account for expression equality when type-checking.
//!    This means that `Vector<T, 2>` and `Vector<T, {1 + 1}>` are *not* treated as the same type.
//!    That alone makes this design infeasible, since breaking `Vector::{extend/truncate/compact}`
//!    makes it impossible to implement the `vector` creation macro and leads to several other QOL
//!    issues.
//!
//! So, it's time to pack up our bags and move on to the next experiment.

use std::ops::{Deref, DerefMut};

pub mod coordinates;
#[doc(hidden)]
pub mod macros;

/// An arbitrary-dimension vector.
///
/// We don't implement any of the operations on it since this is an API sketch, but you can easily
/// imagine implementing them.
#[repr(transparent)]
pub struct Vector<T, const D: usize> {
    pub elements: [T; D],
}

#[repr(transparent)]
pub struct Matrix<T, const C: usize, const R: usize> {
    pub elements: [Vector<T, {C}>; R]
}

macro_rules! impl_vector {
    ($base:ty, $d:expr, $to:ty, new($($coord:ident),+)) => {
        impl<T> Deref for $base {
            type Target = $to;

            fn deref(&self) -> &Self::Target {
                Self::Target::from_array_ref(&self.elements)
            }
        }

        impl<T> DerefMut for $base {
            fn deref_mut(&mut self) -> &mut Self::Target {
                Self::Target::from_array_ref_mut(&mut self.elements)
            }
        }

        impl<T> From<[T; $d]> for $base {
            fn from(elements: [T; $d]) -> $base {
                Self {
                    elements,
                }
            }
        }
    };
}

impl<T> From<T> for Vector<T, 1> {
    fn from(t: T) -> Vector<T, 1> {
        Self {
            elements: [t]
        }
    }
}

impl_vector!(Vector<T, 1>, 1, coordinates::X<T>, new(x));
impl_vector!(Vector<T, 2>, 2, coordinates::XY<T>, new(x, y));
impl_vector!(Vector<T, 3>, 3, coordinates::XYZ<T>, new(x, y, z));
impl_vector!(Vector<T, 4>, 4, coordinates::XYZW<T>, new(x, y, z, w));

impl<T, const D: usize> Vector<T, {D}> {
    pub fn concat<const E: usize>(self, other: Vector<T, {E}>) -> Vector<T, {D + E}> {
        #[repr(C)]
        struct Concat<A, B>(A, B);
        let concat = Concat(self, other);
        let out = unsafe{ std::mem::transmute_copy(&concat) };
        std::mem::forget(concat);
        out
    }

    pub fn extend(self, _t: T) -> Vector<T, {D + 1}> {
        unimplemented!()
    }

    pub fn truncate(self) -> Vector<T, {D - 1}> {
        unimplemented!()
    }

    /// Vector dot (or inner) product.
    pub fn dot(self, _other: Self) -> T {unimplemented!()}

    /// Returns the squared magnitude.
    ///
    /// This does not perform an expensive square root operation like in
    /// `InnerSpace::magnitude` method, and so can be used to compare magnitudes
    /// more efficiently.
    #[inline]
    pub fn magnitude2(self) -> T {unimplemented!()}

    /// The distance from the tail to the tip of the vector.
    #[inline]
    pub fn magnitude(self) -> T {unimplemented!()}

    /// Returns a vector with the same direction, but with a magnitude of `1`.
    #[inline]
    pub fn normalize(self) -> Self {unimplemented!()}

    /// Returns a vector with the same direction and a given magnitude.
    #[inline]
    pub fn normalize_to(self, _magnitude: T) -> Self {unimplemented!()}

    /// Returns the
    /// [vector projection](https://en.wikipedia.org/wiki/Vector_projection)
    /// of the current inner space projected onto the supplied argument.
    #[inline]
    pub fn project_on(self, _other: Self) -> Self {unimplemented!()}
}

impl<T, const C: usize, const R: usize> Matrix<T, {C}, {R}> {
    pub fn concat_row(self, other: Vector<T, {C}>) -> Matrix<T, {C}, {R + 1}> {
        #[repr(C)]
        struct Concat<A, B>(A, B);
        let concat = Concat(self, other);
        let out = unsafe{ std::mem::transmute_copy(&concat) };
        std::mem::forget(concat);
        out
    }
}

macro_rules! impl_swizzle {
    ($t:ty; $($swizzle:ident $($coord:ident)+ -> $out:ty,)+) => {
        /// Swizzle functions.
        impl<T: Clone> $t {
            $(
                pub fn $swizzle(&self) -> $out {
                    <$out>::from([$(self.$coord.clone()),+])
                }
            )+
        }
    };
}

/// Typedef of `Vector` used to pull the swizzling functions out of `Vector`'s main documentation
/// page.
pub type VectorSwizzle<T, const D: usize> = Vector<T, {D}>;

impl_swizzle!{
    VectorSwizzle<T, 1>;
    x x -> Vector<T, 1>,
    xx x x -> Vector<T, 2>,
    xxx x x x -> Vector<T, 3>,
    xxxx x x x x -> Vector<T, 4>,
}
impl_swizzle!{
    VectorSwizzle<T, 2>;
    x x -> Vector<T, 1>, y y -> Vector<T, 1>,
    xx x x -> Vector<T, 2>, xy x y -> Vector<T, 2>, yx y x -> Vector<T, 2>, yy y y -> Vector<T, 2>,
    xxx x x x -> Vector<T, 3>, xxy x x y -> Vector<T, 3>, xyx x y x -> Vector<T, 3>, xyy x y y -> Vector<T, 3>, yxx y x x -> Vector<T, 3>, yxy y x y -> Vector<T, 3>, yyx y y x -> Vector<T, 3>, yyy y y y -> Vector<T, 3>,
    xxxx x x x x -> Vector<T, 4>, xxxy x x x y -> Vector<T, 4>, xxyx x x y x -> Vector<T, 4>, xxyy x x y y -> Vector<T, 4>, xyxx x y x x -> Vector<T, 4>, xyxy x y x y -> Vector<T, 4>, xyyx x y y x -> Vector<T, 4>, xyyy x y y y -> Vector<T, 4>, yxxx y x x x -> Vector<T, 4>, yxxy y x x y -> Vector<T, 4>, yxyx y x y x -> Vector<T, 4>, yxyy y x y y -> Vector<T, 4>, yyxx y y x x -> Vector<T, 4>, yyxy y y x y -> Vector<T, 4>, yyyx y y y x -> Vector<T, 4>, yyyy y y y y -> Vector<T, 4>,
}
impl_swizzle!{
    VectorSwizzle<T, 3>;
    x x -> Vector<T, 1>, y y -> Vector<T, 1>, z z -> Vector<T, 1>,
    xx x x -> Vector<T, 2>, xy x y -> Vector<T, 2>, xz x z -> Vector<T, 2>, yx y x -> Vector<T, 2>, yy y y -> Vector<T, 2>, yz y z -> Vector<T, 2>, zx z x -> Vector<T, 2>, zy z y -> Vector<T, 2>, zz z z -> Vector<T, 2>,
    xxx x x x -> Vector<T, 3>, xxy x x y -> Vector<T, 3>, xxz x x z -> Vector<T, 3>, xyx x y x -> Vector<T, 3>, xyy x y y -> Vector<T, 3>, xyz x y z -> Vector<T, 3>, xzx x z x -> Vector<T, 3>, xzy x z y -> Vector<T, 3>, xzz x z z -> Vector<T, 3>, yxx y x x -> Vector<T, 3>, yxy y x y -> Vector<T, 3>, yxz y x z -> Vector<T, 3>, yyx y y x -> Vector<T, 3>, yyy y y y -> Vector<T, 3>, yyz y y z -> Vector<T, 3>, yzx y z x -> Vector<T, 3>, yzy y z y -> Vector<T, 3>, yzz y z z -> Vector<T, 3>, zxx z x x -> Vector<T, 3>, zxy z x y -> Vector<T, 3>, zxz z x z -> Vector<T, 3>, zyx z y x -> Vector<T, 3>, zyy z y y -> Vector<T, 3>, zyz z y z -> Vector<T, 3>, zzx z z x -> Vector<T, 3>, zzy z z y -> Vector<T, 3>, zzz z z z -> Vector<T, 3>,
    xxxx x x x x -> Vector<T, 4>, xxxy x x x y -> Vector<T, 4>, xxxz x x x z -> Vector<T, 4>, xxyx x x y x -> Vector<T, 4>, xxyy x x y y -> Vector<T, 4>, xxyz x x y z -> Vector<T, 4>, xxzx x x z x -> Vector<T, 4>, xxzy x x z y -> Vector<T, 4>, xxzz x x z z -> Vector<T, 4>, xyxx x y x x -> Vector<T, 4>, xyxy x y x y -> Vector<T, 4>, xyxz x y x z -> Vector<T, 4>, xyyx x y y x -> Vector<T, 4>, xyyy x y y y -> Vector<T, 4>, xyyz x y y z -> Vector<T, 4>, xyzx x y z x -> Vector<T, 4>, xyzy x y z y -> Vector<T, 4>, xyzz x y z z -> Vector<T, 4>, xzxx x z x x -> Vector<T, 4>, xzxy x z x y -> Vector<T, 4>, xzxz x z x z -> Vector<T, 4>, xzyx x z y x -> Vector<T, 4>, xzyy x z y y -> Vector<T, 4>, xzyz x z y z -> Vector<T, 4>, xzzx x z z x -> Vector<T, 4>, xzzy x z z y -> Vector<T, 4>, xzzz x z z z -> Vector<T, 4>, yxxx y x x x -> Vector<T, 4>, yxxy y x x y -> Vector<T, 4>, yxxz y x x z -> Vector<T, 4>, yxyx y x y x -> Vector<T, 4>, yxyy y x y y -> Vector<T, 4>, yxyz y x y z -> Vector<T, 4>, yxzx y x z x -> Vector<T, 4>, yxzy y x z y -> Vector<T, 4>, yxzz y x z z -> Vector<T, 4>, yyxx y y x x -> Vector<T, 4>, yyxy y y x y -> Vector<T, 4>, yyxz y y x z -> Vector<T, 4>, yyyx y y y x -> Vector<T, 4>, yyyy y y y y -> Vector<T, 4>, yyyz y y y z -> Vector<T, 4>, yyzx y y z x -> Vector<T, 4>, yyzy y y z y -> Vector<T, 4>, yyzz y y z z -> Vector<T, 4>, yzxx y z x x -> Vector<T, 4>, yzxy y z x y -> Vector<T, 4>, yzxz y z x z -> Vector<T, 4>, yzyx y z y x -> Vector<T, 4>, yzyy y z y y -> Vector<T, 4>, yzyz y z y z -> Vector<T, 4>, yzzx y z z x -> Vector<T, 4>, yzzy y z z y -> Vector<T, 4>, yzzz y z z z -> Vector<T, 4>, zxxx z x x x -> Vector<T, 4>, zxxy z x x y -> Vector<T, 4>, zxxz z x x z -> Vector<T, 4>, zxyx z x y x -> Vector<T, 4>, zxyy z x y y -> Vector<T, 4>, zxyz z x y z -> Vector<T, 4>, zxzx z x z x -> Vector<T, 4>, zxzy z x z y -> Vector<T, 4>, zxzz z x z z -> Vector<T, 4>, zyxx z y x x -> Vector<T, 4>, zyxy z y x y -> Vector<T, 4>, zyxz z y x z -> Vector<T, 4>, zyyx z y y x -> Vector<T, 4>, zyyy z y y y -> Vector<T, 4>, zyyz z y y z -> Vector<T, 4>, zyzx z y z x -> Vector<T, 4>, zyzy z y z y -> Vector<T, 4>, zyzz z y z z -> Vector<T, 4>, zzxx z z x x -> Vector<T, 4>, zzxy z z x y -> Vector<T, 4>, zzxz z z x z -> Vector<T, 4>, zzyx z z y x -> Vector<T, 4>, zzyy z z y y -> Vector<T, 4>, zzyz z z y z -> Vector<T, 4>, zzzx z z z x -> Vector<T, 4>, zzzy z z z y -> Vector<T, 4>, zzzz z z z z -> Vector<T, 4>,
}
impl_swizzle!{
    VectorSwizzle<T, 4>;
    x x -> Vector<T, 1>, y y -> Vector<T, 1>, z z -> Vector<T, 1>, w w -> Vector<T, 1>,
    xx x x -> Vector<T, 2>, xy x y -> Vector<T, 2>, xz x z -> Vector<T, 2>, xw x w -> Vector<T, 2>, yx y x -> Vector<T, 2>, yy y y -> Vector<T, 2>, yz y z -> Vector<T, 2>, yw y w -> Vector<T, 2>, zx z x -> Vector<T, 2>, zy z y -> Vector<T, 2>, zz z z -> Vector<T, 2>, zw z w -> Vector<T, 2>, wx w x -> Vector<T, 2>, wy w y -> Vector<T, 2>, wz w z -> Vector<T, 2>, ww w w -> Vector<T, 2>,
    xxx x x x -> Vector<T, 3>, xxy x x y -> Vector<T, 3>, xxz x x z -> Vector<T, 3>, xxw x x w -> Vector<T, 3>, xyx x y x -> Vector<T, 3>, xyy x y y -> Vector<T, 3>, xyz x y z -> Vector<T, 3>, xyw x y w -> Vector<T, 3>, xzx x z x -> Vector<T, 3>, xzy x z y -> Vector<T, 3>, xzz x z z -> Vector<T, 3>, xzw x z w -> Vector<T, 3>, xwx x w x -> Vector<T, 3>, xwy x w y -> Vector<T, 3>, xwz x w z -> Vector<T, 3>, xww x w w -> Vector<T, 3>, yxx y x x -> Vector<T, 3>, yxy y x y -> Vector<T, 3>, yxz y x z -> Vector<T, 3>, yxw y x w -> Vector<T, 3>, yyx y y x -> Vector<T, 3>, yyy y y y -> Vector<T, 3>, yyz y y z -> Vector<T, 3>, yyw y y w -> Vector<T, 3>, yzx y z x -> Vector<T, 3>, yzy y z y -> Vector<T, 3>, yzz y z z -> Vector<T, 3>, yzw y z w -> Vector<T, 3>, ywx y w x -> Vector<T, 3>, ywy y w y -> Vector<T, 3>, ywz y w z -> Vector<T, 3>, yww y w w -> Vector<T, 3>, zxx z x x -> Vector<T, 3>, zxy z x y -> Vector<T, 3>, zxz z x z -> Vector<T, 3>, zxw z x w -> Vector<T, 3>, zyx z y x -> Vector<T, 3>, zyy z y y -> Vector<T, 3>, zyz z y z -> Vector<T, 3>, zyw z y w -> Vector<T, 3>, zzx z z x -> Vector<T, 3>, zzy z z y -> Vector<T, 3>, zzz z z z -> Vector<T, 3>, zzw z z w -> Vector<T, 3>, zwx z w x -> Vector<T, 3>, zwy z w y -> Vector<T, 3>, zwz z w z -> Vector<T, 3>, zww z w w -> Vector<T, 3>, wxx w x x -> Vector<T, 3>, wxy w x y -> Vector<T, 3>, wxz w x z -> Vector<T, 3>, wxw w x w -> Vector<T, 3>, wyx w y x -> Vector<T, 3>, wyy w y y -> Vector<T, 3>, wyz w y z -> Vector<T, 3>, wyw w y w -> Vector<T, 3>, wzx w z x -> Vector<T, 3>, wzy w z y -> Vector<T, 3>, wzz w z z -> Vector<T, 3>, wzw w z w -> Vector<T, 3>, wwx w w x -> Vector<T, 3>, wwy w w y -> Vector<T, 3>, wwz w w z -> Vector<T, 3>, www w w w -> Vector<T, 3>,
    xxxx x x x x -> Vector<T, 4>, xxxy x x x y -> Vector<T, 4>, xxxz x x x z -> Vector<T, 4>, xxxw x x x w -> Vector<T, 4>, xxyx x x y x -> Vector<T, 4>, xxyy x x y y -> Vector<T, 4>, xxyz x x y z -> Vector<T, 4>, xxyw x x y w -> Vector<T, 4>, xxzx x x z x -> Vector<T, 4>, xxzy x x z y -> Vector<T, 4>, xxzz x x z z -> Vector<T, 4>, xxzw x x z w -> Vector<T, 4>, xxwx x x w x -> Vector<T, 4>, xxwy x x w y -> Vector<T, 4>, xxwz x x w z -> Vector<T, 4>, xxww x x w w -> Vector<T, 4>, xyxx x y x x -> Vector<T, 4>, xyxy x y x y -> Vector<T, 4>, xyxz x y x z -> Vector<T, 4>, xyxw x y x w -> Vector<T, 4>, xyyx x y y x -> Vector<T, 4>, xyyy x y y y -> Vector<T, 4>, xyyz x y y z -> Vector<T, 4>, xyyw x y y w -> Vector<T, 4>, xyzx x y z x -> Vector<T, 4>, xyzy x y z y -> Vector<T, 4>, xyzz x y z z -> Vector<T, 4>, xyzw x y z w -> Vector<T, 4>, xywx x y w x -> Vector<T, 4>, xywy x y w y -> Vector<T, 4>, xywz x y w z -> Vector<T, 4>, xyww x y w w -> Vector<T, 4>, xzxx x z x x -> Vector<T, 4>, xzxy x z x y -> Vector<T, 4>, xzxz x z x z -> Vector<T, 4>, xzxw x z x w -> Vector<T, 4>, xzyx x z y x -> Vector<T, 4>, xzyy x z y y -> Vector<T, 4>, xzyz x z y z -> Vector<T, 4>, xzyw x z y w -> Vector<T, 4>, xzzx x z z x -> Vector<T, 4>, xzzy x z z y -> Vector<T, 4>, xzzz x z z z -> Vector<T, 4>, xzzw x z z w -> Vector<T, 4>, xzwx x z w x -> Vector<T, 4>, xzwy x z w y -> Vector<T, 4>, xzwz x z w z -> Vector<T, 4>, xzww x z w w -> Vector<T, 4>, xwxx x w x x -> Vector<T, 4>, xwxy x w x y -> Vector<T, 4>, xwxz x w x z -> Vector<T, 4>, xwxw x w x w -> Vector<T, 4>, xwyx x w y x -> Vector<T, 4>, xwyy x w y y -> Vector<T, 4>, xwyz x w y z -> Vector<T, 4>, xwyw x w y w -> Vector<T, 4>, xwzx x w z x -> Vector<T, 4>, xwzy x w z y -> Vector<T, 4>, xwzz x w z z -> Vector<T, 4>, xwzw x w z w -> Vector<T, 4>, xwwx x w w x -> Vector<T, 4>, xwwy x w w y -> Vector<T, 4>, xwwz x w w z -> Vector<T, 4>, xwww x w w w -> Vector<T, 4>, yxxx y x x x -> Vector<T, 4>, yxxy y x x y -> Vector<T, 4>, yxxz y x x z -> Vector<T, 4>, yxxw y x x w -> Vector<T, 4>, yxyx y x y x -> Vector<T, 4>, yxyy y x y y -> Vector<T, 4>, yxyz y x y z -> Vector<T, 4>, yxyw y x y w -> Vector<T, 4>, yxzx y x z x -> Vector<T, 4>, yxzy y x z y -> Vector<T, 4>, yxzz y x z z -> Vector<T, 4>, yxzw y x z w -> Vector<T, 4>, yxwx y x w x -> Vector<T, 4>, yxwy y x w y -> Vector<T, 4>, yxwz y x w z -> Vector<T, 4>, yxww y x w w -> Vector<T, 4>, yyxx y y x x -> Vector<T, 4>, yyxy y y x y -> Vector<T, 4>, yyxz y y x z -> Vector<T, 4>, yyxw y y x w -> Vector<T, 4>, yyyx y y y x -> Vector<T, 4>, yyyy y y y y -> Vector<T, 4>, yyyz y y y z -> Vector<T, 4>, yyyw y y y w -> Vector<T, 4>, yyzx y y z x -> Vector<T, 4>, yyzy y y z y -> Vector<T, 4>, yyzz y y z z -> Vector<T, 4>, yyzw y y z w -> Vector<T, 4>, yywx y y w x -> Vector<T, 4>, yywy y y w y -> Vector<T, 4>, yywz y y w z -> Vector<T, 4>, yyww y y w w -> Vector<T, 4>, yzxx y z x x -> Vector<T, 4>, yzxy y z x y -> Vector<T, 4>, yzxz y z x z -> Vector<T, 4>, yzxw y z x w -> Vector<T, 4>, yzyx y z y x -> Vector<T, 4>, yzyy y z y y -> Vector<T, 4>, yzyz y z y z -> Vector<T, 4>, yzyw y z y w -> Vector<T, 4>, yzzx y z z x -> Vector<T, 4>, yzzy y z z y -> Vector<T, 4>, yzzz y z z z -> Vector<T, 4>, yzzw y z z w -> Vector<T, 4>, yzwx y z w x -> Vector<T, 4>, yzwy y z w y -> Vector<T, 4>, yzwz y z w z -> Vector<T, 4>, yzww y z w w -> Vector<T, 4>, ywxx y w x x -> Vector<T, 4>, ywxy y w x y -> Vector<T, 4>, ywxz y w x z -> Vector<T, 4>, ywxw y w x w -> Vector<T, 4>, ywyx y w y x -> Vector<T, 4>, ywyy y w y y -> Vector<T, 4>, ywyz y w y z -> Vector<T, 4>, ywyw y w y w -> Vector<T, 4>, ywzx y w z x -> Vector<T, 4>, ywzy y w z y -> Vector<T, 4>, ywzz y w z z -> Vector<T, 4>, ywzw y w z w -> Vector<T, 4>, ywwx y w w x -> Vector<T, 4>, ywwy y w w y -> Vector<T, 4>, ywwz y w w z -> Vector<T, 4>, ywww y w w w -> Vector<T, 4>, zxxx z x x x -> Vector<T, 4>, zxxy z x x y -> Vector<T, 4>, zxxz z x x z -> Vector<T, 4>, zxxw z x x w -> Vector<T, 4>, zxyx z x y x -> Vector<T, 4>, zxyy z x y y -> Vector<T, 4>, zxyz z x y z -> Vector<T, 4>, zxyw z x y w -> Vector<T, 4>, zxzx z x z x -> Vector<T, 4>, zxzy z x z y -> Vector<T, 4>, zxzz z x z z -> Vector<T, 4>, zxzw z x z w -> Vector<T, 4>, zxwx z x w x -> Vector<T, 4>, zxwy z x w y -> Vector<T, 4>, zxwz z x w z -> Vector<T, 4>, zxww z x w w -> Vector<T, 4>, zyxx z y x x -> Vector<T, 4>, zyxy z y x y -> Vector<T, 4>, zyxz z y x z -> Vector<T, 4>, zyxw z y x w -> Vector<T, 4>, zyyx z y y x -> Vector<T, 4>, zyyy z y y y -> Vector<T, 4>, zyyz z y y z -> Vector<T, 4>, zyyw z y y w -> Vector<T, 4>, zyzx z y z x -> Vector<T, 4>, zyzy z y z y -> Vector<T, 4>, zyzz z y z z -> Vector<T, 4>, zyzw z y z w -> Vector<T, 4>, zywx z y w x -> Vector<T, 4>, zywy z y w y -> Vector<T, 4>, zywz z y w z -> Vector<T, 4>, zyww z y w w -> Vector<T, 4>, zzxx z z x x -> Vector<T, 4>, zzxy z z x y -> Vector<T, 4>, zzxz z z x z -> Vector<T, 4>, zzxw z z x w -> Vector<T, 4>, zzyx z z y x -> Vector<T, 4>, zzyy z z y y -> Vector<T, 4>, zzyz z z y z -> Vector<T, 4>, zzyw z z y w -> Vector<T, 4>, zzzx z z z x -> Vector<T, 4>, zzzy z z z y -> Vector<T, 4>, zzzz z z z z -> Vector<T, 4>, zzzw z z z w -> Vector<T, 4>, zzwx z z w x -> Vector<T, 4>, zzwy z z w y -> Vector<T, 4>, zzwz z z w z -> Vector<T, 4>, zzww z z w w -> Vector<T, 4>, zwxx z w x x -> Vector<T, 4>, zwxy z w x y -> Vector<T, 4>, zwxz z w x z -> Vector<T, 4>, zwxw z w x w -> Vector<T, 4>, zwyx z w y x -> Vector<T, 4>, zwyy z w y y -> Vector<T, 4>, zwyz z w y z -> Vector<T, 4>, zwyw z w y w -> Vector<T, 4>, zwzx z w z x -> Vector<T, 4>, zwzy z w z y -> Vector<T, 4>, zwzz z w z z -> Vector<T, 4>, zwzw z w z w -> Vector<T, 4>, zwwx z w w x -> Vector<T, 4>, zwwy z w w y -> Vector<T, 4>, zwwz z w w z -> Vector<T, 4>, zwww z w w w -> Vector<T, 4>, wxxx w x x x -> Vector<T, 4>, wxxy w x x y -> Vector<T, 4>, wxxz w x x z -> Vector<T, 4>, wxxw w x x w -> Vector<T, 4>, wxyx w x y x -> Vector<T, 4>, wxyy w x y y -> Vector<T, 4>, wxyz w x y z -> Vector<T, 4>, wxyw w x y w -> Vector<T, 4>, wxzx w x z x -> Vector<T, 4>, wxzy w x z y -> Vector<T, 4>, wxzz w x z z -> Vector<T, 4>, wxzw w x z w -> Vector<T, 4>, wxwx w x w x -> Vector<T, 4>, wxwy w x w y -> Vector<T, 4>, wxwz w x w z -> Vector<T, 4>, wxww w x w w -> Vector<T, 4>, wyxx w y x x -> Vector<T, 4>, wyxy w y x y -> Vector<T, 4>, wyxz w y x z -> Vector<T, 4>, wyxw w y x w -> Vector<T, 4>, wyyx w y y x -> Vector<T, 4>, wyyy w y y y -> Vector<T, 4>, wyyz w y y z -> Vector<T, 4>, wyyw w y y w -> Vector<T, 4>, wyzx w y z x -> Vector<T, 4>, wyzy w y z y -> Vector<T, 4>, wyzz w y z z -> Vector<T, 4>, wyzw w y z w -> Vector<T, 4>, wywx w y w x -> Vector<T, 4>, wywy w y w y -> Vector<T, 4>, wywz w y w z -> Vector<T, 4>, wyww w y w w -> Vector<T, 4>, wzxx w z x x -> Vector<T, 4>, wzxy w z x y -> Vector<T, 4>, wzxz w z x z -> Vector<T, 4>, wzxw w z x w -> Vector<T, 4>, wzyx w z y x -> Vector<T, 4>, wzyy w z y y -> Vector<T, 4>, wzyz w z y z -> Vector<T, 4>, wzyw w z y w -> Vector<T, 4>, wzzx w z z x -> Vector<T, 4>, wzzy w z z y -> Vector<T, 4>, wzzz w z z z -> Vector<T, 4>, wzzw w z z w -> Vector<T, 4>, wzwx w z w x -> Vector<T, 4>, wzwy w z w y -> Vector<T, 4>, wzwz w z w z -> Vector<T, 4>, wzww w z w w -> Vector<T, 4>, wwxx w w x x -> Vector<T, 4>, wwxy w w x y -> Vector<T, 4>, wwxz w w x z -> Vector<T, 4>, wwxw w w x w -> Vector<T, 4>, wwyx w w y x -> Vector<T, 4>, wwyy w w y y -> Vector<T, 4>, wwyz w w y z -> Vector<T, 4>, wwyw w w y w -> Vector<T, 4>, wwzx w w z x -> Vector<T, 4>, wwzy w w z y -> Vector<T, 4>, wwzz w w z z -> Vector<T, 4>, wwzw w w z w -> Vector<T, 4>, wwwx w w w x -> Vector<T, 4>, wwwy w w w y -> Vector<T, 4>, wwwz w w w z -> Vector<T, 4>, wwww w w w w -> Vector<T, 4>,
}
