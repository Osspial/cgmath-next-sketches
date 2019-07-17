#![feature(specialization)]
#[macro_use]
mod macros;
pub mod traits;

macro_rules! vector_struct {
    (struct $Vector:ident($($dim:ident),+): $len:literal) => {
        #[repr(C)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $Vector<T> {
            $(pub $dim: T),+
        }

        impl<T> From<[T; $len]> for $Vector<T> {
            fn from(elements: [T; $len]) -> $Vector<T> {
                let [$($dim),+] = elements;

                Self {
                    $($dim),+
                }
            }
        }

        impl<T> $Vector<T> {
            pub const LEN: usize = $len;
        }
    };
}

macro_rules! vector_concat {
    ($lhs:ty, $rhs:ty => $out:ty) => {
        impl<T> traits::Concat<$rhs> for $lhs {
            type Output = $out;

            fn concat(self, other: $rhs) -> Self::Output {
                #[repr(C)]
                struct Concat<A, B>(A, B);

                assert_eq!(std::mem::size_of::<Concat<$lhs, $rhs>>(), std::mem::size_of::<Self::Output>());

                let concat = Concat(self, other);
                let out = unsafe{ std::mem::transmute_copy(&concat) };
                std::mem::forget(concat);
                out
            }
        }
    };
}

vector_struct!(struct Vector2(x, y): 2);
vector_struct!(struct Vector3(x, y, z): 3);
vector_struct!(struct Vector4(x, y, z, w): 4);

vector_concat!([T; 0], T => [T; 1]);
vector_concat!([T; 0], Vector2<T> => [T; 2]);
vector_concat!([T; 0], Vector3<T> => [T; 3]);
vector_concat!([T; 0], Vector4<T> => [T; 4]);
vector_concat!([T; 1], T => [T; 2]);
vector_concat!([T; 1], Vector2<T> => [T; 3]);
vector_concat!([T; 1], Vector3<T> => [T; 4]);
vector_concat!([T; 2], T => [T; 3]);
vector_concat!([T; 2], Vector2<T> => [T; 4]);
vector_concat!([T; 3], T => [T; 4]);

vector_concat!([T; 0], [T; 2] => [T; 2]);
vector_concat!([T; 0], [T; 3] => [T; 3]);
vector_concat!([T; 0], [T; 4] => [T; 4]);
vector_concat!([T; 1], [T; 2] => [T; 3]);
vector_concat!([T; 1], [T; 3] => [T; 4]);
vector_concat!([T; 2], [T; 2] => [T; 4]);

macro_rules! array_or_expr {
    ($item:expr) => {{$item}};
    ($($item:expr),+) => {{[$($item),+]}};
}

macro_rules! impl_swizzle {
    ($t:ty; $($swizzle:ident $($coord:ident)+ -> $out:ty,)+) => {
        /// Swizzle functions.
        impl<T: Clone> $t {
            $(
                pub fn $swizzle(&self) -> $out {
                    <$out>::from(array_or_expr!($(self.$coord.clone()),+))
                }
            )+
        }
    };
}

impl_swizzle!{
    Vector2<T>;
    x x -> T, y y -> T,
    xx x x -> Vector2<T>, xy x y -> Vector2<T>, yx y x -> Vector2<T>, yy y y -> Vector2<T>,
    xxx x x x -> Vector3<T>, xxy x x y -> Vector3<T>, xyx x y x -> Vector3<T>, xyy x y y -> Vector3<T>, yxx y x x -> Vector3<T>, yxy y x y -> Vector3<T>, yyx y y x -> Vector3<T>, yyy y y y -> Vector3<T>,
    xxxx x x x x -> Vector4<T>, xxxy x x x y -> Vector4<T>, xxyx x x y x -> Vector4<T>, xxyy x x y y -> Vector4<T>, xyxx x y x x -> Vector4<T>, xyxy x y x y -> Vector4<T>, xyyx x y y x -> Vector4<T>, xyyy x y y y -> Vector4<T>, yxxx y x x x -> Vector4<T>, yxxy y x x y -> Vector4<T>, yxyx y x y x -> Vector4<T>, yxyy y x y y -> Vector4<T>, yyxx y y x x -> Vector4<T>, yyxy y y x y -> Vector4<T>, yyyx y y y x -> Vector4<T>, yyyy y y y y -> Vector4<T>,
}
impl_swizzle!{
    Vector3<T>;
    x x -> T, y y -> T, z z -> T,
    xx x x -> Vector2<T>, xy x y -> Vector2<T>, xz x z -> Vector2<T>, yx y x -> Vector2<T>, yy y y -> Vector2<T>, yz y z -> Vector2<T>, zx z x -> Vector2<T>, zy z y -> Vector2<T>, zz z z -> Vector2<T>,
    xxx x x x -> Vector3<T>, xxy x x y -> Vector3<T>, xxz x x z -> Vector3<T>, xyx x y x -> Vector3<T>, xyy x y y -> Vector3<T>, xyz x y z -> Vector3<T>, xzx x z x -> Vector3<T>, xzy x z y -> Vector3<T>, xzz x z z -> Vector3<T>, yxx y x x -> Vector3<T>, yxy y x y -> Vector3<T>, yxz y x z -> Vector3<T>, yyx y y x -> Vector3<T>, yyy y y y -> Vector3<T>, yyz y y z -> Vector3<T>, yzx y z x -> Vector3<T>, yzy y z y -> Vector3<T>, yzz y z z -> Vector3<T>, zxx z x x -> Vector3<T>, zxy z x y -> Vector3<T>, zxz z x z -> Vector3<T>, zyx z y x -> Vector3<T>, zyy z y y -> Vector3<T>, zyz z y z -> Vector3<T>, zzx z z x -> Vector3<T>, zzy z z y -> Vector3<T>, zzz z z z -> Vector3<T>,
    xxxx x x x x -> Vector4<T>, xxxy x x x y -> Vector4<T>, xxxz x x x z -> Vector4<T>, xxyx x x y x -> Vector4<T>, xxyy x x y y -> Vector4<T>, xxyz x x y z -> Vector4<T>, xxzx x x z x -> Vector4<T>, xxzy x x z y -> Vector4<T>, xxzz x x z z -> Vector4<T>, xyxx x y x x -> Vector4<T>, xyxy x y x y -> Vector4<T>, xyxz x y x z -> Vector4<T>, xyyx x y y x -> Vector4<T>, xyyy x y y y -> Vector4<T>, xyyz x y y z -> Vector4<T>, xyzx x y z x -> Vector4<T>, xyzy x y z y -> Vector4<T>, xyzz x y z z -> Vector4<T>, xzxx x z x x -> Vector4<T>, xzxy x z x y -> Vector4<T>, xzxz x z x z -> Vector4<T>, xzyx x z y x -> Vector4<T>, xzyy x z y y -> Vector4<T>, xzyz x z y z -> Vector4<T>, xzzx x z z x -> Vector4<T>, xzzy x z z y -> Vector4<T>, xzzz x z z z -> Vector4<T>, yxxx y x x x -> Vector4<T>, yxxy y x x y -> Vector4<T>, yxxz y x x z -> Vector4<T>, yxyx y x y x -> Vector4<T>, yxyy y x y y -> Vector4<T>, yxyz y x y z -> Vector4<T>, yxzx y x z x -> Vector4<T>, yxzy y x z y -> Vector4<T>, yxzz y x z z -> Vector4<T>, yyxx y y x x -> Vector4<T>, yyxy y y x y -> Vector4<T>, yyxz y y x z -> Vector4<T>, yyyx y y y x -> Vector4<T>, yyyy y y y y -> Vector4<T>, yyyz y y y z -> Vector4<T>, yyzx y y z x -> Vector4<T>, yyzy y y z y -> Vector4<T>, yyzz y y z z -> Vector4<T>, yzxx y z x x -> Vector4<T>, yzxy y z x y -> Vector4<T>, yzxz y z x z -> Vector4<T>, yzyx y z y x -> Vector4<T>, yzyy y z y y -> Vector4<T>, yzyz y z y z -> Vector4<T>, yzzx y z z x -> Vector4<T>, yzzy y z z y -> Vector4<T>, yzzz y z z z -> Vector4<T>, zxxx z x x x -> Vector4<T>, zxxy z x x y -> Vector4<T>, zxxz z x x z -> Vector4<T>, zxyx z x y x -> Vector4<T>, zxyy z x y y -> Vector4<T>, zxyz z x y z -> Vector4<T>, zxzx z x z x -> Vector4<T>, zxzy z x z y -> Vector4<T>, zxzz z x z z -> Vector4<T>, zyxx z y x x -> Vector4<T>, zyxy z y x y -> Vector4<T>, zyxz z y x z -> Vector4<T>, zyyx z y y x -> Vector4<T>, zyyy z y y y -> Vector4<T>, zyyz z y y z -> Vector4<T>, zyzx z y z x -> Vector4<T>, zyzy z y z y -> Vector4<T>, zyzz z y z z -> Vector4<T>, zzxx z z x x -> Vector4<T>, zzxy z z x y -> Vector4<T>, zzxz z z x z -> Vector4<T>, zzyx z z y x -> Vector4<T>, zzyy z z y y -> Vector4<T>, zzyz z z y z -> Vector4<T>, zzzx z z z x -> Vector4<T>, zzzy z z z y -> Vector4<T>, zzzz z z z z -> Vector4<T>,
}
impl_swizzle!{
    Vector4<T>;
    x x -> T, y y -> T, z z -> T, w w -> T,
    xx x x -> Vector2<T>, xy x y -> Vector2<T>, xz x z -> Vector2<T>, xw x w -> Vector2<T>, yx y x -> Vector2<T>, yy y y -> Vector2<T>, yz y z -> Vector2<T>, yw y w -> Vector2<T>, zx z x -> Vector2<T>, zy z y -> Vector2<T>, zz z z -> Vector2<T>, zw z w -> Vector2<T>, wx w x -> Vector2<T>, wy w y -> Vector2<T>, wz w z -> Vector2<T>, ww w w -> Vector2<T>,
    xxx x x x -> Vector3<T>, xxy x x y -> Vector3<T>, xxz x x z -> Vector3<T>, xxw x x w -> Vector3<T>, xyx x y x -> Vector3<T>, xyy x y y -> Vector3<T>, xyz x y z -> Vector3<T>, xyw x y w -> Vector3<T>, xzx x z x -> Vector3<T>, xzy x z y -> Vector3<T>, xzz x z z -> Vector3<T>, xzw x z w -> Vector3<T>, xwx x w x -> Vector3<T>, xwy x w y -> Vector3<T>, xwz x w z -> Vector3<T>, xww x w w -> Vector3<T>, yxx y x x -> Vector3<T>, yxy y x y -> Vector3<T>, yxz y x z -> Vector3<T>, yxw y x w -> Vector3<T>, yyx y y x -> Vector3<T>, yyy y y y -> Vector3<T>, yyz y y z -> Vector3<T>, yyw y y w -> Vector3<T>, yzx y z x -> Vector3<T>, yzy y z y -> Vector3<T>, yzz y z z -> Vector3<T>, yzw y z w -> Vector3<T>, ywx y w x -> Vector3<T>, ywy y w y -> Vector3<T>, ywz y w z -> Vector3<T>, yww y w w -> Vector3<T>, zxx z x x -> Vector3<T>, zxy z x y -> Vector3<T>, zxz z x z -> Vector3<T>, zxw z x w -> Vector3<T>, zyx z y x -> Vector3<T>, zyy z y y -> Vector3<T>, zyz z y z -> Vector3<T>, zyw z y w -> Vector3<T>, zzx z z x -> Vector3<T>, zzy z z y -> Vector3<T>, zzz z z z -> Vector3<T>, zzw z z w -> Vector3<T>, zwx z w x -> Vector3<T>, zwy z w y -> Vector3<T>, zwz z w z -> Vector3<T>, zww z w w -> Vector3<T>, wxx w x x -> Vector3<T>, wxy w x y -> Vector3<T>, wxz w x z -> Vector3<T>, wxw w x w -> Vector3<T>, wyx w y x -> Vector3<T>, wyy w y y -> Vector3<T>, wyz w y z -> Vector3<T>, wyw w y w -> Vector3<T>, wzx w z x -> Vector3<T>, wzy w z y -> Vector3<T>, wzz w z z -> Vector3<T>, wzw w z w -> Vector3<T>, wwx w w x -> Vector3<T>, wwy w w y -> Vector3<T>, wwz w w z -> Vector3<T>, www w w w -> Vector3<T>,
    xxxx x x x x -> Vector4<T>, xxxy x x x y -> Vector4<T>, xxxz x x x z -> Vector4<T>, xxxw x x x w -> Vector4<T>, xxyx x x y x -> Vector4<T>, xxyy x x y y -> Vector4<T>, xxyz x x y z -> Vector4<T>, xxyw x x y w -> Vector4<T>, xxzx x x z x -> Vector4<T>, xxzy x x z y -> Vector4<T>, xxzz x x z z -> Vector4<T>, xxzw x x z w -> Vector4<T>, xxwx x x w x -> Vector4<T>, xxwy x x w y -> Vector4<T>, xxwz x x w z -> Vector4<T>, xxww x x w w -> Vector4<T>, xyxx x y x x -> Vector4<T>, xyxy x y x y -> Vector4<T>, xyxz x y x z -> Vector4<T>, xyxw x y x w -> Vector4<T>, xyyx x y y x -> Vector4<T>, xyyy x y y y -> Vector4<T>, xyyz x y y z -> Vector4<T>, xyyw x y y w -> Vector4<T>, xyzx x y z x -> Vector4<T>, xyzy x y z y -> Vector4<T>, xyzz x y z z -> Vector4<T>, xyzw x y z w -> Vector4<T>, xywx x y w x -> Vector4<T>, xywy x y w y -> Vector4<T>, xywz x y w z -> Vector4<T>, xyww x y w w -> Vector4<T>, xzxx x z x x -> Vector4<T>, xzxy x z x y -> Vector4<T>, xzxz x z x z -> Vector4<T>, xzxw x z x w -> Vector4<T>, xzyx x z y x -> Vector4<T>, xzyy x z y y -> Vector4<T>, xzyz x z y z -> Vector4<T>, xzyw x z y w -> Vector4<T>, xzzx x z z x -> Vector4<T>, xzzy x z z y -> Vector4<T>, xzzz x z z z -> Vector4<T>, xzzw x z z w -> Vector4<T>, xzwx x z w x -> Vector4<T>, xzwy x z w y -> Vector4<T>, xzwz x z w z -> Vector4<T>, xzww x z w w -> Vector4<T>, xwxx x w x x -> Vector4<T>, xwxy x w x y -> Vector4<T>, xwxz x w x z -> Vector4<T>, xwxw x w x w -> Vector4<T>, xwyx x w y x -> Vector4<T>, xwyy x w y y -> Vector4<T>, xwyz x w y z -> Vector4<T>, xwyw x w y w -> Vector4<T>, xwzx x w z x -> Vector4<T>, xwzy x w z y -> Vector4<T>, xwzz x w z z -> Vector4<T>, xwzw x w z w -> Vector4<T>, xwwx x w w x -> Vector4<T>, xwwy x w w y -> Vector4<T>, xwwz x w w z -> Vector4<T>, xwww x w w w -> Vector4<T>, yxxx y x x x -> Vector4<T>, yxxy y x x y -> Vector4<T>, yxxz y x x z -> Vector4<T>, yxxw y x x w -> Vector4<T>, yxyx y x y x -> Vector4<T>, yxyy y x y y -> Vector4<T>, yxyz y x y z -> Vector4<T>, yxyw y x y w -> Vector4<T>, yxzx y x z x -> Vector4<T>, yxzy y x z y -> Vector4<T>, yxzz y x z z -> Vector4<T>, yxzw y x z w -> Vector4<T>, yxwx y x w x -> Vector4<T>, yxwy y x w y -> Vector4<T>, yxwz y x w z -> Vector4<T>, yxww y x w w -> Vector4<T>, yyxx y y x x -> Vector4<T>, yyxy y y x y -> Vector4<T>, yyxz y y x z -> Vector4<T>, yyxw y y x w -> Vector4<T>, yyyx y y y x -> Vector4<T>, yyyy y y y y -> Vector4<T>, yyyz y y y z -> Vector4<T>, yyyw y y y w -> Vector4<T>, yyzx y y z x -> Vector4<T>, yyzy y y z y -> Vector4<T>, yyzz y y z z -> Vector4<T>, yyzw y y z w -> Vector4<T>, yywx y y w x -> Vector4<T>, yywy y y w y -> Vector4<T>, yywz y y w z -> Vector4<T>, yyww y y w w -> Vector4<T>, yzxx y z x x -> Vector4<T>, yzxy y z x y -> Vector4<T>, yzxz y z x z -> Vector4<T>, yzxw y z x w -> Vector4<T>, yzyx y z y x -> Vector4<T>, yzyy y z y y -> Vector4<T>, yzyz y z y z -> Vector4<T>, yzyw y z y w -> Vector4<T>, yzzx y z z x -> Vector4<T>, yzzy y z z y -> Vector4<T>, yzzz y z z z -> Vector4<T>, yzzw y z z w -> Vector4<T>, yzwx y z w x -> Vector4<T>, yzwy y z w y -> Vector4<T>, yzwz y z w z -> Vector4<T>, yzww y z w w -> Vector4<T>, ywxx y w x x -> Vector4<T>, ywxy y w x y -> Vector4<T>, ywxz y w x z -> Vector4<T>, ywxw y w x w -> Vector4<T>, ywyx y w y x -> Vector4<T>, ywyy y w y y -> Vector4<T>, ywyz y w y z -> Vector4<T>, ywyw y w y w -> Vector4<T>, ywzx y w z x -> Vector4<T>, ywzy y w z y -> Vector4<T>, ywzz y w z z -> Vector4<T>, ywzw y w z w -> Vector4<T>, ywwx y w w x -> Vector4<T>, ywwy y w w y -> Vector4<T>, ywwz y w w z -> Vector4<T>, ywww y w w w -> Vector4<T>, zxxx z x x x -> Vector4<T>, zxxy z x x y -> Vector4<T>, zxxz z x x z -> Vector4<T>, zxxw z x x w -> Vector4<T>, zxyx z x y x -> Vector4<T>, zxyy z x y y -> Vector4<T>, zxyz z x y z -> Vector4<T>, zxyw z x y w -> Vector4<T>, zxzx z x z x -> Vector4<T>, zxzy z x z y -> Vector4<T>, zxzz z x z z -> Vector4<T>, zxzw z x z w -> Vector4<T>, zxwx z x w x -> Vector4<T>, zxwy z x w y -> Vector4<T>, zxwz z x w z -> Vector4<T>, zxww z x w w -> Vector4<T>, zyxx z y x x -> Vector4<T>, zyxy z y x y -> Vector4<T>, zyxz z y x z -> Vector4<T>, zyxw z y x w -> Vector4<T>, zyyx z y y x -> Vector4<T>, zyyy z y y y -> Vector4<T>, zyyz z y y z -> Vector4<T>, zyyw z y y w -> Vector4<T>, zyzx z y z x -> Vector4<T>, zyzy z y z y -> Vector4<T>, zyzz z y z z -> Vector4<T>, zyzw z y z w -> Vector4<T>, zywx z y w x -> Vector4<T>, zywy z y w y -> Vector4<T>, zywz z y w z -> Vector4<T>, zyww z y w w -> Vector4<T>, zzxx z z x x -> Vector4<T>, zzxy z z x y -> Vector4<T>, zzxz z z x z -> Vector4<T>, zzxw z z x w -> Vector4<T>, zzyx z z y x -> Vector4<T>, zzyy z z y y -> Vector4<T>, zzyz z z y z -> Vector4<T>, zzyw z z y w -> Vector4<T>, zzzx z z z x -> Vector4<T>, zzzy z z z y -> Vector4<T>, zzzz z z z z -> Vector4<T>, zzzw z z z w -> Vector4<T>, zzwx z z w x -> Vector4<T>, zzwy z z w y -> Vector4<T>, zzwz z z w z -> Vector4<T>, zzww z z w w -> Vector4<T>, zwxx z w x x -> Vector4<T>, zwxy z w x y -> Vector4<T>, zwxz z w x z -> Vector4<T>, zwxw z w x w -> Vector4<T>, zwyx z w y x -> Vector4<T>, zwyy z w y y -> Vector4<T>, zwyz z w y z -> Vector4<T>, zwyw z w y w -> Vector4<T>, zwzx z w z x -> Vector4<T>, zwzy z w z y -> Vector4<T>, zwzz z w z z -> Vector4<T>, zwzw z w z w -> Vector4<T>, zwwx z w w x -> Vector4<T>, zwwy z w w y -> Vector4<T>, zwwz z w w z -> Vector4<T>, zwww z w w w -> Vector4<T>, wxxx w x x x -> Vector4<T>, wxxy w x x y -> Vector4<T>, wxxz w x x z -> Vector4<T>, wxxw w x x w -> Vector4<T>, wxyx w x y x -> Vector4<T>, wxyy w x y y -> Vector4<T>, wxyz w x y z -> Vector4<T>, wxyw w x y w -> Vector4<T>, wxzx w x z x -> Vector4<T>, wxzy w x z y -> Vector4<T>, wxzz w x z z -> Vector4<T>, wxzw w x z w -> Vector4<T>, wxwx w x w x -> Vector4<T>, wxwy w x w y -> Vector4<T>, wxwz w x w z -> Vector4<T>, wxww w x w w -> Vector4<T>, wyxx w y x x -> Vector4<T>, wyxy w y x y -> Vector4<T>, wyxz w y x z -> Vector4<T>, wyxw w y x w -> Vector4<T>, wyyx w y y x -> Vector4<T>, wyyy w y y y -> Vector4<T>, wyyz w y y z -> Vector4<T>, wyyw w y y w -> Vector4<T>, wyzx w y z x -> Vector4<T>, wyzy w y z y -> Vector4<T>, wyzz w y z z -> Vector4<T>, wyzw w y z w -> Vector4<T>, wywx w y w x -> Vector4<T>, wywy w y w y -> Vector4<T>, wywz w y w z -> Vector4<T>, wyww w y w w -> Vector4<T>, wzxx w z x x -> Vector4<T>, wzxy w z x y -> Vector4<T>, wzxz w z x z -> Vector4<T>, wzxw w z x w -> Vector4<T>, wzyx w z y x -> Vector4<T>, wzyy w z y y -> Vector4<T>, wzyz w z y z -> Vector4<T>, wzyw w z y w -> Vector4<T>, wzzx w z z x -> Vector4<T>, wzzy w z z y -> Vector4<T>, wzzz w z z z -> Vector4<T>, wzzw w z z w -> Vector4<T>, wzwx w z w x -> Vector4<T>, wzwy w z w y -> Vector4<T>, wzwz w z w z -> Vector4<T>, wzww w z w w -> Vector4<T>, wwxx w w x x -> Vector4<T>, wwxy w w x y -> Vector4<T>, wwxz w w x z -> Vector4<T>, wwxw w w x w -> Vector4<T>, wwyx w w y x -> Vector4<T>, wwyy w w y y -> Vector4<T>, wwyz w w y z -> Vector4<T>, wwyw w w y w -> Vector4<T>, wwzx w w z x -> Vector4<T>, wwzy w w z y -> Vector4<T>, wwzz w w z z -> Vector4<T>, wwzw w w z w -> Vector4<T>, wwwx w w w x -> Vector4<T>, wwwy w w w y -> Vector4<T>, wwwz w w w z -> Vector4<T>, wwww w w w w -> Vector4<T>,
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn create() {
        let vec1: f32 = vector![0.0];
        let vec2: Vector2<f32> = vector![0.0, 1.0];
        let vec3: Vector3<f32> = vector![0.0, 1.0, 2.0];
        let vec4: Vector4<f32> = vector![0.0, 1.0, 2.0, 3.0];

        let vec1_infer = vector![0.0];
        let vec2_infer = vector![0.0, 1.0];
        let vec3_infer = vector![0.0, 1.0, 2.0];
        let vec4_infer = vector![0.0, 1.0, 2.0, 3.0];

        assert_eq!(vec1, vec1_infer);
        assert_eq!(vec2, vec2_infer);
        assert_eq!(vec3, vec3_infer);
        assert_eq!(vec4, vec4_infer);

        let vec_swizzle = vector![vec2.yx(), 0.0];
    }
}
