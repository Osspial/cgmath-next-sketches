use super::{Vector2, Vector3, Vector4};

pub trait Concat<Rhs> {
    type Output;

    fn concat(self, rhs: Rhs) -> Self::Output;
}
