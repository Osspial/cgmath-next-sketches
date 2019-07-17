pub trait Concat<Rhs> {
    type Output;

    fn concat(self, rhs: Rhs) -> Self::Output;
}
