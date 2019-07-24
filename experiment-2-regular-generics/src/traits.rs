pub trait Concat<Rhs> {
    type Output;

    fn concat(self, rhs: Rhs) -> Self::Output;
}

#[doc(hidden)]
pub trait LocalInto<T> {
    fn local_into(self) -> T;
}
