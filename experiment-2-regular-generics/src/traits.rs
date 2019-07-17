use super::{Vector2, Vector3, Vector4};

pub trait Concat<Rhs> {
    type Output;

    fn concat(self, rhs: Rhs) -> Self::Output;
}

pub trait ToVector {
	type Output;

	fn to_vector(self) -> Self::Output;
}

impl<T> ToVector for [T; 1] {
	type Output = T;

	fn to_vector(self) -> Self::Output {
		let [t] = self;
		t
	}
}

impl<T> ToVector for [T; 2] {
	type Output = Vector2<T>;

	fn to_vector(self) -> Self::Output {
		Vector2::from(self)
	}
}

impl<T> ToVector for [T; 3] {
	type Output = Vector3<T>;

	fn to_vector(self) -> Self::Output {
		Vector3::from(self)
	}
}

impl<T> ToVector for [T; 4] {
	type Output = Vector4<T>;

	fn to_vector(self) -> Self::Output {
		Vector4::from(self)
	}
}
