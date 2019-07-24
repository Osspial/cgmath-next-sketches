#[macro_export]
macro_rules! vec2 {
    ($($item:expr),* $(,)?) => {{
        let vector: [_; 0] = [];
        $(
            let vector = $crate::traits::Concat::concat(vector, $item);
        )*
        $crate::traits::ToVector::to_vector(vector)
    }};
}
