#[macro_export]
macro_rules! vector {
    ([$tt:tt]) => {{
        vector![$tt]
    }};
    ($($item:expr),* $(,)?) => {{
        let vector: [_; 0] = [];
        $(
            let vector = $crate::traits::Concat::concat(vector, $item);
        )*
        $crate::traits::ToVector::to_vector(vector)
    }};
}

#[macro_export]
macro_rules! matrix {
    ($($row:tt),+ $(,)?) => {{
        let matrix = $crate::Matrix {elements: []};
        $(
            let matrix = matrix.concat_row(vector![$row]);
        )+
        matrix
    }};
}
