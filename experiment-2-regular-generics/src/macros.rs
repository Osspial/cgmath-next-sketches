#[macro_export]
macro_rules! vector {
    ([$tt:tt]) => {{
        vector![$tt]
    }};
    ($first:expr $(, $item:expr)* $(,)?) => {{
        let vector = $crate::MacroBootstrap($first);
        $(
            let vector = $crate::traits::Concat::concat(vector, $item);
        )*
        vector.unwrap_macro_bootstrap()
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
