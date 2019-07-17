#[macro_export]
macro_rules! vector {
    ([$tt:tt]) => {{
        vector![$tt]
    }};
    ($($item:expr),+ $(,)?) => {{
        let vector = $crate::Vector {elements: []};
        $(
            let vector = vector.concat($crate::Vector::from($item));
        )+
        vector
    }};
}
