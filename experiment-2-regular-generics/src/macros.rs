#[macro_export]
macro_rules! vec2 {
    ($item:expr) => {{
        $crate::Vector2::splat($item)
    }};
    ($($item:expr),* $(,)?) => {{
        let v: $crate::Vector2<_> = $crate::traits::LocalInto::local_into(($($item,)+));
        v
    }};
}

#[macro_export]
macro_rules! vec3 {
    ($item:expr) => {{
        $crate::Vector3::splat($item)
    }};
    ($($item:expr),* $(,)?) => {{
        let v: $crate::Vector3<_> = $crate::traits::LocalInto::local_into(($($item,)+));
        v
    }};
}

#[macro_export]
macro_rules! vec4 {
    ($item:expr) => {{
        $crate::Vector4::splat($item)
    }};
    ($($item:expr),* $(,)?) => {{
        let v: $crate::Vector4<_> = $crate::traits::LocalInto::local_into(($($item,)+));
        v
    }};
}
