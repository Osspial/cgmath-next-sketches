fn main() {
    let input: &[(&str, usize)] = &[
        ("T", 1),
        ("Vector2<T>", 2),
        ("Vector3<T>", 3),
        ("Vector4<T>", 4),
    ];
    let output: &[(&str, usize)] = &[
        ("T", 1),
        ("Vector2<T>", 2),
        ("Vector3<T>", 3),
        ("Vector4<T>", 4),
    ];

    let mut valid_combinations = Vec::new();

    for (o, l) in output {
        let mut buf = Vec::new();
        macro_rules! check_buf {
            () => {{
                let len: usize = buf.iter().map(|&(_, i)| *i).sum();
                if len == *l {
                    valid_combinations.push((buf.clone(), o));
                }
            }};
        }
        for i0 in input {
            buf.push(i0);
            check_buf!();
            for i1 in input {
                buf.push(i1);
                check_buf!();
                for i2 in input {
                    buf.push(i2);
                    check_buf!();
                    buf.pop();
                    for i3 in input {
                        buf.push(i3);
                        check_buf!();
                        buf.pop();
                    }
                }
                buf.pop();
            }
            buf.pop();
        }
    }

    valid_combinations.dedup();
    for (c, o) in valid_combinations {
        print!("(");
        for b in &c {
            print!("{}, ", b.0);
        }
        println!(") -> {}", o);
    }
}