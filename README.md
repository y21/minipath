# `minipath`
A minimal Rust type and path syntax parser, for when `syn` is too big.

## Usage
```rs
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = "<fn(*const dyn SomeTrait) -> ! as Function>::Output";
    let (_, path) = minipath::parse_path(input)?;
    println!("{path:#?}");

    Ok(())
}
```
