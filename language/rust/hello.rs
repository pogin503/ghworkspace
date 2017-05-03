// 1 line comment
/* block comment */

//! Generate library docs for the enclosing item.
/// generate library docs for the following item.
fn main() {
    println!("hello world");
    println!("{} days", 31);

    println!("{0}, this is {1}, {1}, this is {0}", "Alice", "Bob");

    println!("{suspect} {verb} {object}",
             object="the lazy dog",
             suspect="the quick brown forx",
             verb="jumps over");

    println!("{}, binary: {:b}, ", 1, 2);
    println!("{number:>width$}", number=1, width=6);
    println!("{number:>0width$}", number=1, width=6);

}
