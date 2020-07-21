extern crate anyhow;

fn main() -> anyhow::Result<()>{
    lispy::repl::do_repl();
    Ok(())
}
