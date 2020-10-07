extern crate anyhow;

fn main() -> anyhow::Result<()> {
    lispy::repl::run_lispy()
}
