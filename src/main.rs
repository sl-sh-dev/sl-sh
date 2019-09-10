use std::io;

use ::slsh::*;

fn main() -> io::Result<()> {
    let config = get_config();
    if let Ok(config) = config {
        if config.command.is_none() {
            start_interactive();
        } else {
            let command = config.command.unwrap();
            if let Err(err) = run_one_command(&command, &config.args) {
                if let Err(err2) = run_one_script(&command, &config.args) {
                    eprintln!("Error running {}: {}, {}", command, err, err2);
                    return Err(err2);
                }
            }
        }
    }
    Ok(())
}
