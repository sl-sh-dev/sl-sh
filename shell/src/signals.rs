use std::sync::atomic::{AtomicBool, Ordering};

use nix::{
    libc,
    sys::signal::{self, sigaction, SigHandler, Signal},
};

static SIG_INT: AtomicBool = AtomicBool::new(false);

extern "C" fn sig_int_handle(_: libc::c_int, _: *mut libc::siginfo_t, _: *mut libc::c_void) {
    SIG_INT.store(true, Ordering::Relaxed);
}

pub fn install_sigint_handler() -> bool {
    let result = unsafe {
        let sig_action = signal::SigAction::new(
            signal::SigHandler::SigAction(sig_int_handle),
            signal::SaFlags::empty(),
            signal::SigSet::empty(),
        );
        sigaction(signal::SIGINT, &sig_action)
    };

    if let Err(errno) = result {
        eprint!("ERROR Failed to install SIGINT handler due to: ");
        eprintln!("{} ({}).", errno.desc(), errno);
        false
    } else {
        true
    }
}

pub fn mask_signals() {
    /* Ignore interactive and job-control signals.  */
    unsafe {
        // Do not ignore SIGINT because we want the thread below to handle it (block it instead).
        //signal::signal(Signal::SIGINT, SigHandler::SigIgn).unwrap();
        signal::signal(Signal::SIGQUIT, SigHandler::SigIgn).unwrap();
        signal::signal(Signal::SIGTSTP, SigHandler::SigIgn).unwrap();
        signal::signal(Signal::SIGTTIN, SigHandler::SigIgn).unwrap();
        signal::signal(Signal::SIGTTOU, SigHandler::SigIgn).unwrap();
        // Ignoring sigchild will mess up waitpid and cause Command::spawn to panic under some conditions.
        //signal::signal(Signal::SIGCHLD, SigHandler::SigIgn).unwrap();
    }
}

pub fn test_clear_sigint() -> bool {
    SIG_INT.swap(false, Ordering::Relaxed)
}
