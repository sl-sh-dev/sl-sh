(function() {
    var type_impls = Object.fromEntries([["shell",[["<details class=\"toggle implementors-toggle\" open><summary><section id=\"impl-Platform-for-Sys\" class=\"impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#34-428\">Source</a><a href=\"#impl-Platform-for-Sys\" class=\"anchor\">§</a><h3 class=\"code-header\">impl <a class=\"trait\" href=\"shell/platform/trait.Platform.html\" title=\"trait shell::platform::Platform\">Platform</a> for <a class=\"struct\" href=\"shell/platform/unix/struct.Sys.html\" title=\"struct shell::platform::unix::Sys\">Sys</a></h3></section></summary><div class=\"impl-items\"><details class=\"toggle method-toggle\" open><summary><section id=\"method.get_term_settings\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#40-42\">Source</a><a href=\"#method.get_term_settings\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.get_term_settings\" class=\"fn\">get_term_settings</a>(terminal: <a class=\"struct\" href=\"shell/platform/unix/struct.UnixFileDesc.html\" title=\"struct shell::platform::unix::UnixFileDesc\">UnixFileDesc</a>) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;<a class=\"struct\" href=\"shell/platform/unix/struct.UnixTermSettings.html\" title=\"struct shell::platform::unix::UnixTermSettings\">UnixTermSettings</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section></summary><div class=\"docblock\"><p>If terminal is a terminal then get it’s term settings.</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.restore_terminal\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#45-54\">Source</a><a href=\"#method.restore_terminal\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.restore_terminal\" class=\"fn\">restore_terminal</a>(\n    term_settings: &amp;<a class=\"struct\" href=\"shell/platform/unix/struct.UnixTermSettings.html\" title=\"struct shell::platform::unix::UnixTermSettings\">UnixTermSettings</a>,\n    shell_pid: <a class=\"struct\" href=\"shell/platform/unix/struct.UnixPid.html\" title=\"struct shell::platform::unix::UnixPid\">UnixPid</a>,\n) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.unit.html\">()</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section></summary><div class=\"docblock\"><p>Restore terminal settings and put the shell back into the foreground.</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.terminal_foreground\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#58-67\">Source</a><a href=\"#method.terminal_foreground\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.terminal_foreground\" class=\"fn\">terminal_foreground</a>(terminal: <a class=\"struct\" href=\"shell/platform/unix/struct.UnixFileDesc.html\" title=\"struct shell::platform::unix::UnixFileDesc\">UnixFileDesc</a>)</h4></section></summary><div class=\"docblock\"><p>Put terminal in the foreground, loop until this succeeds.\nUsed during shell startup.</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.set_self_pgroup\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#71-81\">Source</a><a href=\"#method.set_self_pgroup\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.set_self_pgroup\" class=\"fn\">set_self_pgroup</a>() -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.unit.html\">()</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section></summary><div class=\"docblock\"><p>Puts the running process into its own process group.\nDo this during shell initialization.</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.grab_terminal\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#85-89\">Source</a><a href=\"#method.grab_terminal\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.grab_terminal\" class=\"fn\">grab_terminal</a>(terminal: <a class=\"struct\" href=\"shell/platform/unix/struct.UnixFileDesc.html\" title=\"struct shell::platform::unix::UnixFileDesc\">UnixFileDesc</a>) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.unit.html\">()</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section></summary><div class=\"docblock\"><p>Grab control of terminal.\nUsed for shell startup.</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.anon_pipe\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#92-119\">Source</a><a href=\"#method.anon_pipe\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.anon_pipe\" class=\"fn\">anon_pipe</a>() -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;(<a class=\"struct\" href=\"shell/platform/unix/struct.UnixFileDesc.html\" title=\"struct shell::platform::unix::UnixFileDesc\">UnixFileDesc</a>, <a class=\"struct\" href=\"shell/platform/unix/struct.UnixFileDesc.html\" title=\"struct shell::platform::unix::UnixFileDesc\">UnixFileDesc</a>), <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section></summary><div class=\"docblock\"><p>Return the input and output file descriptors for an anonymous pipe.</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.close_fd\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#122-127\">Source</a><a href=\"#method.close_fd\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.close_fd\" class=\"fn\">close_fd</a>(fd: <a class=\"struct\" href=\"shell/platform/unix/struct.UnixFileDesc.html\" title=\"struct shell::platform::unix::UnixFileDesc\">UnixFileDesc</a>) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.unit.html\">()</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section></summary><div class=\"docblock\"><p>Close a raw Unix file descriptor.</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.foreground_job\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#317-342\">Source</a><a href=\"#method.foreground_job\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.foreground_job\" class=\"fn\">foreground_job</a>(\n    job: &amp;mut <a class=\"struct\" href=\"shell/jobs/struct.Job.html\" title=\"struct shell::jobs::Job\">Job</a>,\n    term_settings: &amp;<a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/option/enum.Option.html\" title=\"enum core::option::Option\">Option</a>&lt;<a class=\"struct\" href=\"shell/platform/unix/struct.UnixTermSettings.html\" title=\"struct shell::platform::unix::UnixTermSettings\">UnixTermSettings</a>&gt;,\n) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.unit.html\">()</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section></summary><div class=\"docblock\"><p>Move the job for job_num to te foreground.</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.background_job\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#345-353\">Source</a><a href=\"#method.background_job\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.background_job\" class=\"fn\">background_job</a>(job: &amp;mut <a class=\"struct\" href=\"shell/jobs/struct.Job.html\" title=\"struct shell::jobs::Job\">Job</a>) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.unit.html\">()</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section></summary><div class=\"docblock\"><p>Move the job for job_num to te background and running (start a stopped job in the background).</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.dup2_fd\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#356-360\">Source</a><a href=\"#method.dup2_fd\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.dup2_fd\" class=\"fn\">dup2_fd</a>(\n    src_fd: <a class=\"struct\" href=\"shell/platform/unix/struct.UnixFileDesc.html\" title=\"struct shell::platform::unix::UnixFileDesc\">UnixFileDesc</a>,\n    dst_fd: <a class=\"struct\" href=\"shell/platform/unix/struct.UnixFileDesc.html\" title=\"struct shell::platform::unix::UnixFileDesc\">UnixFileDesc</a>,\n) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;<a class=\"struct\" href=\"shell/platform/unix/struct.UnixFileDesc.html\" title=\"struct shell::platform::unix::UnixFileDesc\">UnixFileDesc</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section></summary><div class=\"docblock\"><p>Duplicate a raw file descriptor to another file descriptor.</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.getpid\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#363-365\">Source</a><a href=\"#method.getpid\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.getpid\" class=\"fn\">getpid</a>() -&gt; <a class=\"struct\" href=\"shell/platform/unix/struct.UnixPid.html\" title=\"struct shell::platform::unix::UnixPid\">UnixPid</a></h4></section></summary><div class=\"docblock\"><p>Get the current PID.</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.gethostname\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#368-370\">Source</a><a href=\"#method.gethostname\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.gethostname\" class=\"fn\">gethostname</a>() -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/option/enum.Option.html\" title=\"enum core::option::Option\">Option</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/ffi/os_str/struct.OsString.html\" title=\"struct std::ffi::os_str::OsString\">OsString</a>&gt;</h4></section></summary><div class=\"docblock\"><p>Get the current machines hostname if available.</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.current_uid\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#373-375\">Source</a><a href=\"#method.current_uid\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.current_uid\" class=\"fn\">current_uid</a>() -&gt; <a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.u32.html\">u32</a></h4></section></summary><div class=\"docblock\"><p>Get current UID of the process.</p>\n</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.effective_uid\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#378-380\">Source</a><a href=\"#method.effective_uid\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.effective_uid\" class=\"fn\">effective_uid</a>() -&gt; <a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.u32.html\">u32</a></h4></section></summary><div class=\"docblock\"><p>Get effective UID of the process.</p>\n</div></details><section id=\"associatedtype.Pid\" class=\"associatedtype trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#35\">Source</a><a href=\"#associatedtype.Pid\" class=\"anchor\">§</a><h4 class=\"code-header\">type <a href=\"shell/platform/trait.Platform.html#associatedtype.Pid\" class=\"associatedtype\">Pid</a> = <a class=\"struct\" href=\"shell/platform/unix/struct.UnixPid.html\" title=\"struct shell::platform::unix::UnixPid\">UnixPid</a></h4></section><section id=\"associatedtype.FileDesc\" class=\"associatedtype trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#36\">Source</a><a href=\"#associatedtype.FileDesc\" class=\"anchor\">§</a><h4 class=\"code-header\">type <a href=\"shell/platform/trait.Platform.html#associatedtype.FileDesc\" class=\"associatedtype\">FileDesc</a> = <a class=\"struct\" href=\"shell/platform/unix/struct.UnixFileDesc.html\" title=\"struct shell::platform::unix::UnixFileDesc\">UnixFileDesc</a></h4></section><section id=\"associatedtype.TermSettings\" class=\"associatedtype trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#37\">Source</a><a href=\"#associatedtype.TermSettings\" class=\"anchor\">§</a><h4 class=\"code-header\">type <a href=\"shell/platform/trait.Platform.html#associatedtype.TermSettings\" class=\"associatedtype\">TermSettings</a> = <a class=\"struct\" href=\"shell/platform/unix/struct.UnixTermSettings.html\" title=\"struct shell::platform::unix::UnixTermSettings\">UnixTermSettings</a></h4></section><section id=\"method.fork_run\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#129-161\">Source</a><a href=\"#method.fork_run\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.fork_run\" class=\"fn\">fork_run</a>(run: &amp;<a class=\"enum\" href=\"shell/command_data/enum.Run.html\" title=\"enum shell::command_data::Run\">Run</a>, job: &amp;mut <a class=\"struct\" href=\"shell/jobs/struct.Job.html\" title=\"struct shell::jobs::Job\">Job</a>, jobs: &amp;mut <a class=\"struct\" href=\"shell/jobs/struct.Jobs.html\" title=\"struct shell::jobs::Jobs\">Jobs</a>) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.unit.html\">()</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section><section id=\"method.fork_exec\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#163-245\">Source</a><a href=\"#method.fork_exec\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.fork_exec\" class=\"fn\">fork_exec</a>(\n    command: &amp;<a class=\"struct\" href=\"shell/command_data/struct.CommandWithArgs.html\" title=\"struct shell::command_data::CommandWithArgs\">CommandWithArgs</a>,\n    job: &amp;mut <a class=\"struct\" href=\"shell/jobs/struct.Job.html\" title=\"struct shell::jobs::Job\">Job</a>,\n    jobs: &amp;mut <a class=\"struct\" href=\"shell/jobs/struct.Jobs.html\" title=\"struct shell::jobs::Jobs\">Jobs</a>,\n) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.unit.html\">()</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section><section id=\"method.try_wait_pid\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#247-280\">Source</a><a href=\"#method.try_wait_pid\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.try_wait_pid\" class=\"fn\">try_wait_pid</a>(pid: <a class=\"struct\" href=\"shell/platform/unix/struct.UnixPid.html\" title=\"struct shell::platform::unix::UnixPid\">UnixPid</a>, job: &amp;mut <a class=\"struct\" href=\"shell/jobs/struct.Job.html\" title=\"struct shell::jobs::Job\">Job</a>) -&gt; (<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.bool.html\">bool</a>, <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/option/enum.Option.html\" title=\"enum core::option::Option\">Option</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.i32.html\">i32</a>&gt;)</h4></section><section id=\"method.wait_job\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#282-314\">Source</a><a href=\"#method.wait_job\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.wait_job\" class=\"fn\">wait_job</a>(job: &amp;mut <a class=\"struct\" href=\"shell/jobs/struct.Job.html\" title=\"struct shell::jobs::Job\">Job</a>) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/option/enum.Option.html\" title=\"enum core::option::Option\">Option</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.i32.html\">i32</a>&gt;</h4></section><section id=\"method.is_tty\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#382-384\">Source</a><a href=\"#method.is_tty\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.is_tty\" class=\"fn\">is_tty</a>(terminal: <a class=\"struct\" href=\"shell/platform/unix/struct.UnixFileDesc.html\" title=\"struct shell::platform::unix::UnixFileDesc\">UnixFileDesc</a>) -&gt; <a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.bool.html\">bool</a></h4></section><section id=\"method.set_rlimit\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#386-398\">Source</a><a href=\"#method.set_rlimit\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.set_rlimit\" class=\"fn\">set_rlimit</a>(rlimit: <a class=\"enum\" href=\"shell/platform/enum.RLimit.html\" title=\"enum shell::platform::RLimit\">RLimit</a>, values: <a class=\"struct\" href=\"shell/platform/struct.RLimitVals.html\" title=\"struct shell::platform::RLimitVals\">RLimitVals</a>) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.unit.html\">()</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section><section id=\"method.get_rlimit\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#400-415\">Source</a><a href=\"#method.get_rlimit\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.get_rlimit\" class=\"fn\">get_rlimit</a>(rlimit: <a class=\"enum\" href=\"shell/platform/enum.RLimit.html\" title=\"enum shell::platform::RLimit\">RLimit</a>) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;<a class=\"struct\" href=\"shell/platform/struct.RLimitVals.html\" title=\"struct shell::platform::RLimitVals\">RLimitVals</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section><details class=\"toggle method-toggle\" open><summary><section id=\"method.merge_and_set_umask\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#417-419\">Source</a><a href=\"#method.merge_and_set_umask\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.merge_and_set_umask\" class=\"fn\">merge_and_set_umask</a>(\n    current_umask: <a class=\"type\" href=\"shell/platform/type.mode_t.html\" title=\"type shell::platform::mode_t\">mode_t</a>,\n    mask_string: &amp;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.str.html\">str</a>,\n) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;<a class=\"type\" href=\"shell/platform/type.mode_t.html\" title=\"type shell::platform::mode_t\">mode_t</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section></summary><div class='docblock'>If mask_string is a mode string then merge it with umask and set the current umask.\nIf mask_string is an int then treat it as a umask and set the current umask (no merge)).</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.get_and_clear_umask\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#421-423\">Source</a><a href=\"#method.get_and_clear_umask\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.get_and_clear_umask\" class=\"fn\">get_and_clear_umask</a>() -&gt; <a class=\"type\" href=\"shell/platform/type.mode_t.html\" title=\"type shell::platform::mode_t\">mode_t</a></h4></section></summary><div class='docblock'>Clears the current umask and returns the previous umask.</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.set_umask\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform/unix.rs.html#425-427\">Source</a><a href=\"#method.set_umask\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#tymethod.set_umask\" class=\"fn\">set_umask</a>(umask: <a class=\"type\" href=\"shell/platform/type.mode_t.html\" title=\"type shell::platform::mode_t\">mode_t</a>) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.84.1/std/primitive.unit.html\">()</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section></summary><div class='docblock'>Set current umask to umask.</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.to_octal_string\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/shell/platform.rs.html#71-82\">Source</a><a href=\"#method.to_octal_string\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"shell/platform/trait.Platform.html#method.to_octal_string\" class=\"fn\">to_octal_string</a>(mode: <a class=\"type\" href=\"shell/platform/type.mode_t.html\" title=\"type shell::platform::mode_t\">mode_t</a>) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.84.1/core/result/enum.Result.html\" title=\"enum core::result::Result\">Result</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/alloc/string/struct.String.html\" title=\"struct alloc::string::String\">String</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/1.84.1/std/io/error/struct.Error.html\" title=\"struct std::io::error::Error\">Error</a>&gt;</h4></section></summary><div class='docblock'>Convert mode to the octal string umask format.</div></details></div></details>","Platform","shell::platform::Pid","shell::platform::FileDesc","shell::platform::TermSettings"]]]]);
    if (window.register_type_impls) {
        window.register_type_impls(type_impls);
    } else {
        window.pending_type_impls = type_impls;
    }
})()
//{"start":55,"fragment_lengths":[26020]}