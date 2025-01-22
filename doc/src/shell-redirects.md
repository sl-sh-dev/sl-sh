Still figuring this out currently allows things like this (syntax will probably change):

Current iteration uses :< (same as :0<) to connect a file stdin and :> (same as :1>) to connect a file to stdout. These can also be used with file descriptors (for example :2> will connect a file to stderr). They can be mixed with other shell redirects and will just become part of the redirect stack. Each lisp redirect will return a file in a list (first element will be the PID of the final process).

Examples:
(let ([pid, out] (sh "ls" :>)) (iter::for l in (iter::iter out) (pr l)))

(let ([pid, out, er] (sh "ls vm/src/ sdsdf" :> :2>))(prn "PID: " pid) (iter::for l in (iter::iter out)(pr "from out: " l))(iter::for l in (iter::iter er)(pr "from err: " l)

Set the lisp pipe then use a shell redirect to also send stderr to the same pipe:
(let ([pid, out] (sh "ls vm/src/ sdsdf" :> "2>&1"))(prn "PID: " pid) (iter::for l in (iter::iter out)(pr "from grep: " l)))

(let ([pid, in, out] (sh :< "grep XX" :>))(prn "PID: " pid) (fprn in "XXsls")(fprn in "sls")(fprn in "dfdXX")(fclose in)(iter::for l in (iter::iter out)(pr "from grep: " l)))

(same as above but include a pipe between shell commands)
(let ([pid, in, out] (sh :< "grep XX | cat -" :>)) (fprn in "XXsls")(fprn in "sls")(fprn in "dfdXX")(fclose in)(iter::for l in (iter::iter out)(pr l)))
