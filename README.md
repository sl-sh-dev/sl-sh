# Simple Lisp Shell

This is a shell build around a simple version of lisp for scripting.  The prompt
also follows this pattern (with the exception that you can leave out the outer
parentheses).  It is NOT a POSIX shell and makes to attempts to be one.

It supports quote and backquote (with , and ,@ expansion).

It supports Common Lisp style keyword symbols with colon.

The config directory is \~/.config/sl-sh.  The binary will have a built in config (lisp/slshrc),
you can put your own slshrc file the config directory (\~/.config/sl-sh).
See the file slshrc.example (at least look at this one), lisp/slshrc (this is the built in config)
or the contrib directory for example configs (contrib/gpwclark/ contains an example of using bash
completions with sl-sh as well as other handy things).  The other files in lisp/ are also built
into the binary but versions can be copied to \~/.config/sl-sh and those will be used instead.
These files contain the lisp code for the shell (anything from the tables below that is not builtin).

## Building

* `cargo build --release`

## Tasks
- [ ] Test scripts to exercise everything.
- [ ] Better Docs.

## Examples
Currently sparse but if you grep lisp/ and contrib/ for a form you want an example of you will probably find one.

## ~/.config/sl-sh/slshrc extensions
The following section details functionality to extend and modify the behavior of
the shell.

### Prompt/PS1
The command prompt for the shell is customize-able via the `__prompt` function. The
function takes no arguments and expects a string to be returned. The canonical
prompt in sl-sh would simple be:
```
	(defn __prompt()
		(str "$ "))
```

### Command processing
sl-sh offers two "hooks" for intercepting commands being executed:
`__completion_hook` and `__exec_hook`.
- `__completion_hook` is used to aid in tab completions. It takes a varargs and
expects a list of string to be returned. For convenience 'path and 'default
are also allowable return values. 'path represents the list of paths and
'default is 'path concatenated with all valid sl-sh forms.
- `__exec_hook` is an intercept function for every command sent to sl-sh. The
function takes a string and expects a string or list to be returned. The string
returned will be evaluated as a sl-sh form and a list will just be evaluated.
This is useful if you want to check all input and modify said input if it meets
certain conditions. For instance, if one were to decide that inputting any
valid path to sl-sh should result in the shell changing the current working
directory to that directory:
```
	(defn change-dir-if-arg-is-dir (cmd)
			(if (fs-dir? cmd)
				(str "cd " cmd)
				cmd))

	(defn __exec_hook (cmd-to-execute)
		(let ((args-list (str-split " " cmd-to-execute)))
				(match (length args-list)
					(1 (change-dir-if-arg-is-dir (first args-list)))
					(nil cmd-to-execute))))
```


### Readline Functionality
sl-sh uses a readline-like library to make using the shell ergonomic. Like bash
there are two "modes" vi and emacs, the default is emacs. Setting the mode
explicitly to emacs:
```
	(hash-set! *repl-settings* :keybindings :emacs)
```
Or setting the mode explicitly to vi:
```
	(hash-set! *repl-settings* :keybindings :vi)
```
Setting the max number of history items (default 1000):
```
	(hash-set! *repl-settings* :max-history 1000)
```

#### vi mods

For the convenience of vi users the vi escape char can be changed:
```
	(hash-set! *repl-settings* :vi_esc_sequence '("jk" 200))
```
The first argument is the list of chars that indicate the esc sequence and the
seconds argument is the number of ms the readline library will wait to receive
the full escape sequence.

Because vi uses modal editing and because modal editing has state the readline
library allows modifying the last line of PS1 in any way the user desires The
following four settings apply:
```
	(hash-set! *repl-settings* :vi-insert-prompt-prefix "")
	(hash-set! *repl-settings* :vi-insert-prompt-suffix "")
	(hash-set! *repl-settings* :vi-normal-prompt-prefix "")
	(hash-set! *repl-settings* :vi-normal-prompt-suffix "")
```
In practice it is useful to extend the last line of PS1 in one or both of vi's
editing modes to be prefixed or suffixed with strings To provide context to
the user. The -suffix commands are especially useful if using foreground or
background color codes and restoration to the default is desired.

### Look and Feel
If syntax highlighting is desired it can be explicitly turned on:
```
	(syntax-on)
```
The distinction is made between, sl-sh forms, invalid commands, valid
executables, and any token that is not a sl-sh form and not the first string
input as a command.

Default colors are as follows:
```
	(defq tok-slsh-form-color shell::*fg-blue*)
	(defq tok-slsh-fcn-color shell::*fg-cyan*)
	(defq tok-default-color shell::*fg-default*)
	(defq tok-sys-command-color shell::*fg-white*)
	(defq tok-string-color shell::*fg-magenta*)
	(defq tok-invalid-color shell::*fg-red*)
```

The function `set-tok-colors` can be used to modify these, the function takes
six arguments defined in the order the defaults are listed above.

#### Color

The following
colors are defined in the shell namespace:
```
	(def '*fg-default* "\x1b[39m")
	(def '*fg-black* "\x1b[30m")
	(def '*fg-red* "\x1b[31m")
	(def '*fg-green* "\x1b[32m")
	(def '*fg-yellow* "\x1b[33m")
	(def '*fg-blue* "\x1b[34m")
	(def '*fg-magenta* "\x1b[35m")
	(def '*fg-cyan* "\x1b[36m")
	(def '*fg-white* "\x1b[37m")

	(def '*bg-default* "\x1b[49m")
	(def '*bg-black* "\x1b[40m")
	(def '*bg-red* "\x1b[41m")
	(def '*bg-green* "\x1b[42m")
	(def '*bg-yellow* "\x1b[43m")
	(def '*bg-blue* "\x1b[44m")
	(def '*bg-magenta* "\x1b[45m")
	(def '*bg-cyan* "\x1b[46m")
	(def '*bg-white* "\x1b[47m")
```
Functions that take rgb values are also defined `(fg-color-rgb)` and
`(bg-color-rgb)`. For example:
```
	(println
		(str
			(fg-color-rgb 255 0 0)
			(bg-color-rgb 0 0 255)
			"I have red text and a blue background"
			shell::*fg-default*
			shell::*bg-default*))
```
Prints this "truth-telling" string while also not changing any chars printed after
by setting the fg and bg back to the default.

### Error reporting
Use
```
	(error-stack-on)
```
if stack traces are desired. The default is
```
	(error-stack-off)
```

## Available forms:

Note that builtins are somewhat stable but things (macros, etc) are more likely to change (some macros
are just tossed together and need replacing- match to cond for instance).

### Core Forms
Form | Args | Type | description
-----|------|------|------------
eval | Form or string to evaluate | builtin | 
fncall | fn form+ | builtin | Calls the first argument (lambda or builtin function) with the rest of the args.
apply | fn form* list | builtin | Calls the first argument (lambda or builtin function) with the rest of the args and spreads the final arg out (must be a list).
unwind-protect | form/form* | builtin | Evals the first form and returns it's result, all of the other forms will eval even if the first form error's out.
err | string | builtin | Raises an error with the provided string as it's message.
load | | builtin |
if | | builtin |
print | | builtin |
println | | builtin |
eprint | | builtin |
eprintln | | builtin |
format | | builtin |
progn | forms+ | builtin | Runs each form in turn left to right.
def | symbol/value | builtin | Creates and sets a value into a symbol in the current scope.
undef | symbol | builtin | Removes the symbol from the current scope (does not try any other scope if not in current).
set | symbol/value | builtin | Changes the value of an existing symbol in first enclosing scope.  Use quote to set a symbol directly (see setq).
fn | args_form/body | builtin | Defines a lambda, has to be set into a symbol to have a name (see defn).
length | form | builtin | Returns the length of the provided object.
let | | macro |
quote | | builtin |
spawn | | builtin | Currently unavailable.  Use run-bg for background processes.
and | form* | builtin | Evaluate each form left to right and stop on a nil (produce nil). Produce the last form's value if no nils.  Produce true on no arguments.
or | form* | builtin | Evaluate each form left to right and produce the first non-nil result (stop evaluating). Produce nil on no arguments.
not | | builtin |
null | | builtin |
def? | symbol | builtin | Return true if symbol is defined for current scope.
macro | | builtin |
defmacro | | macro | Defines a macro and puts in a symbol (def's it).
setmacro | | macro | Defines a macro and update's an existing symbol to reference it (set's it).
expand-macro | | builtin |
recur | | builtin |
gensym | | builtin |
error-stack-on | | builtin | Print the eval stack on error.
error-stack-off | | builtin | Do not print the eval stack on error.
get-error | form* | builtin | Like progn but on error return #(:error msg).
global-scope? | | builtin | Is code running in the global (root) scope.
to-symbol | form | builtin | Converts a string, int or float to a symbol.
loose-symbols | form* | builtin | Allow loose symbols for any forms run under it (like the repl).
dyn | symbol value form | Sets dynamic var to symbol to value for the execution of form.
'=' | | builtin |
'>' | | builtin |
'>=' | | builtin |
'<' | | builtin |
'<=' | | builtin |
setq | symbol/value | macro | Same as set but it quotes the parameter name for you ("set 'xx '(1 2 3)" == "setq xx '(1 2 3).
defn | name/args_form/body | macro | Define a lambda.
setfn | name/args_form/body | macro | Define a lambda and assign it to an existing symbol.
loop | | macro |
dotimes | | macro |
dotimesi | | macro |
for | | macro |
fori | | macro |


### Namespace Forms
Form | Args | Type | description
-----|------|------|------------
ns-create | name | builtin | Create and enter a new namespace.
ns-enter | name | builtin | Enter an existing namespace.
ns-exists? | name | builtin | True if name is a namespace, nil otherwise.
ns-list | | builtin | Returns a vector of all the namespace names (strings).
ns-import | namespace (string) | macro | Bring all the exported symbols from a namespace into the current namespace.
ns-export | symbol or list of symbols| macro | Make the provided symbols importable.

### Type Forms
These forms provide information/tests about an objects underlying type.

Form | Args | Type | description
-----|------|------|------------
type | obj | builtin | Produces the string representation of objects type.
nil? | obj | builtin | True if obj is the nil type/false otherwise.
true? | obj | builtin | True if obj is the true type/false otherwise.
float? | obj | builtin | True if obj is the float type/false otherwise.
int? | obj | builtin | True if obj is the int type/false otherwise.
symbol? | obj | builtin | True if obj is the symbol type/false otherwise.
string? | obj | builtin | True if obj is the string type/false otherwise.
lambda? | obj | builtin | True if obj is the lambda type/false otherwise.
macro? | obj | builtin | True if obj is the macro type/false otherwise.
vec? | obj | builtin | True if obj is the vec type/false otherwise.
pair? | obj | builtin | True if obj is the pair type/false otherwise.
builtin? | obj | builtin | True if obj is the builtin (special form) type/false otherwise.
process? | obj | builtin | True if obj is the process type/false otherwise.
file? | obj | builtin | True if obj is the file type/false otherwise.
hash? | obj | builtin | True if obj is a hashmap/false otherwise.
list? | obj | builtin | True if obj is a pair that is a proper list (last pair in chain has a nil cdr).


### Pair/List (aka Cons List) Forms
Operations on the 'Pair' type (aka Cons Cell) that can be used to create
traditional Lisp list structures.  These are the default list structure and
are produced with bare parentheses in code.  These lists can also be created by 
building them up with joins or with the list form.

Builtins:

Form | Args | Type | description
-----|------|------|------------
join | obj/obj | builtin | Creates a Pair with the provided objects as car and cdr.
list | obj+ | builtin | Will produce a proper list with the provided objects as elements (last pair has a cdr of nil).
car | pair | builtin | Produces the car (first/left) element of a pair.
cdr | pair | builtin | Produces the cdr (second/right) element of a pair.
xar! | pair/obj | builtin | Modifies pair by settings it's car to obj, produces the modified pair.
xdr! | pair/obj | builtin | Modifies pair by settings it's cdr to obj, produces the modified pair.


### Vector Forms
Forms ending in '!' are destructive and change the underlying vector, other forms
do not make changes to the the provided vector.  They are usable in place of a
list for purposes of lambda calls, parameters, etc (they work the same as a list
made from pairs but are vectors not linked lists).  Use #() to declare them in
code (i.e. '#(1 2 3) or #(+ 1 2)).

Builtins:

Form | Args | Type | description
-----|------|------|------------
vec-empty? | vector | builtin | Returns true if the provided vector is empty, nil/false otherwise.
make-vec | capacity/default | builtin | Make a vector with capacity and all values set to default.  Both args are optional, default is nil.
vec-pop! | vector | builtin | Removes the last item from a vector and produces it.
vec-push! | vector/obj | builtin | Pushes the provided object onto the end of a vector.
vec-clear! | vector | builtin | Removes all elements from a vector.
vec | obj+ | builtin | Produces a vector with provided objects as elements.
vec-nth | int vector | builtin | Produces the element at the provided index (0 based), error if index is out of bounds.
vec-insert-nth! | index/obj/vector | builtin | Inserts a new element at index, all other elements shift right (ie is additive).
vec-remove-nth! | index/vector | builtin | Removes the element at index from a vector (all other elements will shift left).
vec-setnth! | index/form/vector | builtin | Sets the nth element of a vector with the provided object.
vec-slice | vector/start/end | builtin | Returns a new vector containing elements start (inclusive) so end (exclusive).


### Macros that work on vectors or lists
These macros will work on either a vector or a pair made into a proper list
(cons list).  Use these in preference to the vector/list specific versions when
possible (ie first vs car).
NOTE: list on this table can be a vector or a list.

Form | Args | Type | description
-----|------|------|------------
append | 2+ lists | macro | Produces a new list (type will be same as first parameter) by appending the other lists onto the first.
append! | 2+ lists | macro | Modifies the first list by appending the other lists onto it.
butlast | list | macro | Produces the provided list minus the last element.  Nil if the list is empty or one element.
copy-seq | list | macro | Produces a copy of the provided list (copy has same type as the parameter).
first | list | macro | Produces the first element of the provided list.  Nil if the list is empty.
last | list | macro | Produces the last element in the list.  Nil if the list is empty.
map | lambda list | macro | Returns a new list made by applying the lambda to each item in the provided list.
map! | lambda list | macro | Modifies a list by applying the lambda to each item in the list.
nth | int list | builtin | Produces the element at the provided index (0 based), error if index is out of bounds.
rest | list | macro | Produces the provided list minus the first element.  Nil if the list is empty or one element.
reverse | list | macro | Returns a new list made by reversing the elements of the provided list.
reverse! | list | macro | Modifies a list by reversing it's elements.
seq? | obj | macro | Return true if the obj is a sequence.
setnth! | idx/obj/list | macro | Sets idx item in the vector or list to obj, produces nil or errors on invalid input.


### HashMap Forms
Form | Args | Type | description
-----|------|------|------------
hash-clear! | | builtin |
hash-get | | builtin |
hash-haskey | | builtin |
make-hash | |builtin |
hash-keys | | builtin |
hash-remove! | | builtin |
hash-set! | | builtin |


### String Forms
Form | Args | Type | description
-----|------|------|------------
str | form* | builtin | Creates a new string with the values of it's arguments.  Any command's run under it will have stdout captured as a string.
str-trim | string | builtin | Trims both left and right on string.
str-ltrim | string | builtin | Left trims string.
str-rtrim | string | builtin | Right trims string.
str-replace | string/old/new | builtin | Produces a new string by replacing all occurrences of old with new.
str-split | split_string/string | builtin | Produces a list by splitting the string on the provided split_string.
str-rsplit | split_string/string | builtin | Produces a list by splitting the string on the provided split_string in reverse order.
str-splitn | n/split_string/string | builtin | Produces a list by splitting the string on the provided split_string with at most n items.
str-rsplitn | n/split_string/string | builtin | Produces a list by splitting the string on the provided split_string with at most n items in reverse order.
str-cat-list | string/list | builtin | Produces a string by joining a list with the provided string as a divider.
str-sub | index/length/string | builtin | Returns a new substring of provided string.
str-append | string string | builtin | Returns a new string from appending two other strings.
str-empty? | string | builtin | Returns true if the string is empty, false (nil) otherwise.
str-nth | int string | builtin | Returns the character at the nth position.
str-lower | string | builtin | Return the all lowercase string for provided string.
str-upper | string | builtin | Return the all uppercase string for provided string.
str-bytes | string | builtin | Return the bytes in a string (can be more then the chars- utf8).
str-starts-with | string string | builtin | Return true if the second string starts with the first.
str-contains | string string | builtin | Return true if the second string contains the first.
str-map | lambda string | builtin | Creates a new string by applying the lambda to each character of the provided string (or string buffer).
str-buf | form* | builtin | Creates a new string buffer with the values of it's arguments.  Any command's run under it will have stdout captured as a string.  Use a string buffer to build strings without excess allocations.
str-buf-push! | form* | builtin | First form is a string buffer that the following forms are appended to.
str-buf-clear! | string buffer | builtin | Clear the provided string buffer.
str-buf-map | lambda string | builtin | Creates a new string buffer by applying the lambda to each character of the provided string (or string buffer).


### Char Forms
Form | Args | Type | description
-----|------|------|------------
char= | char+ | builtin | Return true of all provided chars are equal.
char!= | char+ | builtin | Return true of all provided chars are not equal.
char> | char+ | builtin | Return true of each provided char is greater then the next.
char< | char+ | builtin | Return true of each provided char is less then the next.
char>= | char+ | builtin | Return true of each provided char is greater then or equal to the next.
char<= | char+ | builtin | Return true of each provided char is less then or equal to the next.
char-lower | char | builtin | Return the ascii lowercase char for char or char if not an ascii uppercase.
char-upper | char | builtin | Return the ascii uppercase char for char or char if not an ascii lowercase.
char-whitespace? | char | builtin | Return true if char is whitespace, false/nil otherwise.


### Shell Forms
Forms to do shell operations like file tests, pipes, redirects, etc.

Form | Args | Type | description
-----|------|------|------------
cd | path | builtin (builtins_file.rs) | Change to provided directory.
fs-exists? | path | builtin (builtins_file.rs) | Boolean, does path exist.
fs-file? | path | builtin (builtins_file.rs) | Boolean, is path a file.
fs-dir? | path | builtin (builtins_file.rs) | Boolean, is path a directory.
glob | string+ | builtin (builtins_file.rs) | Glob expand each string argument and return a list of all files.
pipe | form+ | builtin (builtins_file.rs) | Creates a pipe (job) consisting of the provided forms.
wait | form | builtin (builtins_file.rs) | Waits for a pid to finish and returns the status code (fine to use on a process that was not in the background).
pid | form | builtin (builtins_file.rs) | Returns the pid of a form that resolves to a process.
export | symbol/string | builtin (builtins.rs) | Sets symbol as an environment variable to string.
unexport | symbol | builtin (builtins.rs) | Removes symbol as an environment variable.
jobs | | builtin (builtins.rs) | List running jobs and status (stopped/running).
bg | job_id | builtin (builtins.rs) | Make a stopped job run in the background (defaults to last stopped job or select by index from jobs form).
fg | job_id | builtin (builtins.rs) | Make a stopped job run in the foreground again (defaults to last stopped job or select by index from jobs form).
version | | builtin (builtins.rs) | Display the current version.
command | forms* | builtin (builtins.rs) | All forms run under this form will only execute system commands not lisp functions.
run-bg | form* | builtin (builtins.rs) | Any system commands started under this form will be in the background.
form | form* | builtin (builtins.rs) | Any forms run under this will not execute system commands, only lisp functions.
out> | file/form+ | macro | Redirect stdout for sub-forms to the file, this one truncates first.
out>> | file/form+ | macro | Redirect stdout for sub-forms to the file, this one appends.
err> | file/form+ | macro | Redirect stderr for sub-forms to the file, this one truncates first.
err>> | file/form+ | macro | Redirect stderr for sub-forms to the file, this one appends.
out-err> | file/form+ | macro | Redirect stdout and stderr for sub-forms to the file, this one truncates first.
out-err>> | file/form+ | macro | Redirect stdout and stderr for sub-forms to the file, this one appends.
out>null | form+ | macro | Redirect stdout for sub-forms to null.
err>null | form+ | macro | Redirect stderr for sub-forms to null.
out-err>null | form+ | macro | Redirect stdout and stderr for sub-forms to null.
\| | one or more forms | macro | Creates a pipe (job) consisting of the provided forms.
alias | new_name/command | macro | Defines an alias for commands (meant for executables not builtins).
pushd | path | lambda | Changes directory to path and saves old directory on directory stack.
popd | | lambda | Pops the last directory off directory stack and changes to it.
dirs | | lambda | Display the directory stack.
get-dirs | | lambda | Returns the vector that contains the pushd/popd stack (newest item is at end).
clear-dirs | | lambda | Clears the directory stack.
set-dirs-max | max | lambda | Sets the maximum number of dirs to keep in stack (default 20), must be greater then 1.
let-env | list/commands | macro | Sets environment variables that are reset once the macro is done.  Uses the same conventions as let.


### File IO Forms
Forms to read and write files.

Form | Args | Type | description
-----|------|------|------------
open | file-name options* | builtin | Open the given file, see table below for open options.
close | file | builtin | Close the file, if a file has multiple references they all must be closed.  Going out of scope also closes the file.
flush | file | builtin | Flush the file to disk.
read-line | file | builtin | Reads and returns a line.  Return nil if the file is at EOF.
read | file | builtin | Reads the file and parses it into an Expression (optional keyword :add-parens will add outer parens is needed).
write-line | file line | builtin | Writes the line, adds a newline at end.
write-string | file string | builtin | Writes the string, does not add a newline at end.

Options to open, one or more of these can be added to open after the filename.
A file can only be opened for reading or writing (read is default).

Option | Description
-------|-----------
:read | Open file for reading, this is the default.
:write | Open file for writing.
:append | Open file for writing and append new data to end.
:truncate | Open file for write and delete all existing data.
:create | Create the file if it does not exist and open for writing.
:create-new | Create if does not exist, error if it does and open for writing.
:on-error-nil | If open has an error then return nil instead of producing an error.

Notes on closing.  Files will close when they go out of scope.  Using close will
cause a reference to a file to be marked close (removes that reference).  If
there are more then one references to a file it will not actually close until
all are released.  Close will also flush the file even if it is not the final
reference.  If a reference to a file is captured in a closure that can also keep
it open (closures currently capture the entire scope not just used symbols).


### Math Forms
Form | Args | Type | description
-----|------|------|------------
'+' | two or more ints or floats | builtin | Addition
'*' | two or more ints or floats | builtin | Multiplication
'-' | two or more ints or floats | builtin | Subtraction
'/' | two or more ints or floats | builtin | Division
