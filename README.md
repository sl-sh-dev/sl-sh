# Simple Lisp Shell

This is a shell build around a simple version of lisp for scripting.  The prompt
also follows this pattern (with the exception that you can leave out the outer
parentheses).  It is NOT a POSIX shell and makes to attempts to be one.

Slsh has enough job control to ctrl-z out of an app and fg back into it but it is not complete.

It support quote and backquote (with , and ,@ expansion).

To install you need to copy the two files from the lisp subdirectory to ~/.config/slsh (otherwise will not have any of the macros).
The shell config file is ~/.config/slsh/slshrc , see the file slshrc.example.

## Building

* `cargo build --release`

## Tasks
- [ ] Test scripts to exercise everything.
- [ ] Better Docs.

## Available forms:

### Core Forms
Form | Args | Type | description
-----|------|------|------------
eval | Form or string to evalute | builtin | 
fncall | fn form+ | builtin | Calls the first argument (lambda or builtin function) with the rest of the args.
apply | fn form* list | builtin | Calls the first argument (lambda or builtin function) with the rest of the args and spreads the final arg out (must be a list).
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
set | symbol/value | builtin | Changes the value of an existing symbol in first enclosing scope.  Use quote to set a symbol directly (see setq).
fn | args_form/body | builtin | Defines a lambda, has to be set into a symbol to have a name (see defn).
length | form | builtin | Returns the length of the provided object.
let | | macro |
quote | | builtin |
spawn | | builtin | Currently unavailable.  Use run-bg for background processes.
and | form* | builtin | Evaluate each form left to right and stop on a nil (produce nil). Produce the last form's value if no nils.  Produce true on no arguments.
or | form* | builtin | Evaluate each form left to right and produce the first non-nil result (stop evaluting). Produce nil on no arguments.
not | | builtin |
null | | builtin |
is-def | symbol | builtin | Return true if symbol is defined for current scope.
get-type | form | builtin | Evalute the form and return the type as a string.
macro | | builtin |
defmacro | | macro |
expand-macro | | builtin |
recur | | builtin |
gensym | | builtin |
jobs | | builtin | List running jobs and status (stopped/running).
bg | job_id | builtin | Make a stopped job run in the background (defaults to last stopped job or select by index from jobs form).
fg | job_id | builtin | Make a stopped job run in the foreground again (defaults to last stopped job or select by index from jobs form).
version | | builtin | Display the current version.
command | forms* | builtin | All forms run under this form will only execute system commands not lisp functions.
run-bg | form* | builtin | Any system commands started under this form will be in the background.
form | form* | builtin | Any forms run under this will not execute system commands, only lisp functions.
'=' | | builtin |
'>' | | builtin |
'>=' | | builtin |
'<' | | builtin |
'<=' | | builtin |
setq | symbol/value | macro | Same as set but it quotes the parameter name for you ("set 'xx '(1 2 3)" == "setq xx '(1 2 3).
defn | name/args_form/body | macro | Define a lambda.
loop | | macro |
dotimes | | macro |
dotimesi | | macro |
for | | macro |
fori | | macro |


### Type Forms
These forms provide information/tests about an objects underlying type.

Form | Args | Type | description
-----|------|------|------------
type | obj | builtin | Produces the string representation of objects type.
is-nil | obj | builtin | True if obj is the nil type/false otherwise.
is-true | obj | builtin | True if obj is the true type/false otherwise.
is-float | obj | builtin | True if obj is the float type/false otherwise.
is-int | obj | builtin | True if obj is the int type/false otherwise.
is-symbol | obj | builtin | True if obj is the symbol type/false otherwise.
is-string | obj | builtin | True if obj is the string type/false otherwise.
is-lambda | obj | builtin | True if obj is the lambda type/false otherwise.
is-macro | obj | builtin | True if obj is the macro type/false otherwise.
is-vec | obj | builtin | True if obj is the vec type/false otherwise.
is-pair | obj | builtin | True if obj is the pair type/false otherwise.
is-builtin | obj | builtin | True if obj is the builtin (special form) type/false otherwise.
is-process | obj | builtin | True if obj is the process type/false otherwise.
is-file | obj | builtin | True if obj is the file type/false otherwise.
is-proper-list | obj | builtin | True if obj is a pair that is a proper list (last pair in chain has a nil cdr).


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
Currently slsh uses vectors not cons lists for its internal list structure.
Forms ending in '!' are destructive and change the underlying vector other forms
do not make changes to the the provided vector.  They are usable in place of a
list for purposes of lambda calls, parameters, etc (they work the same as a list
made froms pairs but are vectors not linked lists).  Use #() to declare them in
code (i.e. '#(1 2 3) or #(+ 1 2)).

Builtins:

Form | Args | Type | description
-----|------|------|------------
is-empty | vector | builtin | Returns true if the provided vector is empty, nil/false otherwise.
make-vec | capacity/default | builtin | Make a vector with capacity and all values set to default.  Both args are optional, default is nill.
pop! | vector | builtin | Removes the last item from a vector and produces it.
push! | vector/obj | builtin | Pushes the provided object onto the end of a vector.
vclear! | vector | builtin | Removes all elements from a vector.
vec | obj+ | builtin | Produces a vector with provided objects as elements.
vnth | int vector | builtin | Produces the element at the provided index (0 based), error if index is out of bounds.
vinsert-nth! | index/obj/vector | builtin | Inserts a new element at index, all other elements shift right (ie is additive).
vremove-nth! | index/vector | builtin | Removes the element at index from a vector (all other elements will shift left).
vsetnth! | index/form/vector | builtin | Sets the nth element of a vector with the provided object.


### Macros that work on vectors or lists
These macros will work on either a vector or a pair made into a proper list
(cons list).  Use these in preference to the vector/list specific versions when
possible (ie first vs car).
NOTE: list on this tables can be a vector or a list.

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
setnth! | idx/obj/list | macro | Sets idx item in the vector or list to obj, produces nil or errors on invalid input.


### String Forms
Form | Args | Type | description
-----|------|------|------------
str-trim | string | builtin | Trims both left and right on string.
str-ltrim | string | builtin | Left trims string.
str-rtrim | string | builtin | Right trims string.
str-replace | string/old/new | builtin | Produces a new string by replacing all occurances of old with new.
str-split | split_string/string | builtin | Produces a list by splitting the string on the provided split_string.
str-cat-list | string/list | builtin | Produces a string by joining a list with the provided string as a divider.
str-sub | index/length/string | builtin | Returns a new substring of provided string.
str-append | string string | builtin | Returns a new string from appending two other strings.


### File Forms
Forms to do file tests, pipes, redirects, etc.  You probably want to use the 
macros not the builtins.

Form | Args | Type | description
-----|------|------|------------
cd | path | builtin | Change to provided directory.
path-exists | path | builtin | Boolean, does path exist.
is-file | path | builtin | Boolean, is path a file.
is-dir | path | builtin | Boolean, is path a directory.
pipe | form+ | builtin | Creates a pipe (job) consisting of the provided forms.
wait | form | builtin | Waits for a pid to finish and returns the status code (fine to use on a process that was not in the background).
pid | form | builtin | Returns the pid of a form that resolves to a process.
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
pushd | path | macro | Changes directory to path and saves old directory on directory stack.
popd | | macro | Pops the last directory off directory stack and changes to it.
dirs | | macro | Display the directory stack.
clear-dirs | | macro | Clears the directory stack.
set-dirs-max | max | macro | Sets the maximum number of dirs to keep in stack (default 20), must be greater then 1.


### File IO Forms
Forms to read and write files.

Form | Args | Type | description
-----|------|------|------------
open | file-name options* | builtin | Open the given file, see table below for open options.
close | file | builtin | Close the file, if a file has multiple references they all must be closed.  Going out of scope also closes the file.
flush | file | builtin | Flush the file to disk.
read-line | file | builtin | Reads and returns a line.  Return nil if the file is at EOF.
read | file | builtin | Reads the file and parses it into an Expression.
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
