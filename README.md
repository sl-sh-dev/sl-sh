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
- [ ] Add autocompletion hooks for custom completions.
- [ ] Finish job control.
- [ ] Test scripts to exercise everything.
- [ ] Better Docs.

## Available forms:

### Core Forms
Form | Args | Type | description
-----|------|------|------------
eval | Form or string to evalute | builtin | 
load | | builtin |
if | | builtin |
print | | builtin |
println | | builtin |
format | | builtin |
progn | forms+ | builtin | Runs each form in turn left to right.
def | symbol/value | builtin | Creates and sets a value into a symbol in the current scope.
set | symbol/value | builtin | Changes the value of an existing symbol in first enclosing scope.  Use quote to set a symbol directly (see setq).
fn | args_form/body | builtin | Defines a lambda, has to be set into a symbol to have a name (see defn).
let | | macro |
quote | | builtin |
spawn | | builtin | Currently unavailable.  Use run-bg for background processes.
and | | builtin |
or | | builtin |
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
command | | builtin |
run-bg | | builtin |
form | | builtin |
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


### List Forms
Currently slsh uses vectors not cons lists for its internal list structure.
It uses the first, rest, list names to help reinforce this fact.
These forms are non-destructive, they will not change any input lists.

Form | Args | Type | description
-----|------|------|------------
list | forms+ | builtin | Produces a list with provided forms as elements.
make-list | capacity/default | builtin | Make a list with capacity and all values set to default.  Both args are optional, default is nill.
first | list | builtin | Produces the first element of the provided list.  Nil if the list is empty.
rest | list | builtin | Produces the provided list minus the first element.  Nil if the list is empty or one element.
length | list/str | builtin | Returns the length of the provided list or string.
last | list | builtin | Produces the last element in the list.  Nil if the list is empty.
butlast | list | builtin | Produces the provided list minus the last element.  Nil if the list is empty or one element.
nth | int list | builtin | Produces the element at the provided index, error if index is out of bounds.
setfirst | form/list | builtin | Produces a new list with the provided form as the first element.
setrest | list/list | builtin | Produces a new list with the first element from then first list and the rest all the elements from the second.
setlast | list/form | builtin | Produces a new list with the provided form appended to the list.
setbutlast | list/list | builtin | Produces a new list with the last element from then second list and the rest all the elements from the first.
append | list/list | builtin | Produces a new list by appending the second onto the first.
is-empty | list | builtin | Returns true if the provided list is empty, nil/false otherwise.
map | lambda list | macro | Returns a new list made by applying the lambda to each item in the provided list.
reverse | list | macro | Returns a new list made by reversing the elements of the provided list.

### Destructive List Forms
These list forms will change the underlying list they work on.

Form | Args | Type | description
-----|------|------|------------
setnth | index form list | builtin | Produces a new list by replacing the element at index with then provided form, error if index is out of bounds.
pop | list | builtin | Pops the last elememt from the end of the list, list will be one element shorter.
push | list form | builtin | Pushes the provided form onto the end of the list.
clear | list | builtin | Removes all elements from list, leaves capacity the same.
remove-nth | index list | builtin | Removes the element at index and shifts all the remaining to the left.
insert-nth | index form list | builtin | Inserts the provided form into index and moves all other elements to the right.
map! | lambda list | macro | Modifies a list by applying the lambda to each item in the provided list.
reverse! | list | macro | Modifies a list by reversing it's elements.


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


### File Forms
Forms to do file tests, pipes, redirects, etc.  You probably want to use the 
macros not the builtins.

Form | Args | Type | description
-----|------|------|------------
cd | path | builtin | Change to provided directory.
use-stdout | | builtin |
out-null | | builtin |
err-null | | builtin |
file-rdr | | builtin |
stdout-to | | builtin |
stderr-to | | builtin |
file-trunc | | builtin |
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
