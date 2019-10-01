# Simple Lisp Shell

This is a shell build around a simple version of lisp for scripting.  The prompt
also follows this pattern (with the exception that you can leave out the outer
parentheses).  It is NOT a POSIX shell and makes to attempts to be one.

Slsh has enough job control to ctrl-z out of an app and fg back into it but it is not complete.

It support quote and backquote (with , and ,@ expansion).

To install you need to copy the two files from the lisp subdirectory to ~/.config/slsh (otherwise will not have any of the macros).
The shell config file is ~/.config/slsh/slshrc , see the file slshrc.example.

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
set | symbol/value | builtin | Sets something into the current scopes symbol table.  Use quote to set a symbol directly (see setq).
fn | args_form/body | builtin | Defines a lambda, has to be set into a symbol to have a name (see defn).
let | | builtin |
quote | | builtin |
spawn | | builtin | Currently unavailable.  Use run-bg for background processes.
and | | builtin |
or | | builtin |
not | | builtin |
null | | builtin |
is-def | | builtin |
defmacro | | builtin |
expand-macro | | builtin |
recur | | builtin |
gensym | | builtin |
jobs | | builtin |
fg | | builtin |
version | | builtin |
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

Form | Args | Type | description
-----|------|------|------------
list | forms+ | builtin | Produces a list with provided forms as elements.
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
setnth | int form list | builtin | Produces a new list by replacing the element at index with then provided form, error if index is out of bounds.
append | list/list | builtin | Produces a new list by appending the second onto the first.


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


### Math Forms
Form | Args | Type | description
-----|------|------|------------
'+' | two or more ints or floats | builtin | Addition
'*' | two or more ints or floats | builtin | Multiplication
'-' | two or more ints or floats | builtin | Subtraction
'/' | two or more ints or floats | builtin | Division
