# ERRORS

## Error Type
The error type consists of an identifing keywork and a data/payload value.  This
value is typically a string but can be any valid value.

(type [error]) returns :Error
The form (car [error]) returns the keyword identifing this error.
The form (cdr [error]) returns the data value for the error.
Note the use of car/cdr to pull apart an error even though it is not of the pair type.
Use (mk-err :[ID] value) to create an error type or (err :[ID] value) to "raise" an error (see below).

## Raising an error
Runtime errors will be "raised".  This means the program execution will halt and the debugger will be entered.  Currently this only allows examing the running state but will eventually include the ability to modify state and restart as other Lisps allow.  Code can raise an error with the err form, for instance (err :some-error-type "This is my error") will raise an error and interrupt the program.

Note, the (get-error FORM+) form can be used to programmaticly return a raised error instead of breaking to the debugger.

## Returning an error
Code can return an error instead of breaking into the debugger.  Use the (mk-err :[ERROR ID] vallue) to create an error and then use it as any other value (return it from a function for instance).  This may be appropraite for a common error that does not warrent breaking to the debugger.

## References
See the docs string for:
- err
- mk-err
- err?
- ok?
- get-error
