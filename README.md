# fluent
Simple model for building commands in emacs to be executed using compile.

# features
* Add commands to be appended in front of the execution list, both programmatic and interactive
* Move commands in the execution list
* Execute commands over ssh
* Stores last executed command but not the ssh part nor the appended commands, to be able to run consecutive executions
* Possibility to add elisp-function calls and -variables for building commands (notation: {} everything within the curly braces are interpreted as elisp)

## Usage
Add commands using 'fluent-add or 'fluent-add-interactive and call
'fluent-compile for compiling the current list of commands in 'compile,
stacked with double ampersands. The commands are executed in the order
they were added. 
for instance:
(fluent-add "cmake --build . --target all")
(fluent-add "ctest -j8")
(fluent-compile)
would result in calling compile with
"cmake --build . --target all && ctest -j8"

If a command needs modification, invoke 'fluent-modify for interactive
selection and modification of the command. Two commands may be switched
with 'fluent-switch-two-commands. To remove commandsm invoke
'fluent-remove-command and to clear the execution
list invoke 'fluent-clear.

It is possible to add hooks to functions to call generating strings to
be appended before the compilation, adding callbacks to
'fluent-prepend-compilations-commands. Adding:
(push "whoami" 'fluent-prepend-compilations-commands)
(push "uptime" 'fluent-prepend-compilations-commands)
would result in the command "whoami && uptime" would be prepended to
the full compilation string. Useful if working in an environment
using modules for instance.

It is also possible to make the compilation call ssh {build-host} and
give the full command as argument to ssh. Setting build host is done
using 'fluent-set-remote-host-interative or 'fluent-set-remote-host
and toggling the option is done using 'fluent-toggle-remote-compile

A command may contain an expression surrounded with {}. Those expressions
will be interpreted as elisp expressions and evaluated at compile time.

When compiling a command that command will be stored in 'fluent--last-command
and the fluent-recompile will invoke compilation using the
'fluent--last-command as input. The pre-compilation parts and remote parts
are not stored there, so they is re-evaluated at compilation. This means
that a command that is executed over ssh may be invoked locally using
recompile. Also, the last command is storing the elisp-expression that is
to be evaluated at compilation. This is useful when, for instance,
compiling code using cmake and different build-types. Then the build-type
may be stored as a lisp-function and that function is returning the
build type. That way the command for compiling different build types
can be the same.

Single commands stored in the execution list may be executed using
'fluent-compile-single-command.
It is possible to use fluent in single-execution-mode which is enabled using
'fluent-toggle-single-command-execution. This is causing adding of commands
to be invoked direct instead of beeing stored in the execution list.
