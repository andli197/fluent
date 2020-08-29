# fluent
Simple model for building commands in emacs to be executed using compile.

# features
* Add commands to be appended in front of the execution list, both programmatic and interactive
* Move commands in the execution list
* Execute commands over ssh
* Stores last executed command but not the ssh part nor the appended commands, to be able to run consecutive executions
* Possibility to add elisp-function calls and -variables for building commands (notation: {} everything within the curly braces are interpreted as elisp)
