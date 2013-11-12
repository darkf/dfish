**dfish** (DarkF's Interpreting SHell)

A minimal POSIX-like shell written in Haskell.

Example usage:

    ./dfish
    $ echo hi
    hi
    $ echo there > foo.txt
    $ echo hi | cat - foo.txt
    hi
    there
    $ quit

It supports running commands with arguments, command piping (`|`) and file redirection (`<` to pipe to a command's input, `>` to pipe a command's output to a file).

Arguments and operators must be separated with a space.

You may run the shell with the `-c` argument and supply a command after it to immediately execute the command and return, for example: `./dfish -c "echo hi"`.
