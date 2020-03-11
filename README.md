# Side Notes #

Quickly display your quick side notes in quick side window.

Side notes live in a file in the current directory or any parent
directory thereof and is displayed in a side window with
`side-notes-toggle-notes`. The filename to look for is defined by user
option `side-notes-file`, which defaults to "notes.txt".

To really mix things up, there's the optional
`side-notes-secondary-file`, which, when non-nil, will display a
separate notes file in a lower side window when the command
`side-notes-toggle-notes` is prefixed with an argument (`C-u`).

For more info, see `(info "(elisp) Side Windows")`

## Installation ##

Install from [MELPA stable][] then add something like the following to
your init file:

    (define-key (current-global-map) (kbd "M-s n") #'side-notes-toggle-notes)

[melpa stable]: https://stable.melpa.org/#/side-notes
