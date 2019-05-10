Side Notes
==========

Quickly display your quick side notes in quick side window.

Side notes live in a file defined by custom option `side-notes-file`,
which defaults to `notes.txt`. This file can be placed anywhere in the
current directory heirarchy (i.e. `default-directory` of any parent
directory).

Installation
------------

Add something like the following to your init file:

    (define-key (current-global-map) (kbd "M-s n") #'side-notes-toggle-notes)

Report Issues
-------------

Send an email to <~pwr/elisp@todo.sr.ht>.
