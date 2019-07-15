# Side Notes #

Quickly display your quick side notes in quick side window.

Side notes live in a file in the current directory or any parent directory
thereof. The filename to look for is defined by custom option
side-notes-file, which defaults to "notes.txt".

## Installation ##

Add something like the following to your init file:

    (define-key (current-global-map) (kbd "M-s n")
      #'side-notes-toggle-notes)
