#!/bin/bash

emacs -Q --batch \
      --eval "(progn
            (add-to-list 'load-path \"$(pwd)\")
            (add-to-list 'load-path \"/home/jdyer/.emacs.d/elpa/async-20250325.509\"))" \
		-f batch-byte-compile bank-buddy.el

emacs -Q --batch \
  --eval "(progn
            (add-to-list 'load-path \"$(pwd)\"))" \
  -f batch-byte-compile bank-buddy-cat-mode.el

