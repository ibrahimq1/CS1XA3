#!/bin/bash

clear

whoami
date

git status

git diff > changes.log

grep TODO -nr * > todo.log

find . -name "*.hs" -exec ghc -fno-code {} \; 2>> error.log
