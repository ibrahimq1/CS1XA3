#!/bin/bash

clear

date

git status

git diff > changes.log

grep TODO -nr * > todo.log

