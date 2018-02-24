#!/bin/bash

clear

date
echo -n "User: " ; whoami

echo " "
echo -e "### \e[1mAny recent changes are in the changes.log file\e[0m"

git diff > changes.log 

echo " "
echo -e "### \e[1mThe #TODO tasks for today can be found in todo.log\e[0m"
 
grep -r --exclude={\*.log,ProjectAnalyze.sh}  "#TODO" . > todo.log

echo " "
echo -e "### \e[1mAny error in haskell files can be found in error.log\e[0m"

find  . -name "*.hs" -exec ghc -fno-code {} \; &> error.log

echo " "


count=0
max=5



echo "Which log file do you want to see? [Type 1,2,3,q(to exit) or write their names ex: todo.log]"

while [ $count -lt $max ]
do
count=$((count + 1))
read option

case $option in
	changes|changes.log|1)
			vim changes.log
			;;
	todo|todo.log|2)
			vim todo.log
			;;
	error|error.log|3)
			vim error.log
			;;
	esc|exit|x|Exit|Esc|q)
			exit 1
			;;
	*)
			echo "Please enter a valid option."
			;;
esac
done			
