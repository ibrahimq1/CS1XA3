### ProjectAnalyze Documentation ###

The ProjectAnalyze script has the following features: 

	1) Shows current User and day-date-time
	2) Checks the status of github
	3) Puts all uncommited changes in a file changes.log
	4) Puts all lines containing #TODO listing the todos in todo.log file
	5) Checks all haskell files for syntax errors and puts errors in error.log file
	6) Gives a dialogue GUI to see the log files.


To use the script and the features simply run the script (./ProjectAnalyze).
You will be informed of the updates regarding the status of your repo with the
changes,todos and errors in their respective log files. Aditionally an user input prompt will 
appear to take your choice of which log file you would like
to view in a vim editor. [Max 5 input till script terminates to keep it simple]

Example : 

	Input:

	./ProjectAnalyze.sh

	Output:
	
	Sat Feb 24 17:34:57 EST 2018
	User: ibrahimq

	On branch master
	Your branch is up-to-date with 'origin/master'.
	nothing to commit, working tree clean

	### Any recent changes are in the changes.log file

	### The #TODO tasks for today can be found in todo.log

	### Any error in haskell files can be found in error.log

	Which log file do you want to see? [Type 1,2,3,q(to exit) or write their names ex: todo.log]
	

