<h1>Haskell Math Library 2018</h1>


<h3>Basic Functionalities:</h3>
	<li> An expression datatype that can encode:</li>
			<li>1)Addition</li>
			<li>2)Multiplication</li>
			<li>3)Cos, Sin, Exp (natural)</li>
			<li>4)Variables</li>
			<li>5)Constants</li>
	- <li>Can partially evaluate an expression</li>
	- <li>Can perform partial differentiation</li>
	- <li>Can perform some simplification of expressions</li>
	- <li>Can parse certain strings into an expression datatype (specify required format in documentation)</li>

Note: All the evaluation, simplification and partDifferention are basic 1st level.


<h1>Core Functionality Testing:</h1>

1) simplify (Map.fromList [("y", 15)]) ((Var "x")  !+ (Var "y") !+ (Const 42) !+ (Const 23))
> ((((Val. 15.0)) !+ ((Var "x"))) !+ ((Val. 42.0))) !+ ((Val. 23.0))

2) partDiff "x" (Mult (Var "x") (Var "y"))
> (((Val. 1.0)) !* ((Var "y"))) !+ (((Var "x")) !* ((Val. 0.0)))

3) eval (Map.fromList [("x", 25), ("y", 10.45), ("z", 20)]) ((Var "x") !+ (Var "y") !+ (Var "z") !+ (Const 2) !+ (Const 3) !+ (Const 4))
> 64.45
