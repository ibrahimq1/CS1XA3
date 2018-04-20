<h1>Basic Functionality:</h1>

<h3>Functionalities:</h3>
	- An expression datatype that can encode (at least)
			Addition
			Multiplication
			Cos, Sin, Exp (natural)
			Variables
			Constants
	- Can partially evaluate an expression
	- Can perform partial differentiation
	- Can perform some simplification of expressions
	- Can parse certain strings into an expression datatype (specify required format in documentation)

<font color = "red">-- Note: All the evaluation, simplification and partDifferention are basic 1st level.</font>


<h1>Core Functionality Testing:</h1>

simplify (Map.fromList [("y", 15)]) ((Var "x")  !+ (Var "y") !+ (Const 42) !+ (Const 23))

partDiff "x" (Mult (Var "x") (Var "y"))

eval (Map.fromList [("x", 5), ("y", 10.5), ("z", 20)]) ((Var "x") !+ (Var "y") !+ (Var "z") !+ (Const 2) !+ (Const 3) !+ (Const 4))
