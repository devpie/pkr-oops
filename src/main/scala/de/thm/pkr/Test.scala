package de.thm.pkr

object Main extends App {
	Interpreter interpret 
		"""BEGIN 
				VAR x = 5,
				CONST c = 1,
				PROC p(a) BEGIN x := x+a; END,
				VAR i = 0,
				WHILE i < 10 DO BEGIN
					CALL p(i);
					PRINT(x);
					i := i+c;
				END 
				PRINT(x); 
		   END"""
}