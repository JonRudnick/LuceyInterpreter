Jonathan Rudnick
Assignment 7 Due 3/31

6/6: in-class exercises
6/6: all operators supported
0/2: short-circuted &&, || (didn't attempt this)
6/6: new statement (implemented an else-less if statement and declaration-assignment)


- val ifAST = LuceyInterpret.parse "testIf.txt";
val ifAST =
  Block
    [DeclAssignment ("int","x",Lit #),Decl ("int","y"),
     If (Binary #,Assignment #)] : LuceyInterpret.LuceyInterpretParser.result
- AST.meaning ifAST [];
val it = [("y",Intval 5),("x",Intval 5)] : (string * AST.Value) list

- val blockAST = LuceyInterpret.parse "testBlock.txt";
val blockAST =
  Block
    [Decl ("int","myVar"),Assignment ("myVar",Lit #),Decl ("int","x"),
     Assignment ("x",Unary #),Assignment ("myVar",Binary #)]
  : LuceyInterpret.LuceyInterpretParser.result
- AST.meaning blockAST []; 
val it = [("x",Intval ~1),("myVar",Intval ~99)] : (string * AST.Value) list

- val decAssignmentAST = LuceyInterpret.parse "testDeclAssignment.txt";
val decAssignmentAST = Block [DeclAssignment ("int","x",Lit #)]
  : LuceyInterpret.LuceyInterpretParser.result
- AST.meaning decAssignmentAST [];
val it = [("x",Intval 5)] : (string * AST.Value) list

- val equalAST = LuceyInterpret.parse "testEqual.txt";
val equalAST =
  Block [DeclAssignment ("int","x",Lit #),If (Binary #,Assignment #)]
  : LuceyInterpret.LuceyInterpretParser.result
- AST.meaning equalAST [];
val it = [("x",Intval 4)] : (string * AST.Value) list

- val greaterAST = LuceyInterpret.parse "testGreaterThan.txt";
val greaterAST =
  Block [DeclAssignment ("int","x",Lit #),If (Binary #,Assignment #)]
  : LuceyInterpret.LuceyInterpretParser.result
- AST.meaning greaterAST [];
val it = [("x",Intval 4)] : (string * AST.Value) list

- val greaterthanorequalAST = LuceyInterpret.parse "testGreaterThanOrEqual.txt";
val greaterthanorequalAST =
  Block [DeclAssignment ("int","x",Lit #),If (Binary #,Assignment #)]
  : LuceyInterpret.LuceyInterpretParser.result
- AST.meaning greaterthanorequalAST [];
val it = [("x",Intval 4)] : (string * AST.Value) list

- val ifelseAST = LuceyInterpret.parse "testIfElse.txt";
val ifelseAST =
  Block
    [Decl ("int","x"),Assignment ("x",Lit #),
     IfElse (Binary #,Assignment #,Assignment #)]
  : LuceyInterpret.LuceyInterpretParser.result
- AST.meaning ifelseAST [];
val it = [("x",Intval 2)] : (string * AST.Value) list

- val insertnewAST = LuceyInterpret.parse "testInsertNew.txt";
val insertnewAST =
  Block [Decl ("int","x"),Assignment ("x",Lit #),Decl ("bool","y")]
  : LuceyInterpret.LuceyInterpretParser.result
- AST.meaning insertnewAST [];
val it = [("y",Boolval false),("x",Intval 7)] : (string * AST.Value) list

- val lessthanorequalAST = LuceyInterpret.parse "testLessThanOrEqual.txt";
val lessthanorequalAST =
  Block [DeclAssignment ("int","x",Lit #),If (Binary #,Assignment #)]
  : LuceyInterpret.LuceyInterpretParser.result
- AST.meaning lessthanorequalAST [];
val it = [("x",Intval 4)] : (string * AST.Value) list

- val loopAST = LuceyInterpret.parse "testLoop.txt";
val loopAST =
  Block [Decl ("int","y"),Assignment ("y",Lit #),Loop (Binary #,Block #)]
  : LuceyInterpret.LuceyInterpretParser.result
- AST.meaning loopAST [];
val it = [("y",Intval 12)] : (string * AST.Value) list

- val notAST = LuceyInterpret.parse "testNot.txt";
val notAST =
  Block [Decl ("bool","x"),Assignment ("x",Lit #),Assignment ("x",Unary #)]
  : LuceyInterpret.LuceyInterpretParser.result
- AST.meaning notAST [];
val it = [("x",Boolval true)] : (string * AST.Value) list

- val notequalAST = LuceyInterpret.parse "testNotEqual.txt";
val notequalAST =
  Block [DeclAssignment ("int","x",Lit #),If (Binary #,Assignment #)]
  : LuceyInterpret.LuceyInterpretParser.result
- AST.meaning notequalAST [];
val it = [("x",Intval 4)] : (string * AST.Value) list

- val orAST = LuceyInterpret.parse "testOr.txt";
val orAST =
  Block
    [Decl ("bool","x"),Assignment ("x",Lit #),Decl ("bool","y"),
     Assignment ("y",Lit #),If (Binary #,Assignment #)]
  : LuceyInterpret.LuceyInterpretParser.result
- AST.meaning orAST [];
val it = [("y",Boolval true),("x",Boolval true)] : (string * AST.Value) list

- val andAST = LuceyInterpret.parse "testAnd.txt";
val andAST =
  Block
    [Decl ("bool","x"),Assignment ("x",Lit #),Decl ("bool","y"),
     Assignment ("y",Lit #),If (Binary #,Assignment #),Assignment ("y",Lit #)]
  : LuceyInterpret.LuceyInterpretParser.result
- AST.meaning andAST [];
val it = [("y",Boolval false),("x",Boolval false)] : (string * AST.Value) list