#author: Jonathan Rudnick
#date: 3/31/15
#assignment: Homework 7 – Lucey Interpreter

structure AST = 
struct

datatype Value
= Intval of int
| Boolval of bool

datatype ExprAST 
= Lit of Value
| Var of string
| Binary of (string * ExprAST * ExprAST)
| Unary of (string * ExprAST)


datatype StmtAST 
= Skip
| Assignment of string * ExprAST
| Block of StmtAST list
| Loop of ExprAST * StmtAST
| If of ExprAST * StmtAST
| IfElse of ExprAST * StmtAST * StmtAST
| Decl of string * string 
| Dec2 of string * string * ExprAST

fun lookup v ((varName,varValue)::stateTail) = if v = varName then varValue else lookup v stateTail

fun update v newVal ((varName,varValue)::stateTail) =
     if v = varName
          then (v,newVal)::stateTail
          else (varName,varValue)::(update v newVal stateTail)

fun insertNew newVarName startVal state = (newVarName, startVal)::state

fun applyBinOp "+" (Intval a) (Intval b) = (Intval (a+b))
| applyBinOp "*" (Intval a) (Intval b) = (Intval (a*b))
| applyBinOp "-" (Intval a) (Intval b) = (Intval (a-b))
| applyBinOp "/" (Intval a) (Intval b) = (Intval (a div b))
| applyBinOp "%" (Intval a) (Intval b) = (Intval (a mod b))
| applyBinOp "<" (Intval a) (Intval b) = (Boolval (a < b))
| applyBinOp ">" (Intval a) (Intval b) = (Boolval (a > b))
| applyBinOp "<=" (Intval a) (Intval b) = (Boolval (a <= b))
| applyBinOp ">=" (Intval a) (Intval b) = (Boolval (a >= b))
| applyBinOp "&&" (Boolval a) (Boolval b) = (Boolval (a andalso b))
| applyBinOp "||" (Boolval a) (Boolval b) = (Boolval (a orelse b))
| applyBinOp "==" (Intval a) (Intval b) = (Boolval (a = b))
| applyBinOp "!=" (Intval a) (Intval b) = (Boolval (a <> b))

fun applyUnOp "-" (Intval a) = (Intval (~a))
| applyUnOp "!" (Boolval a) = (Boolval (not a))

fun eval (Lit litval) state = litval
| eval (Var v) state = lookup v state
| eval (Binary (oper, e1, e2)) state = applyBinOp oper (eval e1 state) (eval e2 state)
| eval (Unary (oper, e)) state = applyUnOp oper (eval e state)

fun meaning Skip state = state
| meaning (Assignment (targetVar, sourceExpr)) state = update targetVar (eval sourceExpr state) state
| meaning (Decl (varType, varName)) state = 
	if varType = "int"
		then insertNew varName (Intval 0) state
		else insertNew varName (Boolval false) state
| meaning (Block []) state = state
| meaning (Block (firstStmt::restOfStmts)) state = (meaning (Block restOfStmts)(meaning firstStmt state))
| meaning (Dec2 (varType, varName, sourceExpr)) state =
	if varType = "int"
		then update varName (eval sourceExpr state) (insertNew varName (Intval 0) state)
		else update varName (eval sourceExpr state) (insertNew varName (Boolval false) state)
| meaning (Loop (test,body)) state
	= if (eval test state) = (Boolval true)
		then meaning (Loop (test, body)) (meaning body state)
		else state 
| meaning (If (test, thenbranch)) state = 
	if (eval test state) = (Boolval true)
		then meaning (thenbranch) state
		else state
| meaning (IfElse (test, thenbranch, elsebranch)) state =
	if (eval test state) = (Boolval true)
	then (meaning thenbranch state)
	else (meaning elsebranch state)

end
