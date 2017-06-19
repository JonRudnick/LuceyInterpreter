signature LuceyInterpret_TOKENS =
sig
type ('a,'b) token
type svalue
val FACTORIAL:  'a * 'a -> (svalue,'a) token
val GREATEQUAL:  'a * 'a -> (svalue,'a) token
val LESSEQUAL:  'a * 'a -> (svalue,'a) token
val NOTEQUAL:  'a * 'a -> (svalue,'a) token
val EQUALEQUAL:  'a * 'a -> (svalue,'a) token
val EITHEROR:  'a * 'a -> (svalue,'a) token
val ANDAND:  'a * 'a -> (svalue,'a) token
val MORETHAN:  'a * 'a -> (svalue,'a) token
val BOOL_KW:  'a * 'a -> (svalue,'a) token
val INT_KW:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val SETEQ:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val Identifier: (string) *  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val IntLit: (int) *  'a * 'a -> (svalue,'a) token
val LESSTHAN:  'a * 'a -> (svalue,'a) token
val MOD:  'a * 'a -> (svalue,'a) token
val DIVIDE:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature LuceyInterpret_LRVALS=
sig
structure Tokens : LuceyInterpret_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
