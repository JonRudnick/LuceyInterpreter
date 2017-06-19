
type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token
val pos = ref 1
val error = fn x => TextIO.output(TextIO.stdErr, x ^ "\n")
val eof = fn () => Tokens.EOF(!pos, !pos)

fun countnewlines s = 
    let val lst = explode s
        fun count (c:char) nil = 0
          | count c (h::t) = 
            let val tcount = count c t
            in
              if c = h then 1+tcount else tcount
            end
    in
      pos:= (!pos) + (count #"\n" lst)
    end

%%

%header (functor LuceyInterpretLexFun(structure Tokens : LuceyInterpret_TOKENS));

%%

\n  => (pos := (!pos) + 1; lex());
[\ \t]+  => (lex());

"*"  => (Tokens.TIMES(!pos,!pos));
"+"  => (Tokens.PLUS(!pos,!pos));
"-"  => (Tokens.MINUS(!pos,!pos));
"/"  => (Tokens.DIVIDE(!pos,!pos));
"("  => (Tokens.LPAREN(!pos,!pos));
")"  => (Tokens.RPAREN(!pos,!pos));
"%"  => (Tokens.MOD(!pos,!pos));
"<"  => (Tokens.LESSTHAN(!pos,!pos));
"true" => (Tokens.TRUE(!pos,!pos));
"false" => (Tokens.FALSE(!pos,!pos));
";"  => (Tokens.SEMICOLON(!pos,!pos));
"{"  => (Tokens.LBRACE(!pos,!pos));
"}"  => (Tokens.RBRACE(!pos,!pos));
"="  => (Tokens.SETEQ(!pos,!pos));
"if" => (Tokens.IF(!pos,!pos));
"else" => (Tokens.ELSE(!pos,!pos));
"while" => (Tokens.WHILE(!pos,!pos));
"int" => (Tokens.INT_KW(!pos,!pos));
"bool" => (Tokens.BOOL_KW(!pos,!pos));

">"  => (Tokens.MORETHAN(!pos,!pos));
"&&" => (Tokens.ANDAND(!pos,!pos));
"||" => (Tokens.EITHEROR(!pos,!pos));
"==" => (Tokens.EQUALEQUAL(!pos,!pos));
"!=" => (Tokens.NOTEQUAL(!pos,!pos));
"<=" => (Tokens.LESSEQUAL(!pos,!pos));
">=" => (Tokens.GREATEQUAL(!pos,!pos));
"!" => (Tokens.FACTORIAL(!pos,!pos));

[a-zA-Z][a-zA-Z0-9_]* => (Tokens.Identifier(yytext,!pos,!pos));	 
[0-9][0-9]*  => (Tokens.IntLit(Option.getOpt(Int.fromString(yytext),0),!pos,!pos));
.  => (error ("\nerror: bad token "^yytext^"\n"); lex());
