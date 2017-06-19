structure LuceyInterpret =
struct
open AST;

   (* these three structures and the invoke function are required and take from the ML-Yacc documentation:
       http://www.smlnj.org/doc/ML-Yacc/mlyacc005.html#create-parser
   *)
   structure LuceyInterpretLrVals = LuceyInterpretLrValsFun(structure Token = LrParser.Token)

   structure LuceyInterpretLex = LuceyInterpretLexFun(structure Tokens = LuceyInterpretLrVals.Tokens)

   structure LuceyInterpretParser = Join(structure ParserData = LuceyInterpretLrVals.ParserData
                              structure Lex=LuceyInterpretLex
                              structure LrParser=LrParser)

   fun invoke lexstream = 
      let
         fun print_error (s,i:int,_) = TextIO.output(TextIO.stdOut, "Error, line "^(Int.toString i)^", "^s^"\n")
      in
         LuceyInterpretParser.parse(0,lexstream,print_error,())
      end

(* everything below this line was adapted from Kent Lee's mlcomp compiler *)

     val input_line =
       fn f =>
          let val sOption = TextIO.inputLine f
          in
            if isSome(sOption) then
               Option.valOf(sOption)
            else
               ""
          end

     val parseAux = 
         fn filename =>
           let val instrm = TextIO.openIn filename
               val lexer = LuceyInterpretParser.makeLexer(fn i => input_line instrm)
               val _ = LuceyInterpretLex.UserDeclarations.pos := 1
               val error = fn (e,i:int,_) => 
                               TextIO.output(TextIO.stdOut," line " ^ (Int.toString i) ^ ", Error: " ^ e ^ "\n")
           in 
                LuceyInterpretParser.parse(30,lexer,error,()) before TextIO.closeIn instrm
           end


    fun parse file = let val (ast,_) = parseAux file in ast end 

(* This is for when it is invoked at the command line, but we won't use it like that yet. *)
   fun run(a,b::c) = 
      let
         val ast = parse b
      in
         (meaning ast []; OS.Process.success)
      end
   | run(a,b) = (TextIO.print("usage: sml @SMLload=LuceyInterpret\n"); OS.Process.success)


end


