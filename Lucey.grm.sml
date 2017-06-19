functor LuceyInterpretLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : LuceyInterpret_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open AST;

      

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\088\000\000\000\
\\001\000\001\000\089\000\016\000\089\000\000\000\
\\001\000\001\000\090\000\013\000\019\000\014\000\018\000\015\000\017\000\
\\016\000\090\000\018\000\016\000\020\000\015\000\021\000\014\000\
\\022\000\013\000\000\000\
\\001\000\001\000\091\000\013\000\091\000\014\000\091\000\015\000\091\000\
\\016\000\091\000\018\000\091\000\019\000\091\000\020\000\091\000\
\\021\000\091\000\022\000\091\000\000\000\
\\001\000\001\000\092\000\013\000\092\000\014\000\092\000\015\000\092\000\
\\016\000\092\000\018\000\092\000\019\000\092\000\020\000\092\000\
\\021\000\092\000\022\000\092\000\000\000\
\\001\000\001\000\093\000\013\000\093\000\014\000\093\000\015\000\093\000\
\\016\000\093\000\018\000\093\000\019\000\093\000\020\000\093\000\
\\021\000\093\000\022\000\093\000\000\000\
\\001\000\001\000\094\000\013\000\094\000\014\000\094\000\015\000\094\000\
\\016\000\094\000\018\000\094\000\019\000\094\000\020\000\094\000\
\\021\000\094\000\022\000\094\000\000\000\
\\001\000\001\000\095\000\013\000\095\000\014\000\095\000\015\000\095\000\
\\016\000\095\000\018\000\095\000\019\000\095\000\020\000\095\000\
\\021\000\095\000\022\000\095\000\000\000\
\\001\000\001\000\096\000\013\000\096\000\014\000\096\000\015\000\096\000\
\\016\000\096\000\018\000\096\000\019\000\096\000\020\000\096\000\
\\021\000\096\000\022\000\096\000\000\000\
\\001\000\001\000\097\000\013\000\097\000\014\000\097\000\015\000\097\000\
\\016\000\097\000\018\000\097\000\019\000\097\000\020\000\097\000\
\\021\000\097\000\022\000\097\000\000\000\
\\001\000\001\000\098\000\013\000\098\000\014\000\098\000\015\000\098\000\
\\016\000\098\000\018\000\098\000\019\000\098\000\020\000\098\000\
\\021\000\098\000\022\000\098\000\000\000\
\\001\000\001\000\099\000\013\000\099\000\014\000\099\000\015\000\099\000\
\\016\000\099\000\018\000\099\000\019\000\099\000\020\000\099\000\
\\021\000\099\000\022\000\099\000\000\000\
\\001\000\001\000\100\000\013\000\100\000\014\000\100\000\015\000\100\000\
\\016\000\100\000\018\000\100\000\019\000\100\000\020\000\100\000\
\\021\000\100\000\022\000\100\000\000\000\
\\001\000\001\000\101\000\013\000\101\000\014\000\101\000\015\000\101\000\
\\016\000\101\000\018\000\101\000\019\000\085\000\020\000\101\000\
\\021\000\101\000\022\000\101\000\000\000\
\\001\000\001\000\102\000\013\000\102\000\014\000\102\000\015\000\102\000\
\\016\000\102\000\018\000\102\000\019\000\102\000\020\000\102\000\
\\021\000\102\000\022\000\102\000\000\000\
\\001\000\001\000\103\000\013\000\103\000\014\000\103\000\015\000\103\000\
\\016\000\103\000\018\000\103\000\019\000\103\000\020\000\103\000\
\\021\000\103\000\022\000\103\000\000\000\
\\001\000\001\000\104\000\013\000\104\000\014\000\104\000\015\000\104\000\
\\016\000\104\000\018\000\104\000\019\000\104\000\020\000\104\000\
\\021\000\104\000\022\000\104\000\000\000\
\\001\000\001\000\105\000\013\000\105\000\014\000\105\000\015\000\105\000\
\\016\000\105\000\018\000\105\000\019\000\105\000\020\000\105\000\
\\021\000\105\000\022\000\105\000\000\000\
\\001\000\001\000\022\000\000\000\
\\001\000\002\000\023\000\000\000\
\\001\000\002\000\024\000\000\000\
\\001\000\002\000\044\000\005\000\043\000\010\000\042\000\011\000\041\000\
\\012\000\040\000\013\000\039\000\030\000\038\000\000\000\
\\001\000\003\000\108\000\014\000\108\000\024\000\060\000\025\000\108\000\000\000\
\\001\000\003\000\109\000\014\000\109\000\024\000\060\000\025\000\109\000\000\000\
\\001\000\003\000\110\000\014\000\110\000\024\000\110\000\025\000\110\000\000\000\
\\001\000\003\000\111\000\014\000\111\000\024\000\111\000\025\000\111\000\000\000\
\\001\000\003\000\112\000\014\000\112\000\024\000\112\000\025\000\112\000\000\000\
\\001\000\003\000\113\000\014\000\113\000\024\000\113\000\025\000\113\000\000\000\
\\001\000\003\000\114\000\014\000\114\000\024\000\114\000\025\000\114\000\
\\026\000\059\000\027\000\058\000\000\000\
\\001\000\003\000\115\000\004\000\057\000\005\000\056\000\014\000\115\000\
\\024\000\115\000\025\000\115\000\026\000\115\000\027\000\115\000\000\000\
\\001\000\003\000\116\000\004\000\057\000\005\000\056\000\014\000\116\000\
\\024\000\116\000\025\000\116\000\026\000\116\000\027\000\116\000\000\000\
\\001\000\003\000\117\000\004\000\057\000\005\000\056\000\014\000\117\000\
\\024\000\117\000\025\000\117\000\026\000\117\000\027\000\117\000\000\000\
\\001\000\003\000\118\000\004\000\057\000\005\000\056\000\014\000\118\000\
\\024\000\118\000\025\000\118\000\026\000\118\000\027\000\118\000\000\000\
\\001\000\003\000\119\000\004\000\057\000\005\000\056\000\009\000\055\000\
\\014\000\119\000\023\000\054\000\024\000\119\000\025\000\119\000\
\\026\000\119\000\027\000\119\000\028\000\053\000\029\000\052\000\000\000\
\\001\000\003\000\120\000\004\000\120\000\005\000\120\000\006\000\051\000\
\\007\000\050\000\008\000\049\000\009\000\120\000\014\000\120\000\
\\023\000\120\000\024\000\120\000\025\000\120\000\026\000\120\000\
\\027\000\120\000\028\000\120\000\029\000\120\000\000\000\
\\001\000\003\000\121\000\004\000\121\000\005\000\121\000\006\000\051\000\
\\007\000\050\000\008\000\049\000\009\000\121\000\014\000\121\000\
\\023\000\121\000\024\000\121\000\025\000\121\000\026\000\121\000\
\\027\000\121\000\028\000\121\000\029\000\121\000\000\000\
\\001\000\003\000\122\000\004\000\122\000\005\000\122\000\006\000\051\000\
\\007\000\050\000\008\000\049\000\009\000\122\000\014\000\122\000\
\\023\000\122\000\024\000\122\000\025\000\122\000\026\000\122\000\
\\027\000\122\000\028\000\122\000\029\000\122\000\000\000\
\\001\000\003\000\123\000\004\000\123\000\005\000\123\000\006\000\123\000\
\\007\000\123\000\008\000\123\000\009\000\123\000\014\000\123\000\
\\023\000\123\000\024\000\123\000\025\000\123\000\026\000\123\000\
\\027\000\123\000\028\000\123\000\029\000\123\000\000\000\
\\001\000\003\000\124\000\004\000\124\000\005\000\124\000\006\000\124\000\
\\007\000\124\000\008\000\124\000\009\000\124\000\014\000\124\000\
\\023\000\124\000\024\000\124\000\025\000\124\000\026\000\124\000\
\\027\000\124\000\028\000\124\000\029\000\124\000\000\000\
\\001\000\003\000\125\000\004\000\125\000\005\000\125\000\006\000\125\000\
\\007\000\125\000\008\000\125\000\009\000\125\000\014\000\125\000\
\\023\000\125\000\024\000\125\000\025\000\125\000\026\000\125\000\
\\027\000\125\000\028\000\125\000\029\000\125\000\000\000\
\\001\000\003\000\126\000\004\000\126\000\005\000\126\000\006\000\126\000\
\\007\000\126\000\008\000\126\000\009\000\126\000\014\000\126\000\
\\023\000\126\000\024\000\126\000\025\000\126\000\026\000\126\000\
\\027\000\126\000\028\000\126\000\029\000\126\000\000\000\
\\001\000\003\000\127\000\004\000\127\000\005\000\127\000\006\000\127\000\
\\007\000\127\000\008\000\127\000\009\000\127\000\014\000\127\000\
\\023\000\127\000\024\000\127\000\025\000\127\000\026\000\127\000\
\\027\000\127\000\028\000\127\000\029\000\127\000\000\000\
\\001\000\003\000\128\000\004\000\128\000\005\000\128\000\006\000\128\000\
\\007\000\128\000\008\000\128\000\009\000\128\000\014\000\128\000\
\\023\000\128\000\024\000\128\000\025\000\128\000\026\000\128\000\
\\027\000\128\000\028\000\128\000\029\000\128\000\000\000\
\\001\000\003\000\129\000\004\000\129\000\005\000\129\000\006\000\129\000\
\\007\000\129\000\008\000\129\000\009\000\129\000\014\000\129\000\
\\023\000\129\000\024\000\129\000\025\000\129\000\026\000\129\000\
\\027\000\129\000\028\000\129\000\029\000\129\000\000\000\
\\001\000\003\000\130\000\004\000\130\000\005\000\130\000\006\000\130\000\
\\007\000\130\000\008\000\130\000\009\000\130\000\014\000\130\000\
\\023\000\130\000\024\000\130\000\025\000\130\000\026\000\130\000\
\\027\000\130\000\028\000\130\000\029\000\130\000\000\000\
\\001\000\003\000\131\000\004\000\131\000\005\000\131\000\006\000\131\000\
\\007\000\131\000\008\000\131\000\009\000\131\000\014\000\131\000\
\\023\000\131\000\024\000\131\000\025\000\131\000\026\000\131\000\
\\027\000\131\000\028\000\131\000\029\000\131\000\000\000\
\\001\000\003\000\132\000\004\000\132\000\005\000\132\000\006\000\132\000\
\\007\000\132\000\008\000\132\000\009\000\132\000\014\000\132\000\
\\023\000\132\000\024\000\132\000\025\000\132\000\026\000\132\000\
\\027\000\132\000\028\000\132\000\029\000\132\000\000\000\
\\001\000\003\000\133\000\004\000\133\000\005\000\133\000\006\000\133\000\
\\007\000\133\000\008\000\133\000\009\000\133\000\014\000\133\000\
\\023\000\133\000\024\000\133\000\025\000\133\000\026\000\133\000\
\\027\000\133\000\028\000\133\000\029\000\133\000\000\000\
\\001\000\003\000\134\000\004\000\134\000\005\000\134\000\006\000\134\000\
\\007\000\134\000\008\000\134\000\009\000\134\000\014\000\134\000\
\\023\000\134\000\024\000\134\000\025\000\134\000\026\000\134\000\
\\027\000\134\000\028\000\134\000\029\000\134\000\000\000\
\\001\000\003\000\135\000\004\000\135\000\005\000\135\000\006\000\135\000\
\\007\000\135\000\008\000\135\000\009\000\135\000\014\000\135\000\
\\023\000\135\000\024\000\135\000\025\000\135\000\026\000\135\000\
\\027\000\135\000\028\000\135\000\029\000\135\000\000\000\
\\001\000\003\000\062\000\025\000\061\000\000\000\
\\001\000\003\000\066\000\025\000\061\000\000\000\
\\001\000\003\000\083\000\025\000\061\000\000\000\
\\001\000\013\000\106\000\000\000\
\\001\000\013\000\107\000\000\000\
\\001\000\013\000\019\000\014\000\018\000\015\000\017\000\018\000\016\000\
\\020\000\015\000\021\000\014\000\022\000\013\000\000\000\
\\001\000\013\000\020\000\000\000\
\\001\000\014\000\028\000\017\000\027\000\000\000\
\\001\000\014\000\067\000\025\000\061\000\000\000\
\\001\000\014\000\068\000\025\000\061\000\000\000\
\\001\000\016\000\046\000\000\000\
\\001\000\017\000\026\000\000\000\
\"
val actionRowNumbers =
"\056\000\057\000\011\000\010\000\
\\009\000\008\000\007\000\006\000\
\\005\000\003\000\019\000\055\000\
\\054\000\020\000\021\000\056\000\
\\004\000\062\000\058\000\002\000\
\\001\000\022\000\022\000\061\000\
\\022\000\022\000\017\000\045\000\
\\044\000\041\000\037\000\034\000\
\\029\000\026\000\024\000\051\000\
\\022\000\046\000\050\000\049\000\
\\048\000\022\000\022\000\052\000\
\\012\000\059\000\060\000\022\000\
\\022\000\022\000\022\000\022\000\
\\022\000\022\000\022\000\022\000\
\\022\000\022\000\022\000\022\000\
\\056\000\043\000\042\000\053\000\
\\056\000\013\000\018\000\040\000\
\\039\000\038\000\033\000\031\000\
\\032\000\030\000\035\000\036\000\
\\028\000\027\000\025\000\023\000\
\\016\000\047\000\014\000\056\000\
\\015\000\000\000"
val gotoT =
"\
\\001\000\085\000\002\000\010\000\003\000\009\000\004\000\008\000\
\\005\000\007\000\006\000\006\000\007\000\005\000\008\000\004\000\
\\009\000\003\000\010\000\002\000\011\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\019\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\023\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\035\000\013\000\034\000\014\000\033\000\015\000\032\000\
\\016\000\031\000\017\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\000\000\
\\012\000\043\000\013\000\034\000\014\000\033\000\015\000\032\000\
\\016\000\031\000\017\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\000\000\
\\000\000\
\\012\000\045\000\013\000\034\000\014\000\033\000\015\000\032\000\
\\016\000\031\000\017\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\000\000\
\\012\000\046\000\013\000\034\000\014\000\033\000\015\000\032\000\
\\016\000\031\000\017\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\018\000\061\000\019\000\028\000\020\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\018\000\062\000\019\000\028\000\020\000\027\000\000\000\
\\012\000\063\000\013\000\034\000\014\000\033\000\015\000\032\000\
\\016\000\031\000\017\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\018\000\067\000\019\000\028\000\020\000\027\000\000\000\
\\018\000\068\000\019\000\028\000\020\000\027\000\000\000\
\\018\000\069\000\019\000\028\000\020\000\027\000\000\000\
\\016\000\070\000\017\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\000\000\
\\016\000\071\000\017\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\000\000\
\\016\000\072\000\017\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\000\000\
\\016\000\073\000\017\000\030\000\018\000\029\000\019\000\028\000\
\\020\000\027\000\000\000\
\\017\000\074\000\018\000\029\000\019\000\028\000\020\000\027\000\000\000\
\\017\000\075\000\018\000\029\000\019\000\028\000\020\000\027\000\000\000\
\\015\000\076\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\000\000\
\\015\000\077\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\000\000\
\\014\000\078\000\015\000\032\000\016\000\031\000\017\000\030\000\
\\018\000\029\000\019\000\028\000\020\000\027\000\000\000\
\\013\000\079\000\014\000\033\000\015\000\032\000\016\000\031\000\
\\017\000\030\000\018\000\029\000\019\000\028\000\020\000\027\000\000\000\
\\003\000\080\000\004\000\008\000\005\000\007\000\006\000\006\000\
\\007\000\005\000\008\000\004\000\009\000\003\000\010\000\002\000\
\\011\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\082\000\004\000\008\000\005\000\007\000\006\000\006\000\
\\007\000\005\000\008\000\004\000\009\000\003\000\010\000\002\000\
\\011\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\084\000\004\000\008\000\005\000\007\000\006\000\006\000\
\\007\000\005\000\008\000\004\000\009\000\003\000\010\000\002\000\
\\011\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 86
val numrules = 48
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | Identifier of  (string)
 | IntLit of  (int) | Literal of  (ExprAST) | Primary of  (ExprAST)
 | Factor of  (ExprAST) | Term of  (ExprAST) | Addition of  (ExprAST)
 | Relation of  (ExprAST) | Equality of  (ExprAST)
 | Conjunction of  (ExprAST) | Expression of  (ExprAST)
 | Type of  (string) | DeclarationAssignment of  (StmtAST)
 | DeclarationStatement of  (StmtAST) | WhileStatement of  (StmtAST)
 | IfElseStatement of  (StmtAST) | IfStatement of  (StmtAST)
 | AssignmentStatement of  (StmtAST) | BlockStatement of  (StmtAST)
 | Statement of  (StmtAST) | StatementList of  (StmtAST list)
 | Program of  (StmtAST)
end
type svalue = MlyValue.svalue
type result = StmtAST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "LPAREN"
  | (T 2) => "RPAREN"
  | (T 3) => "PLUS"
  | (T 4) => "MINUS"
  | (T 5) => "TIMES"
  | (T 6) => "DIVIDE"
  | (T 7) => "MOD"
  | (T 8) => "LESSTHAN"
  | (T 9) => "IntLit"
  | (T 10) => "TRUE"
  | (T 11) => "FALSE"
  | (T 12) => "Identifier"
  | (T 13) => "SEMICOLON"
  | (T 14) => "LBRACE"
  | (T 15) => "RBRACE"
  | (T 16) => "SETEQ"
  | (T 17) => "IF"
  | (T 18) => "ELSE"
  | (T 19) => "WHILE"
  | (T 20) => "INT_KW"
  | (T 21) => "BOOL_KW"
  | (T 22) => "MORETHAN"
  | (T 23) => "ANDAND"
  | (T 24) => "EITHEROR"
  | (T 25) => "EQUALEQUAL"
  | (T 26) => "NOTEQUAL"
  | (T 27) => "LESSEQUAL"
  | (T 28) => "GREATEQUAL"
  | (T 29) => "FACTORIAL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23)
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 11) $$ (T 10) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, EOF1right)) :: ( _, ( MlyValue.StatementList 
StatementList, StatementList1left, _)) :: rest671)) => let val  result
 = MlyValue.Program (Block StatementList)
 in ( LrTable.NT 0, ( result, StatementList1left, EOF1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.StatementList StatementList, _, 
StatementList1right)) :: ( _, ( MlyValue.Statement Statement, 
Statement1left, _)) :: rest671)) => let val  result = 
MlyValue.StatementList (Statement::StatementList)
 in ( LrTable.NT 1, ( result, Statement1left, StatementList1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.Statement Statement, Statement1left, 
Statement1right)) :: rest671)) => let val  result = 
MlyValue.StatementList ([Statement])
 in ( LrTable.NT 1, ( result, Statement1left, Statement1right), 
rest671)
end
|  ( 3, ( ( _, ( _, SEMICOLON1left, SEMICOLON1right)) :: rest671)) =>
 let val  result = MlyValue.Statement (Skip)
 in ( LrTable.NT 2, ( result, SEMICOLON1left, SEMICOLON1right), 
rest671)
end
|  ( 4, ( ( _, ( MlyValue.BlockStatement BlockStatement, 
BlockStatement1left, BlockStatement1right)) :: rest671)) => let val  
result = MlyValue.Statement (BlockStatement)
 in ( LrTable.NT 2, ( result, BlockStatement1left, 
BlockStatement1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.AssignmentStatement AssignmentStatement, 
AssignmentStatement1left, AssignmentStatement1right)) :: rest671)) =>
 let val  result = MlyValue.Statement (AssignmentStatement)
 in ( LrTable.NT 2, ( result, AssignmentStatement1left, 
AssignmentStatement1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.IfStatement IfStatement, IfStatement1left, 
IfStatement1right)) :: rest671)) => let val  result = 
MlyValue.Statement (IfStatement)
 in ( LrTable.NT 2, ( result, IfStatement1left, IfStatement1right), 
rest671)
end
|  ( 7, ( ( _, ( MlyValue.IfElseStatement IfElseStatement, 
IfElseStatement1left, IfElseStatement1right)) :: rest671)) => let val 
 result = MlyValue.Statement (IfElseStatement)
 in ( LrTable.NT 2, ( result, IfElseStatement1left, 
IfElseStatement1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.WhileStatement WhileStatement, 
WhileStatement1left, WhileStatement1right)) :: rest671)) => let val  
result = MlyValue.Statement (WhileStatement)
 in ( LrTable.NT 2, ( result, WhileStatement1left, 
WhileStatement1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.DeclarationStatement DeclarationStatement, 
DeclarationStatement1left, DeclarationStatement1right)) :: rest671))
 => let val  result = MlyValue.Statement (DeclarationStatement)
 in ( LrTable.NT 2, ( result, DeclarationStatement1left, 
DeclarationStatement1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.DeclarationAssignment DeclarationAssignment
, DeclarationAssignment1left, DeclarationAssignment1right)) :: rest671
)) => let val  result = MlyValue.Statement (DeclarationAssignment)
 in ( LrTable.NT 2, ( result, DeclarationAssignment1left, 
DeclarationAssignment1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( 
MlyValue.StatementList StatementList, _, _)) :: ( _, ( _, LBRACE1left,
 _)) :: rest671)) => let val  result = MlyValue.BlockStatement (
Block StatementList)
 in ( LrTable.NT 3, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 12, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( 
MlyValue.Expression Expression, _, _)) :: _ :: ( _, ( 
MlyValue.Identifier Identifier, Identifier1left, _)) :: rest671)) =>
 let val  result = MlyValue.AssignmentStatement (
Assignment (Identifier, Expression))
 in ( LrTable.NT 4, ( result, Identifier1left, SEMICOLON1right), 
rest671)
end
|  ( 13, ( ( _, ( MlyValue.Statement Statement, _, Statement1right))
 :: _ :: ( _, ( MlyValue.Expression Expression, _, _)) :: _ :: ( _, (
 _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.IfStatement (If (Expression,Statement))
 in ( LrTable.NT 5, ( result, IF1left, Statement1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.Statement Statement2, _, Statement2right))
 :: _ :: ( _, ( MlyValue.Statement Statement1, _, _)) :: _ :: ( _, ( 
MlyValue.Expression Expression, _, _)) :: _ :: ( _, ( _, IF1left, _))
 :: rest671)) => let val  result = MlyValue.IfElseStatement (
IfElse (Expression,Statement1,Statement2))
 in ( LrTable.NT 6, ( result, IF1left, Statement2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.Statement Statement, _, Statement1right))
 :: _ :: ( _, ( MlyValue.Expression Expression, _, _)) :: _ :: ( _, (
 _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.WhileStatement (Loop (Expression, Statement))
 in ( LrTable.NT 7, ( result, WHILE1left, Statement1right), rest671)

end
|  ( 16, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( 
MlyValue.Identifier Identifier, _, _)) :: ( _, ( MlyValue.Type Type, 
Type1left, _)) :: rest671)) => let val  result = 
MlyValue.DeclarationStatement (Decl (Type, Identifier))
 in ( LrTable.NT 8, ( result, Type1left, SEMICOLON1right), rest671)

end
|  ( 17, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( 
MlyValue.Expression Expression, _, _)) :: _ :: ( _, ( 
MlyValue.Identifier Identifier, _, _)) :: ( _, ( MlyValue.Type Type, 
Type1left, _)) :: rest671)) => let val  result = 
MlyValue.DeclarationAssignment (Dec2 (Type, Identifier, Expression))
 in ( LrTable.NT 9, ( result, Type1left, SEMICOLON1right), rest671)

end
|  ( 18, ( ( _, ( _, INT_KW1left, INT_KW1right)) :: rest671)) => let
 val  result = MlyValue.Type ("int")
 in ( LrTable.NT 10, ( result, INT_KW1left, INT_KW1right), rest671)

end
|  ( 19, ( ( _, ( _, BOOL_KW1left, BOOL_KW1right)) :: rest671)) => let
 val  result = MlyValue.Type ("bool")
 in ( LrTable.NT 10, ( result, BOOL_KW1left, BOOL_KW1right), rest671)

end
|  ( 20, ( ( _, ( MlyValue.Conjunction Conjunction, _, 
Conjunction1right)) :: _ :: ( _, ( MlyValue.Expression Expression, 
Expression1left, _)) :: rest671)) => let val  result = 
MlyValue.Expression (Binary ("||",Expression,Conjunction))
 in ( LrTable.NT 11, ( result, Expression1left, Conjunction1right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.Conjunction Conjunction, Conjunction1left, 
Conjunction1right)) :: rest671)) => let val  result = 
MlyValue.Expression (Conjunction)
 in ( LrTable.NT 11, ( result, Conjunction1left, Conjunction1right), 
rest671)
end
|  ( 22, ( ( _, ( MlyValue.Equality Equality, _, Equality1right)) :: _
 :: ( _, ( MlyValue.Conjunction Conjunction, Conjunction1left, _)) :: 
rest671)) => let val  result = MlyValue.Conjunction (
Binary ("&&",Conjunction,Equality))
 in ( LrTable.NT 12, ( result, Conjunction1left, Equality1right), 
rest671)
end
|  ( 23, ( ( _, ( MlyValue.Equality Equality, Equality1left, 
Equality1right)) :: rest671)) => let val  result = 
MlyValue.Conjunction (Equality)
 in ( LrTable.NT 12, ( result, Equality1left, Equality1right), rest671
)
end
|  ( 24, ( ( _, ( MlyValue.Relation Relation2, _, Relation2right)) ::
 _ :: ( _, ( MlyValue.Relation Relation1, Relation1left, _)) :: 
rest671)) => let val  result = MlyValue.Equality (
Binary ("==",Relation1,Relation2))
 in ( LrTable.NT 13, ( result, Relation1left, Relation2right), rest671
)
end
|  ( 25, ( ( _, ( MlyValue.Relation Relation2, _, Relation2right)) ::
 _ :: ( _, ( MlyValue.Relation Relation1, Relation1left, _)) :: 
rest671)) => let val  result = MlyValue.Equality (
Binary ("!=",Relation1,Relation2))
 in ( LrTable.NT 13, ( result, Relation1left, Relation2right), rest671
)
end
|  ( 26, ( ( _, ( MlyValue.Relation Relation, Relation1left, 
Relation1right)) :: rest671)) => let val  result = MlyValue.Equality (
Relation)
 in ( LrTable.NT 13, ( result, Relation1left, Relation1right), rest671
)
end
|  ( 27, ( ( _, ( MlyValue.Addition Addition2, _, Addition2right)) ::
 _ :: ( _, ( MlyValue.Addition Addition1, Addition1left, _)) :: 
rest671)) => let val  result = MlyValue.Relation (
Binary ("<",Addition1,Addition2))
 in ( LrTable.NT 14, ( result, Addition1left, Addition2right), rest671
)
end
|  ( 28, ( ( _, ( MlyValue.Addition Addition2, _, Addition2right)) ::
 _ :: ( _, ( MlyValue.Addition Addition1, Addition1left, _)) :: 
rest671)) => let val  result = MlyValue.Relation (
Binary ("<=",Addition1,Addition2))
 in ( LrTable.NT 14, ( result, Addition1left, Addition2right), rest671
)
end
|  ( 29, ( ( _, ( MlyValue.Addition Addition2, _, Addition2right)) ::
 _ :: ( _, ( MlyValue.Addition Addition1, Addition1left, _)) :: 
rest671)) => let val  result = MlyValue.Relation (
Binary (">",Addition1,Addition2))
 in ( LrTable.NT 14, ( result, Addition1left, Addition2right), rest671
)
end
|  ( 30, ( ( _, ( MlyValue.Addition Addition2, _, Addition2right)) ::
 _ :: ( _, ( MlyValue.Addition Addition1, Addition1left, _)) :: 
rest671)) => let val  result = MlyValue.Relation (
Binary (">=",Addition1,Addition2))
 in ( LrTable.NT 14, ( result, Addition1left, Addition2right), rest671
)
end
|  ( 31, ( ( _, ( MlyValue.Addition Addition, Addition1left, 
Addition1right)) :: rest671)) => let val  result = MlyValue.Relation (
Addition)
 in ( LrTable.NT 14, ( result, Addition1left, Addition1right), rest671
)
end
|  ( 32, ( ( _, ( MlyValue.Term Term, _, Term1right)) :: _ :: ( _, ( 
MlyValue.Addition Addition, Addition1left, _)) :: rest671)) => let
 val  result = MlyValue.Addition (Binary ("-",Addition,Term))
 in ( LrTable.NT 15, ( result, Addition1left, Term1right), rest671)

end
|  ( 33, ( ( _, ( MlyValue.Term Term, _, Term1right)) :: _ :: ( _, ( 
MlyValue.Addition Addition, Addition1left, _)) :: rest671)) => let
 val  result = MlyValue.Addition (Binary ("+",Addition,Term))
 in ( LrTable.NT 15, ( result, Addition1left, Term1right), rest671)

end
|  ( 34, ( ( _, ( MlyValue.Term Term, Term1left, Term1right)) :: 
rest671)) => let val  result = MlyValue.Addition (Term)
 in ( LrTable.NT 15, ( result, Term1left, Term1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.Factor Factor, _, Factor1right)) :: _ :: (
 _, ( MlyValue.Term Term, Term1left, _)) :: rest671)) => let val  
result = MlyValue.Term (Binary ("*",Term,Factor))
 in ( LrTable.NT 16, ( result, Term1left, Factor1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.Factor Factor, _, Factor1right)) :: _ :: (
 _, ( MlyValue.Term Term, Term1left, _)) :: rest671)) => let val  
result = MlyValue.Term (Binary ("/",Term,Factor))
 in ( LrTable.NT 16, ( result, Term1left, Factor1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.Factor Factor, _, Factor1right)) :: _ :: (
 _, ( MlyValue.Term Term, Term1left, _)) :: rest671)) => let val  
result = MlyValue.Term (Binary ("%",Term,Factor))
 in ( LrTable.NT 16, ( result, Term1left, Factor1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.Factor Factor, Factor1left, Factor1right))
 :: rest671)) => let val  result = MlyValue.Term (Factor)
 in ( LrTable.NT 16, ( result, Factor1left, Factor1right), rest671)

end
|  ( 39, ( ( _, ( MlyValue.Factor Factor, _, Factor1right)) :: ( _, (
 _, MINUS1left, _)) :: rest671)) => let val  result = MlyValue.Factor
 (Unary ("-",Factor))
 in ( LrTable.NT 17, ( result, MINUS1left, Factor1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.Factor Factor, _, Factor1right)) :: ( _, (
 _, FACTORIAL1left, _)) :: rest671)) => let val  result = 
MlyValue.Factor (Unary ("!",Factor))
 in ( LrTable.NT 17, ( result, FACTORIAL1left, Factor1right), rest671)

end
|  ( 41, ( ( _, ( MlyValue.Primary Primary, Primary1left, 
Primary1right)) :: rest671)) => let val  result = MlyValue.Factor (
Primary)
 in ( LrTable.NT 17, ( result, Primary1left, Primary1right), rest671)

end
|  ( 42, ( ( _, ( MlyValue.Literal Literal, Literal1left, 
Literal1right)) :: rest671)) => let val  result = MlyValue.Primary (
Literal)
 in ( LrTable.NT 18, ( result, Literal1left, Literal1right), rest671)

end
|  ( 43, ( ( _, ( MlyValue.Identifier Identifier, Identifier1left, 
Identifier1right)) :: rest671)) => let val  result = MlyValue.Primary
 (Var Identifier)
 in ( LrTable.NT 18, ( result, Identifier1left, Identifier1right), 
rest671)
end
|  ( 44, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Expression 
Expression, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.Primary (Expression)
 in ( LrTable.NT 18, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 45, ( ( _, ( MlyValue.IntLit IntLit, IntLit1left, IntLit1right))
 :: rest671)) => let val  result = MlyValue.Literal (
Lit (Intval IntLit))
 in ( LrTable.NT 19, ( result, IntLit1left, IntLit1right), rest671)

end
|  ( 46, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.Literal (Lit (Boolval true))
 in ( LrTable.NT 19, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 47, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.Literal (Lit (Boolval false))
 in ( LrTable.NT 19, ( result, FALSE1left, FALSE1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : LuceyInterpret_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun IntLit (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.IntLit i,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun Identifier (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.Identifier i,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun SETEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun INT_KW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL_KW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun MORETHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun ANDAND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun EITHEROR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun NOTEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun FACTORIAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
end
end
