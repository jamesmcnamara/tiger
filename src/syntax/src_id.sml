functor SrcIdFun(Token : Tiger_TOKENS) = struct

val lookupTable: (string, (int * int -> (Token.svalue, int) Token.token)) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (100, Fail "ident not found");

fun reset () =
  (HashTable.clear lookupTable;
   HashTable.insert lookupTable ("type", Token.TYPE);
   HashTable.insert lookupTable ("var", Token.VAR);
   HashTable.insert lookupTable ("function", Token.FUNCTION);
   HashTable.insert lookupTable ("break", Token.BREAK);
   HashTable.insert lookupTable ("of", Token.OF);
   HashTable.insert lookupTable ("end", Token.END);
   HashTable.insert lookupTable ("in", Token.IN);
   HashTable.insert lookupTable ("nil", Token.NIL);
   HashTable.insert lookupTable ("let", Token.LET);
   HashTable.insert lookupTable ("do", Token.DO);
   HashTable.insert lookupTable ("to", Token.TO);
   HashTable.insert lookupTable ("for", Token.FOR);
   HashTable.insert lookupTable ("while", Token.WHILE);
   HashTable.insert lookupTable ("else", Token.ELSE);
   HashTable.insert lookupTable ("then", Token.THEN);
   HashTable.insert lookupTable ("if", Token.IF);
   HashTable.insert lookupTable ("array", Token.ARRAY))

fun lookup (text, startPos, endPos) =
  (if (HashTable.numItems lookupTable) = 0 then reset() else ();
   case HashTable.find lookupTable text of
       SOME tokenFn => tokenFn (startPos, endPos)
     | NONE => Token.ID(text, startPos, endPos))

end
