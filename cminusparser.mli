type token =
  | Tnum of (int)
  | Tidentifier of (string)
  | Telse
  | Tif
  | Tint
  | Treturn
  | Tvoid
  | Twhile
  | Tplus
  | Tdash
  | Ttimes
  | Tslash
  | Tlt
  | Tle
  | Tgt
  | Tge
  | Tequal
  | Tne
  | Tassign
  | Tsemicolon
  | Tcomma
  | Tlparen
  | Trparen
  | Tlbracket
  | Trbracket
  | Tlbrace
  | Trbrace
  | Teof

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Asttype.program
