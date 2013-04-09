type literal = 
    LitInt of int
  | LitFloat of float
  | LitChar of char (* todo *)
  | LitString of string

type tokVal =
    Whitespace of int
  | Comment of string
  | Meta of string
  | Indentation of int
  | Syntax of string
  | Operator of string
  | Ident of string
  | Literal of literal

type tokAttr = { lineBuf: string; line: int; pos: int}

type token = { tok: tokVal; attr: tokAttr }

(* let string_of_lit = function *)
(* let string_of_tokVal = function *)
(* let string_of_tokAttr {} = *)
(* let string_of_token {} = *)

let lexError msg attr = failwith ("todo: add attr, "^msg)

let appendTok rest = function
    {tok=Whitespace _} | {tok=Comment _} -> rest
  | t -> t::rest

type lexGroup = Separating | NonSeparating
let mkGroup grp = List.map (fun (r, mkTok) -> ((Str.regexp r, mkTok), grp))

let strToChar s = if String.length s == 1 then s.[0]
else failwith ("strToChar string does not represent a valid char: '"^s^"'")

let unescapedChar = function (* todo lexError *)
    '\\' -> '\\'
  | 't' -> '\t'
  | 'v' -> '\x0b'
  | 'n' -> '\n'
  | 'r' -> '\r'
  | c -> failwith ("invalid escape char '" ^ (String.make 1 c) ^"'")
let unescapedOne s =
  if String.length s == 2 then String.make 1 (unescapedChar s.[1])
else failwith ("unescapeOne received too many chars: "^s)
let unescaped s = Str.global_substitute (Str.regexp "\\\\.")
  (fun s -> unescapedOne (Str.matched_string s)) s

let digit = "0-9"
let alpha = "a-zA-Z_"
let alnum = alpha^digit
let wspace = " \t\x0b" (* " \t\v" *)
let oper = "`~!@$%^&*\\=+|;:,.<>/?-"
let synt = "()\\[\\]{}"

let sepLexers = mkGroup Separating [
  ("["^wspace^"]+", fun s -> Whitespace (String.length s));
  ("##.*", fun s -> Comment s);
  ("#?["^synt^"]", fun s -> Syntax s);
  ("["^oper^"]+", fun s -> Operator s);
  ("#\\(["^alnum^"]+\\)\\|\\(["^oper^"]+\\)", fun s -> Meta s);
]

let (whitespaceLexer::_) = sepLexers

let dequote s = let len = String.length s in String.sub s 1 (len-2)

let allLexers = List.append
  (mkGroup NonSeparating [
     ("\\(["^alpha^"]\\|\\(\\\\.\\)\\)\\(["^alnum^"]\\|\\(\\\\.\\)\\)*",
      fun s -> Ident s);
     ("-?\\("^digit^"+\\."^digit^"+\\)\\([eE][+-]?["^digit^"]+\\)?",
      fun s -> Literal (LitFloat (float_of_string s)));
     ("-?\\(0x\\)?["^digit^"]+", fun s -> Literal (LitInt (int_of_string s)));
     ("'\\(\\(\\\\.\\)\\|[^\\\\']\\)'",
      fun s -> Literal (LitChar (strToChar (unescaped (dequote s)))));
     ("\"\\(\\(\\\\.\\)\\|[^\\\\\"]\\)*\"",
      fun s -> Literal (LitString (unescaped (dequote s))));
   ]) sepLexers

let nextLexers = function
    Separating -> (NonSeparating, allLexers)
  | NonSeparating -> (Separating, sepLexers)

let rec firstSome f = function
    [] -> None
  | x::xs -> (match f x with
		  None -> firstSome f xs
		| s -> s)

let strRest s start = String.sub s start ((String.length s) - start)

let matchLex s ((re, mkTok), grp) =
  if Str.string_match re s 0 then let m = Str.matched_string s in
  let len = String.length m in Some ((mkTok m), (strRest s len), len, grp)
  else None

let matchFirst les s = firstSome (matchLex s) les

let lineTokens lineNum line = 
  let mkAttr linePos = {lineBuf=line; line=lineNum; pos=linePos}
  in let mkTok t pos = {tok=t; attr=mkAttr pos}
  in let matchIndent s = match matchLex s whitespaceLexer with
      None -> (mkTok (Indentation 0) 0, 0, s)
    | Some (Whitespace indent, s, len, _) ->
	(mkTok (Indentation indent) 0, indent, s)
    | _ -> assert false
  in let rec inner (grp, les) linePos = function
	"" -> []
    | s -> match matchFirst les s with
	  None -> let err msg = lexError msg (mkAttr linePos) in
	    (match grp with
		 Separating -> (match matchFirst sepLexers s with
				    None -> err "unknown token type" (* todo *)
				  | _ -> err "expected valid separator; ")
	       | NonSeparating -> err "unknown token type")
	| Some (t, s, len, grp) ->
	    appendTok (inner (nextLexers grp) (linePos+len) s) (mkTok t linePos)
  in let (indent, pos, s) = matchIndent line
  in match indent::inner (NonSeparating, allLexers) pos s with
      [_] -> [] (* a line with only an indent should produce no tokens *)
    | ts -> ts

(* matchFirst allLexers "hello world";; *)
(* matchFirst allLexers "'c'";; *)
(* lineTokens 3 "hello world";; *)
(* lineTokens 3 "hello \"yes\\no\\t\" 123-456 'c' '\\n' -78++*>>world";; *)
