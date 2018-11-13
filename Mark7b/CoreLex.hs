-------------------------------------------------
-- Lexical definitions for the language 
--
--  "Implementing Functional languages"
--
-- José E. Gallardo, December 1997
-------------------------------------------------

module CoreLex (
         tkCase, tkElse, tkIf, tkIn, tkLet, tkLetrec, tkOf, tkPack, tkThen,
         keyWords,
         tkLT, tkLE, tkEQT, tkNEQT, tkGT, tkGE,
         tkEq, tkAdd, tkSub, tkMul, tkDiv,
         tkOB, tkCB, tkOCB, tkCCB, tkOL, tkCL,
         tkSemicolon, tkColon, tkComma, tkLambda, tkDot, tkArrow,
         tkOr, tkAnd,
         tkNegate, tkNot, tkSeq, tkStrict,
         tkTrue, tkFalse, tkNil, tkCons, tkTuple2, tkTuple3
       ) where


-------------------------------------------------------
-- Keywords
-------------------------------------------------------

tkCase		= "case"
tkElse		= "else"
tkIf		= "if"
tkIn		= "in"
tkLet 		= "let"
tkLetrec	= "letrec"
tkOf		= "of"
tkPack		= "Pack"
tkThen		= "then"


keyWords 	:: [String]
keyWords 	= [tkCase, tkElse, tkIf, tkIn, tkLet, tkLetrec, tkOf, tkPack, tkThen]

-------------------------------------------------------
-- Symbols
-------------------------------------------------------


-- Relational operators

tkLT            = "<" 
tkLE            = "<="
tkEQT           = "=="
tkNEQT          = "~="
tkGT            = ">"
tkGE            = ">="


-- Arithmetical operators

tkAdd          = "+" 
tkSub          = "-" 
tkMul          = "*" 
tkDiv          = "/" 


-- Boolean operators

tkOr            = "|"
tkAnd           = "&"


-- Separators

tkEq            = "=" 
tkOB            = "(" 
tkCB            = ")" 
tkOCB           = "{" 
tkCCB           = "}" 
tkOL            = "[" 
tkCL            = "]" 
tkSemicolon     = ";" 
tkColon          = ":" 
tkComma         = "," 
tkLambda        = "\\"
tkDot           = "." 
tkArrow         = "->"

-- Other primitives

tkNegate 	= "negate"
tkNot		= "not"
tkStrict        = "strict"
tkSeq	        = "seq"


-- Constructors defined in Prelude

tkTrue		= "True"
tkFalse		= "False"
tkNil		= "Nil"
tkCons		= "Cons"
tkTuple2	= "Tuple2"
tkTuple3	= "Tuple3"
