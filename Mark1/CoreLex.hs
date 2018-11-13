-------------------------------------------------
-- Lexical definitions for the language 
--
--  "Implementing Functional languages"
--
-- José E. Gallardo, December 1997
-------------------------------------------------

module CoreLex (
         tkCase, tkIn, tkLet, tkLetrec, tkOf, tkPack,
         keyWords,
         tkLT, tkLE, tkEQT, tkNEQT, tkGT, tkGE,
         tkEq,tkPlus,tkMinus,tkTimes,tkDiv,
         tkOB,tkCB,tkOCB,tkCCB,tkOL,tkCL,
         tkSemicolon,tkCons,tkComma,tkLambda,tkDot,tkArrow,
         tkOr,tkAnd        
       ) where


-------------------------------------------------------
-- Keywords
-------------------------------------------------------

tkCase		= "case"
tkIn		= "in"
tkLet 		= "let"
tkLetrec	= "letrec"
tkOf		= "of"
tkPack		= "Pack"


keyWords 	:: [String]
keyWords 	= [tkCase, tkIn, tkLet, tkLetrec, tkOf, tkPack]

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

tkPlus          = "+" 
tkMinus         = "-" 
tkTimes         = "*" 
tkDiv           = "/" 


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
tkCons          = ":" 
tkComma         = "," 
tkLambda        = "\\"
tkDot           = "." 
tkArrow         = "->"
