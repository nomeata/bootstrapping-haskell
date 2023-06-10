-----------------------------------------------------------
-- Daan Leijen (c) 1999, daan@cs.uu.nl
--
-- $version: 17 May 2000, release version 0.3$
-----------------------------------------------------------
module ParsecLanguage( haskellDef, haskell
                     , mondrianDef, mondrian
                   
                     , emptyDef
                     , haskellStyle
                     , javaStyle   
                     , LanguageDef (..)                
                     ) where
import Parsec
import ParsecToken 

           
-----------------------------------------------------------
-- Styles: haskellStyle, javaStyle
-----------------------------------------------------------               
haskellStyle= emptyDef                      
                { commentStart   = "{-"
                , commentEnd     = "-}"
                , commentLine    = "--"
                , nestedComments = True
                , identStart     = letter
                , identLetter	 = alphaNum <|> oneOf "_'"
                , opStart	 = opLetter haskellStyle
                , opLetter	 = oneOf ":!#$%&*+./<=>?@\\^|-~"              
                , reservedOpNames= []
                , reservedNames  = []
                , caseSensitive  = True                                   
                }         
                           
javaStyle   = emptyDef
		{ commentStart	 = "/*"
		, commentEnd	 = "*/"
		, commentLine	 = "//"
		, nestedComments = True
		, identStart	 = letter
		, identLetter	 = alphaNum <|> oneOf "_'"
		-- fixed set of operators: use 'symbol'
		, reservedNames  = []
		, reservedOpNames= []	
                , caseSensitive  = False				  
		}

-----------------------------------------------------------
-- minimal language definition
-----------------------------------------------------------                
emptyDef    = LanguageDef 
               { commentStart   = ""
               , commentEnd     = ""
               , commentLine    = ""
               , nestedComments = True
               , identStart     = unexpected "identifier"
               , identLetter    = unexpected "identifier"
               , opStart        = unexpected "operator"
               , opLetter       = unexpected "operator"
               , reservedOpNames= []
               , reservedNames  = []
               , caseSensitive  = True
               }
                


-----------------------------------------------------------
-- Haskell
-----------------------------------------------------------               
haskell :: TokenParser
haskell      = makeTokenParser haskellDef

haskellDef   = haskell98Def
	        { identLetter	 = identLetter haskell98Def <|> char '#'
	        , reservedNames	 = reservedNames haskell98Def ++ 
    				   ["foreign","import","export","primitive"
    				   ,"_ccall_","_casm_"
    				   ,"forall"
    				   ]
                }
			    
haskell98Def = haskellStyle
                { reservedOpNames= ["::","..","=","\\","|","<-","->","@","~","=>"]
                , reservedNames  = ["let","in","case","of","if","then","else",
                                    "data","type",
                                    "class","default","deriving","do","import",
                                    "infix","infixl","infixr","instance","module",
                                    "newtype","where",
                                    "primitive"
                                    -- "as","qualified","hiding"
                                   ]
                }         
                
                
-----------------------------------------------------------
-- Mondrian
-----------------------------------------------------------               
mondrian :: TokenParser
mondrian    = makeTokenParser mondrianDef

mondrianDef = javaStyle
		{ reservedNames = [ "case", "class", "default", "extends"
				  , "import", "in", "let", "new", "of", "package"
				  ]	
                , caseSensitive  = True				  
		}

				
