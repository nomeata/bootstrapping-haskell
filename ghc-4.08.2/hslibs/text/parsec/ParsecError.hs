-----------------------------------------------------------
-- Daan Leijen (c) 1999-2000, daan@cs.uu.nl
--
-- $version: 17 May 2000, release version 0.3$
-----------------------------------------------------------
module ParsecError( SourceName, Line, Column                 
                  , SourcePos, sourceLine, sourceColumn, sourceName
                  , newPos, initialPos, updatePos, updatePosString
                  
                  , Message(SysUnExpect,UnExpect,Expect,Message)
                  , messageString, messageCompare, messageEq
                  
                  , ParseError, errorPos, errorMessages, errorIsUnknown
                  , showErrorMessages
                  
                  , newErrorMessage, newErrorUnknown
                  , addErrorMessage, setErrorPos, setErrorMessage
                  , mergeError
                  )
                  where


import List     (nub,sortBy)
                  
-----------------------------------------------------------
-- Source Positions
-----------------------------------------------------------                         
type SourceName     = String
type Line           = Int
type Column         = Int

data SourcePos      = SourcePos SourceName !Line !Column
		     deriving (Eq,Ord)
		

newPos :: SourceName -> Line -> Column -> SourcePos
newPos sourceName line column
    = SourcePos sourceName line column

initialPos sourceName
    = newPos sourceName 1 1

sourceName   (SourcePos name line column)   = name    
sourceLine   (SourcePos name line column)   = line    
sourceColumn (SourcePos name line column)   = column


updatePosString :: SourcePos -> String -> SourcePos
updatePosString pos string
    = forcePos (foldl updatePos pos string)

updatePos       :: SourcePos -> Char -> SourcePos
updatePos pos@(SourcePos name line column) c   
    = forcePos $
      case c of
        '\n' -> SourcePos name (line+1) 1
        '\r' -> pos
        '\t' -> SourcePos name line (column + 8 - ((column-1) `mod` 8))
        _    -> SourcePos name line (column + 1)
        
  
forcePos :: SourcePos -> SourcePos      
forcePos pos@(SourcePos name line column)
    = seq line (seq column (pos))
        
                
instance Show SourcePos where
  show (SourcePos name line column)
    | null name = showLineColumn
    | otherwise = "\"" ++ name ++ "\" " ++ showLineColumn
    where
      showLineColumn    = "(line " ++ show line ++
                          ", column " ++ show column ++
                          ")" 
                          

-----------------------------------------------------------
-- Messages
-----------------------------------------------------------                         
data Message        = SysUnExpect !String   --library generated unexpect            
                    | UnExpect    !String   --unexpected something     
                    | Expect      !String   --expecting something
                    | Message     !String   --raw message
                    
messageToEnum msg
    = case msg of SysUnExpect _ -> 0
                  UnExpect _    -> 1
                  Expect _      -> 2
                  Message _     -> 3                                  
                                      
messageCompare msg1 msg2
    = compare (messageToEnum msg1) (messageToEnum msg2)
  
messageString msg
    = case msg of SysUnExpect s -> s
                  UnExpect s    -> s
                  Expect s      -> s
                  Message s     -> s                                  

messageEq msg1 msg2
    = (messageCompare msg1 msg2 == EQ)
    
    
-----------------------------------------------------------
-- Parse Errors
-----------------------------------------------------------                           
data ParseError     = ParseError !SourcePos [Message]

errorPos :: ParseError -> SourcePos
errorPos (ParseError pos msgs)
    = pos
                  
errorMessages :: ParseError -> [Message]
errorMessages (ParseError pos msgs)
    = sortBy messageCompare msgs      
        
errorIsUnknown :: ParseError -> Bool
errorIsUnknown (ParseError pos msgs)
    = null msgs
            
            
-----------------------------------------------------------
-- Create parse errors
-----------------------------------------------------------                         
newErrorUnknown pos
    = ParseError pos []
    
newErrorMessage msg pos  
    = ParseError pos [msg]

addErrorMessage msg (ParseError pos msgs)
    = ParseError pos (msg:msgs)
    
setErrorPos pos (ParseError _ msgs)
    = ParseError pos msgs
    
setErrorMessage msg (ParseError pos msgs)
    = ParseError pos (msg:filter (not . messageEq msg) msgs)
 
    
mergeError :: ParseError -> ParseError -> ParseError
mergeError (ParseError pos msgs1) (ParseError _ msgs2)
    = ParseError pos (msgs1 ++ msgs2)
    


-----------------------------------------------------------
-- Show Parse Errors
-----------------------------------------------------------                         
instance Show ParseError where
  show err
    = show (errorPos err) ++ ":" ++ 
      showErrorMessages "or" "unknown parse error" 
                        "expecting" "unexpected" "end of input"
                       (errorMessages err)


-- Language independent show function
showErrorMessages msgOr msgUnknown msgExpecting msgUnExpected msgEndOfInput msgs
    | null msgs = msgUnknown
    | otherwise = concat $ map ("\n"++) $ clean $
                 [showSysUnExpect,showUnExpect,showExpect,showMessages]
    where
      (sysUnExpect,msgs1)   = span (messageEq (SysUnExpect "")) msgs
      (unExpect,msgs2)      = span (messageEq (UnExpect "")) msgs1
      (expect,messages)     = span (messageEq (Expect "")) msgs2
    
      showExpect        = showMany msgExpecting expect
      showUnExpect      = showMany msgUnExpected unExpect
      showSysUnExpect   | not (null unExpect) ||
                          null sysUnExpect       = ""
                        | null firstMsg          = msgUnExpected ++ " " ++ msgEndOfInput
                        | otherwise              = msgUnExpected ++ " " ++ firstMsg
                        where
                          firstMsg  = messageString (head sysUnExpect)
                        
      showMessages      = showMany "" messages

      
      --helpers                                                                                                                                        
      showMany pre msgs = case (clean (map messageString msgs)) of
                            [] -> ""
                            ms | null pre  -> commasOr ms
                               | otherwise -> pre ++ " " ++ commasOr ms
                            
      commasOr []       = ""                
      commasOr [m]      = m                
      commasOr ms       = commaSep (init ms) ++ " " ++ msgOr ++ " " ++ last ms
        
      commaSep          = seperate ", " . clean
      semiSep           = seperate "; " . clean       
        
      seperate sep []   = ""
      seperate sep [m]  = m
      seperate sep (m:ms) = m ++ sep ++ seperate sep ms                            
      
      clean             = nub . filter (not.null)                  
      
