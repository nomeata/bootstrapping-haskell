{- this module coordinates the whole shebang.
  First splits input into `of interest' and `computer generated'
  Then parses 'of interest', and plucks out data and newtype declarations and 
  processor commands
  The commands are combined with the parsed data, and if any data is missing, 
  derive goes hunting for it, looking in likely script and interface files in
  the path variable DERIVEPATH. Derive searches recusively though modules 
  imported until all the types needed are found, or it runs out of links, 
  which causes an error -}
  
--GHC version
module ChaseImports where

import StandardRules (Tag)
import DataP
import CommandP 
import ParseLib2
import System
import List
import qualified Literate
import Monad

--- ATTENTION -------------------------------------------------------------
-- you need to uncomment the appropriate section here, as Hugs and GHC 
-- currently keeps things in different places

-- For Hugs
--import IO (try)
--

-- For GHC
--import IOBase (tryIO)
--try = tryIO

-- For nhc98
try x = catch (x >>= return . Right) (return . Left)
--

#if defined(__HASKELL98__)
#define FMAP fmap
#else
#define FMAP map
#endif

--- Split up input ---------------------------------------------------------
splitString :: String -> String -> (String,String)
splitString s = (\(x,y) -> (unlines x,unlines y)) . 
		break (\x -> x == s || x == '>':s) 
		. lines
userCode = splitString codeSeperator      
codeSeperator = "{-* Generated by DrIFT-v1.0 : Look, but Don't Touch. *-}"

-- Parser - extract data and newtypes from code  

type ToDo = [([Tag],Data)]

parser :: String -> ToDo
parser = sanitycheck . papply p (0,0) . \s -> ((0,0),s)
	where    
       p = parse . skipUntilOff $ statement +++ command
       statement = do d <- datadecl +++ newtypedecl
		      ts <- opt local
		      return (ts,d)
       sanitycheck [] = error "***Error: no DrIFT directives found\n"
       sanitycheck [(x,_)] = x
       sanitycheck ((x,_):_) = error "***Error: ambiguous DriFT directives?"

importParser :: String -> [Data]
importParser text = fst . head . papply p (0,-1) $ ((0,0),ip)
	where
	ip =  snd $ splitString "_declarations_" text
	p = parse $ skipUntilParse  ';' info 
	info = do integer
                  (datadecl+++newtypedecl)


-------Go Hunting for files, recursively ----------------------------------

chaseImports :: String -> ToDo -> IO ToDo
chaseImports txt dats = do
	(left,found) <- chaseImports' txt dats
	if (not . null) left then error ("can't find type " ++ show left)
	 else return found

chaseImports' :: String -> ToDo ->  IO (ToDo,ToDo)
chaseImports' text dats = 
  case papply (parse header) (0,-1) ((0,0),text) of
	[] -> return (dats,[])
	(modnames:_) -> foldM action (dats,[]) (fst modnames)
    where
	action :: (ToDo,ToDo) -> FilePath -> IO (ToDo,ToDo)     
	action (dats,done) m | null dats = return ([],done)
			     | otherwise = do 
	     paths <- FMAP breakPaths (getEnv "DERIVEPATH")
	     -- may want a few more envs here ...
	     c <- findModule paths m
	     let (found,rest) = scanModule dats c
	     if (null rest) then return ([],done ++ found) -- finished
	       else do  (dats',done') <- chaseImports' c rest
			return (dats',done' ++ done ++ found) 
			
-- break DERIVEPATH into it's components			
breakPaths :: String -> [String]
breakPaths x = case break (==':') x of
	(p,(_:pp)) -> p: breakPaths pp
	(p,[]) -> [p]

-- search though paths, using try
findModule :: [String] -> String -> IO String
findModule paths modname = let
	action p = try $ do 
			    h <- readFile p 
 	                    return (h,p)
	fnames = combine paths modname
	isLeft (Left _ ) = True
	isLeft _ = False
     in do
	hh <- mapM action fnames
	let (h,p) = case dropWhile (isLeft) hh of
	           ((Right h):_) -> h
		   _ -> error ("can't find module " ++ modname)
	putStrLn p
       	return $ fromLit (isLiterate h) h

-- generate filepaths by combining module names with different suffixes.
-- Note : Dedicated Hugs-only users may wish to remove ".hi" from the list of
-- file types to search.  
combine :: [String] -> String -> [FilePath]	
combine paths modname = [p++'/':f| f <- toFile modname, p <- ("." :paths)]
	where
	     toFile :: String -> [String]
	     toFile l = [l++".hs",l++".lhs",l++".hi"] 

-- pluck out the bits of interest
scanModule :: ToDo -> String -> (ToDo,ToDo)
scanModule dats txt = let 
	newDats = filter isData . parse $ txt
	parse l = case head ( words l) of 
			"_interface_" -> importParser $ l
			_ -> map snd . parser . fst . userCode $ l
	in (resolve newDats dats ([],[]))

-- update what's still missing
resolve :: [Data] -> ToDo -> (ToDo,ToDo) -> (ToDo,ToDo)
resolve _ [] acc = acc
resolve parsed ((tags,TypeName t):tt) (local,imports) = 
	case filter ((== t) . name) parsed of
		[x] -> resolve parsed tt ((tags,x):local,imports)
		_ -> resolve parsed tt (local,(tags,TypeName t):imports)


--handle literate scripts ---------------------------------------------------
-- NB we don't do the latex-style literate scripts currently.
fromLit True txt = case Literate.process txt of
			  ([],s) -> s
			  (e,_) -> error e
fromLit False txt = txt

toLit True = unlines . map (\l -> '>':l)  . lines
toLit False = id       
 
isLiterate :: String -> Bool
isLiterate = any ((=='>'). head) . words

-- utils -- this should be the sort of thing automatically generated !!
isData D{} = True
isData _ = False


