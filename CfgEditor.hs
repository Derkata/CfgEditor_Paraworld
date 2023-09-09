{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Applicative
import System.IO
import Data.List.Split (splitOn)
import System.Environment
import System.Directory
import Data.Char (isAsciiLower, isAsciiUpper, isDigit,toUpper,isUpper,isLower, isSpace)
import Control.Exception
import System.Exit
import System.FilePath(takeDirectory)
import Text.ParserCombinators.ReadP (skipSpaces)
import Foreign (ptrToIntPtr)

newtype Parser a = Parser { parse :: String -> [(a,String)] }
--Basic bind for parsers
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser (concatMap (\(a, s') -> parse (f a) s') . parse p)
--Unit mainly used for type convert (a->Parser a)
unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

--taketoken , useful to apply parser to a string and just return the parsed value
taketoken::Parser a->String->a
taketoken p str = fst (head (parse p str))
--takerest , ignores the token and just returns whats left of a string after parsing
takerest::Parser a->String ->String
takerest p str = snd (head (parse p str))

{-
Parsers are instances of certain typeclasses
-}
instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

--Single Char parsing
item :: Parser Char
item = Parser (\s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)])

--Name not to be confused with Data.List.Split functions
--oneOf1 takes a string [Char] and parses a single char that is from the string
oneOf1 :: String -> Parser Char
oneOf1 s = satisfy (`elem` s)
--Satisfy creates parser of char, only if that char satisfies the predicate p
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = bind item (\c ->
  if p c
  then unit c
  else Parser (const []))
--combines two parsers
combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)
--fails on any input
failure :: Parser a
failure = Parser (const [])
-- if can parse p then parse p if can parse q parse q
option :: Parser a -> Parser a -> Parser a
option  p q = Parser $ \s ->
  case parse p s of
    []     -> parse q s
    res    -> res
--like item but can isolate char as par
char :: Char -> Parser Char
char c = satisfy (c ==)

--parses the string
string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}
--token space eater
spaces :: Parser String
spaces = many (oneOf1 "' '\n\r\\\b\t")

token :: Parser a -> Parser a
token p = do
  {
    a <- p;
    spaces ;
    return a
    }

reserved :: String -> Parser String
reserved s = token (string s)

digit :: Parser Char
digit = satisfy isDigit

uscore :: Parser Char
uscore = satisfy (=='_')


lower::Parser Char
lower = satisfy isLower

upper::Parser Char
upper = satisfy isUpper

letter::Parser Char
letter = combine lower upper

alphanum::Parser Char
alphanum =  letter <|> digit
--integrated seq for some reason didn't work as intended,so I rework it
seqStr::Parser String -> Parser String-> Parser String
seqStr p q = bind p (\x-> bind q (\xs-> unit (x++xs)))

sepBy1::Parser a->Parser b->Parser [b]
sepBy1 sep element = (:) <$> element <*> many (sep *> element) <|> pure []


validData::[(a,String)]->Bool
validData [(a,"")] = True
validData _ = False

--Settings.cfg paraworld
type VarName = String
type Value = String
type RootName = String
data Tree = CV VarName Value | Node RootName [Tree]
instance Show Tree where
 show t = showTree t 0

fixRootName str
 |elem '=' str = (takeWhile (/='=') str) ++ " = " ++ (tail . dropWhile (/='=')) str
 |otherwise = str
tabNode (CV _ _ ) = []
tabNode (Node _ _) = "\t"
showTree::Tree->Int->String
showTree (CV varname value) i = tabs++varname++" = "++"'"++value++"'\n"
  where tabs =replicate i '\t'
showTree (Node rootName ls) 0 = fixRootName rootName++" {\n"++concatMap (\x-> tabNode x ++showTree x 1 ) ls ++"}\n"
showTree (Node rootName ls) i = init tabs++fixRootName rootName++" {\n"++concatMap (\x-> tabNode x ++showTree x (i+1) ) ls ++tabs++"}\n"
  where tabs =replicate i '\t'
--Paraworldstuffparse
set = ['a'..'z']++['A'..'Z']++[':','$','#','(',')','[',']','_','/','-']++['0'..'9']
var :: Parser String
var = negative <|>  many (oneOf1 set)
varSpecial = negative <|>  many (oneOf1 set)
negative =
  do
    a<-reserved "-"
    b<- many digit
    return (a++b)
parseNull::Parser Tree
parseNull =
  do
     varname <- var
     reserved "=\'\'"
     spaces <|> many (char '{')
     return (CV varname "")

parseVal::Parser Tree
parseVal = do
  varname <- var
  return (CV varname "")
parseNullStr::Parser String
parseNullStr =
  do
     varname <- var
     reserved "=''"
     spaces <|> many (char '{')
     return (varname++"=\'\'")
parseSpecial::Parser String
parseSpecial = do
  a<-sepBy1 (char ':') var
  return $ init $concatMap (++":") a

parseNumRoot::Parser String
parseNumRoot = do
  varname <-var
  reserved "="
  spaces
  a<-negative <|> many digit
  spaces
  return (varname++"="++a)

quotes::Parser String
quotes = do
    --char '\''
    s <- many $ satisfy (/= '\'')
    reserved "\'"

    return s
errorCode1 a b =  do putStrLn $ "Detected: "++a++"="++b
stop::Parser Tree
stop = do
  a<- var <|> negative <|> many digit
  char '='
  b<- negative <|> many digit 
  let s = errorCode1 a b
  return (CV a b)
parseCV::Parser Tree
parseCV =
    parseNull
    <|>
    do
    varname <- var
    spaces
    reserved "="
    CV varname <$> quotes
    <|>
    stop
parseValue::Parser Tree
parseValue = do
  p <- var
  return (CV p "") 


eqPart::Parser String
eqPart = do
     parseNullStr
     <|> do
     a<-reserved "="
     --spaces *> reserved "\'"
     b<-many $ satisfy (/= '\'')
     char '\''
     return (a++"\'"++b++"\'")
     <|>
     spaces

parseTree::Parser Tree
parseTree =
  do
     root <-  var <|> parseSpecial <|> parseNumRoot
     a<-eqPart
     reserved "{"
     --ls<- parseCV
     ls <-sepBy1 (char ';') (parseTree <|> parseValue)
     reserved "}"
     return  (Node (root++a) ls)
   <|>
   parseCV
{-
safetyFirst::Parser String 
safetyFirst = do
    spaces
    varname <- var
    s <- many $ satisfy (/= '\'')
    return (varname++s++"|")
allParse::Parser Char
allParse = do
  s <-  satisfy (/= '}')
  return (s)
parseCVS::Parser String
parseCVS =
    do
    varname <- var
    spaces
    reserved "="
    spaces
    s <- many $ satisfy (/= '\'')
    char '\''
    return (varname++" = "++ s++"|")
-- next time will try something for now is gg 
freeParseTree :: Parser String
freeParseTree =  
  do 
     spaces 
     root <- var
     spaces
     a<-eqPart
     spaces
     reserved "{"
     spaces
     ls <-sepBy1 (spaces *> char '|' <* spaces) freeParseTree
     spaces
     return  ((root++a)++"{"++concat ls++"}")
   <|>
   parseCVS
   <|>
   many allParse
   




unparsedPart str = if null p then do putStrLn "Parse failed miserably" else do putStrLn $ "Parsed part : \n"++show a++"\n Unparsed: "++b
  where p@[(a,b)] =  parse freeParseTree str
-}
tryExtract str = if null $ fst datas then CV "Parse Error" ""
 else part1
 where
      datas = (parse parseTree str,[])
      part1 = fst . head $ fst datas

alphanumeric arg = any (\x->x arg) [isDigit,isAsciiLower,isAsciiUpper,(=='_')]
 --spacenewlinetrick pls dont write inside the cfg urself

modifySymbol :: Char -> Bool -> String -> String
modifySymbol _ b [] = []
modifySymbol _ b [x]  = [x]
modifySymbol s b (x:y:xs)
 -- |x==s && y==s && not b= modifySymbol s b (y:xs)
 |isSpace x && not b = modifySymbol s b (y:xs)
 |elem x set && y=='\n' && not b =modifySymbol s b (x:s:xs)
 |x==s && y=='}' && not b = modifySymbol s b (y:xs)
 |x=='\'' && elem y set && b = x:s:modifySymbol s False (y:xs)
 |x=='\'' &&  elem y set = x:y:modifySymbol s True xs
 |x=='}' &&  elem y set  = x:s:modifySymbol s b (y:xs)
 |x=='\'' = x:modifySymbol s (not b) (y:xs)
 |otherwise = x:modifySymbol s b (y:xs)
iotest::Bool->String->IO String
iotest b hpath =
  do
     tempPath <- getAppUserDataDirectory "SpieleEntwicklungsKombinat\\Paraworld"
     check <- checkMonadSpam hpath
     handle <- openFile hpath ReadMode
     contents <- hGetContents handle
     let filt = modifySymbol ';' False . modifySymbol  ';' False $ contents
     putStrLn "Parsing..."
     let ptr = tryExtract filt
     if not$ eqTree ptr (CV "Parse Error" []) then do
       putStrLn "Parsing Successful"
       if b then do
          putStrLn "Commands are:\n Set(path,value) - set argpath argvalue \n Get(path) - get argpath \n Remove(path) - remove path \n Q - exit \n CD path - change directory \n PWD - current directory"
          iotest False hpath
       else do
       arg<-getLine
       if map toUpper arg=="Q" then return "Exit"
       else if map toUpper (head (cdfix arg)) =="CD" then do
        let p = head $ tail $cdfix arg
        putStrLn $ "Command: CD \n| argpath = "++p++"\n"
        check<-checkMonadSingle p
        if check then do
        iotest False p
        else do
          putStrLn "Not a valid path"
          iotest False hpath
       else if map toUpper arg=="PWD" then do
        putStrLn $ "Command: PWD \n |Current Dir:\n"++hpath
        iotest False hpath else do
       let com = trComm arg
       let cs = "-s"
       let get = "-g"
       let rem = "-r"
       if length com == 3 && head com == cs then do
          let arg1 = com!!1
          let arg2 = com!!2
          putStrLn $ "Command: Set \n| argpath = "++arg1++"\n| argvalue = "++arg2
          let carg1= convertSet (arg1,arg2)
          let set = uncurry (addToTree ptr) carg1
          if strongEqTree set ptr || not (parseCheck set) then do
                  putStrLn $"Set Failed | Nothing was changed"
                  iotest False hpath
          else do
          (tempName, tempHandle) <- openTempFile tempPath "temp"
          hPutStr tempHandle $ show set
          hClose handle
          hClose tempHandle
          renameFile tempName hpath
          iotest False hpath
        else if length com == 2 then
            if head com == get then do
               putStrLn $ "Command: Get \n| argpath = "++com!!1
               let carg = convertGet $com!!1
               putStrLn $ "Value:"++getTree ptr carg
               iotest False hpath
            else if head com == rem then do
               putStrLn $ "Command: Remove \n| argpath = "++com!!1
               let carg2 = convertGet $ com!!1
               let r = deleteTree ptr carg2
               if strongEqTree r ptr then do
                  putStrLn "Remove Failed | Nothing was changed"
                  iotest False hpath
               else
                do
                   (tempName, tempHandle) <- openTempFile tempPath "temp"
                   hPutStr tempHandle $ show r
                   hClose handle
                   hClose tempHandle
                   renameFile tempName hpath
                   iotest False hpath
            else do
              putStrLn $ "Not a valid command with "++show (length com-1)++" arguments"
              iotest False hpath
          else do
            putStrLn $"Not a valid command with "++show (length com-1)++" arguments"
            iotest False hpath
        else
          do
             putStrLn "Parsing Failed"
             return "False"
     --writeFile "Settings1.cfg" $show d

strongEqTree::Tree->Tree->Bool
strongEqTree (CV _ _) (Node _ _) = False
strongEqTree (Node _ _) (CV _ _) = False
strongEqTree (CV n1 t1) (CV n2 t2) = n1==n2 && t1==t2
strongEqTree (Node n1 t1) (Node n2 t2) = n1 == n2 && length t1==length t2 && and (zipWith strongEqTree t1 t2)
--s
eqTree (CV _ _) (Node _ _) = False
eqTree (Node _ _) (CV _ _) = False
eqTree (CV n1 _) (CV n2 _) = n1==n2
eqTree (Node n1 t1) (Node n2 t2) = n1 == n2

rootHandler root = if length first == 1 then head first else init $ head first
  where
    first = splitOn "\'" root
nodeTrees (Node _ tree) = tree
rootTree (Node root _) = root
rootTree (CV _ _) = []
addToTree::Tree->[String]->(VarName,Value)->Tree
addToTree (CV "Root" value) [] ("Root",vvalue) = CV "Root" vvalue
addToTree (CV "Root" value)  ls (vname,vvalue) = addToTree (Node "Root" []) ls (vname,vvalue)
addToTree y@(Node root tree) [] (vname,vvalue) = y
addToTree y@(Node root tree) [x] (vname,vvalue)
  |rootHandler root == x && null search && null searchTree= Node root $tree++[CV vname vvalue]
  |rootHandler root == x && null searchTree = Node root filtered
  |rootHandler root == x && not (null searchTree) = Node root filteredTree
  |otherwise = y
  where
      search = [CV n vvalue|(CV n v)<-tree,vname==n]
      searchTree = [Node r t | (Node r t)<-tree,vname==rootHandler r]
      filtered = map (\x->if not$ eqTree x (CV vname "") then x else CV vname vvalue) tree
      filteredTree = map (\x->if not$ eqTree (Node (rootHandler (rootTree x)) []) (Node vname []) then x else Node (rootHandler vname++"="++['\'']++vvalue++['\'']) (nodeTrees x) ) tree
addToTree z@(Node root tree) (x:y:xs) cv@(vname,vvalue)
  |rootHandler root == x = if null search then addToTree (Node root (tree++[Node y []])) (x:y:xs) cv else Node root filtered
  |otherwise = z
   where
      search = [Node r t|(Node r t)<-tree,rootHandler r==y]
      filtered = map (\x->if eqTree (Node (rootHandler . rootTree $ x) []) (Node y []) then addToTree x (y:xs) cv else x) tree

convertSet1 (t,v) = (mrh $ init split,(l,v))
  where
    split = splitOn "/" t
    l = last split
    mrh = map rootHandler

rsplitOn delim str= case sp of
                              [a,b,c]->[a,b,c]
                              [a,b]->[a,b,""]
                              [a]->[a,"",""]
                              []->["","",""]
  where sp = splitOn delim str

convertSet :: ([Char], b) -> ([[Char]], ([Char], b))
convertSet (t,v) =if cnt<3 then convertSet1 (t,v) else (mrh final,(l,v))
        where
         sp@[a,b,c] = rsplitOn "\'" t
         cnt = length [s|s<-sp,s==""]
         fixFirst =  as++["\'"++b++"\'"]++cs
         as = splitOn "/" a
         cs = splitOn "/" c
         l = last cs
         mrh = map rootHandler
         final = init as ++ [last as ++ "\'"++b++"\'"]++init cs



convertGet  = splitOn "/"

getTree (CV name v) [x]
 |name==x = v
 |otherwise = "Err:No value found"
getTree (Node root _) [x] = "Err:No value found"
getTree (Node root tree) (x:y:xs)
  |root == x && not (null filtNodes) = getTree (head filtNodes) (y:xs)
  |root == x && not (null filtLeafs) = getTree (head filtLeafs) (y:xs)
  |otherwise = "Err:No value found"
    where
      filtNodes = [Node r t|(Node r t)<-tree,r==y]
      filtLeafs = [CV n v|(CV n v)<-tree,n==y]


deleteTree1::Tree->[String]->Tree
deleteTree1 q@(CV _ _ ) _ = q
deleteTree1 q [x] = q
deleteTree1 a@(Node root tree) [x,y]
  |rootHandler root == x && not (null filtNodes) = Node root filteredNodes
  |rootHandler root == x && not (null filtLeafs) = Node root filteredLeafs
  |otherwise = a
    where
     filtNodes = [Node r t | (Node r t)<-tree,rootHandler r==y]
     filtLeafs = [CV n v|(CV n v)<-tree,n==y]
     filteredNodes = filter (\x->not $eqTree (Node (rootHandler $ rootTree x) []) (Node y [])) tree
     filteredLeafs = filter (\x->not $eqTree x (CV y [])) tree
deleteTree1 a@(Node root tree) (x:y:xs)
 |x == rootHandler root = Node root (map (\x->deleteTree1 x (y:xs)) tree)
 |otherwise = a

deleteTree::Tree->[String]->Tree
deleteTree q@(CV _ _) _ = q
deleteTree a@(Node root tree) b =Node a1 $map (`deleteTree` b) filt
 where
  filt = filter (not . isEmptyNode) b2
  isEmptyNode::Tree->Bool
  isEmptyNode (Node _ []) = True
  isEmptyNode _ = False
  (Node a1 b2) = deleteTree1 a b

openMonad path = handle (\(e::IOException) -> return Nothing) $ do
  h <- openFile path ReadMode
  return (Just h)
checkMonadSingle :: String -> IO Bool
checkMonadSingle path = do
     tryopen <- openMonad path
     case tryopen of
      Nothing -> return False
      (Just h) ->do
         hClose h
         return True

checkMonadSpam :: String -> IO Bool
checkMonadSpam path = do
     tryopen <- openMonad path
     case tryopen of
      Nothing -> do
        putStrLn "File not found | Please write a valid path or close the program:"
        p <- getLine
        checkMonadSpam p
      (Just h) ->do
         hClose h
         return True
mhm::[String]->IO () --microHandleMonad
mhm com@[r,p,path]
 |r=="-r" || r=="--remove" = do
               check <- checkMonadSingle path
               if not check then die $"No Settings file found in "++path
               else do
               handle <- openFile path ReadMode
               contents <- hGetContents handle
               let filt = modifySymbol ';' False . modifySymbol  ';' False $ contents
               let ptr = tryExtract filt
               if eqTree ptr (CV "Parse Error" []) then die "File could't parse in the new location"
               else do
               let carg2 = convertGet p
               let r = deleteTree ptr carg2
               if strongEqTree r ptr then
                die "Exit with Code (201) - Nothing was changed" --Remove changed nothing
               else do
                 let tempPath = sp path
                 (tempName, tempHandle) <- openTempFile tempPath "temp"
                 hPutStr tempHandle $ show r
                 hClose handle
                 hClose tempHandle
                 renameFile tempName path

mhm com@[c,p,value,path]
  |c=="-s" || c=="--set" = do
               check <- checkMonadSingle path
               if not check then die $"No Settings file found in "++path
               else do
               handle <- openFile path ReadMode
               contents <- hGetContents handle
               let filt = modifySymbol ';' False . modifySymbol  ';' False $ contents
               let ptr = tryExtract filt
               if eqTree ptr (CV "Parse Error" []) then die "File could't parse"
               else do
               let carg1= convertSet (p,value)
               --putStrLn $ p++","++value++"|"++show ptr
               let set = uncurry (addToTree ptr) carg1
               if not (parseCheck set) then do
                hClose handle
                die "Set failed , nothing was changed"
               else do
               let tempPath =sp path
               (tempName, tempHandle) <- openTempFile tempPath "temp"
               hPutStr tempHandle $ show set
               hClose handle
               hClose tempHandle
               renameFile tempName path
  |otherwise = die "If you get to this error you are insane"
mhm com  = die $"Set didn't have the right arguments"++concatMap ("\n"++) com
main:: IO String
main =
  do
     hcodepath <- getAppUserDataDirectory "SpieleEntwicklungsKombinat\\Paraworld\\Settings.cfg"
     tempPath <- getAppUserDataDirectory "SpieleEntwicklungsKombinat\\Paraworld"
     check <- checkMonadSingle hcodepath
     if not check then die "No Settings file found in AppData"
     else do
     args<-getArgs
     handle <- openFile hcodepath ReadMode
     contents <- hGetContents handle
 
     let filt = modifySymbol ';' False . modifySymbol  ';' False $ contents
     let ptr = tryExtract filt
    -- putStrLn $ filt
     if not$ eqTree ptr (CV "Parse Error" []) then
       case args
          of
            com@[command,path,value,cdd]
             |com `elem` [["-s",path,value,cdd],["--set",path,value,cdd]]->
              do
                hClose handle
                mhm com
                return "Spawned"

            com@[command,path,value]
             |com `elem` [["-s",path,value],["--set",path,value]] ->
             do
               let carg1= convertSet (path,value)
               let set = uncurry (addToTree ptr) carg1
               if not (parseCheck set) then die "Set failed nothing was changed" else do
               (tempName, tempHandle) <- openTempFile tempPath "temp"
               hPutStr tempHandle $ show set
               hClose handle
               hClose tempHandle
               renameFile tempName hcodepath
               return "Spawned"
              |com `elem` [["-g",path,value],["--get",path,value]] ->
              do
                hClose handle
                check1 <- checkMonadSingle value
                if not check1 then die $"No Settings file found in "++value
                else
                  do
                    handle1 <- openFile value ReadMode
                    contents1 <- hGetContents handle1
                    let filt1 = modifySymbol ';' False . modifySymbol  ';' False $ contents1
                    let ptr1 = tryExtract filt1
                    let cs = convertGet path
                    --putStrLn $ show $ convertGet path
                    if getTree ptr1 cs == "Err:No value found" then die "Exit with Code (301) - Get returned nothing"-- !vfound
                    else do
                      putStrLn $ getTree ptr1 cs
                      return "End"
              |com `elem` [["-r",path,value],["--remove",path,value]] -> do
                hClose handle
                mhm com
                return "End"

              |otherwise -> if command `elem` ["-s","--set"] then die "Exit with Code (101) - wrong number of parameters" -- !#par
               else
                die "Exit with Code (102) - Wrong Command"
            com@[command,path]
             |com `elem` [["-r",path],["--remove",path]] ->
               do
                 let carg2 = convertGet path
                 let r = deleteTree ptr carg2
                 if strongEqTree r ptr then
                  die "Exit with Code (201) - Nothing was changed" --Remove changed nothing
                 else do
                 (tempName, tempHandle) <- openTempFile tempPath "temp"
                 hPutStr tempHandle $ show r
                 hClose handle
                 hClose tempHandle
                 renameFile tempName hcodepath
                 return "Deded"
             |com `elem` [["-g",path],["--get",path]] ->
              do
                 let carg = convertGet path
                 if getTree ptr carg == "Err:No value found" then die "Exit with Code (301) - Get returned nothing"-- !vfound
                 else do
                  putStrLn $ getTree ptr carg
                  return "GetLow"
             |otherwise -> die "Exit with Code (102) - Wrong Command"

            [] -> do
                  putStrLn hcodepath
                  iotest True hcodepath
            _ -> die "Exit with Code (666) No valid arguments"
     else do
              --unparsedPart filt
              die "Exit with Code (100) - Parse Error"



--Set("path","value")
--Remove("path")
--Get("path")

trComm::String->[String]
trComm str = case com of
   "SET" -> "-s":args
   "GET" -> "-g":args
   "REMOVE" -> "-r":args
   _ -> ["Error"]
  where
    [t,d] = map ($str) [takeWhile (/='('),dropWhile (/='(') . takeWhile (/=')')]
    remwhite = concat . words
    com = remwhite $ map toUpper t
    args = splitOn ","  $ tail d

cdfix str =if null rest || null (tail rest) then [""] else [cd,tail rest]
  where
    (cd,rest) = splitAt 2 str

parseCheck tree
 |null parsed = False
 |otherwise = True
  where
    str = modifySymbol ';' False . modifySymbol  ';' False $ show tree
    [(a,b)] = parse parseTree str
    parsed = parse parseTree str



sp  = takeDirectory
