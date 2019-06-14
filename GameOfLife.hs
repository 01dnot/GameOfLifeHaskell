import System.IO
import Data.Char
import Data.List.Split
import Control.Concurrent

type Board = [Pos]
type Pos = (Int,Int)
type Rule = (Char, Int, Int)

defaultSurviveRule :: Rule
defaultSurviveRule = ('s',2,3)

defaultBirthRule :: Rule
defaultBirthRule = ('b',3,3)

--Initialize default playingmode
main = play [] 0 defaultSurviveRule defaultBirthRule

play b nr srule brule  =  do
  line <- getLine
  case line of

    "q" -> return ()

    ('c':xs) -> do
      let nr = read xs :: Int
      showBoard nr []
      play [] nr srule brule

    ('n':' ':xs) -> do
      let digits = map (read :: String -> Int) (splitOn " " xs)
          newGen = addCell b (head digits, digits !! 1)
      showBoard nr newGen
      play newGen nr srule brule

    ('d':' ':xs) -> do
      let digits = map (read :: String -> Int) (splitOn " " xs)
          newGen = deleteCell b (head digits, digits !! 1)
      showBoard nr newGen
      play newGen nr srule brule

    ('s':' ':xs) -> do
      let digits = map (read :: String -> Int) (splitOn " " xs)
      play b nr ('s', head digits, digits !! 1) brule

    ('b':' ':xs) -> do
      let digits = map (read :: String -> Int) (splitOn " " xs)
      play b nr  srule ('b', head digits, digits !! 1)

    ('?':xs) -> do
      let ('s', d,e) = srule
          ('b', f,g) = brule
      putStr $ "Survives for " ++ show d ++ "-" ++ show e ++ " neighbs\n"
      putStr $ "Birth for " ++ show f ++ "-" ++ show g ++ " neighbs\n"
      play b nr srule brule

    "CR" -> do
      let newGen = nextgen nr b srule brule
      showBoard nr newGen
      if newGen == b then putStr "Stable configuration is reached\n" else putStr ""
      play newGen nr srule brule

    ('l':' ':xs) -> do
      let gen = read xs :: Int
      newGene <- autoPlay gen b nr srule brule
      play newGene nr srule brule

    ('w':' ':xs) -> do
      let  ('s', x, y) = srule
           ('b', q, w) = brule
           printSrule = "s " ++ show x ++ " " ++ show y
           printBrule = " b " ++ show q ++ " " ++ show w
           printN = " " ++ show nr
           printBoard = concat [" "++ show n ++ " " ++ show m|(n,m)<-b]
      writeFile xs $ printSrule ++ printBrule ++ printN ++ printBoard
      play b nr srule brule

    ('r':' ':xs) -> do
      infile <- readFile xs
      let str = splitOn " " infile
          readRule1 = (head (head str), read (str !! 1) :: Int, read (str !! 2) :: Int)
          readRule2 = (head (str !! 3), read (str !! 4) :: Int, read (str !! 5) :: Int)
          newSRule = if head (head str) == 's' then readRule1 else readRule2
          newBRule = if head (head str) == 's' then readRule2 else readRule1
          readN = read (str !! 6) :: Int
          readB = [(read (str !! (y-1)) :: Int, read (str !! y) :: Int)| y <- [8,10..(length str-1)]]
      showBoard readN readB
      play readB readN newSRule newBRule

    _ -> do
      putStr "Kommando ikke støttet, prøv på nytt:\n"
      play b nr srule brule

autoPlay :: Int -> Board -> Int -> Rule -> Rule -> IO Board
autoPlay x b nr srule brule | x <= 0 = return b
                            | b == nextgen nr b srule brule =
                                do putStr "Stable configuration is reached\n"
                                   return b
                            | otherwise =
                                do
                                  let newgen = nextgen nr b srule brule
                                  showBoard nr newgen
                                  threadDelay (1000*100)
                                  autoPlay (x-1) newgen nr srule brule

showBoard :: Int -> Board -> IO ()
showBoard n b = do clearScreen
                   writeTop n
                   mapM_ (\i -> writerow i n b) [1..n]

writeTop :: Int -> IO ()
writeTop n = printAtPos (5, 0) $ concat [show i ++ if i > 9 then " " else "  " | i <- [1..n]] ++"\n"

printAtPos :: Pos -> String -> IO ()
printAtPos p xs = goto p >> putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr $ "\ESC["++show y++";"++show x++"H"

writerow :: Int -> Int -> Board -> IO ()
writerow y n b = do
                  printAtPos (if y > 9 then 1 else 2, 1+y) (show y)
                  mapM_ (\x -> writePos (x,y) b) [1..n]
                  putStrLn ""

writePos :: Pos -> Board -> IO ()
writePos p b = if isAlive b p then putStr "  X" else putStr "  ."

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not $ isAlive b p

neighbs :: Int -> Pos -> [Pos]
neighbs n (x,y) = filter (insideBoard n) [(x-1,y-1), (x,y-1)
                                         ,(x+1,y-1), (x-1,y)
                                         ,(x+1,y), (x-1,y+1)
                                         ,(x,y+1), (x+1,y+1)]

insideBoard :: Int -> Pos -> Bool
insideBoard n (x,y) = x > 0 && y > 0 && x <= n && y <= n

liveneighbs :: Int -> Board -> Pos -> Int
liveneighbs n b = length . filter (isAlive b) . neighbs n

survivors :: Int -> Board -> Rule -> [Pos]
survivors n b ('s',x,y) = [p | p<-b,  liveneighbs n b p `elem` [x..y]]

births :: Int -> Board -> Rule -> [Pos]
births n b ('b',x,y)  = [p | p <- removeEquals (concatMap (neighbs n) b)
                        , isEmpty b p, liveneighbs n b p >= x, liveneighbs n b p <= y]

removeEquals :: Eq a => [a] ->[a]
removeEquals [] = []
removeEquals (x:xs) = x : removeEquals (filter (/= x) xs)

nextgen :: Int -> Board -> Rule -> Rule -> Board
nextgen n b srule brule = survivors n b srule ++ births n b brule

addCell :: Board -> Pos -> Board
addCell b p = removeEquals (p:b)

deleteCell :: Board -> Pos -> Board
deleteCell b p = filter (/=p) b
