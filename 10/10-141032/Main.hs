import Interpreter
import Parser

main :: IO ()
main = 
  do putStr "> "
     text <- getLine
     case parse start text of
       [] -> putStrLn "Parse Error"
       ((exp, s):xs) -> print (eval exp)
     main