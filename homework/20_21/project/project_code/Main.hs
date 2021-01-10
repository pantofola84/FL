module Main where


import System.IO
import Parse
import ParseProg


readF :: IO String
readF = do
 inh <- openFile "input.txt" ReadMode
 prog <- hGetContents inh
 return prog


main :: IO ()
main = do
 inp <- readF
 print (parse parseProg inp)


comp :: [(Program Name, Name)] -> Program Name
comp [] = error "no parse"
comp [(e,[])] = e
comp [(_,a)] = error ("doesn't use all input"++ a)
