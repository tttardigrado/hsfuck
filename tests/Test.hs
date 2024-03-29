module Main where

import Lang ( IOKind (..), BF, Op(Loop, Out, Inc, Inp, Mul, Set, Mov, Sft, Dup), IOKind (KChr) )
import Optimizer ( optimizeBF )
import Parser ( parseBF )
import System.Exit ( exitSuccess, exitFailure )

main :: IO ()
main = if null testAll
  then putStrLn "All tests passed"           >> exitSuccess
  else putStrLn ("Failed: " ++ show testAll) >> exitFailure

-- run all tests
testAll :: [(Int, String, String)]
testAll = isValidAll 0 allTests

-- validate all tests (tagged with an integer)
isValidAll :: Int -> [(String, String, Bool)] -> [(Int, String, String)]
isValidAll _ [] = []
isValidAll n ((x, y, False):xs) = (n, x,y) : isValidAll (n+1) xs
isValidAll n ((_, _, True):xs) = isValidAll (n+1) xs

-- run test a single test
test :: String -> BF -> (String, String, Bool)
test prog want =
  -- Parse the prog with optimization
  let Right bf = parseBF prog in
  let op = optimizeBF bf in
  -- Parse the want without optimization
  (concatMap show op, concatMap show want, op == want)

allTests =
  [ -- initial loop
    test "[+++++>+++><----<->-]" []
  , test "[+++++>+++><----<->-]+>>" [Inc 1, Mov 2]
  , test "[+++++>+++><----<->-]>>" [Mov 2]

  -- dead loops
  , test "+[+>][.]" [Inc 1, Loop [Inc 1, Mov 1]]
  , test "+[+>][.][-]" [Inc 1, Loop [Inc 1, Mov 1]]
  , test "+[+>][.][-][+]" [Inc 1, Loop [Inc 1, Mov 1]]
  , test "+[+>][.][-][+]." [Inc 1, Loop [Inc 1, Mov 1], Out KChr]
  , test "+[+>][:][-][+]:" [Inc 1, Loop [Inc 1, Mov 1], Out KNum]
  , test "[+>][.]" []
  , test "[+>][.][-]" []
  , test "[+>][.][-][+]" []
  , test "[+>][.][-][+]." [Out KChr]
  , test "[+>][.][-][+]:" [Out KNum]
  , test "+[+>[.][-]][.][-]." [Inc 1, Loop [Inc 1, Mov 1, Loop [Out KChr]], Out KChr]
  , test "+[+>[:][-]][:][-]:" [Inc 1, Loop [Inc 1, Mov 1, Loop [Out KNum]], Out KNum]
  , test "[+>[.][-]][.][-]." [Out KChr]
  , test "[+>[:][-]][:][-]:" [Out KNum]

  -- clear loops
  , test ">>>[-]" [Mov 3, Set 0]
  , test ">>>[+]" [Mov 3, Set 0]
  , test ">>>0" [Mov 3, Set 0]
  , test "[-]>>>" [Mov 3]
  , test "[+]>>>" [Mov 3]
  , test "0>>>" [Mov 3]
  , test ">[-][.]>>>" [Mov 1, Set 0, Mov 3]
  , test ">[+][.]>>>" [Mov 1, Set 0, Mov 3]
  , test ">0[.]>>>" [Mov 1, Set 0, Mov 3]
  , test ">>>[--]" [Mov 3, Loop [Inc (-2)]]
  , test ">>>[++]" [Mov 3, Loop [Inc 2]]
  , test ">>>[---]" [Mov 3, Loop [Inc (-3)]]
  , test ">>>[+++]" [Mov 3, Loop [Inc 3]]
  , test ">>>[<[-]]" [Mov 3, Loop [Mov (-1), Set 0]]
  , test ">>>[<[+]]" [Mov 3, Loop [Mov (-1), Set 0]]
  , test ">>>[<0]" [Mov 3, Loop [Mov (-1), Set 0]]
  , test ">>>[<[---++]]" [Mov 3, Loop [Mov (-1), Set 0]]
  , test ">>>[<[+++--]]" [Mov 3, Loop [Mov (-1), Set 0]]
  , test ">>>[---++]" [Mov 3, Set 0]
  , test ">>>[+--++]" [Mov 3, Set 0]

  -- join ops
  , test "+++++++++++-" [Inc 10]
  , test "-----------+" [Inc (-10)]
  , test "+++-+-+++++-" [Inc 6]
  , test "---+-+-----+" [Inc (-6)]
  , test "+++->-+++++-" [Inc 2, Mov 1, Inc 3]
  , test "---+-+->---+" [Inc (-3), Mov 1, Inc (-2)]
  , test ">>>>>>>>>>><" [Mov 10]
  , test "<<<<<<<<<<<>" [Mov (-10)]
  , test ">>><><>>>>><" [Mov 6]
  , test "<<<><><<<<<>" [Mov (-6)]
  , test ">>><+<>>>>><" [Mov 2, Inc 1, Mov 3]
  , test "<<<><><+<<<>" [Mov (-3), Inc 1, Mov (-2)]
  , test "»»»»»»»»»»»«" [Sft 10]
  , test "«««««««««««»" [Sft (-10)]
  , test "»»»«»«»»»»»«" [Sft 6]
  , test "«««»«»«««««»" [Sft (-6)]
  , test "»»»«+«»»»»»«" [Sft 2, Inc 1, Sft 3]
  , test "«««»«»«+«««»" [Sft (-3), Inc 1, Sft (-2)]

  -- dead Set
  , test "++++[-]" [Set 0]
  , test "»»»»[-]" [Set 0]
  , test "++++0" [Set 0]
  , test "+[-]++++[-]" [Set 0]
  , test ",[-]" [Set 0]
  , test ",0" [Set 0]
  , test "++++,[-]" [Set 0]
  , test "++++," [Inp KChr]
  , test "««««," [Inp KChr]
  , test "+[-]," [Inp KChr]
  , test "++++[-]," [Inp KChr]
  , test ",,,,,,,," [Inp KChr]
  , test "++++;" [Inp KNum]
  , test "««««;" [Inp KNum]
  , test "+[-];" [Inp KNum]
  , test "++++[-];" [Inp KNum]
  , test ";;;;;;;;" [Inp KNum]
  , test ">[++++[-]]]" [Mov 1, Loop [Set 0]]
  , test ">[««««[-]]]" [Mov 1, Loop [Set 0]]
  , test ">[+[-]++++[-]]" [Mov 1, Loop [Set 0]]
  , test ">[,[-]]" [Mov 1, Loop [Set 0]]
  , test ">[++++,[-]]" [Mov 1, Loop [Set 0]]
  , test ">[++++,0]" [Mov 1, Loop [Set 0]]
  , test ">[++++,]" [Mov 1, Loop [Inp KChr]]
  , test ">[««««,]" [Mov 1, Loop [Inp KChr]]
  , test ">[+[-],]" [Mov 1, Loop [Inp KChr]]
  , test ">[++++[-],]" [Mov 1, Loop [Inp KChr]]
  , test ">[++++0,]" [Mov 1, Loop [Inp KChr]]
  , test ">[,,,,,,]" [Mov 1, Loop [Inp KChr]]
  , test ">[++++;]" [Mov 1, Loop [Inp KNum]]
  , test ">[««««;]" [Mov 1, Loop [Inp KNum]]
  , test ">[+[-];]" [Mov 1, Loop [Inp KNum]]
  , test ">[++++[-];]" [Mov 1, Loop [Inp KNum]]
  , test ">[++++0;]" [Mov 1, Loop [Inp KNum]]
  , test ">[;;;;;;]" [Mov 1, Loop [Inp KNum]]

  -- just Set
  , test ">[-]+++++" [Mov 1, Set 5]
  , test ">0+++++" [Mov 1, Set 5]
  , test ">[-]-----" [Mov 1, Set (-5)]
  , test ">0-----" [Mov 1, Set (-5)]
  , test ">[-]++-++" [Mov 1, Set 3]
  , test ">[-]--+--" [Mov 1, Set (-3)]
  , test ">[-]++[-]++" [Mov 1, Set 2]
  , test ">[-]--[+]--" [Mov 1, Set (-2)]  
  , test ">[-]+++---" [Mov 1, Set 0]
  , test ">[-]---+++" [Mov 1, Set 0]
  , test ">[-]+---++" [Mov 1, Set 0]
  , test ">[-]-+++--" [Mov 1, Set 0]
  , test ">[-]+++---[>>>]" [Mov 1, Set 0]
  , test ">[-]---+++[>>>]" [Mov 1, Set 0]
  , test ">[-]+---++[>>>]" [Mov 1, Set 0]
  , test ">[-]-+++--[>>>]" [Mov 1, Set 0]
  , test ">[-]+++---[>>>]+++" [Mov 1, Set 3]
  , test ">[-]---+++[>>>]+++" [Mov 1, Set 3]
  , test ">[-]+---++[>>>]+++" [Mov 1, Set 3]
  , test ">[-]-+++--[>>>]+++" [Mov 1, Set 3]
  , test ">[[-]+++++]" [Mov 1, Loop[Set 5]]
  , test ">[[-]-----]" [Mov 1, Loop[Set (-5)]]
  , test ">[[-]++-++]" [Mov 1, Loop[Set 3]]
  , test ">[[-]--+--]" [Mov 1, Loop[Set (-3)]]
  , test ">[[-]++[-]++]" [Mov 1, Loop[Set 2]]
  , test ">[[-]--[+]--]" [Mov 1, Loop[Set (-2)]  ]
  , test ">[[-]+++---]" [Mov 1, Loop[Set 0]]
  , test ">[[-]---+++]" [Mov 1, Loop[Set 0]]
  , test ">[[-]+---++]" [Mov 1, Loop[Set 0]]
  , test ">[[-]-+++--]" [Mov 1, Loop[Set 0]]
  , test ">[[-]+++---[>>>]]" [Mov 1, Loop[Set 0]]
  , test ">[[-]---+++[>>>]]" [Mov 1, Loop[Set 0]]
  , test ">[[-]+---++[>>>]]" [Mov 1, Loop[Set 0]]
  , test ">[[-]-+++--[>>>]]" [Mov 1, Loop[Set 0]]
  , test ">[[-]+++---[>>>]+++]" [Mov 1, Loop[Set 3]]
  , test ">[[-]---+++[>>>]+++]" [Mov 1, Loop[Set 3]]
  , test ">[[-]+---++[>>>]+++]" [Mov 1, Loop[Set 3]]
  , test ">[[-]-+++--[>>>]+++]" [Mov 1, Loop[Set 3]]

  -- dead Op
  , test "++++++------" []
  , test ">>>>>><<<<<<" []
  , test "»»»»»»««««««" []
  , test ">><<++--««»»" []
  , test "----++++-+-+" []
  , test "<<<<>>>><><>" []
  , test "««««»»»»«»«»" []
  , test ">[++++++------]" [Mov 1, Loop []]
  , test ">[»»»»»»««««««]" [Mov 1, Loop []]
  , test ">[>>>>>><<<<<<]" [Mov 1, Loop []]
  , test ">[>><<++--««»»]" [Mov 1, Loop []]
  , test ">[----++++-+-+]" [Mov 1, Loop []]
  , test ">[<<<<>>>><><>]" [Mov 1, Loop []]
  , test ">[««««»»»»«»«»]" [Mov 1, Loop []]

  -- mul
  , test "+[->+<]" [Inc 1, Mul 1 1]
  , test "+[->>>+<<<]" [Inc 1, Mul 3 1]
  , test "+[->++++<]" [Inc 1, Mul 1 4]
  , test "+[->++++---<]" [Inc 1, Mul 1 1]
  , test "+[->>+<]" [Inc 1, Loop [Inc (-1), Mov 2, Inc 1, Mov (-1)]]
  , test "+[->++++<<]" [Inc 1, Loop [Inc (-1), Mov 1, Inc 4, Mov (-2)]]
  , test "+[->-<]" [Inc 1, Loop [Inc (-1), Mov 1, Inc (-1), Mov (-1)]]
  , test "+[->>>-<<<]" [Inc 1, Loop [Inc (-1), Mov 3, Inc (-1), Mov (-3)]]
  , test "+[->----<]" [Inc 1, Loop [Inc (-1), Mov 1, Inc (-4), Mov (-1)]]
  , test "+[->----+++<]" [Inc 1, Loop [Inc (-1), Mov 1, Inc (-1), Mov (-1)]]
  , test "+[+[->++<]]" [Inc 1, Loop [Inc 1, Mul 1 2]]
  , test "+[>+<-]" [Inc 1, Mul 1 1]
  , test "+[>>>+<<<-]" [Inc 1, Mul 3 1]
  , test "+[>++++<-]" [Inc 1, Mul 1 4]
  , test "+[>++++---<-]" [Inc 1, Mul 1 1]
  , test "+[>>+<-]" [Inc 1, Loop [Mov 2, Inc 1, Mov (-1), Inc (-1)]]
  , test "+[>++++<<-]" [Inc 1, Loop [Mov 1, Inc 4, Mov (-2), Inc (-1)]]
  , test "+[>-<-]" [Inc 1, Loop [Mov 1, Inc (-1), Mov (-1), Inc (-1)]]
  , test "+[>>>-<<<-]" [Inc 1, Loop [Mov 3, Inc (-1), Mov (-3), Inc (-1)]]
  , test "+[>----<-]" [Inc 1, Loop [Mov 1, Inc (-4), Mov (-1), Inc (-1)]]
  , test "+[>----+++<-]" [Inc 1, Loop [Mov 1, Inc (-1), Mov (-1), Inc (-1)]]
  , test "+[+[>++<-]]" [Inc 1, Loop [Inc 1, Mul 1 2]]

  -- dup
  , test "+[->+>+<<]" [Inc 1, Dup]
  , test "+[>+>+<<-]" [Inc 1, Dup]
  , test "+[+[>+>+<<-]]" [Inc 1, Loop [Inc 1, Dup]]
  , test "+[+[->+>+<<]]" [Inc 1, Loop [Inc 1, Dup]]
  ]