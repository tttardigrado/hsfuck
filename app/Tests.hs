module Tests ( testAll ) where

import Lang ( BF, Op(Loop, Out, Inc, Inp, Mul, Set, Mov) )
import Optimizer ( optimizeBF )
import Parser ( parseBF )

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

-- run all tests
testAll :: [(Int, String, String)]
testAll = isValidAll 0 allTests

allTests =
  [ -- initial loop
    test "[+++++>+++><----<->-]" []
  , test "[+++++>+++><----<->-]+>>" [Inc 1, Mov 2]
  , test "[+++++>+++><----<->-]>>" [Mov 2]

  -- dead loops
  , test "+[+>][.]" [Inc 1, Loop [Inc 1, Mov 1]]
  , test "+[+>][.][-]" [Inc 1, Loop [Inc 1, Mov 1]]
  , test "+[+>][.][-][+]" [Inc 1, Loop [Inc 1, Mov 1]]
  , test "+[+>][.][-][+]." [Inc 1, Loop [Inc 1, Mov 1], Out]
  , test "[+>][.]" []
  , test "[+>][.][-]" []
  , test "[+>][.][-][+]" []
  , test "[+>][.][-][+]." [Out]
  , test "+[+>[.][-]][.][-]." [Inc 1, Loop [Inc 1, Mov 1, Loop [Out]], Out]
  , test "[+>[.][-]][.][-]." [Out]

  -- clear loops
  , test ">>>[-]" [Mov 3, Set 0]
  , test ">>>[+]" [Mov 3, Set 0]
  , test "[-]>>>" [Mov 3]
  , test "[+]>>>" [Mov 3]
  , test ">[-][.]>>>" [Mov 1, Set 0, Mov 3]
  , test ">[+][.]>>>" [Mov 1, Set 0, Mov 3]
  , test ">>>[--]" [Mov 3, Loop [Inc (-2)]]
  , test ">>>[++]" [Mov 3, Loop [Inc 2]]
  , test ">>>[---]" [Mov 3, Loop [Inc (-3)]]
  , test ">>>[+++]" [Mov 3, Loop [Inc 3]]
  , test ">>>[<[-]]" [Mov 3, Loop [Mov (-1), Set 0]]
  , test ">>>[<[+]]" [Mov 3, Loop [Mov (-1), Set 0]]
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


  -- dead Set
  , test "++++[-]" [Set 0]
  , test "+[-]++++[-]" [Set 0]
  , test ",[-]" [Set 0]
  , test "++++,[-]" [Set 0]
  , test "++++," [Inp]
  , test "+[-]," [Inp]
  , test "++++[-]," [Inp]
  , test ",,,,,,,," [Inp]
  , test ">[++++[-]]]" [Mov 1, Loop [Set 0]]
  , test ">[+[-]++++[-]]" [Mov 1, Loop [Set 0]]
  , test ">[,[-]]" [Mov 1, Loop [Set 0]]
  , test ">[++++,[-]]" [Mov 1, Loop [Set 0]]
  , test ">[++++,]" [Mov 1, Loop [Inp]]
  , test ">[+[-],]" [Mov 1, Loop [Inp]]
  , test ">[++++[-],]" [Mov 1, Loop [Inp]]
  , test ">[,,,,,,,,]" [Mov 1, Loop [Inp]]

  -- just Set
  , test ">[-]+++++" [Mov 1, Set 5]
  , test ">[-]-----" [Mov 1, Set (-5)]
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
  , test ">>>><<<<++--" []
  , test "----++++-+-+" []
  , test "<<<<>>>><><>" []
  , test ">[++++++------]" [Mov 1, Loop []]
  , test ">[>>>>>><<<<<<]" [Mov 1, Loop []]
  , test ">[>>>><<<<++--]" [Mov 1, Loop []]
  , test ">[----++++-+-+]" [Mov 1, Loop []]
  , test ">[<<<<>>>><><>]" [Mov 1, Loop []]


  -- mul
  , test "+[->+<]" [Inc 1, Mul 1]
  , test "+[->++++<]" [Inc 1, Mul 4]
  , test "+[->++++---<]" [Inc 1, Mul 1]
  , test "+[->+-+-++<]" [Inc 1, Mul 2]
  , test "+[->-<]" [Inc 1, Loop [Inc (-1), Mov 1, Inc (-1), Mov (-1)]]
  , test "+[->----<]" [Inc 1, Loop [Inc (-1), Mov 1, Inc (-4), Mov (-1)]]
  , test "+[->----+++<]" [Inc 1, Loop [Inc (-1), Mov 1, Inc (-1), Mov (-1)]]
  , test "+[->-+-+--<]" [Inc 1, Loop [Inc (-1), Mov 1, Inc (-2), Mov (-1)]]
  , test "+[+[->++<]]" [Inc 1, Loop [Inc 1, Mul 2]]
  ]