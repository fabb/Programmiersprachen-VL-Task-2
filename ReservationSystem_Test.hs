module ReservationSystem_Test where

--http://leiffrenzel.de/papers/getting-started-with-hunit.html

import Test.HUnit
import ReservationSystem


cases1 = TestLabel "Train 1" $ TestList [

  TestCase $ assertEqual "bla" True True

  ]


cases2 = TestLabel "Train 2" $ TestList [

  TestCase $ assertEqual "bla" True True

  ]


testmain = runTestTT $ TestList [cases1, cases2]
