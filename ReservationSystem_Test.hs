module ReservationSystem_Test where

--http://leiffrenzel.de/papers/getting-started-with-hunit.html

import Test.HUnit
import ReservationSystem


testAppData1 = (5,([GroupReservation 10 (2,3,4,5,6),GroupReservation 2 (3,4,5,6,7),IndividualReservation 3 (4,5,6,7,8),IndividualReservation 4 (5,6,7,8,9),GroupReservation 20 (3,4,5,6,7)],[])) :: ApplicationData

{-
maybeTest = maybeDo testAppData1 (\ x -> dummy x >>= dummy) "errrror"

maybeTest2 = do
	x <- maybeTest
	--putStrLn $ show x
	return ()

dummy :: ApplicationData -> Maybe ApplicationData
dummy appdata =
	Nothing
	--return (fst appdata + 1, snd appdata)

-}


cases1 = TestLabel "Train 1" $ TestList [

  TestCase $ assertEqual "bla" True True

  ]


cases2 = TestLabel "maybeDo" $ TestList [

  TestCase $ assertEqual "bla" True True

  ]
  

testmain = runTestTT $ TestList [cases1, cases2]
