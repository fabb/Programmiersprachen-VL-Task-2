--Fabian Ehrentraud, Bernhard Urban, 2011

{- TODO

	how can I both use putStrLn in a function and return some other stuff?
	zipper is in the IO monad in the mainloop, but for calculations i want to use the Maybe monad - how can i combine those?

-}

module Main
where

{---------- Imports ----------}
import Data.List (break) 
import Text.XML.HXT.Core
--import Text.XML.HXT.Curl -- use libcurl for HTTP access, only necessary when reading http://...
 
import System.Environment


{---------- Zipper Type ----------}
--efficiently navigable data structure for representing train reservations
--it's in fact just a list zipper
type ReservationZipper = ([RItem],[RCrumb]) --TODO use RCrumb?

type ReservationNumber = Integer
type GroupReservationData = [Integer] --TODO split into the single items
type IndividualReservationData = [Integer] --TODO split into the single items
data RItem = GroupReservation ReservationNumber GroupReservationData
             | IndividualReservation ReservationNumber GroupReservationData deriving (Show)
type RCrumb = RItem 


testZipper = ([GroupReservation 1 [1,2], GroupReservation 2 [2,3], IndividualReservation 3 [3,4], IndividualReservation 4 [4,5]],[])


{---------- Main ----------} 
main :: IO ()
main = do
		--[src, dst] <- getArgs
		xmlFilename <- return "reservations.xml"
		
		printWelcome
		
		zipper <- loadData xmlFilename
		
		zipper <- return testZipper --TODO test
		
		printMenu
		
		zipper <- mainloop zipper xmlFilename
		
		writeData zipper xmlFilename
		
		putStrLn $ show zipper --TODO test
		
		printGoodbye
		
		{-
		runX ( readDocument [withValidate no
                            ] xmlFilename --beware if it not yet exists
		       >>>
		       writeDocument [withIndent yes
		                     ,withOutputEncoding utf8
		                     ] xmlFilename
		     )
		-}
		
		return ()


--reads in user input and processes wanted changes on reservations
--returns the changed data structure when finished TODO does it?
mainloop :: ReservationZipper -> String -> IO ReservationZipper
mainloop zipper xmlFilename = do
		
		choice <- getLine
		--putStr choice

		--TODO process requests
		--TODO is it stupid to change zipper like this - especially for non-changing functions?
		zipper <- case choice of
			"a" -> do -- New individual reservation, needs as input FROM, TO, TRAIN, CAR, COUNT
					printDummy choice
					--return changedZipper
					return zipper
					
			"s" -> do -- New group reservation, needs as input FROM, TO, TRAIN, CAR, SEAT
					printDummy choice
					--return changedZipper
					return zipper
					
			"y" -> do -- Delete reservation, needs as input RESERVATIONNUMBER
					printDummy choice
					changedZipper <- return $ removeFirst zipper --TODO test
					return changedZipper
		
			"r" -> do -- Show train stations, trains, train cars, and seats, needs no input
					printDummy choice
					return zipper
					
			"d" -> do -- Show group reservations, needs as input TRAIN, CAR
					printDummy choice
					return zipper
					
			"f" -> do -- Show free seat count, needs as input TRAIN, CAR, FROM, TO
					printDummy choice
					return zipper		
					
			"g" -> do -- Show individual reservations, needs as input TRAIN, CAR, SEAT
					printDummy choice
					return zipper
					
			"q" -> do
					--TODO could I quit from here when the result is assigned to zipper with <- ?
					printDummy choice
					return zipper
			
			_   -> do
					putStrLn $ "Wrong input of '" ++ choice ++ "', please choose again."
					return zipper

		if choice == "q"
			then return zipper
			else do
				mainloop zipper xmlFilename




{---------- Print Output ----------}
printDummy input = putStrLn $ "You pressed key " ++ input

--prints a welcome message when starting the application
printWelcome :: IO ()
printWelcome = putStrLn "Welcome to the >Management Application for Train Reservation And Information Navigation< (Ma_Train)\n"

--prints a goodbye message when ending the application
printGoodbye :: IO ()
printGoodbye = putStrLn "\nWe wish you a nice day."


--prints the available main menu operations	
printMenu :: IO ()
printMenu = putStrLn "Your Options:\n\
                     \ (a) New individual reservation\n\
                     \ (s) New group reservation\n\
                     \ (y) Delete reservation\n\
                     \ (r) Show train stations, trains, train cars, and seats\n\
                     \ (d) Show group reservations\n\
                     \ (f) Show free seat count\n\
                     \ (g) Show individual reservations\n\
                     \ (q) Quit Ma_Train and save reservations to disk"
{-
	new individual reservation needs as input FROM, TO, TRAIN, CAR, COUNT
	new group reservation needs as input FROM, TO, TRAIN, CAR, SEAT
	Delete reservation needs as input RESERVATIONNUMBER
	Show train stations, trains, train cars, and seats needs no input
	Show group reservations needs as input TRAIN, CAR
	Show free seat count needs as input TRAIN, CAR, FROM, TO
	Show individual reservations needs as input TRAIN, CAR, SEAT
-}


{---------- XML Handling ----------}
--write the zipper data to xml
writeData :: ReservationZipper -> String -> IO ()
writeData zipper xmlFilename = do
	--TODO write to file
	--writeDocument [withIndent yes, withOutputEncoding utf8] xmlFilename
	--putStrLn "Trying to write data to " ++ xmlFilename
	return ()


--loads data from the given xml and prints status
--TODO return zipper
loadData :: String -> IO ReservationZipper
loadData xmlFilename = do
	--putStrLn $ "Trying to load reservation data from " ++ xmlFilename
	--TODO load xml into zipper data structure
	--readDocument [withValidate no] xmlFilename --beware if it not yet exists
	--if xml does not yet exist, say so and initialize zipper with default values (issued reservations number = 0)
	return ([],[]) --ReservationZipper
	

{---------- Zipper Stuff ----------}

--forwards for one item
goForward :: ReservationZipper -> Maybe ReservationZipper
goForward (x:xs, bs) = Just (xs, x:bs)
goForward ([], _) = Nothing

--rewinds to the previous item
goBack :: ReservationZipper -> Maybe ReservationZipper
goBack (xs, b:bs) = Just (b:xs, bs)
goBack (_, []) = Nothing

--rewinds to the first item
goFirst :: ReservationZipper -> Maybe ReservationZipper
goFirst z@(xs, b:bs) = goBack z >>= goFirst
goFirst z@(_, []) = Just z

--forwards until after the last item
goLast :: ReservationZipper -> Maybe ReservationZipper
goLast z@(x:xs, bs) = goForward z >>= goLast
goLast z@([], _) = Just z

--sets the current item to the one with the given number
reservationTo :: ReservationNumber -> ReservationZipper -> Maybe ReservationZipper
reservationTo resnum z@(xs, bs) = do
	(items, _) <- goFirst z
	(ls, item:rs) <- return $ break (reservationIs resnum) items
	return (item:rs, reverse ls) --TODO better without break but with recursive goForward?

--True when the given item has the given reservation number
reservationIs :: ReservationNumber -> RItem -> Bool
reservationIs resnum (GroupReservation reservationNumber _)      = resnum == reservationNumber
reservationIs resnum (IndividualReservation reservationNumber _) = resnum == reservationNumber

--inserts a new reservation before the current item
reservationNew :: RItem -> ReservationZipper -> Maybe ReservationZipper
reservationNew item (xs, bs) = Just (item:xs, bs)

--inserts a new reservation as last item
reservationNewLast :: RItem -> ReservationZipper -> Maybe ReservationZipper
reservationNewLast item z = goLast z >>= reservationNew item

--delete the current item
reservationDeleteCurrent :: ReservationZipper -> Maybe ReservationZipper
reservationDeleteCurrent (x:xs, bs) = return (xs, bs)
reservationDeleteCurrent ([], _) = Nothing

--deletes the reservation with the given number
reservationDelete :: ReservationNumber -> ReservationZipper -> Maybe ReservationZipper
reservationDelete resnum z = reservationTo resnum z >>= reservationDeleteCurrent

--TODO test for removing the current item (=first if zipper not forwarded)
removeFirst :: ReservationZipper -> ReservationZipper
removeFirst zipper@(i,c) = (tail i, c)
{-
removeFirst :: ReservationZipper -> Maybe ReservationZipper
removeFirst zipper@(i:is,c) = Just (is, c)
removeFirst ([],c) = Nothing
-}
