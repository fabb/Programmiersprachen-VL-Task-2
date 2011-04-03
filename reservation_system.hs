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


{---------- Globals ----------}
xmlFilename = "reservations.xml" :: String


{---------- Types ----------}
type IssuedReservations = Integer
type XMLData = (IssuedReservations, [RItem])
type ApplicationData = (IssuedReservations, ReservationZipper)

{---------- Zipper Type ----------}
--efficiently navigable data structure for representing train reservations
--it's in fact just a list zipper
type ReservationZipper = ([RItem],[RCrumb])

type ReservationNumber = Integer
type FromStation = Integer
type ToStation = Integer
type Train = Integer
type TrainCar = Integer
type SeatCount = Integer
type SeatNumber = Integer

type GroupReservationData = (FromStation, ToStation, Train, TrainCar, SeatCount)
type IndividualReservationData = (FromStation, ToStation, Train, TrainCar, SeatNumber)

data RItem = GroupReservation ReservationNumber GroupReservationData
             | IndividualReservation ReservationNumber GroupReservationData deriving (Show)
type RCrumb = RItem 



{---------- Main ----------} 
main :: IO ()
main = do
		--[src, dst] <- getArgs
		xmlFilename <- return xmlFilename
		
		printWelcome
		
		d@(icount, zipper) <- loadData xmlFilename
		
		putStrLn $ show d --TODO test

		printMenu
		
		d@(icount, zipper) <- mainloop d xmlFilename
		
		writeData (icount, zipper) xmlFilename
		
		putStrLn $ show d --TODO test
		
		printGoodbye
		
		return ()


--reads in user input and processes wanted changes on reservations
--returns the changed data structure when finished TODO does it?
mainloop :: ApplicationData -> String -> IO ApplicationData
mainloop d@(icount, zipper) xmlFilename = do
		
		choice <- getLine
		--putStr choice

		--TODO process requests
		--TODO is it stupid to change zipper like this - especially for non-changing functions?
		d@(icount, zipper) <- case choice of
			"a" -> do -- New individual reservation, needs as input FROM, TO, TRAIN, CAR, COUNT
					printDummy choice
					--return changedZipper
					return d
					
			"s" -> do -- New group reservation, needs as input FROM, TO, TRAIN, CAR, SEAT
					printDummy choice
					--return changedZipper
					return d
					
			"y" -> do -- Delete reservation, needs as input RESERVATIONNUMBER
					printDummy choice
					changedZipper <- return $ removeFirst zipper --TODO test
					return (icount, changedZipper)
		
			"r" -> do -- Show train stations, trains, train cars, and seats, needs no input
					printDummy choice
					return d
					
			"d" -> do -- Show group reservations, needs as input TRAIN, CAR
					printDummy choice
					return d
					
			"f" -> do -- Show free seat count, needs as input TRAIN, CAR, FROM, TO
					printDummy choice
					return d
					
			"g" -> do -- Show individual reservations, needs as input TRAIN, CAR, SEAT
					printDummy choice
					return d
					
			"q" -> do
					--TODO could I quit from here when the result is assigned to zipper with <- ?
					printDummy choice
					return d
			
			_   -> do
					putStrLn $ "Wrong input of '" ++ choice ++ "', please choose again."
					return d

		if choice == "q"
			then return d
			else do
				mainloop d xmlFilename




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
printMenu = putStrLn "\nYour Options:\n\
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
writeData :: ApplicationData -> String -> IO ()
writeData (icount, zipper) xmlFilename = do
	putStrLn $ "Trying to write data to " ++ xmlFilename
	res <- return $ unpackRZipper zipper
	runX
			( constA (icount, res)
			>>>
			xpickleDocument xpReservationData
				[ withIndent yes
				, withOutputEncoding utf8
				] xmlFilename
            )
	return ()


--loads data from the given xml and prints status
--TODO return zipper
loadData :: String -> IO ApplicationData
loadData xmlFilename = do
	putStrLn $ "Trying to load reservation data from " ++ xmlFilename
	--readDocument [withValidate no] xmlFilename --beware if it not yet exists
	--if xml does not yet exist, say so and initialize zipper with default values (issued reservations number = 0)
	r@[(c, z)] <- runX
			( xunpickleDocument xpReservationData
				[ withRemoveWS yes   -- remove redundant whitespace
				, withValidate no    -- don't validate source
				] xmlFilename
            )
	if null r
		then return (0, makeRZipper []) --FIXME pattern match failure at [(c, z)] and xunpickleDocument will return a fatal error
		else return (c, makeRZipper z)

{- test
testReadXml xmlFilename = runX
			( xunpickleDocument xpReservationData
				[ withRemoveWS yes   -- remove redundant whitespace
				, withTrace 1
				, withValidate no    -- don't validate source
				] xmlFilename
            )

testReadWriteXml xmlFilename = runX
			( xunpickleDocument xpReservationData
				[ withRemoveWS yes   -- remove redundant whitespace
				, withTrace 1
				, withValidate no    -- don't validate source
				] xmlFilename
			>>>
			xpickleDocument xpReservationData
				[ withIndent yes
				] "test.xml"
            )
-}

--start point for xml<->data transformations
xpReservationData :: PU XMLData
xpReservationData = xpElem "reservations" $ --xml root element
		xpAddFixedAttr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance" $
		xpAddFixedAttr "xsi:noNamespaceSchemaLocation" "reservations.xsd" $
		xpPair
			xpIssuedReservations
			xpReservations
 
--xpMissingRootElement :: PU XMLData
--xpMissingRootElement = xpickle

xpIssuedReservations :: PU IssuedReservations
xpIssuedReservations = xpElem "issued_reservations" $
		xpPrim

{- another way of doing pickles is to just write "xpickle" and let a fitting one be chosen
instance XmlPickler IssuedReservations where
	xpickle = xpElem "issued_reservations" $
		xpPrim
-}

xpReservations :: PU [RItem]
xpReservations = 
	--xpList xpGroupReservation
	--xpList xpIndividualReservation
	xpList $
	xpAlt tag ps --doesn't this work easier? it's just a choice between xpIndividualReservation and xpGroupReservation
	where
	tag ( IndividualReservation _ _ ) = 0
	tag ( GroupReservation _ _ ) = 1
	ps = [ xpIndividualReservation
		 , xpGroupReservation
		 ]

xpIndividualReservation :: PU RItem
xpIndividualReservation = xpElem "individual_reservation" $
		xpWrap ( \((resnum,from,to,train,car,seat)) -> IndividualReservation resnum (from, to, train, car, seat)
		       , \ir@(IndividualReservation resnum (from, to, train, car, seat)) -> (resnum,from,to,train,car,seat)
		       ) $
		xp6Tuple (xpElem "reservation_number" xpPrim)
				(xpElem "from" xpPrim)
				(xpElem "to" xpPrim)
				(xpElem "train" xpPrim)
				(xpElem "car" xpPrim)
				(xpElem "seat" xpPrim)

xpGroupReservation :: PU RItem
xpGroupReservation = xpElem "group_reservation" $
		xpWrap ( \((resnum,from,to,train,car,count)) -> GroupReservation resnum (from, to, train, car, count)
		       , \ir@(GroupReservation resnum (from, to, train, car, count)) -> (resnum,from,to,train,car,count)
		       ) $
		xp6Tuple (xpElem "reservation_number" xpPrim)
				(xpElem "from" xpPrim)
				(xpElem "to" xpPrim)
				(xpElem "train" xpPrim)
				(xpElem "car" xpPrim)
				(xpElem "count" xpPrim)


{---------- Zipper Stuff ----------}

--creates a zipper from a list of reservations
makeRZipper :: [RItem] -> ReservationZipper
makeRZipper xs = (xs, [])

--unpacks the zipper back to a list of reservations
unpackRZipper :: ReservationZipper -> [RItem]
unpackRZipper z = maybe [] fst $ goFirst z

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
