--Fabian Ehrentraud, Bernhard Urban, 2011

module ReservationSystem where

{---------- Imports ----------}

import Data.List (break)
import Data.Maybe
import Data.Char
--import IO
import Text.XML.HXT.Core
import Text.XML.HXT.RelaxNG
--import Text.XML.HXT.Curl -- use libcurl for HTTP access, only necessary when reading http://...
 
import System.Environment


{---------- Globals ----------}

xmlFilename = "reservations.xml" :: String
--Relax NG Schema
rngFilename = "reservations.rng" :: String

--a fixed number of stations is assumed for simplification reasons
stationCount = 10 :: Integer
stations = [1..stationCount] :: Stations

trains =
	[
		(1, [(1, [1..10]),(2, [1..10]),(3, [1..10])]),
		(2, [(1, [1..10]),(2, [1..10]),(3, [1..10])]),
		(3, [(1, [1..10]),(2, [1..10]),(3, [1..10])]),
		(4, [(1, [1..10]),(2, [1..10]),(3, [1..10])])
	] :: Trains


{---------- Types ----------}

type IssuedReservations = Integer
type XMLData = (IssuedReservations, [RItem])
type ApplicationData = (Stations, Trains, IssuedReservations, ReservationZipper)

{---------- Station Type ----------}

type Stations = [Station] --stations are connected in a row from element 0 on
type Station = StationId
type StationId = Integer

{---------- Train Types ----------}

type Trains = [Train]
type Train = (TrainId, Cars)
type TrainId = Integer
type Cars = [Car]
type Car = (CarId, Seats)
--CarId's are only unique within their Train
type CarId = Integer
type Seats = [Seat]
type Seat = SeatId
--SeatId's are only unique within their Car
type SeatId = Integer

{---------- Zipper Type ----------}

--efficiently navigable data structure for representing train reservations
--it's in fact just a list zipper
type ReservationZipper = ([RItem],[RCrumb])

type ReservationNumber = Integer
type FromStation = StationId
type ToStation = StationId
type SeatCount = Integer

type GroupReservationData = (FromStation, ToStation, TrainId, CarId, SeatCount)
type IndividualReservationData = (FromStation, ToStation, TrainId, CarId, SeatId)

data RItem = GroupReservation ReservationNumber GroupReservationData
             | IndividualReservation ReservationNumber GroupReservationData deriving (Show)
type RCrumb = RItem 



{---------- Main ----------} 

main :: IO ()
main = do
		--[src, dst] <- getArgs
		xmlFilename <- return xmlFilename
		
		printWelcome
		
		d@(s, t, icount, zipper) <- loadData xmlFilename
		
		putStrLn $ show d --TODO test

		printMenu
		
		d@(s, t, icount, zipper) <- mainloop d
		
		writeData (s, t, icount, zipper) xmlFilename
		
		putStrLn $ show d --TODO test
		
		printGoodbye
		
		return ()


--reads in user input and processes wanted changes on reservations
--returns the changed data structure when finished
mainloop :: ApplicationData -> IO ApplicationData
mainloop d@(s, t, icount, zipper) = do
		
		choice <- getLine
		--putStr choice

		--TODO process requests
		--TODO is it stupid to change zipper like this - especially for non-changing functions?
		d@(s, t, icount, zipper) <- case choice of
			"a" -> mNewIndividualReservation d
					
			"s" -> mNewGroupReservation d
					
			"y" -> mDeleteReservation d
		
			"r" -> do
					mShowTrains d
					return d
					
			"d" -> do
					mShowGroupReservations d
					return d
					
			"f" -> do
					mShowFreeSeats d
					return d
					
			"g" -> do
					mShowIndividualReservations d
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
				mainloop d


{---------- Menu Option Navigation ----------}

--dialog for issuing a new individual reservation
--needs as input FROM, TO, TRAIN, CAR, COUNT
mNewIndividualReservation :: ApplicationData -> IO ApplicationData
mNewIndividualReservation appdata = do
	putStrLn $ "New individual reservation TODO"
	return appdata

--dialog for issuing a new group reservation
--needs as input FROM, TO, TRAIN, CAR, SEAT
mNewGroupReservation :: ApplicationData -> IO ApplicationData
mNewGroupReservation appdata = do
	putStrLn $ "New group reservation TODO"
	return appdata

--dialog for removing an existing reservation
--needs as input RESERVATIONNUMBER
mDeleteReservation :: ApplicationData -> IO ApplicationData
mDeleteReservation appdata@(s, t, icount, zipper) = do
	putStrLn $ "Remove existing reservation TODO"
	--changedZipper <- return $ fromMaybe (makeRZipper []) $ reservationDeleteCurrent zipper --TODO test
	changedZipper <- maybeDo zipper (\ z -> reservationDeleteCurrent z) "Error: Could not delete first item" --TODO test
	return (s, t, icount, changedZipper)

--show train stations, trains, train cars, and seats
--needs no input
mShowTrains :: ApplicationData -> IO ()
mShowTrains appdata = do
	putStrLn $ "Show trains TODO"

--Show group reservations
--needs as input TRAIN, CAR
mShowGroupReservations :: ApplicationData -> IO ()
mShowGroupReservations appdata = do
	putStrLn $ "Show group reservations TODO"

--show free seat count
--needs as input TRAIN, CAR, FROM, TO
mShowFreeSeats :: ApplicationData -> IO ()
mShowFreeSeats appdata = do
	putStrLn $ "Show free seat count TODO"

--show individual reservations
--needs as input TRAIN, CAR, SEAT
mShowIndividualReservations :: ApplicationData -> IO ()
mShowIndividualReservations appdata = do
	putStrLn $ "Show individual reservations TODO"


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


{---------- Maybe Error Handling ----------}

--do the Maybe stuff, if the result is nothing print out the given error message and return the unchanged input data
--maybeDo :: ApplicationData -> (ApplicationData -> Maybe ApplicationData) -> String -> IO ApplicationData
maybeDo :: a -> (a -> Maybe a) -> String -> IO a
maybeDo d f error = do
	-- maybe more elegant with fromMaybe
	newD <- return $ f d
	case newD of
		Nothing -> do
			putStrLn error
			return d
		
		_ -> return $ fromJust newD


{---------- XML Handling ----------}

--write the zipper data to xml
writeData :: ApplicationData -> String -> IO ()
writeData (_, _, icount, zipper) xmlFilename = do
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
loadData :: String -> IO ApplicationData
loadData xmlFilename = do
	putStrLn $ "Trying to load reservation data from " ++ xmlFilename

	v <- runX
			( -- errorMsgCollect --do not output errors to stderr but collect them
			  -- >>>
			  readDocument
				[ withRemoveWS yes   -- remove redundant whitespace
				, withValidate no    -- don't validate source by DTD
				] xmlFilename
			  >>>
			  -- validate source by Relax NG
			  validateDocumentWithRelaxSchema [] rngFilename
			  -- >>>
			  -- getErrorMessages --returns collected error messages
			  >>>
			  getErrStatus
			)
	--putStrLn $ show v
	
	--TODO flush stderr and stdout before further output, otherwise an error message on stderr will be interspersed with stdout output
	--does not work
	--hFlush stderr
	--hFlush stdout
	
	--using both readDocument and xunpickleDocument would output "file not found" twice, so xunpickleDocument is only executed when validated
	r <- case v of
		--severity [0]=[c_ok]
		[0] -> do
			r <- runX
				( xunpickleDocument xpReservationData
					[ withRemoveWS yes   -- remove redundant whitespace
					, withValidate no    -- don't validate source by DTD
					] xmlFilename
				)
			--putStrLn $ show r
			return r
		
		--severity: [1]=[c_warn], [2]=[c_err], [3]=[c_fatal], else=something_really_really_bad_happened
		_ -> return [] --maybe not the most elegant way, but an error while unpickling also results in []
	
	case r of
		[] -> do
			--xunpickleDocument will print a fatal error *anywhere in the output* when file does not exist -> can that be catched?
			--if xml does not yet exist (or another error has occurred), say so and initialize zipper with default values (issued reservations number = 0)
			putStrLn $ "Could not read database file " ++ xmlFilename
			putStrLn $ "Do you want to load an empty data structure?"
			putStrLn $ "Warning: an existing file " ++ xmlFilename ++ " will be overwritten upon exit then."
			putStrLn $ "Sure? Then type 'YES'."
			
			choice <- getLine
			
			case (map toLower choice) == "yes" of --whitespaces are not ignored yet
				True -> return (stations, trains, 0, makeRZipper [])
				False -> error "Voted for no, aborting."
		
		[(c, z)] -> do
			return (stations, trains, c, makeRZipper z)
		
		_ -> do
			--putStrLn $ "Unknown error while reading database file " ++ xmlFilename ++ ", loading empty data structure"
			--return (0, makeRZipper [])
			error $ "Unknown error while reading database file " ++ xmlFilename

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
		--xpAddFixedAttr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance" $
		--with Relax NG, there is no standard way of referencing the .rng from the .xml
		--xpAddFixedAttr "xsi:noNamespaceSchemaLocation" "reservations.xsd" $
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
