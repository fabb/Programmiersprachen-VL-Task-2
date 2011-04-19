--Fabian Ehrentraud, Bernhard Urban, 2011

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ReservationSystem where

{---------- Imports ----------}

import Data.Maybe
import Data.Char
import Data.List
--import IO
import Text.XML.HXT.Core
import Text.XML.HXT.RelaxNG
 
import System.Environment

import Control.Failure
import Control.Monad.Instances

instance Failure e (Either e) where failure = Left


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

--global minimum of free seats per train
minFreeSeats = 10 :: Integer

--minimum of free seats for the given Train
--for simplification reasons this is static for all Trains, but using this function instead allows to easier incorporate a maximum per Train into the Train type
minFreeTrainSeats :: Train -> Integer
minFreeTrainSeats _ = minFreeSeats

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

--Reservations do not contain date for simplifications purposes. a Train is assumed to only pull out the route once
type GroupReservationData = (FromStation, ToStation, TrainId, CarId, SeatCount)
type IndividualReservationData = (FromStation, ToStation, TrainId, CarId, SeatId)

data RItem = GroupReservation ReservationNumber GroupReservationData
             | IndividualReservation ReservationNumber IndividualReservationData
type RCrumb = RItem 


{---------- Failure String ----------}

tryM :: Failure StringException m => String -> Maybe a -> m a
tryM e Nothing = failureString e
tryM e (Just x) = return x


{---------- Main ----------} 

main :: IO ()
main = do
		--[src, dst] <- getArgs
		xmlFilename <- return xmlFilename
		
		printWelcome
		
		d <- loadData xmlFilename
		
		--putStrLn $ show d --TODO test

		d <- mainloop d
		
		writeData d xmlFilename
		
		--putStrLn $ show d --TODO test
		
		printGoodbye
		
		return ()


--reads in user input and processes wanted changes on reservations
--returns the changed data structure when finished
mainloop :: ApplicationData -> IO ApplicationData
mainloop appData = do
		
		printMenu
		
		choice <- getLine
		--putStr choice
		
		putStrLn ""

		--TODO is it stupid to change zipper like this - especially for non-changing functions?
		appData <- case choice of
			"a" -> mNewIndividualReservation appData
					
			"s" -> mNewGroupReservation appData
					
			"y" -> mDeleteReservation appData
		
			"r" -> mShowTrains appData >> return appData
					
			"d" -> mShowGroupReservations appData >> return appData
					
			"f" -> mShowFreeSeats appData >> return appData
					
			"g" -> mShowIndividualReservations appData >> return appData
					
			"q" -> {- printDummy choice >> -} return appData
			
			_   -> putStrLn ("Wrong input of '" ++ choice ++ "', please choose again.") >> return appData

		if choice == "q"
			then return appData
			else do
				mainloop appData


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


{---------- Menu Option Navigation ----------}

--dialog for issuing a new individual reservation
--needs as input FROM, TO, TRAIN, CAR, COUNT
mNewIndividualReservation :: ApplicationData -> IO ApplicationData
mNewIndividualReservation appdata = do
	putStrLn $ "New Individual Reservation\n"

	putStrLn $ "Please input Starting-Station-ID, Destination-Station-ID, Train-ID, Car-ID and Seat-ID separated by spaces"
	
	l <- getLine
	x <- return $ tokenizeWS l
	
	putStrLn ""
	
	newD <- case x of
		[startstation, endstation, trainid, carid, seatid] -> do
			y <- return (maybeReadTWS startstation :: Maybe StationId, maybeReadTWS endstation :: Maybe StationId, maybeReadTWS trainid :: Maybe TrainId, maybeReadTWS carid :: Maybe CarId, maybeReadTWS seatid :: Maybe SeatId)
			case y of
				(Just startstation, Just endstation, Just trainid, Just carid, Just seatid) -> do
					z <- return $ newIndividualReservation appdata startstation endstation trainid carid seatid
			
					case z of
						Left (StringException error) -> do
							putStrLn error
							return appdata
							--TODO instead of returning to main menu ask for parameters again? but then some breakout must be possible when just wanting back
						Right (newAppdata, resnum) -> do
							putStrLn $ "Successfully added new Individual Reservation from Station " ++ show startstation ++ " to Station " ++ show endstation ++ " for Train " ++ show trainid ++ ", Car " ++ show carid ++ ", Seat " ++ show seatid
							putStrLn $ "Reservation Number: " ++ show resnum
							return newAppdata

				e -> wrongTypes e >> return appdata
			
		e -> wrongArgumentCount e >> return appdata
	
	wait
	
	return newD

--dialog for issuing a new group reservation
--needs as input FROM, TO, TRAIN, CAR, SEAT
mNewGroupReservation :: ApplicationData -> IO ApplicationData
mNewGroupReservation appdata = do
	putStrLn $ "New Group Reservation\n"

	putStrLn $ "Please input Starting-Station-ID, Destination-Station-ID, Train-ID, Car-ID and Seat-Count separated by spaces"
	
	l <- getLine
	x <- return $ tokenizeWS l
	
	putStrLn ""
	
	newD <- case x of
		[startstation, endstation, trainid, carid, seatcount] -> do
			y <- return (maybeReadTWS startstation :: Maybe StationId, maybeReadTWS endstation :: Maybe StationId, maybeReadTWS trainid :: Maybe TrainId, maybeReadTWS carid :: Maybe CarId, maybeReadTWS seatcount :: Maybe SeatCount)
			case y of
				(Just startstation, Just endstation, Just trainid, Just carid, Just seatcount) -> do
					z <- return $ newGroupReservation appdata startstation endstation trainid carid seatcount
			
					case z of
						Left (StringException error) -> do
							putStrLn error
							return appdata
							--TODO instead of returning to main menu ask for parameters again? but then some breakout must be possible when just wanting back
						Right (newAppdata, resnum) -> do
							putStrLn $ "Successfully added new Group Reservation for " ++ show seatcount ++ " Persons from Station " ++ show startstation ++ " to Station " ++ show endstation ++ " for Train " ++ show trainid ++ ", Car " ++ show carid
							putStrLn $ "Reservation Number: " ++ show resnum
							return newAppdata

				e -> wrongTypes e >> return appdata
			
		e -> wrongArgumentCount e >> return appdata
	
	wait
	
	return newD

--dialog for removing an existing reservation
--needs as input RESERVATIONNUMBER
mDeleteReservation :: ApplicationData -> IO ApplicationData
mDeleteReservation appdata = do
	putStrLn $ "Remove Existing Reservation\n"

	putStrLn $ "Please input the Reservation-Number"
	
	l <- getLine
	x <- return $ tokenizeWS l
	
	putStrLn ""
	
	newD <- case x of
		[reservationnumber] -> do
			y <- return (maybeReadTWS reservationnumber :: Maybe ReservationNumber)
			case y of
				(Just reservationnumber) -> do
					z <- return $ deleteReservation appdata reservationnumber
			
					case z of
						Left (StringException error) -> do
							putStrLn error
							return appdata
							--TODO instead of returning to main menu ask for parameters again? but then some breakout must be possible when just wanting back
						Right newAppdata -> do
							putStrLn $ "Successfully removed Reservation " ++ show reservationnumber
							return newAppdata

				e -> wrongTypes e >> return appdata
			
		e -> wrongArgumentCount e >> return appdata
	
	wait
	
	return newD

--show train stations, trains, train cars, and seats
--needs no input
mShowTrains :: ApplicationData -> IO ()
mShowTrains appdata = do
	putStrLn $ "Show Trains and Stations\n"

	putStrLn $ "Stations:"	
	putStrLn $ showStations $ getStations appdata --TODO if Station was a data type, it could have a nice instance Show
	putStrLn ""
	putStrLn $ "Trains:"	
	putStrLn $ showTrains $ getTrains appdata --TODO if Train was a data type, it could have a nice instance Show
	
	wait

--Show group reservations
--needs as input TRAIN, CAR
mShowGroupReservations :: ApplicationData -> IO ()
mShowGroupReservations appdata = do
	putStrLn $ "Show Group Reservations\n"

	putStrLn $ "Please input Train-ID and Car-ID separated by spaces"
	
	l <- getLine
	x <- return $ tokenizeWS l
	
	putStrLn ""
	
	case x of
		[trainid, carid] -> do
			y <- return (maybeReadTWS trainid :: Maybe TrainId, maybeReadTWS carid :: Maybe CarId)
			case y of
				(Just trainid, Just carid) -> do
					z <- return $ groupReservations appdata trainid carid
			
					case z of
						Left (StringException error) -> putStrLn error
							--TODO instead of returning to main menu ask for parameters again? but then some breakout must be possible when just wanting back
						Right groupreservations -> if isREmpty groupreservations
							then putStrLn $ "NO group reservations exist for Train " ++ show trainid ++ ", Car " ++ show carid
							else do
								putStrLn $ "The following group reservations exist for Train " ++ show trainid ++ ", Car " ++ show carid ++ ":"
								putStrLn $ showReservations groupreservations

				e -> wrongTypes e
			
		e -> wrongArgumentCount e
	
	wait

--show individual reservations
--needs as input TRAIN, CAR, SEAT
mShowIndividualReservations :: ApplicationData -> IO ()
mShowIndividualReservations appdata = do
	putStrLn $ "Show Individual Reservations\n"
	
	putStrLn $ "Please input Train-ID, Car-ID and Seat-ID separated by spaces"
	
	l <- getLine
	x <- return $ tokenizeWS l
	
	putStrLn ""
	
	case x of
		[trainid, carid, seatid] -> do
			y <- return (maybeReadTWS trainid :: Maybe TrainId, maybeReadTWS carid :: Maybe CarId, maybeReadTWS seatid :: Maybe SeatId)
			case y of
				(Just trainid, Just carid, Just seatid) -> do
					z <- return $ individualReservations appdata trainid carid seatid
			
					case z of
						Left (StringException error) -> putStrLn error
							--TODO instead of returning to main menu ask for parameters again? but then some breakout must be possible when just wanting back
						Right individualreservations -> if isREmpty individualreservations
							then putStrLn $ "NO individual reservations exist for Train " ++ show trainid ++ ", Car " ++ show carid ++ ", Seat " ++ show seatid
							else do
								putStrLn $ "The following individual reservations exist for Train " ++ show trainid ++ ", Car " ++ show carid ++ ", Seat " ++ show seatid ++ ":"
								putStrLn $ showReservations individualreservations

				e -> wrongTypes e
			
		e -> wrongArgumentCount e
	
	wait

--show free seat count
--needs as input TRAIN, CAR, FROM, TO
mShowFreeSeats :: ApplicationData -> IO ()
mShowFreeSeats appdata = do
	putStrLn $ "Show Free Seat Count\n"

	putStrLn $ "Please input Train-ID, Car-ID, Starting-Station-ID and Destination-Station-ID separated by spaces"
	
	l <- getLine
	x <- return $ tokenizeWS l
	
	putStrLn ""
	
	case x of
		[trainid, carid, startstation, endstation] -> do
			y <- return (maybeReadTWS trainid :: Maybe TrainId, maybeReadTWS carid :: Maybe CarId, maybeReadTWS startstation :: Maybe StationId, maybeReadTWS endstation :: Maybe StationId)
			case y of
				(Just trainid, Just carid, Just startstation, Just endstation) -> do
					z <- return $ freeSeats appdata trainid carid startstation endstation
			
					case z of
						Left (StringException error) -> putStrLn error
							--TODO instead of returning to main menu ask for parameters again? but then some breakout must be possible when just wanting back
						Right seats -> do
							putStrLn $ "The following count of free seats are available at minimum for Train " ++ show trainid ++ ", Car " ++ show carid ++ " between the Stations " ++ show startstation ++ " and " ++ show endstation ++ ":"
							putStrLn $ show seats

				e -> wrongTypes e
			
		e -> wrongArgumentCount e
	
	wait


--output error message in case of wrong types
wrongTypes :: a -> IO ()
wrongTypes e = do
	putStrLn "At Least One Input Argument of Wrong Type"
	mainBack

--output error message in case of wrong count of arguments
wrongArgumentCount :: a -> IO ()
wrongArgumentCount e = do
	putStrLn "Wrong Count of Input Arguments"
	mainBack

mainBack :: IO ()
mainBack = putStrLn "Returning to Main Menu"

--wait for user input to continue
wait :: IO ()
wait = do
	putStr "\nPress any key to continue..."
	getLine
	return ()


{---------- Input Unpacking ----------}

{- example on how to read several different types from a single string
readbound :: IO ()
readbound = do
	l <- getLine
	x <- return $ tokenizeWS l
	case x of
		[a,b] -> do
			r <- return (maybeReadTWS a :: Maybe Integer, maybeReadTWS b :: Maybe Int)
			case r of
				(Just ja, Just jb) -> putStrLn $ "Ok, Integer a: " ++ show ja ++ " and Int b: " ++ show jb

				_ -> putStrLn "at least one wrong type"
			
		_ -> putStrLn "wrong argument count"
-}

--read in the String to the wanted type
--if not fitting to that type, returns Nothing
--leading and trailing whitespaces are ignored (leading already by function reads)
maybeReadTWS :: (Failure StringException m, Read a) => String -> m a
maybeReadTWS = (return . fst =<<) . (tryM "Error: Could not read wanted type" . listToMaybe) . filter (null . dropWhile isSpace . snd) . reads

{-
--like listToMaybe, but for all monads with a Failure instance
listToF :: Failure StringException m => [a] -> m a
listToF [] = failureString "Empty List"
listToF (a:_) = return a
-}

--splits the given String into substrings
--spaces separate substrings and are thrown away
tokenizeWS :: String -> [String]
tokenizeWS xs
	| null noleading = []
	| otherwise = current : tokenizeWS rest
	where
		noleading = dropWhile isSpace xs
		current = takeWhile (not . isSpace) noleading
		rest = dropWhile (not . isSpace) noleading


{---------- Real Working Functions ----------}

--issues a new individual reservation when there is enough place left
newIndividualReservation :: Failure StringException m => ApplicationData -> FromStation -> ToStation -> TrainId -> CarId -> SeatId -> m (ApplicationData, ReservationNumber)
newIndividualReservation appdata startstation endstation trainid carid seatid = do
	newData1 <- return $ incIssuedReservations appdata
	resnum <- return $ getIssuedReservations newData1
	stations <- return $ getStations appdata
	trains <- return $ getTrains appdata
	newIR <- newRIndividualReservation resnum (startstation, endstation, trainid, carid, seatid) stations trains
	isReservationPossible appdata newIR
	rz <- return $ getReservationZipper newData1
	mrz <- return $ reservationNewLast newIR rz
	case mrz of
		Nothing -> error "Program Error, could not add new reservation" --could also return a Left for not aborting program
		Just newData2 -> return (setReservationZipper newData1 newData2, resnum)

--issues a new group reservation when there is enough place left
newGroupReservation :: Failure StringException m => ApplicationData -> FromStation -> ToStation -> TrainId -> CarId -> SeatCount -> m (ApplicationData, ReservationNumber)
newGroupReservation appdata startstation endstation trainid carid seatcount = do
	newData1 <- return $ incIssuedReservations appdata
	resnum <- return $ getIssuedReservations newData1
	stations <- return $ getStations appdata
	trains <- return $ getTrains appdata
	newGR <- newRGroupReservation resnum (startstation, endstation, trainid, carid, seatcount) stations trains
	isReservationPossible appdata newGR
	rz <- return $ getReservationZipper newData1
	mrz <- return $ reservationNewLast newGR rz
	case mrz of
		Nothing -> error "Program Error, could not add new reservation" --could also return a Left for not aborting program
		Just newData2 -> return (setReservationZipper newData1 newData2, resnum)

--deletes the reservation with the given reservation number
deleteReservation :: Failure StringException m => ApplicationData -> ReservationNumber -> m ApplicationData
deleteReservation appdata reservationnumber = do
	--changedZipper <- return $ fromMaybe (makeRZipper []) $ reservationDeleteCurrent zipper --test
	--changedZipper <- maybeDo zipper (\ z -> reservationDeleteCurrent z) "Error: Could not delete first item" --test
	--changedZipper <- return $ reservationDeleteCurrent (getReservationZipper appdata) --test, just deletes first item
	--changedZipper <- return $ reservationTo reservationnumber (getReservationZipper appdata) >>= reservationDeleteCurrent
	changedZipper <- return $ reservationDelete reservationnumber (getReservationZipper appdata)
	case changedZipper of
		Nothing -> failureString "Error: No such Reservation found"
		Just x -> return $ setReservationZipper appdata x

--calculates group reservations for given Train Car
groupReservations :: Failure StringException m => ApplicationData -> TrainId -> CarId -> m [RItem]
groupReservations appdata trainid carid = do
	trains <- return $ getTrains appdata
	if existsTrain trainid trains
		then return True else failureString "Error: No such Train-ID"
	if existsTrainCar trainid carid trains
		then return True else failureString "Error: No such Car-ID attached to existing Train-ID"
	res <- return $ unpackRZipper $ getReservationZipper appdata
	return $ filterTrainCar trainid carid $ filterGroupReservations res

--calculates individual reservations for the given seat (in the given car (which is part of the given train))
individualReservations :: Failure StringException m => ApplicationData -> TrainId -> CarId -> SeatId -> m [RItem]
individualReservations appdata trainid carid seatid = do
	trains <- return $ getTrains appdata
	if existsTrain trainid trains
		then return True else failureString "Error: No such Train-ID"
	if existsTrainCar trainid carid trains
		then return True else failureString "Error: No such Car-ID attached to existing Train-ID"
	if existsTrainCarSeat trainid carid seatid trains
		then return True else failureString "Error: No such Seat-ID in existing Car-ID attached to existing Train-ID"
	res <- return $ unpackRZipper $ getReservationZipper appdata
	return $ filterTrainCarSeat trainid carid seatid $ filterIndividualReservations res --filterIndividualReservations wouldn't be necessary as filterTrainCarSeat already does that

--calculates minimum free seat count in given Train Car between given Stations
--if the train is so full as a whole (minimum free seats per train) that it's free seats , that value will be displayed instead
freeSeats :: Failure StringException m => ApplicationData -> TrainId -> CarId -> FromStation -> ToStation -> m SeatCount
freeSeats appdata trainid carid startstation endstation = do
	minfree <- return (getTrains appdata) >>= getTrain trainid >>= return . minFreeTrainSeats
	--trainseatcount <- return (getTrains appdata) >>= getSeatCountTrainId trainid	
	trainfree <- freeSeatsTrain appdata trainid startstation endstation
	carfree <- freeSeatsCar appdata trainid carid startstation endstation
	return $ min carfree (trainfree - minfree)


--returns the number of free seats for the given Train between the provided stations
freeSeatsTrain :: Failure StringException m => ApplicationData -> TrainId -> FromStation -> ToStation -> m Integer
freeSeatsTrain appdata trainid fromstation tostation = do
	stations <- return $ getStations appdata
	trains <- return $ getTrains appdata
	res <- return $ unpackRZipper $ getReservationZipper appdata
	wholeseats <- getSeatCountTrainId trainid trains
	used <- getUsedSeatCountTrainMaximum fromstation tostation stations trainid res
	return $ wholeseats - used

--returns the number of free seats for the given Train Car between the provided stations
freeSeatsCar :: Failure StringException m => ApplicationData -> TrainId -> CarId -> FromStation -> ToStation -> m Integer
freeSeatsCar appdata trainid carid fromstation tostation = do
	stations <- return $ getStations appdata
	trains <- return $ getTrains appdata
	res <- return $ unpackRZipper $ getReservationZipper appdata
	wholeseats <- getSeatCountTrainCarId trainid carid trains
	used <- getUsedSeatCountTrainCarMaximum fromstation tostation stations trainid carid res
	return $ wholeseats - used

--returns the number of free seats for the given Seat between the provided stations (1 or 0)
freeSeat :: Failure StringException m => ApplicationData -> TrainId -> CarId -> SeatId -> FromStation -> ToStation -> m Integer
freeSeat appdata trainid carid seatid fromstation tostation = do
	stations <- return $ getStations appdata
	res <- return $ unpackRZipper $ getReservationZipper appdata
	used <- getUsedSeatCountTrainCarSeatMaximum fromstation tostation stations trainid carid seatid res
	return $ 1 - used


{---------- Check Reservation Possibility ----------}

--calculates whether the given, not yet issued, reservation is possible
--also checks whether StartStation and EndStation are valid
--Bool does not hold any information
isReservationPossible :: Failure StringException m => ApplicationData -> RItem -> m Bool

isReservationPossible appdata ritem =
	if isGroupReservation ritem then do
		--freeSeats does also take account for minFreeSeats
		freeseats <- freeSeats appdata (getRTrainId ritem) (getRCarId ritem) (getRFromStationId ritem) (getRToStationId ritem)
		groupseats <- maybe
			(error "Program Error: Could not get Seat Count for Group Reservation")
			return $ getRSeatCount ritem
		
		if freeseats >= groupseats
			then return True
			else failureString "Fail: Could not issue Group Reservation - not enough free seats" --TODO discern finer

	else if isIndividualReservation ritem then do
		seatid <- maybe
			(error "Program Error: Could not get Seat-ID for Individual Reservation")
			return $ getRSeatId ritem

		isseatfree <- freeSeat appdata (getRTrainId ritem) (getRCarId ritem) seatid (getRFromStationId ritem) (getRToStationId ritem)
		
		if isseatfree >= 1
			then return True
			else failureString "Fail: Could not issue Individual Reservation - Seat is already reserved, at least on part of the provided Station Range"
		
		--freeSeats does also take account for minFreeSeats
		freeseats <- freeSeats appdata (getRTrainId ritem) (getRCarId ritem) (getRFromStationId ritem) (getRToStationId ritem)
		
		if freeseats >= 1
			then return True
			else failureString "Fail: Could not issue Individual Reservation - Train Car is already full, or Minimum free Seats for Train undercut" --TODO discern finer
	
	else error "Program Error: Unknown Reservation Type encountered"


{---------- Access ApplicationData ----------}

--type ApplicationData = (Stations, Trains, IssuedReservations, ReservationZipper)

getStations :: ApplicationData -> Stations
getStations (s, _, _, _) = s

getTrains :: ApplicationData -> Trains
getTrains (_, t, _, _) = t

getIssuedReservations :: ApplicationData -> IssuedReservations
getIssuedReservations (_, _, i, _) = i

setIssuedReservations :: ApplicationData -> IssuedReservations -> ApplicationData
setIssuedReservations (s, t, _, z) i = (s, t, i, z)

getReservationZipper :: ApplicationData -> ReservationZipper
getReservationZipper (_, _, _, z) = z

setReservationZipper :: ApplicationData -> ReservationZipper -> ApplicationData
setReservationZipper (s, t, i, _) z = (s, t, i, z)


{---------- Access IssuedReservations ADT ----------}

--increase unique reservatin number counter (after issuiong a reservation)
incIssuedReservations :: ApplicationData -> ApplicationData
incIssuedReservations appdata = setIssuedReservations appdata $ 1 + getIssuedReservations appdata


{---------- Access Stations ADT ----------}

showStations :: Stations -> String
showStations (x:[]) = show x
showStations (x:xs) = show x ++ " - " ++ showStations (xs)

--could also be dependent of Train if it does not stop in all Stations, not implemented for simplification reasons
getStationsBetween :: Failure StringException m => FromStation -> ToStation -> Stations -> m [Station]
getStationsBetween = takeRange


--gets the range from the given list where all items between and including the first occurrence of the "first" item and the first ocurrence of the "last" item are returned
--when the "first" item occurrs after the "last" item in the given list, the result is reversed in order to let the "first" item really appear first
takeRange :: (Failure StringException m, Eq a) => a -> a -> [a] -> m [a]
takeRange first last list
	| notElem first list || notElem last list = failureString "Error: Provided First and Last Elements not part of List"
	| first == last = return [first]
	| occurrsBefore first last list = return $ takeWhileInclusive (/=last) $ dropWhile (/=first) list
	| not $ occurrsBefore first last list = return $ reverse $ takeWhileInclusive (/=first) $ dropWhile (/=last) list

--returns whether the first element occurrs before the second one in the given list
--when the two elements are equal, return False
--both elements must be guaranteed to be contained in the list
occurrsBefore :: Eq a => a -> a -> [a] -> Bool
occurrsBefore a b l = notElem b $ takeWhileInclusive (/=a) l

--same as takeWhile, but also adds the last, non-matching element
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs)
	| p x       =  x : takeWhileInclusive p xs
	| otherwise =  [x]


--maps the function f over all inbetween stations, returns Nothing when FromStation==ToStation
mapStationsM :: Failure StringException m => (FromStation -> ToStation -> Stations -> a) -> FromStation -> ToStation -> Stations -> m [a]
mapStationsM f fromstation tostation stations = sfe
	where
		sf = mapStations f fromstation tostation stations 
		sfe = if fromstation == tostation
			then failureString "Error: Starting-Station-ID same as Destination-Station-ID"
			else sf

--maps the function f over all inbetween stations
mapStations :: Failure StringException m => (FromStation -> ToStation -> Stations -> a) -> FromStation -> ToStation -> Stations -> m [a]
mapStations f fromstation tostation stations = (return . (map (\ (from,to) -> f from to stations)) =<<) (stationTuples fromstation tostation stations)


--calculates tuples of stations next to each other in the given range
stationTuples :: Failure StringException m => FromStation -> ToStation -> Stations -> m [(Station,Station)]
stationTuples fromstation tostation stations = tuples
	where
		stationrange = getStationsBetween fromstation tostation stations
		tuples = (return . tuplifyNeighbors =<<) stationrange

--converts a list into a list with tuples of adjacent elements from the original list
tuplifyNeighbors :: [a] -> [(a,a)]
tuplifyNeighbors [] = []
tuplifyNeighbors (x:[]) = []
tuplifyNeighbors (x:y:xs) = (x,y) : tuplifyNeighbors (y:xs)


existsStation :: StationId -> Stations -> Bool
existsStation stationid stations = elem stationid stations


{---------- Access Trains ADT ----------}

showTrains :: Trains -> String
showTrains = concatMap ((++"\n") . showTrain)

showTrain :: Train -> String
showTrain train = "Train " ++ show (getTrainId train) ++ ": " ++ showCars (getCars train)

showCars :: Cars -> String
showCars = concatMap showCar

showCar :: Car -> String
showCar car = "\n\tCar " ++ show (getCarId car) ++ ": " ++ showSeats (getSeats car)

showSeats :: Seats -> String
showSeats seats = "Seats: " ++ showSeat (head seats) ++ concatMap ((" - " ++) . showSeat) (tail seats)

showSeat :: Seat -> String
showSeat = show . getSeatId


--type Train = (TrainId, Cars)
getTrainId :: Train -> TrainId
getTrainId = fst

getCars :: Train -> Cars
getCars = snd

getTrainIds :: Trains -> [TrainId]
getTrainIds trains = map getTrainId trains

--type Car = (CarId, Seats)
getCarId :: Car -> CarId
getCarId = fst

getSeats :: Car -> Seats
getSeats = snd

getCarIds :: Cars -> [CarId]
getCarIds cars = map getCarId cars

--type Seat = SeatId
getSeatId :: Seat -> SeatId
getSeatId = id

getSeatIds :: Seats -> [SeatId]
getSeatIds seats = map getSeatId seats


getTrain :: Failure StringException m => TrainId -> Trains -> m Train
getTrain tId trains = tryM "Error: Train-ID not found" $ find (\ t -> getTrainId t == tId) trains

getCar :: Failure StringException m => CarId -> Train -> m Car
getCar cId train = tryM "Error: Car-ID not found" $ find (\ c -> getCarId c == cId) $ getCars train

getTrainCar :: Failure StringException m => TrainId -> CarId -> Trains -> m Car
getTrainCar tId cId trains = getTrain tId trains >>= getCar cId

getSeat :: Failure StringException m => SeatId -> Car -> m Seat
getSeat sId car = tryM "Error: Seat-ID not found" $ find (\ s -> getSeatId s == sId) $ getSeats car

getTrainCarSeat :: Failure StringException m => TrainId -> CarId -> SeatId -> Trains -> m Seat
getTrainCarSeat tId cId sId trains = getTrain tId trains >>= getCar cId >>= getSeat sId


existsTrain :: TrainId -> Trains -> Bool
existsTrain tId trains = getTrain tId trains /= Nothing

existsCar :: CarId -> Train -> Bool
existsCar cId train = getCar cId train /= Nothing

existsTrainCar :: TrainId -> CarId -> Trains -> Bool
existsTrainCar tId cId trains = (getTrain tId trains >>= getCar cId) /= Nothing

existsSeat :: SeatId -> Car -> Bool
existsSeat sId car = getSeat sId car /= Nothing

existsTrainCarSeat :: TrainId -> CarId -> SeatId -> Trains -> Bool
existsTrainCarSeat tId cId sId train = (getTrain tId trains >>= getCar cId >>= getSeat sId) /= Nothing

{- only working with data, not with type
class SeatCountable a where
	getSeatCount :: a -> Integer

instance SeatCountable Train where
	getSeatCount train = sum $ map getSeatCount $ getCars train

instance SeatCountable Car where
	getSeatCount car = length $ getSeats car
-}


getSeatCountTrainId :: Failure StringException m => TrainId -> Trains -> m Integer
getSeatCountTrainId trainid trains = (return . getSeatCountTrain =<<) (getTrain trainid trains)

getSeatCountTrainCarId :: Failure StringException m => TrainId -> CarId -> Trains -> m Integer
getSeatCountTrainCarId trainid carid trains = (return . getSeatCountCar =<<) (getTrainCar trainid carid trains)


getSeatCountTrain :: Train -> Integer
getSeatCountTrain train = sum $ map getSeatCountCar $ getCars train

getSeatCountCar :: Car -> Integer
getSeatCountCar car = length' $ getSeats car

length' :: [a] -> Integer
length' (x:xs) = 1 + length' xs
length' [] = 0


{---------- Access [RItem] Read-Only ----------}

instance Show RItem where
	show (GroupReservation resnum (fromstation, tostation, trainid, carid, seatcount)) =
		"Group Reservation - Reservation Number " ++ show resnum ++
		" - " ++ show seatcount ++ " Persons" ++
		" - from Station " ++ show fromstation ++ " to " ++ show tostation ++
		" - in Train " ++ show trainid ++ ", Car " ++ show carid
	show (IndividualReservation resnum (fromstation, tostation, trainid, carid, seatid)) =
		"Individual Reservation - Reservation Number " ++ show resnum ++
		" - from Station " ++ show fromstation ++ " to " ++ show tostation ++
		" - in Train " ++ show trainid ++ ", Car " ++ show carid ++ ", Seat " ++ show seatid

--convert a reservation list into a better readable String
--TODO this could be its own data which its own instance Show, but that would also need a constructor
showReservations :: [RItem] -> String
showReservations = concatMap ((++"\n") . show)


filterTrain :: TrainId -> [RItem] -> [RItem]
filterTrain trainid = filter (\ r -> getRTrainId r == trainid)

filterCar :: CarId -> [RItem] -> [RItem]
filterCar carid = filter (\ r -> getRCarId r == carid)

filterTrainCar :: TrainId -> CarId -> [RItem] -> [RItem]
filterTrainCar trainid carid = filterCar carid . filterTrain trainid

--only returns Individual Reservations as Group Reservations have no SeatId
filterSeat :: SeatId -> [RItem] -> [RItem]
filterSeat seatid = filter (\ r -> fromJust (getRSeatId r) == seatid) . filterIndividualReservations

filterTrainCarSeat :: TrainId -> CarId -> SeatId -> [RItem] -> [RItem]
filterTrainCarSeat trainid carid seatid = filterSeat seatid . filterCar carid . filterTrain trainid


filterGroupReservations :: [RItem] -> [RItem]
filterGroupReservations = filter isGroupReservation

filterIndividualReservations :: [RItem] -> [RItem]
filterIndividualReservations = filter isIndividualReservation


isREmpty :: [RItem] -> Bool
isREmpty = null


--gets all given reservations which are active between the given stations
filterActiveBetween :: FromStation -> ToStation -> Stations -> [RItem] -> [RItem]
filterActiveBetween fromstation tostation stations = filter (fromMaybe False . (isActiveBetween fromstation tostation stations))


--gets the used seat count (from Group and Individual Reservations) between the given stations in the given Train
--this is a simple sum and no minimum
getUsedSeatCountTrain :: FromStation -> ToStation -> Stations -> TrainId -> [RItem] -> SeatCount
getUsedSeatCountTrain fromstation tostation stations trainid = sum . map getRUsedSeats . filterActiveBetween fromstation tostation stations . filterTrain trainid

--gets the used seat count (from Group and Individual Reservations) between the given stations in the given Car
--this is a simple sum and no minimum
getUsedSeatCountTrainCar :: FromStation -> ToStation -> Stations -> TrainId -> CarId -> [RItem] -> SeatCount
getUsedSeatCountTrainCar fromstation tostation stations trainid carid = sum . map getRUsedSeats . filterActiveBetween fromstation tostation stations . filterTrainCar trainid carid

--gets the used seat count (from Individual Reservations) between the given stations for the given Seat
--this is a simple sum and no minimum
getUsedSeatCountTrainCarSeat :: FromStation -> ToStation -> Stations -> TrainId -> CarId -> SeatId -> [RItem] -> SeatCount
getUsedSeatCountTrainCarSeat fromstation tostation stations trainid carid seatid = sum . map getRUsedSeats . filterActiveBetween fromstation tostation stations . filterTrainCarSeat trainid carid seatid


--calculates the maximum used seats for the given Train in the given Station range
--takes care of overlapping and non-overlapping reservations
getUsedSeatCountTrainMaximum :: Failure StringException m => FromStation -> ToStation -> Stations -> TrainId -> [RItem] -> m SeatCount
getUsedSeatCountTrainMaximum fromstation tostation stations trainid ritems = maximumF =<< getUsedSeatCountTrainStationwise fromstation tostation stations trainid ritems

--calculates the maximum used seats for the given Car in the given Station range
--takes care of overlapping and non-overlapping reservations
getUsedSeatCountTrainCarMaximum :: Failure StringException m => FromStation -> ToStation -> Stations -> TrainId -> CarId -> [RItem] -> m SeatCount
getUsedSeatCountTrainCarMaximum fromstation tostation stations trainid carid ritems = maximumF =<< getUsedSeatCountTrainCarStationwise fromstation tostation stations trainid carid ritems

--calculates the maximum used seats for the given Seat in the given Station range
--takes care of overlapping and non-overlapping reservations
getUsedSeatCountTrainCarSeatMaximum :: Failure StringException m => FromStation -> ToStation -> Stations -> TrainId -> CarId -> SeatId -> [RItem] -> m SeatCount
getUsedSeatCountTrainCarSeatMaximum fromstation tostation stations trainid carid seatid ritems = maximumF =<< getUsedSeatCountTrainCarSeatStationwise fromstation tostation stations trainid carid seatid ritems


--calculates the maximum of the list
--returns Failure only when the list is empty
maximumF :: (Failure StringException m, Ord a) => [a] -> m a
maximumF [] = failureString "Error: List is empty"
maximumF ls = return $ maximum ls


--calculates the reservations for the given Train for all stations in the given range
getUsedSeatCountTrainStationwise :: Failure StringException m => FromStation -> ToStation -> Stations -> TrainId -> [RItem] -> m [SeatCount]
getUsedSeatCountTrainStationwise fromstation tostation stations trainid ritems = 
	(return . (map (\ f -> f trainid ritems)) =<<) (mapStationsM getUsedSeatCountTrain fromstation tostation stations)

--calculates the reservations for the given Car for all stations in the given range
getUsedSeatCountTrainCarStationwise :: Failure StringException m => FromStation -> ToStation -> Stations -> TrainId -> CarId -> [RItem] -> m [SeatCount]
getUsedSeatCountTrainCarStationwise fromstation tostation stations trainid carid ritems = 
	(return . (map (\ f -> f trainid carid ritems)) =<<) (mapStationsM getUsedSeatCountTrainCar fromstation tostation stations)

--calculates the reservations for the given Seat for all stations in the given range
getUsedSeatCountTrainCarSeatStationwise :: Failure StringException m => FromStation -> ToStation -> Stations -> TrainId -> CarId -> SeatId -> [RItem] -> m [SeatCount]
getUsedSeatCountTrainCarSeatStationwise fromstation tostation stations trainid carid seatid ritems = 
	(return . (map (\ f -> f trainid carid seatid ritems)) =<<) (mapStationsM getUsedSeatCountTrainCarSeat fromstation tostation stations)


{---------- Access RItem ----------}

--creates a new Individual Reservation
--checks parameters for validity - except to ReservationNumber
newRIndividualReservation :: Failure StringException m => ReservationNumber -> (FromStation, ToStation, TrainId, CarId, SeatId) -> Stations -> Trains -> m RItem
newRIndividualReservation resnum (fromstation, tostation, trainid, carid, seatid) stations trains = do
	checkRReservationData fromstation tostation trainid carid stations trains
	if existsTrainCarSeat trainid carid seatid trains then return True else failureString "Error: Seat-ID does not exist"
	return (IndividualReservation resnum (fromstation, tostation, trainid, carid, seatid))

--creates a new Individual Reservation
--checks parameters for validity - except to ReservationNumber
newRGroupReservation :: Failure StringException m => ReservationNumber -> (FromStation, ToStation, TrainId, CarId, SeatCount) -> Stations -> Trains -> m RItem
newRGroupReservation resnum (fromstation, tostation, trainid, carid, seatcount) stations trains = do
	checkRReservationData fromstation tostation trainid carid stations trains
	if seatcount > 1 then return True else failureString "Error: Seat-Count must be at least 2"
	return (GroupReservation resnum (fromstation, tostation, trainid, carid, seatcount))

--checks parameters, which are common to group and individual reservations, for validity
checkRReservationData :: Failure StringException m => FromStation -> ToStation -> TrainId -> CarId -> Stations -> Trains -> m Bool
checkRReservationData fromstation tostation trainid carid stations trains = do
	if existsStation fromstation stations then return True else failureString "Error: Starting-Station-ID does not exist"
	if existsStation tostation stations then return True else failureString "Error: Destination-Station-ID does not exist"
	if fromstation /= tostation then return True else failureString "Error: Starting-Station-ID cannot be the same as Destination-Station-ID"
	if existsTrain trainid trains then return True else failureString "Error: Train-ID does not exist"
	if existsTrainCar trainid carid trains then return True else failureString "Error: Car-ID does not exist"


isGroupReservation :: RItem -> Bool
isGroupReservation (GroupReservation _ _) = True
isGroupReservation _ = False

isIndividualReservation :: RItem -> Bool
isIndividualReservation (IndividualReservation _ _) = True
isIndividualReservation _ = False


getRReservationNumber :: RItem -> ReservationNumber
getRReservationNumber (IndividualReservation resnum _) = resnum
getRReservationNumber (GroupReservation resnum _) = resnum

getRFromStationId :: RItem -> FromStation
getRFromStationId (IndividualReservation _ (fromstationid,_,_,_,_)) = fromstationid
getRFromStationId (GroupReservation _ (fromstationid,_,_,_,_)) = fromstationid

getRToStationId :: RItem -> ToStation
getRToStationId (IndividualReservation _ (_,tostationid,_,_,_)) = tostationid
getRToStationId (GroupReservation _ (_,tostationid,_,_,_)) = tostationid

getRTrainId :: RItem -> TrainId
getRTrainId (IndividualReservation _ (_,_,trainid,_,_)) = trainid
getRTrainId (GroupReservation _ (_,_,trainid,_,_)) = trainid

getRCarId :: RItem -> CarId
getRCarId (IndividualReservation _ (_,_,_,carid,_)) = carid
getRCarId (GroupReservation _ (_,_,_,carid,_)) = carid

getRSeatId :: Failure StringException m => RItem -> m SeatId
getRSeatId (IndividualReservation _ (_,_,_,_,seatid)) = return seatid
getRSeatId (GroupReservation _ _) = failureString "Error: Cannot get Seat-ID as GroupReservation present"

getRSeatCount :: Failure StringException m => RItem -> m SeatCount
getRSeatCount (GroupReservation _ (_,_,_,_,seatcount)) = return seatcount
getRSeatCount (IndividualReservation _ _) = failureString "Error: Cannot get Seat-Count as IndividualReservation present"


getRUsedSeats :: RItem -> SeatCount
getRUsedSeats (GroupReservation _ (_,_,_,_,seatcount)) = seatcount
getRUsedSeats (IndividualReservation _ _) = 1



--checks whether the given reservation is active between the given stations
isActiveBetween :: Failure StringException m => FromStation -> ToStation -> Stations -> RItem -> m Bool
isActiveBetween fromstation tostation stations ritem = do
	if fromstation /= tostation then return True
		else failureString "Error: Starting-Station-ID is the same as Destination-Station-ID"
	resstations <- getStationsBetween (getRFromStationId ritem) (getRToStationId ritem) stations --stations the reservation is active for
	checkstations <- getStationsBetween fromstation tostation stations --stations inbetween the given bounds - this must be a subset
	return $ isInfixOf checkstations resstations


{---------- Zipper Stuff ----------}

--creates a zipper from a list of reservations
makeRZipper :: [RItem] -> ReservationZipper
makeRZipper xs = (xs, [])

--unpacks the zipper back to a list of reservations
unpackRZipper :: ReservationZipper -> [RItem]
unpackRZipper z = maybe [] fst $ goFirst z --TODO this binds the return type of goFirst to the Maybe Monad - is there another way?

--forwards for one item
goForward :: Failure StringException m => ReservationZipper -> m ReservationZipper
goForward (x:xs, bs) = return (xs, x:bs)
goForward ([], _) = failureString "Could not go forward in Zipper, as already at the end"

--rewinds to the previous item
goBack :: Failure StringException m => ReservationZipper -> m ReservationZipper
goBack (xs, b:bs) = return (b:xs, bs)
goBack (_, []) = failureString "Could not go back in Zipper, as already at the beginning"

--rewinds to the first item
goFirst :: Failure StringException m => ReservationZipper -> m ReservationZipper
goFirst z@(xs, b:bs) = goBack z >>= goFirst
goFirst z@(_, []) = return z

--forwards until after the last item
goLast :: Failure StringException m => ReservationZipper -> m ReservationZipper
goLast z@(x:xs, bs) = goForward z >>= goLast
goLast z@([], _) = return z

--sets the current item to the one with the given number
reservationTo :: Failure StringException m => ReservationNumber -> ReservationZipper -> m ReservationZipper
reservationTo resnum z@(xs, bs) = do
	(items, _) <- goFirst z
	(ls, item:rs) <- return $ break (reservationIs resnum) items
	return (item:rs, reverse ls) --TODO better without break but with recursive goForward?

--True when the given item has the given reservation number
reservationIs :: ReservationNumber -> RItem -> Bool
reservationIs resnum ritem = resnum == getRReservationNumber ritem

--inserts a new reservation before the current item
reservationNew :: Failure StringException m => RItem -> ReservationZipper -> m ReservationZipper
reservationNew item (xs, bs) = return (item:xs, bs)

--inserts a new reservation as last item
reservationNewLast :: Failure StringException m => RItem -> ReservationZipper -> m ReservationZipper
reservationNewLast item z = goLast z >>= reservationNew item

--delete the current item
reservationDeleteCurrent :: Failure StringException m => ReservationZipper -> m ReservationZipper
reservationDeleteCurrent (x:xs, bs) = return (xs, bs)
reservationDeleteCurrent ([], _) = failureString "Could not delete current item, as at the end of the Zipper"

--deletes the reservation with the given number
reservationDelete :: Failure StringException m => ReservationNumber -> ReservationZipper -> m ReservationZipper
reservationDelete resnum z = reservationTo resnum z >>= reservationDeleteCurrent


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
