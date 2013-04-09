module Main where
	import Data.List (groupBy, transpose, find)
	import Data.List.Split (splitOn)
	import System.IO
	import Data.Maybe (isNothing, isJust, fromJust, catMaybes)
	import Control.Applicative

	--Strings
	helpForHumans = "Place an X token on the board defined by a single number corresponding to its position on a regular 9-digit keypad\n|7|8|9|\n|4|5|6|\n|1|2|3|"
	endGameMessage = "Thanks for playing!"
	invalidRangeMessage = "Try a number from 1 to 9."
	invalidSpotMessage = "That spot is occupied."

	type Board = [Row]
	-- A Nothing in a Row means nothing is there,
	-- a Just n means there is something in that square
	type Row = [Square]

	data Square = Square {coords :: Coords, token :: Token}
		deriving (Read, Show, Eq)

	type Coords = (Int, Int)
	type Token = Maybe Char
	type Brain = (Player -> Player -> Board -> Board)

	data Player = Human Token | AI Brain Token

	getPlayerToken :: Player -> Token
	getPlayerToken (Human tk) = tk
	getPlayerToken (AI _ tk) = tk

	isHuman :: Player -> Bool
	isHuman (Human _) = True
	isHuman _ = False
	isAI :: Player -> Bool
	isAI (AI _ _) = True
	isAI _ = False
	

	-- Play a standard game of tic-tac-toe against an AI opponent
	play1P = play1PFirst
	play1PFirst = playChoose (Human (Just 'X')) (AI hardAIBrain (Just 'O'))
	play1PSecond = playChoose (AI hardAIBrain (Just 'O')) (Human (Just 'X'))

	-- Play a human vs human game of tic-tac-toe
	play2P = playChoose (Human (Just 'X')) (Human (Just 'O'))

	-- Watch a AI vs AI game of tic-tac-toe
	playAI = playChoose (AI hardAIBrain (Just 'X')) (AI hardAIBrain (Just 'O'))

	-- 1 to start, 2 to play second
	playChoose :: Player -> Player -> IO ()
	playChoose p1 p2 = 
		do 
			if isHuman p1 || isHuman p2 then
				do 
					putStrLn helpForHumans
					playH (getMove p1 p2) (getMove p2 p1) (makeBoard ())
			else
				playH (getMove p1 p2) (getMove p2 p1) (makeBoard ())
		where
			--takes two "move definitions", one for each player, and a board
			playH :: (Board -> IO Board) -> (Board -> IO Board) -> Board -> IO ()
			playH p1 p2 bd =
				do
					bd1 <- p1 bd
					if isDone bd1 then
						endPlay bd1
					else
						do
							bd2 <- p2 bd1
							if isDone bd2 then
								do
									endPlay bd2
							else
								do
									playH p1 p2 bd2
			
			endPlay :: Board -> IO ()
			endPlay bd = 
				do
					putStrLn endGameMessage

			getMove :: Player -> Player -> (Board -> IO Board)
			getMove (AI brain tk) opp bd = 
				let newBd = brain (AI brain tk) opp bd in
					do
						prtBoard newBd
						return newBd
			getMove (Human tk) opp bd = 
				do
					co <- getCoordsInput bd
					let newBd = (setToken tk co bd) in
						do
							prtBoard newBd
							return newBd


			-- gets coordinates as one number according to its position on a regular 9 digit keypad
			getCoordsInput :: Board -> IO Coords
			getCoordsInput bd =
				do 
					line <- getLine
					let inp = (read line) in
						if isOnBoard inp then
							let co = (fromDigitToCoords inp) in
								if isNotOccupied bd co then
									return co
								else
									do
										putStrLn invalidSpotMessage
										getCoordsInput bd
						else
							do
								putStrLn invalidRangeMessage
								getCoordsInput bd
			
			isOnBoard :: Int -> Bool
			isOnBoard k = (k >= 1) && (k <= 9)
			isNotOccupied :: Board -> Coords -> Bool
			isNotOccupied bd co = isNothing (getToken co bd)

			fromDigitToCoords :: Int -> Coords
			fromDigitToCoords s 
				| s==1 = (2,0)
				| s==2 = (2,1)
				| s==3 = (2,2)
				| s==4 = (1,0)
				| s==5 = (1,1)
				| s==6 = (1,2)
				| s==7 = (0,0)
				| s==8 = (0,1)
				| s==9 = (0,2)


	-- creates a tic-tac-toe board
	makeBoard :: () -> Board
	makeBoard () = makeNByNBoard 3
		where
			-- creates an n by n tic-tac-toe board
			makeNByNBoard :: Int -> Board
			makeNByNBoard n = makeNByNBoardH (makeCoords n) (makeTokens n)
			
			makeNByNBoardH [] [] = []
			makeNByNBoardH (x:xs) (y:ys) = 
				(zipWith (\co tk -> Square co tk) x y) : makeNByNBoardH xs ys
			
			-- creates a the tokens for a board
			makeTokens :: Int -> [[Token]]
			makeTokens n = makeTokensH (n, n)
				where
					makeTokensH (0, n) = []
					makeTokensH (c, n) = 
						(take n (repeat Nothing)):(makeTokensH (c-1, n))

			-- creates the coordinates for a board
			makeCoords :: Int -> [[Coords]]
			makeCoords n =
				let
					n1 = n - 1
					ls = [(x,y)| x <- [0..n1], y <- [0..n1]]
				in
					Data.List.groupBy (\a b -> (fst a)==(fst b)) ls

	-- prints out a String representation of a Board
	prtBoard :: Board -> IO ()
	prtBoard bd = putStrLn (boardToStr bd)
		where
			-- converts board to string
			boardToStr :: Board -> String
			boardToStr [] = []
			boardToStr (rw:rows) = "|" ++ rowToStr rw ++ "\n" ++ boardToStr rows
				where
					rowToStr :: Row -> String
					rowToStr [] = []
					rowToStr (sq:sqs) =
						let
							(Square co tk) = sq
						in
							(tokenToStr tk) ++ "|" ++ rowToStr sqs

					tokenToStr :: Token -> String
					tokenToStr tk
						| tk==Nothing = "_"
						| otherwise = (fromJust tk):[]

	-- returns the Token at the spot designated by the i and j coordinates 
	getToken :: Coords -> Board -> Token
	getToken (i, j) bd = token (bd!!i!!j)

	-- returns a new board after changing the token on the square designated by the i and j coordinates
	setToken :: Token -> Coords -> Board -> Board
	setToken tk (i, j) bd =
		let
			newSq = Square (i, j) tk
			newRow = updateAt (bd!!i) j newSq
			newBoard = updateAt bd i newRow
		in
			newBoard
		where
			insertAt :: [a] -> Int -> a -> [a]
			insertAt ls i x =
				let
					(ys,zs) = splitAt i ls
				in
					ys ++ [x] ++ zs
			
			removeAt :: [a] -> Int -> [a] 
			removeAt ls i =
				let
					(left, right) = splitAt i ls
					(h:t) = right
				in
					left ++ t

			updateAt :: [a] -> Int -> a -> [a]
			updateAt ls i x =
				insertAt (removeAt ls i) i x

	-- get all rows to check for win
	getAllRows :: Board -> [Row]
	getAllRows bd = 
		(getHorizontalRows bd ++ getVerticalRows bd ++ getDiagonalRows bd)
		where
			-- get rows of Board
			getHorizontalRows :: Board -> [Row]
			getHorizontalRows bd = bd

			-- get columns of Board 
			getVerticalRows :: Board -> [Row]
			getVerticalRows bd = Data.List.transpose bd
			
			-- get diagonals of Board
			getDiagonalRows :: Board -> [Row]
			getDiagonalRows bd = (top_bot_diag bd) : (bot_top_diag bd) : []
				where
					gen_diag (bd, j1, j2) = gen_diagH (bd, j1, j2, 0)
					gen_diagH (bd, j1, j2, i1)
						| j1 < j2 = ((bd!!i1)!!j1):gen_diagH (bd, j1+1, j2, i1+1)
						| j1 > j2 = ((bd!!i1)!!j1):gen_diagH (bd, j1-1, j2, i1+1)
						| j1 == j2 = ((bd!!i1)!!j1):[]
					top_bot_diag bd = gen_diag (bd, 0, (length bd) - 1)
					bot_top_diag bd = gen_diag (bd, (length bd) - 1, 0)

	-- checks if the tic-tac-toe game is done
	isDone :: Board -> Bool
	isDone bd = isWon bd || all isFullRow bd
		where
			-- determines if Row is full
			isFullRow :: Row -> Bool
			isFullRow ls = all (\sq -> isJust (token sq)) ls

	-- checks the tic-tac-toe win condition
	isWon :: Board -> Bool
	isWon bd = any isWinningRow (getAllRows bd)
		where
			-- ROW OPERATIONS
			-- determines if a Row is a winning Row
			isWinningRow :: Row -> Bool
			isWinningRow xs = (not (isEmptyRow xs)) && allSameTokens xs
			-- determines if all the values of a row are the same
			allSameTokens :: Row -> Bool
			allSameTokens [] = True
			allSameTokens ((Square _ tk1):sqs) = 
				all (\(Square _ tk2) -> tk1==tk2) sqs
			-- determines if Row is empty (no Tokens placed)
			isEmptyRow :: Row -> Bool
			isEmptyRow ls = all (\sq -> isNothing (token sq)) ls

	-- Hard tic-tac-toe AI, the AI player, it's opponent, the board's initial state and returns the board's final state after the AI's move
	--TODO: add Fork, Block Fork and opposite corner functionality to AI
	--http://en.wikipedia.org/wiki/Tic-tac-toe
	hardAIBrain :: Player -> Player -> Board -> Board
	hardAIBrain ai pl bd = 
		if isDone bd then
			bd
		else
			let 
				aiTk = (getPlayerToken ai)
				plTk = (getPlayerToken pl)
				co = hardAIBrainH aiTk plTk bd
			in
				if isNothing co then
					error "AI should always return a move..."
				else
					setToken (getPlayerToken ai) (fromJust co) bd 
		where
			hardAIBrainH :: Token -> Token -> Board -> Maybe Coords
			hardAIBrainH aiTk plTk bd =
				let
					rows = (getAllRows bd)
				in
					(getCoordsFor (oneMoreToWinSq aiTk) rows)--Win
					<|> (getCoordsFor (oneMoreToWinSq plTk) rows)   --Block
															    --Fork
															    --Block Fork
					<|> pickOneCoords (emptySq [getCenter bd])--Center
															  --Opposite Corner
					<|>	pickOneCoords (emptySq (getCorners bd))--Empty Corner
				    <|> pickOneCoords (emptySq (getSides bd))  --Empty Side

	--For AI
	-- TODO: Add random choices if many Just/Valid Coords
	pickOneCoords :: [Maybe Coords] -> Maybe Coords
	pickOneCoords [] = Nothing
	pickOneCoords crds = head crds

	getCoordsFor :: (Row -> Maybe Coords) -> [Row] -> Maybe Coords
	getCoordsFor f rows =
		let
			choices = filter (\x -> isJust x) (map f rows)
		in
			pickOneCoords choices

	getManyCoordsFor :: (Row -> [Maybe Coords]) -> [Row] -> Maybe Coords
	getManyCoordsFor f rows =
		let
			choices = filter (\x -> isJust x) (concat (map f rows))
		in
			pickOneCoords choices

	-- get coords of all empty squares
	emptySq :: Row -> [Maybe Coords]
	emptySq rw = [Just co | (Square co tk) <- rw, isNothing tk]


	oneMoreToWinSq :: Token -> Row -> Maybe Coords
	oneMoreToWinSq tk sqs =
		let
			numberOfTakenSquares = foldl (\acc (Square _ tk1)-> if tk==tk1 then 1+acc else acc ) 0 sqs
			allTakenButOne = numberOfTakenSquares == (length sqs) - 1 
			numberOfEmptySquares = foldl (\acc (Square _ tk1)-> if tk1==Nothing then 1+acc else acc ) 0 sqs
			onlyOneEmptySquareLeft = numberOfEmptySquares == 1

		in
			if allTakenButOne && onlyOneEmptySquareLeft then
				let
					maybeSq = Data.List.find (\(Square _ tk1)->tk1==Nothing) sqs
					(Square co _) = fromJust maybeSq
				in
					Just co
			else
				Nothing

	getSides :: Board -> Row
	getSides bd =
		let
			ln = length bd - 1
			i =  length bd `div` 2
			left = Square (i,0) (getToken (i,0) bd)
			top = Square (0,i) (getToken (0,i) bd)
			right = Square (i,ln) (getToken (i,ln) bd)
			bot = Square (ln,i) (getToken (ln,i) bd)
		in
			[left,top,right,bot]
	getCorners :: Board -> Row
	getCorners bd = [bd!!i!!j | i <- [0, length bd-1], j <- [0, length bd-1]]
	getCenter :: Board -> Square
	getCenter bd = let n2 = (length bd) `div` 2 in bd!!n2!!n2

