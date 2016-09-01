module BlackJack where
import Cards
import Wrapper
import System.Random

-- TASK 3.2
{--
size :: Num a => Hand -> a
size Empty 				= 0
size (Add card hand) 	= 1 + size hand

hand2 = Add (Card (Numeric 2) Hearts)
		(Add (Card Jack Spades) Empty)

size hand2
= size (Add (Card (Numeric 2) Hearts)
		(Add (Card Jack Spades) Empty))
= 1 + size (Add (Card Jack Spades) Empty)
= 1 + 1 + size Empty
= 1 + 1 + 0
= 2
--}

-- TASK 3.3
-- This function accepts Hand type and gives back empty hand
empty :: Hand
empty = Empty

-- This function accepts rank of a card and gives back the corresponding value 
valueRank :: Rank -> Integer
valueRank (Numeric n) 			= n
valueRank rank 	| rank == Ace	= 11
				| otherwise 	= 10

-- This function accepts a card and returns face value
valueCard :: Card -> Integer
valueCard (Card r s) = valueRank r

-- This function accepts a hand of cards and counts the number of Aces in it
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card r s) hand) 	| r == Ace 	= 1 + numberOfAces hand
                                    | otherwise = numberOfAces hand

-- This function accepts a hand of cards and gives back the total value of the cards
value :: Hand -> Integer
value hand 	| handvalue >= 22 	= handvalue - (numberOfAces hand) * 10
			| otherwise 		= handvalue
			where handvalue = faceValue hand 			

-- This function is called by value to know the total value of a hand considering Ace as having a value 11
faceValue :: Hand -> Integer
faceValue Empty = 0
faceValue (Add c h) = valueCard c + faceValue h

-- This function receives a hand and checks whether the hand is busted
gameOver :: Hand -> Bool
gameOver hand = (value hand) >= 22

-- This function receives two hands and determines the winner
-- we assume that the first argument is Guest and the second is Bank
winner :: Hand -> Hand -> Player
winner g b 	| gameOver g  			= Bank
			| gameOver b 			= Guest
			| value g > value b		= Guest
			| otherwise 			= Bank

-- =============================================
-- 						PART B
-- =============================================
-- This function receives two hands and put the first one on top of the second one
(<+) :: Hand -> Hand -> Hand
(<+) h1 Empty = h1
(<+) Empty h2 = h2
(<+) (Add card h1) h2 = Add card ((<+) h1 h2)

-- Tests for associativity of <+ function
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

-- Test whether the size of individual hands is the same as the combined hand
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = (size h1) + (size h2) == size ((<+) h1 h2)

-- This function returns a full deck of cards
fullDeck :: Hand
fullDeck = foldr (<+) Empty [(baseCards Hearts) , (baseCards Spades) , (baseCards Clubs) , (baseCards Diamonds)]

-- Given a suit of a card, this function returns the 13 variant of it as a hand
baseCards :: Suit -> Hand
baseCards suit = foldr Add Empty (addCards suit 10)
				where
				addCards suit 2 = [Card (Ace) suit,Card (Jack) suit,Card (Queen) suit,Card (King) suit, Card (Numeric 2) suit]			
				addCards suit n = addCards suit (n-1) ++ [Card (Numeric n) suit]
				
-- This function takes deck of a card and put the top card in the top of a hand and returns both the deck and hand
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty hand = error "power: negative argument"
draw (Add card deck) hand =  (deck,(Add card hand))

-- This function takes a deck and returns a Hand 
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

-- This function takes a deck and hand to give and takes the top card from the deck to push it to the hand
playBank' :: Hand -> Hand -> Hand
playBank' deck' bankHand'	| (value bankHand') > 21 	= bankHand'
							| otherwise  				= playBank' deck'' bankHand''
							where 	
							(deck'', bankHand'') = (draw deck' bankHand')													

-- This function takes a deck and shuffles using random value given
shuffle :: StdGen -> Hand -> Hand
shuffle _ Empty = Empty
shuffle grt deck = shuffle' grt deck Empty
	
-- This function iteratively select a random card and put it in a hand
shuffle'::StdGen -> Hand -> Hand -> Hand
shuffle' grt Empty shuffled = shuffled
shuffle' grt deck shuffled = shuffle' grt' deck' (Add card shuffled)
							where
							(card, deck') = takeCard n deck 
							(n, grt') = randomR (1, size deck) grt

-- This function picks a random card and gives back the card and the hand without the card
takeCard::Integer -> Hand -> (Card, Hand)
takeCard n hand = takeCard' n hand Empty
				where 
				takeCard' 1 (Add card bottom) upper = (card,bottom<+upper)					
				takeCard' n (Add card bottom) upper = takeCard' (n-1) bottom (Add card upper)

-- This propriety checks if a given card is found in the original as well as in the shuffled deck
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffle g h
	
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h						

-- This propriety checks the size of the original and the shuffled deck
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle grt deck = size deck == size (shuffle grt deck)

implementation = Interface
	{ iEmpty = empty
	, iFullDeck = fullDeck
	, iValue = value
	, iGameOver = gameOver
	, iWinner = winner
	, iDraw = draw
	, iPlayBank = playBank
	, iShuffle = shuffle
	}
	
main::IO()
main = runGame implementation