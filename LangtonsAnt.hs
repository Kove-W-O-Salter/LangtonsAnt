-- Copyright (c) Kove W. Ochre-Salter, <kove.w.o.salter@gmail.com>, 2019.
-- An implimentation of Langton's Ant (<https://en.wikipedia.org/wiki/Langton%27s_ant>).

module LangtonsAnt
  (langtonsAnt
  , Board
  , Square (..)
  , Dir (..)
  , Pos
  ) where

data Square = Black | White
  deriving (Show, Read, Eq)

data Dir = North | East | South | West
  deriving (Show, Read, Eq, Enum)

type Board = [[Square]]

type Pos = (Int, Int)

inList :: Int -> [a] -> Bool
inList n xs = (n < length xs) && (n >= 0)

inBoard :: Pos -> Board -> Maybe a -> Maybe a
inBoard (r, c) b t =
  if r `inList` b
     then if c `inList` (b !! r)
             then t
             else Nothing
     else Nothing

-- Replace the the `i'th index of a list.
replace :: [a] -> Int -> a -> [a]
replace = replaceHelper 0
  where
    replaceHelper _ []     _ _ = []
    replaceHelper n (x:xs) i y = (if n == i then y else x)
                               : replaceHelper (succ n) xs i y

-- Add wrapping functionality to the `Enum' instance.
right, left :: Dir -> Dir

right West = North
right d    = succ d

left North = West
left d     = pred d

getDir :: Square -> (Dir -> Dir)
getDir Black = left
getDir White = right

move :: Pos -> Dir -> Pos
move (r, c) North = (succ r, c)
move (r, c) East  = (r, succ c)
move (r, c) South = (pred r, c)
move (r, c) West  = (r, pred c)

newSquare :: Square -> Square
newSquare Black = White
newSquare White = Black

langtonsAnt :: Board -> Pos -> Int -> Dir -> Maybe Board
langtonsAnt b _ 0 _ = return b
langtonsAnt b p n d = p `inBoard` b $ do
  let (r, c) = p
  let square = b !! r !! c
  let b'     = replace b r (replace (b !! r) c (newSquare square))
  let d'     = getDir square d
  let p'     = p `move` d'
  langtonsAnt b' p' (pred n) d'
