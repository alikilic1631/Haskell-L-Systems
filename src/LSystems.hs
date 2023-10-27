module LSystems ( LSystem(LSystem), ColouredLine, Command(..)
                , angle, axiom, rules, lookupChar
                , expandOne, expand, move, trace1, trace2
                , expandLSystem, commandMap ) where

import IC.Colour

type Rules a = [(Char, [a])]
data LSystem = LSystem Float [Char] (Rules Char)
type Vertex = (Float, Float)
type TurtleState = (Vertex, Float)
data Command = F | L | R | B [Command]
  deriving Show 
type ColouredLine = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (LSystem a _ _ ) = a 

-- Returns the axiom string for the given system.
axiom :: LSystem -> [Char]
axiom (LSystem _ a _ ) = a 

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules Char
rules (LSystem _ _ a ) = a 

--
-- Pre: the character has a binding in the Rules list
--
lookupChar :: Rules a -> Char -> [a]
lookupChar [] _ = []
lookupChar (c:cs) r
    | fst c == r = snd c 
    | otherwise  = lookupChar cs r

--
-- Expand command string s once using rule table r
--
expandOne :: Rules Char -> [Char] -> [Char]
expandOne r [] = "" 
expandOne r (c:cs) = (lookupChar r c)++(expandOne r cs)

--
-- Expand command string s n times using rule table r
--
expand :: [Char] -> Int -> Rules Char -> [Char]
expand ax 0 rul = ax
expand ax count rul = expand (expandOne rul ax) (count-1) rul 

-- Move a turtle.
--
-- F moves distance 1 in the current direction.
-- L rotates left according to the given angle.
-- R rotates right according to the given angle.
move :: Command -> Float -> TurtleState -> TurtleState
move F ang ((x,y),theta) = (((x+cos((theta/180.0)*pi)),(y+sin((theta/180.0)*pi))),theta) 
move L ang ((x,y),theta) = ((x,y),theta+ang)  
move R ang ((x,y),theta) = ((x,y),theta-ang)  


parse :: Rules Command -> [Char] -> [Command]
parse commandMap [] = []
parse commandMap (step:steps) 
    | step == '[' = B (parse commandMap inside) : (parse commandMap outside)
    | otherwise   = (lookupChar commandMap step)++(parse commandMap steps)
    where 
        inside = takeWhile(']' /=) steps 
        outside = dropWhile(']' /=) steps 
     

trace1 :: [Command] -> Float -> Colour -> [ColouredLine]
trace1 [] ang col = []
trace1 step_all ang col = helper step_all ang col ((0.0,0.0), 90)
    where 
      helper :: [Command] -> Float -> Colour -> TurtleState -> [ColouredLine]
      helper [] _ _ _ = []
      
      helper (B branch:steps) ang col prev@(v,f) = helper branch ang col prev ++ helper steps ang col prev

      helper (F:steps) ang col prev@(v,f) = (v,t,col):helper steps ang col new
        where 
          new@(t,d)= move F ang prev

      helper (s:steps) ang col prev@(v,f) = helper steps ang col new
        where 
          new@(t,d)= move s ang prev

-- This version uses an explicit stack of residual commands and turtle states
trace2 :: [Command] -> Float -> Colour -> [ColouredLine]
trace2 com ang col = helper com ((0.0,0.0), 90) [] ang col  
   where 
    helper :: [Command] -> TurtleState -> [([Command],TurtleState)] -> Float -> Colour -> [ColouredLine]
    helper [] _ [] _ _   = []  
    helper [] _ ((com,turt):cs) ang col  = helper com turt cs ang col

    helper (F:cs) prev@(v,f) stack ang col = (v,t,col):helper cs new stack ang col 
      where new@(t,d) = move F ang prev

    helper ((B branch):steps) prev stack ang col = helper branch prev ((steps, prev):stack) ang col  

    helper (s:steps) prev@(v,f) stack ang col  = helper steps new stack ang col 
        where new@(t,d)= move s ang prev
    


 



-- Provided Functions
------------------------------------------------------------------------------

expandLSystem :: LSystem -> Int -> [Command]
expandLSystem (LSystem _ axiom rs) n = parse commandMap (expand axiom n rs)

commandMap :: Rules Command
commandMap = [ ('M', [F])
             , ('N', [F])
             , ('X', [])
             , ('Y', [])
             , ('A', [])
             , ('+', [L])
             , ('-', [R])
             ]

