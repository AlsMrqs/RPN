module Converter where

import Data.Maybe       (fromJust)
import Stack
import Tree             

type Operator = [Char]

convert :: [Char] -> [Char]
convert x = let 
    elements = words x
    in mount . fromJust . fst . pop $ manage elements Stack Stack

manage :: [String] -> Stack Operator -> Stack (Tree [Char]) -> Stack (Tree [Char])
manage []     opStk trStk = snd $ evalAll opStk trStk
manage (x:xs) opStk trStk = 
    if not $ isOperator x then manage xs opStk $ push (Leaf x) trStk
    else
        if peek opStk == Nothing  then 
            manage xs (push x opStk) trStk
        else if (not . precedence . fromJust $ peek opStk) && precedence x then
            manage xs (push x opStk) trStk
        else if (precedence . fromJust $ peek opStk) && precedence x then 
            (\(opStk1, trStk1) -> manage xs (push x opStk1) trStk1) $ evalOne opStk trStk
        else 
            let (opStk1, trStk1) = evalAll opStk trStk
            in  manage xs (push x opStk1) trStk1

isOperator :: [Char] -> Bool
isOperator = (`elem` ["+","-","*","/"])

precedence :: Operator -> Bool
precedence = (`elem` ["*","/"])

evalOne :: Stack Operator -> Stack (Tree [Char]) -> (Stack Operator, Stack (Tree [Char]))
evalOne opStk trStk = let
    (justOp, opStk1) = pop opStk
    operator           = fromJust justOp
    (justTr1, trStk1) = pop trStk
    (justTr2, trStk2) = pop trStk1
    operand1            = fromJust justTr1
    operand2            = fromJust justTr2
    expression          = Node operator operand2 operand1
    in (opStk1, push expression trStk2)

evalAll opStk trStk = if peek opStk == Nothing then (opStk, trStk)
    else evalAll opStk1 trStk1
        where
        (opStk1, trStk1) = evalOne opStk trStk

