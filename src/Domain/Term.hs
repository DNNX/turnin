{-# LANGUAGE MultiParamTypeClasses #-}
module Domain.Term
( Term()
, getStartDate
, setStartDate
, getEndDate
, setEndDate
) where

import Infrastructure.Node
import Domain.Course

data Term = T Node deriving (Show, Eq)

instance Succ Term Course where
instance HasNode Term where toNode (T n) = wrap n; fromNode = T
 
getStartDate :: Term -> String
getStartDate (T node) = getCache node startDate

setStartDate :: Term -> String -> Term
setStartDate (T node) = T . setCache node startDate

getEndDate :: Term -> String
getEndDate (T node) = getCache node endDate

setEndDate :: Term -> String -> Term
setEndDate (T node) = T . setCache node endDate

startDate = "START_DATE"
endDate = "END_DATE"
