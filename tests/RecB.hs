module RecB where

import RecA

data DB = DB (Maybe DA) deriving Show

main = print (DB (Just (DA (DB Nothing))))

