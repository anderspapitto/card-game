{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module Cards where

import DataTypes
import Actions

draw3 :: Card
draw3 = Card
        "draw3"
        (mempty { _mana = 3 })
        False
        Nothing
        [(draw 3)]
        []
        "book"

burn3 :: Card
burn3 = Card
        "burn3"
        (mempty { _mana = 3 })
        False
        Nothing
        [(damageTarget 3)]
        []
        "lightning"

bear :: Card
bear = Card
        "bear"
        (mempty { _gold = 2 })
        True
        (Just (2, 2))
        []
        [damageTarget 2]
        "bear"

getPaid :: Card
getPaid = Card
        "getpaid"
        mempty
        False
        Nothing
        [(gain $ mempty { _gold = 3 })]
        []
        "handful-coins"

base :: Card
base = Card
       "base"
       (mempty { _gold = 5 })
       True
       (Just (6, 6))
       []
       [ (gain $ mempty { _gold = 2 }) ]
       "fortress"
