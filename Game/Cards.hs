{-# LANGUAGE PostfixOperators #-}

module Game.Cards where

import Game.DataTypes
import Game.Actions

draw3 :: Card
draw3 = Card
        "draw3"
        (3 `mana`)
        [AbilityAttr (draw 3)]

burn3 :: Card
burn3 = Card
        "burn3"
        (3 `mana`)
        [AbilityAttr (damageTarget 3)]

bear :: Card
bear = Card
        "bear"
        (2 `gold`)
        [ CombatAttr 2
        , HitsBoard ]

getPaid :: Card
getPaid = Card
        "getpaid"
        (0 `gold`)
        [AbilityAttr (gain (3 `gold`))]
