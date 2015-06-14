{-# LANGUAGE PostfixOperators #-}

module Game.Cards where

import Game.DataTypes
import Game.Actions

draw3 :: Card
draw3 = Card
        "draw3"
        (mempty { _mana = 3 })
        [AbilityAttr (draw 3)]

burn3 :: Card
burn3 = Card
        "burn3"
        (mempty { _mana = 3 })
        [AbilityAttr (damageTarget 3)]

bear :: Card
bear = Card
        "bear"
        (mempty { _gold = 2 })
        [ CombatAttr 2
        , HitsBoard ]

getPaid :: Card
getPaid = Card
        "getpaid"
        mempty
        [AbilityAttr (gain $ mempty { _gold = 3 })]
