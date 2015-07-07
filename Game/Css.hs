{-# LANGUAGE OverloadedStrings #-}

module Game.Css where

import Clay

gameCss :: Css
gameCss = do
  body ? background grey
  ".flex" ? do
    "display" -: "-webkit-flex"
    "-webkit-flex-direction" -: "row"
  ".flex" |> "div" ? do
    border solid (px 1) black
    height (px 200)
    width (px 150)
    background white
  ".imgfloat" ? do
    height (px 120)
    width (px 120)
    display block
    margin auto auto auto auto
    padding (px 2) (px 2) (px 2) (px 2)
