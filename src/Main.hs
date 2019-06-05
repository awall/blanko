{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank

options :: Options
options = Options {
    port = 3000,
    events = [
          "keypress"
        , "keydown"
        , "keyup"
        , "mouseDown"
        , "mouseenter"
        , "mousemove"
        , "mouseout"
        , "mouseover"
        , "mouseup" ],
    debug = False,
    root = ".",
    middleware = [local_only],
    weak = False
}

main = blankCanvas options $ \ context -> do
  send context $ do
    moveTo(50,50)
    lineTo(200,100)
    lineWidth 10
    strokeStyle "red"
    stroke ()