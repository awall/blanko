{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text(unpack)

import Graphics.Blank

main = blankCanvas options program

options :: Options
options = Options {
    port = 3000,
    -- this is all the possible events
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

program :: DeviceContext -> IO ()
program context = do
  event <- wait context
  putStrLn (unpack (eType event))
  send context draw
  program context

draw :: Canvas ()
draw = do
  moveTo(50,50)
  lineTo(200,100)
  lineWidth 10
  strokeStyle "red"
  stroke ()