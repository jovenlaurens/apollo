module Main exposing (main)

import Browser
import Html exposing (..)
import Initialization exposing (..)
import Messages exposing (..)
import Subscription exposing (..)
import Update exposing (..)
import View exposing (..)



--Main


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
