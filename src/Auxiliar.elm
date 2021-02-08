module Auxiliar exposing (..)
import Html exposing (text) 

listSame : List a -> Bool
listSame l =
  case l of
    [] -> True
    x::y::xs ->
      if x /= y then False
      else listSame (y::xs)
    _ -> True

cpfncnpjTrait : String -> Bool -> List (Maybe Int)
cpfncnpjTrait str iscpf =
  let
    strl = String.toList str
    strlen = String.length str
    
    test : List Char -> List (Maybe Int)
    test lch =
      let
        test1 = List.filter isNotDigit lch
        test2 = List.map charToInt lch
      in
        if test1 /= [] then []
      else
        if listSame test2 then []
        else test2
  in
    if iscpf then
      case strlen of
      11 -> test strl
      14 -> 
        case strl of
          a::b::c::p1::d::e::f::p2::g::h::i::p3::j::k::xs ->
            a::b::c::d::e::f::g::h::i::j::k::xs
            |> test
          _ -> []
      _  -> []
    else
      case strlen of
      14 -> test strl
      18 ->
        case strl of
          a::b::p1::c::d::e::p2::f::g::h::p3::i::j::k::l::p4::m::n::xs ->
            a::b::c::d::e::f::g::h::i::j::k::l::m::n::xs
            |> test
          _ -> []
      _  -> []

isNotDigit : Char -> Bool
isNotDigit ch = not (Char.isDigit ch)

intToChar : Maybe Int -> Char
intToChar =
  \int -> case int of
    Just 0  -> '0'
    Just 1  -> '1'
    Just 2  -> '2'
    Just 3  -> '3'
    Just 4  -> '4'
    Just 5  -> '5'
    Just 6  -> '6'
    Just 7  -> '7'
    Just 8  -> '8'
    Just 9  -> '9'
    _       -> ' '

charToInt : Char -> Maybe Int
charToInt =
  \ch -> case ch of
    '0' -> Just 0
    '1' -> Just 1
    '2' -> Just 2
    '3' -> Just 3
    '4' -> Just 4
    '5' -> Just 5
    '6' -> Just 6
    '7' -> Just 7
    '8' -> Just 8
    '9' -> Just 9
    _   -> Nothing

cnpjc : List (Maybe Int) -> List Int
cnpjc l =
  let
    aux : List (Maybe Int) -> Int -> List Int
    aux ln i =
      case i of
        10 -> aux ln 2
        _  ->
          case ln of
            [] -> []
            (Just x)::xs -> x*i::(aux xs (i+1))
            _ -> []
  in
    aux (List.reverse l) 2

cpfc : List (Maybe Int) -> Int -> List Int
cpfc l i =
  case i of
    1 -> []
    _ -> 
      case l of
        (Just x)::xs ->
          x*i::(cpfc xs (i-1))
        _ -> []

digg : List (Maybe Int) -> (Int,Int)
digg l =
  case (List.reverse l) of
    x::y::xs -> 
      case (y,x) of
        (Just n, Just m) -> (n,m)
        _ -> (0,0)
    _ -> (0,0)

digv : List Int -> Int
digv l =
  let
    calc = modBy 11 <| List.sum l
  in
    if calc < 2 then 0 else (11 - calc)

main = text ""
