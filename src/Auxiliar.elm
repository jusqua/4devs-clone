module Auxiliar exposing (..)

-- Este arquivo tem apenas a função de auxilar outros arquivos

{- Auxiliares -}
listSame : List a -> Bool
listSame l =
  case l of
    [] -> True
    x::y::xs ->
      if x /= y then False
      else listSame (y::xs)
    _ -> True

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

isVowel : Char -> Bool
isVowel ch =
  let
    chc = Char.toCode ch
    vowels = [65,69,73,79,85,97,101,105,111,117,129,192,193,194,195,196,197,198,200,201,202,203,204,205,206,207,210,211,212,213,214,216,217,218,219,220,224,225,226,227,228,229,230,232,233,234,235,236,237,238,239,242,243,244,245,246,248,249,250,251,252]
  in
    case List.filter (\c -> c == chc) vowels of
    [x] -> True
    _   -> False

isConsonant : Char -> Bool
isConsonant ch =
  let
    chc = Char.toCode ch
    sconsonants = [138,142,154,158,159,199,209,221,231,241,253,255]
  in
  if Char.isAlpha ch && not (isVowel ch) then True
  else
    case List.filter (\c -> c== chc) sconsonants of
    [x] -> True
    _   -> False
  
isSpecialChar : Char -> Bool
isSpecialChar ch =
  if (ch == ' ' || ch == '\n' || isVowel ch || isConsonant ch) then False
  else True

{- Contador de String -}
getWords : List Char -> Int
getWords lch =
  case lch of
    [] -> 0
    x::y::xs -> 
      if (x /= ' ' && x /= '\n') && (y == ' ' || y == '\n') then 1 + getWords (List.append [y] xs)
      else 0 + getWords (List.append [y] xs)
    [x] -> if x == ' ' || x == '\n' then 0 else 1

{- Calculos -}
basicTrait = \n -> n >= 0
basicTrait2 = \n m -> n >= 0 && m >= 0
basicTrait3 = \n m p -> n > 0 && m > 0 && p > 0

calc1 : (Float -> Float) -> (Float -> Bool) -> String -> String
calc1 calc trait p =
  case String.toFloat p of
    Just n ->
      if trait n then String.fromFloat(calc n)
      else "Indeterminado"
    Nothing -> "Indeterminado"

calc2 : (Float -> Float -> Float) -> (Float -> Float -> Bool) -> String -> String -> String
calc2 calc trait p1 p2 =
  case (String.toFloat p1, String.toFloat p2) of
    (Just n, Just m) ->
      if trait n m then String.fromFloat(calc n m)
      else "Indeterminado"
    _ -> "Indeterminado"

calc3 : (Float -> Float -> Float -> Float) -> (Float -> Float -> Float -> Bool) -> String -> String -> String -> String
calc3 calc trait p1 p2 p3 =
  case (String.toFloat p1, String.toFloat p2, String.toFloat p3) of
    (Just n, Just m, Just p) ->
      if trait n m p then String.fromFloat(calc n m p)
      else "Indeterminado"
    _ -> "Indeterminado"

{- CPF/CNPJ-}
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
            (Just x)::xs ->
              x*i::(aux xs (i+1))
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
