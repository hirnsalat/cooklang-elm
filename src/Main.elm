module Main exposing (main)

import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Parser exposing (..)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { content : String
  }


init : Model
init =
  { content = "" }



-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ Html.textarea [ placeholder "recipe goes here", cols 80, rows 10, value model.content, onInput Change ] []
    , cooklang2html model.content
    ]


cooklang2html : String -> Html Msg
cooklang2html text =
    div [] [ text |> cooklang2steps |> steps2html ]

cooklang2steps text =
    text
    |> String.lines
    |> List.filter (String.isEmpty >> not)
    |> List.map (run parseStep)
    |> List.map (Result.withDefault [])

steps2html steps =
    steps
    |> List.map (step2li)
    |> Html.ol []

step2li step =
    step
    |> List.map (steppart2html)
    |> Html.li []

steppart2html : StepPart -> Html Msg
steppart2html steppart =
    case steppart of
        TextPart text -> Html.text text
        IngredientPart t q -> ingredient2html t q
        CookwarePart t q -> cookware2html t q
        TimerPart t q-> timer2html t q

ingredient2html : String -> Quantity -> Html Msg
ingredient2html text quant =
    Html.span
      [style "background-color" "Pink"]
      [Html.text text]

cookware2html : String -> Quantity -> Html Msg
cookware2html text quant =
    Html.span
      [style "background-color" "LightBlue"]
      [Html.text text]

timer2html : String -> Quantity -> Html Msg
timer2html text quant =
    Html.span
      [style "background-color" "LightGreen"]
      [Html.text <| quantity2text quant]

quantity2text : Quantity -> String
quantity2text (Quantity amount unit) = amount ++ " " ++ unit


-- PARSER


type Quantity = Quantity String String

type StepPart = TextPart String | IngredientPart String Quantity | CookwarePart String Quantity | TimerPart String Quantity

type alias RecipeStep = List(StepPart)

starterGlyph = "@#~"
starterGlyphOrOpenBracket = "{" ++ starterGlyph

getWhileNot string = getChompedString (chompUntil string)

getUntilNextPart = 
    getWhileNotOneOf "@#~"

getWhileNotOneOf chars =
    chompWhile (\c -> not <| String.contains (String.fromChar c) chars)
    |> getChompedString

parseStep : Parser (List StepPart)
parseStep = Parser.loop [] parseStepHelp

parseStepHelp : List StepPart -> Parser (Step (List StepPart) (List StepPart))
parseStepHelp revSteps =
    oneOf
      [ succeed (Done (List.reverse revSteps))
        |. end
      , succeed (\part -> Loop (part :: revSteps))
        |= parseToken
      ]

parseToken : Parser StepPart
parseToken =
    oneOf
        [ parseIngredient
        , parseCookware
        , parseTimer
        , parseText
        ]

parseText =
    succeed TextPart
    |= getUntilNextPart

parseIngredient =
    parseThing "@" IngredientPart

parseCookware =
    parseThing "#" CookwarePart

parseTimer =
    parseThing "~" TimerPart


-- TODO this is potentially a bit of a performance problem
parseThing : String -> (String -> Quantity -> StepPart) -> Parser StepPart
parseThing glyph constructor =
    oneOf
    [ backtrackable <| parseComplexThing glyph constructor
    , parseSimpleThing glyph constructor
    ]

parseSimpleThing : String -> (String -> Quantity -> StepPart) -> Parser StepPart
parseSimpleThing glyph constructor =
    succeed (\name -> constructor name (Quantity "" ""))
    |. symbol glyph
    |= (getChompedString <| chompWhile Char.isAlphaNum)

parseComplexThing : String -> (String -> Quantity -> StepPart) -> Parser StepPart
parseComplexThing glyph constructor =
    succeed constructor
    |. symbol glyph
    |= getWhileNotOneOf starterGlyphOrOpenBracket
    |= parseQuantity

parseQuantity : Parser Quantity
parseQuantity =
    succeed Quantity
    |. symbol "{"
    |= getWhileNotOneOf "%}"
    |= oneOf
        [ succeed identity
          |. symbol "%"
          |= getWhileNot "}"
        , succeed ""
        ]
    |. symbol "}"

parseUnit =
    oneOf
      [ succeed identity
        |. symbol "%"
        |= getWhileNot "}"
      , succeed ""
      ]

