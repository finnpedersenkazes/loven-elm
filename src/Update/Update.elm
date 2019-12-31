module Update.Update exposing (..)

import Debug exposing (log)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, fail, field, float, int, list, map, map2, map5, string, succeed)
import Json.Decode.Pipeline as Pipeline exposing (custom, hardcoded, optional, required)
import Json.Encode as Encode exposing (Value, encode, object)
import Model.Model exposing (..)
import Update.Secrets exposing (appId)


apiLawElements : String
apiLawElements =
    "https://loven-api.herokuapp.com/laws/1/elements"


getElements : Cmd Msg
getElements =
    let
        urlElement =
            apiLawElements
    in
    Http.get
        { url = urlElement
        , expect = Http.expectJson AfterGetElements elementsDecoder
        }


elementsDecoder : Decoder Elements
elementsDecoder =
    Decode.list elementDecoder


elementDecoder : Decoder Element
elementDecoder =
    Decode.succeed Element
        |> required "id" int
        |> required "number" int
        |> required "context" string
        |> optional "chapter_id" string ""
        |> optional "chapter_text" string ""
        |> optional "paragraph_id" string ""
        |> optional "paragraph_text" string ""
        |> optional "content" string ""
        |> optional "law_id" int 0
        |> required "created_at" string
        |> required "updated_at" string


elementJson : Element -> Value
elementJson element =
    object
        [ ( "element"
          , object
                [ ( "id", Encode.int element.id )
                , ( "number", Encode.int element.number )
                , ( "context", Encode.string element.context )
                , ( "chapter_id", Encode.string element.chapterId )
                , ( "chapter_text", Encode.string element.chapterText )
                , ( "paragraph_id", Encode.string element.paragraphId )
                , ( "paragraph_text", Encode.string element.paragraphText )
                , ( "content", Encode.string element.content )
                , ( "law_id", Encode.int element.lawId )
                ]
          )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GoToHomePage ->
            ( HomePage, Cmd.none )

        GetElements ->
            ( LoadingElements, getElements )

        AfterGetElements (Ok elements) ->
            ( DisplayingElements "" elements, Cmd.none )

        AfterGetElements (Err _) ->
            ( Failure, Cmd.none )

        FilterOnTextElements elements filterText ->
            ( DisplayingElements filterText elements, Cmd.none )
