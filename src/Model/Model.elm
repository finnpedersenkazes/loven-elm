module Model.Model exposing (Element, ElementField(..), Elements, Id, Model(..), Msg(..), init)

import Http


type alias Id =
    Int


type Msg
    = NoOp
    | GoToHomePage
    | GetElements
    | AfterGetElements (Result Http.Error Elements)
    | FilterOnTextElements Elements String


type Model
    = Failure
    | NotImplementedYet
    | HomePage
    | LoadingElements
    | DisplayingElements String Elements


type alias Elements =
    List Element


type alias Element =
    { id : Id
    , number : Int
    , context : String
    , chapterId : String
    , chapterText : String
    , paragraphId : String
    , paragraphText : String
    , content : String
    , lawId : Int
    , createdAt : String
    , updatedAt : String
    }


type alias Law =
    { id : Id
    , key : String
    , date : String
    , name : String
    , title : String
    , createdAt : String
    , updatedAt : String
    }


type ElementField
    = Content


init : () -> ( Model, Cmd Msg )
init _ =
    ( HomePage, Cmd.none )
