import Html exposing (..)
import Html.Attributes exposing (href,style)
import Html.Events exposing (onClick)
import Http
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import Json.Decode as Decode
import Task exposing (Task)
import Time exposing (Time)
-- ***MDL-1
import Material
import Material.Scheme
import Material.Menu as Menu
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Card as Card 
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Layout as Layout
import Material.List as Lists

main =
  Navigation.program MsgUrlChange
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =  -- ***MDL-2: Boilerplate: Always use this initial Mdl model store.
  ( Model Nothing (Url.parsePath route location) [] Material.model
  , Cmd.none
  )


-- MODEL
type alias User = {
     id: Int
    ,name : String  
    ,status : String  
    }
type alias Model =
  { loading : Maybe Route
  , page : Maybe Route
  , users : List User
  , mdl : Material.Model -- ***MDL-3 : Boilerplate: model store for any and all Mdl components you use.
  }



-- URL PARSING
type Route
  = RouteHome
  | RouteUsers
  | RouteUserDetail Int
  | RouteMain



route : Url.Parser (Route -> a) a
route =
  Url.oneOf
    [ Url.map RouteHome top
    , Url.map RouteUsers (Url.s "users")
    , Url.map RouteUserDetail (Url.s "users" </> int)
    , Url.map RouteMain (Url.s "Main.elm")
    ]



-- UPDATE
type Msg
  = MsgNewUrl String
  | MsgUrlChange Navigation.Location
  | MsgNewUsers (Result Http.Error (List User))
  | MsgNewUserDetail (Result Http.Error User)
  | Mdl (Material.Msg Msg) -- ***MDL-4 : Boilerplate: Msg clause for internal Mdl messages.

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    MsgNewUrl url ->
      ( model
       ,Navigation.newUrl url
      )
    MsgUrlChange location ->
      let
        _ = Debug.log "location=" location
        newpage = Url.parsePath route location
      in
        case newpage of
            Nothing ->
                ( {model | page=newpage}, Cmd.none)
            Just RouteHome ->
                ( {model | page=newpage}, Cmd.none)
            Just RouteUsers ->
                ( { model | loading = newpage }, getUsers )
            Just (RouteUserDetail n) ->
                ( { model | loading = newpage }, getUserDetail n )
            Just RouteMain ->
                ( { model | loading = newpage }, getUsers )

    MsgNewUsers (Ok newusers) ->
      ( { model | page=model.loading, users=newusers } , Cmd.none)

    MsgNewUsers (Err _) ->
      (model, Cmd.none)

    MsgNewUserDetail (Ok newuser) ->
      ( { model | page=model.loading, users=[newuser] } , Cmd.none)

    MsgNewUserDetail (Err _) ->
      (model, Cmd.none)

    -- ***MDL-5 : Boilerplate: Mdl action handler.
    Mdl msg_ ->
        Material.update Mdl msg_ model


-- HTTP
url_users =
    "http://localhost:3090/users"



requestUsers : Http.Request (List User)
requestUsers =
    Http.get url_users ( Decode.list user )


user : Decode.Decoder User
user =
    Decode.map3 toUser (Decode.field "id" Decode.int) (Decode.field "name" Decode.string) (Decode.field "status" Decode.string)

toUser : Int -> String -> String-> User  
toUser t s m =
    { id=t, name=s, status = m}



getUsers : Cmd Msg
getUsers =
    Http.send MsgNewUsers requestUsers


getUserDetail : Int -> Cmd Msg
getUserDetail n =
    let
        url_post = url_users ++ "/" ++ toString n
    in
    Http.send MsgNewUserDetail ( Http.get url_post user )



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


type alias Mdl = -- ***MDL-6
    Material.Model


-- VIEW
view : Model -> Html Msg
view model =
  Layout.render Mdl model.mdl
    [ Layout.fixedHeader
    ]
    { header = [Layout.title[] [text "Life Support Manager"]]
    , drawer = []
    , tabs = ([viewLink model "/users/"])
    , main = [ viewRoute model ]
    }
  -- div []
  --   [ h1 [] [ text "Life Support User Manager" ]
  --   , ul [] (List.map (viewLink model) [ "/","/users/","/users/1/", "/users/2/", "/users/3/" ])
  --   , div [] [ viewRoute model ]
  --   ]
  --   |> Material.Scheme.top -- ***MDL-7

viewLink : Model -> String -> Html Msg   -- ***MDL-8
viewLink model url =
  -- li [] [ button [ onClick (MsgNewUrl url) ] [ text url ] ]
  li [ style [ ("display", "inline-block") ] ] [ 
          Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.raised
            , Options.onClick  (MsgNewUrl url)
            , css "margin" "0 6px"
            ]
            [ text  url ]
        ]


viewRoute : Model -> Html Msg
viewRoute model =
  let
    _ = Debug.log "maybeRoute=" model.page
  in
  case model.page of
    Nothing ->
      h2 [] [ text "404 Page Not Found!"]

    Just route ->
      viewPage route model


viewPage : Route -> Model -> Html Msg
viewPage route model =
  case route of
    RouteHome ->
      div []
        [ h2 [] [text "Welcomw to My Page!"]
        , p [] [ text "これはテストページのトップです" ]
        ]
    RouteUsers ->
      div []
        [ h2 [] [text "ユーザー一覧"]
        , ul [] (List.map viewUsers model.users)
        ]

    RouteUserDetail id ->
      div []
        [ h2 [] [text "ブログ記事表示"]
        , ul [] (List.map viewUsers model.users)
        ]

    RouteMain ->
      div []
        [ h2 [] [text "ユーザー一覧"]
        , ul [] (List.map viewUsers model.users)
        ]


white : Options.Property c m 
white = 
  Color.text Color.white 

viewUsers a =
  let 
    url = "/users/"++ toString a.id
  in
  case a.status of 
  "red" -> 

  div [ style [("padding","10px"), ("margin","10px")] ]
    [
      Options.div
        [ Elevation.e6
        , css "width"  "800px" 
        , Options.center
        ]
        [
          Card.view
            [ css "width" "800px"
            , Color.background (Color.color Color.Red Color.S500)
            ,Options.onClick (MsgNewUrl url)
            ]
            [ Card.title [] [ Card.head [ white ] [ text (toString a.id ) ] ]
            , Card.text [ white ] [ text <| a.name ]
            ]
        ]
    ]
  "yellow" ->

    div [ style [("padding","10px"), ("margin","10px")] ]
    [
      Options.div
        [ Elevation.e6
        , css "width"  "800px" 
        , Options.center
        ]
        [
          Card.view
            [ css "width" "800px"
            , Color.background (Color.color Color.Yellow Color.S500)
            ,Options.onClick (MsgNewUrl url)
            ]
            [ Card.title [] [ Card.head [ white ] [ text (toString a.id ) ] ]
            , Card.text [ white ] [ text <| a.name ]
            ]
        ]
    ]

  _ -> 
    div [ style [("padding","10px"), ("margin","10px")] ]
    [
      Options.div
        [ Elevation.e6
        , css "width"  "800px" 
        , Options.center
        ]
        [
          Card.view
            [ css "width" "800px"
            , Color.background (Color.color Color.Green Color.S500)
            ,Options.onClick (MsgNewUrl url)
            ]
            [ Card.title [] [ Card.head [ white ] [ text (toString a.id ) ] ]
            , Card.text [ white ] [ text <| a.name ]
            ]
        ]
    ]