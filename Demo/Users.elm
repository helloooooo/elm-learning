module Demo.Users exposing (..)
import Html exposing (..)
import Html.Attributes exposing (href,style)
import Html.Events exposing (onClick)
import Http
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import Json.Decode as Decode
import Task exposing (Task)
import Array exposing (Array)
import Dict exposing (Dict)
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
import Material.Tabs as Tabs
import Material.Icon as Icon

import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Plot exposing (..)


-- MODEL

type alias Data = {
   x : Float,
   y :Float
}
type alias ApiData = {
  id :Int
  ,human : Float
  ,temparture :Float
  ,userId : Int
}
type alias User = {
     id : Int
    ,name : String  
    ,status : String  
    }
type alias Model =
  { loading : Maybe Route
  , page : Maybe Route
  , users : List User
  , humandatas : List Data
  , tempdatas : List Data
  , graphtoggle : Bool
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
  | MsgNewApiData (Result Http.Error (List ApiData))
  | MsgUnti (Maybe User)
  | MsgTemp
  | Msghuman
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
      ( { model | page=model.loading, users=[newuser],humandatas = [] ,tempdatas = [Data 1 30,Data 2 30,Data 3 30,Data 4 30,Data 5 30]} , Cmd.none)

    MsgNewUserDetail (Err _) ->
      (model, Cmd.none)
      
    MsgNewApiData (Ok newdata) ->
      ( { model | page=model.loading, humandatas = ( createHumanData (List.head newdata) model.humandatas ),tempdatas =(createTempData (List.head newdata)  model.tempdatas)} , Cmd.none)

    MsgUnti user ->
      ( { model | loading = model.loading }, getApiData (createGetApiCommand user))
    MsgTemp ->
      ({model | graphtoggle = True },Cmd.none)
    Msghuman ->
      ({model | graphtoggle = False },Cmd.none)
    -- MsgUnti (Err _ ) ->
    --   (model, Cmd.none)    

    MsgNewApiData (Err _) ->
      (model, Cmd.none)    


    -- ***MDL-5 : Boilerplate: Mdl action handler.
    Mdl msg_ ->
        Material.update Mdl msg_ model


-- HTTP
url_users =
    "http://localhost:3090/users"

url_datas = 
  "http://localhost:3090/datas?userId="

createTempData: Maybe ApiData -> List Data ->List Data  
createTempData newdata datas =
  case newdata of 
    Just data ->
      if List.length datas < 5 then
        [ Data ((List.reverse datas |> List.head |> unwrapDatax) + 1.0) data.temparture ]
        |> List.append datas 
      else 
        [ Data ((List.reverse datas |> List.head |> unwrapDatax) + 1.0) data.temparture ]
        |> List.append datas 
        |> List.drop 1
    Nothing ->
      [Data 1 1]

createHumanData: Maybe ApiData -> List Data ->List Data  
createHumanData newdata datas =
  case newdata of 
    Just data ->
      if List.length datas < 5 then
        [ Data ((List.reverse datas |> List.head |> unwrapDatax) + 1.0 ) data.human ]
        |> List.append datas 
      else 
        [ Data ((List.reverse datas |> List.head |> unwrapDatax) + 1.0 ) data.human ]
        |> List.append datas 
        |> List.drop 1

    Nothing -> 
      []

unwrapDatax: Maybe Data -> Float
unwrapDatax mdata = 
  case mdata of 
    Just data -> data.x
    Nothing -> 0

requestUsers : Http.Request (List User)
requestUsers =
    Http.get url_users ( Decode.list user )


user : Decode.Decoder User
user =
    Decode.map3 toUser (Decode.field "id" Decode.int) (Decode.field "name" Decode.string) (Decode.field "status" Decode.string)

apidata : Decode.Decoder ApiData
apidata = 
  Decode.map4 toApiData (Decode.field "id" Decode.int) (Decode.field "human" Decode.float) (Decode.field "temparture" Decode.float) (Decode.field "userId" Decode.int)

toApiData : Int -> Float -> Float -> Int -> ApiData
toApiData i h t ui =
  {id = i , human = h, temparture = t, userId = ui}

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

requestApiData : Int -> Http.Request (List ApiData)
requestApiData id = 
  let 
    url_post =  url_datas ++ (toString id) ++ "&_sort=id&_order=desc&_limit=1"
  in
  Http.get url_post ( Decode.list apidata )

getApiData : Int ->  Cmd Msg
getApiData id =
  Http.send MsgNewApiData (requestApiData id)

createGetApiCommand : Maybe User -> Int
createGetApiCommand user =
  case user of 
    Just data ->
      data.id
    Nothing ->
      1


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
      Time.every Time.second (\_ -> MsgUnti (List.head model.users))

-- addData :Data -> Msg
-- addData data = 
--   Increase data

type alias Mdl = -- ***MDL-6
    Material.Model


-- VIEW
view : Model -> Html Msg
view model =

  div []
    [ h1 [ Html.Attribute.style.header ] [ text "Life Support User Manager" ]
    , ul [] (List.map (viewLink model) [ "/","/users/"])
    , div [] [ viewRoute model ]
    ]
    |>Material.Scheme.topWithScheme Color.Teal Color.Indigo
  -- Layout.render Mdl model.mdl
  -- [ Layout.fixedHeader
  -- ]
  -- { header = [ h1 [] [text "Life support User Manger"] ]
  -- , drawer = [ ]
  -- , tabs = ((List.map (viewLink model) [ "/","/users/" ]) ,[])
  -- , main = [ ] 
  -- }
  |> Material.Scheme.topWithScheme Color.Teal Color.Indigo


viewLink : Model -> String -> Html Msg   -- ***MDL-8
viewLink model url =
  -- li [] [ button [ onClick (MsgNewUrl url) ] [ text url ] ]
  li [ Html.Attributes.style [ ("display", "inline-block") ] ] [ 
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
        , ul [] (List.map viewUsers model.users )
        , ul [] [changeButtons model]
        ,viewgraph  model
        ]

    RouteMain ->
      div []
        [ h2 [] [text "ユーザー一覧"]
        , ul [] (List.map viewUsers model.users )
        ]


white : Options.Property c m 
white = 
  Color.text Color.white 


-- increaseButton : List Data -> Html Msg
-- increaseButton data =
--     div []
--         [ button [ onClick (Increase (Data (List.length data + 1 |> toFloat ) 1 )) ] [ text "Add Data" ]
--         ]

changeButtons : Model ->  Html Msg
changeButtons model = 
  div [] [
  Button.render Mdl [0] model.mdl
  [ Button.raised
  , Button.colored
  , Options.onClick MsgTemp
  ]
  [ text "Temparture"]
  ,
    Button.render Mdl [0] model.mdl
  [ Button.raised
  , Button.colored
  , Options.onClick Msghuman
  ]
  [ text "Human"]
  ]




viewgraph : Model -> Html msg
viewgraph model = 
    if model.graphtoggle then
        viewSeries
        [ line (List.map (\{ x, y } -> circle x y)) ]
        model.tempdatas
    else 
        viewSeries
        [ line (List.map (\{ x, y } -> circle x y)) ]
        model.humandatas
  
viewUsers a = 
  let 
    url = "/users/"++ toString a.id
  in
  case a.status of 
  "red" -> 

  div [ Html.Attributes.style [("padding","10px"), ("margin","10px")] ]
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
        -- ,viewSeries
        -- [ line (List.map (\{ x, y } -> circle x y)) ]
        -- d
    ]


  "yellow" ->

    div [  Html.Attributes.style [("padding","10px"), ("margin","10px")] ]
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
    div [ Html.Attributes.style [("padding","10px"), ("margin","10px")] ]
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