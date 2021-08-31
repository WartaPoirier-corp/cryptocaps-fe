module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html as H exposing (Html, text, div, h1, img)
import Html.Attributes as A exposing (src)
import Html.Events as E
import Url
import Url.Parser exposing (Parser, (</>), (<?>), int, map, oneOf, parse, string, s, top)
import Url.Parser.Query as Query
import Gravatar
import Array
import Http
import Json.Decode as D
import Dict exposing (Dict)
import LineChart
import Chart.Area as Area
import Chart.Container as Container
import Chart.Dot as Dot
import Chart.Events as Events
import Chart.Grid as Grid
import Chart.Interpolation as Interpolation
import Chart.Axis.Intersection as Intersection
import Chart.Junk as Junk
import Chart.Legends as Legends
import Chart.Line as Line
import Chart.Axis as Axis
import Chart.Colors as Colors
import Chart.Axis.Unit as Unit

apiUrl : String
apiUrl = "http://localhost:7878/api/v1/"

type alias Remote a = Maybe (Result Http.Error a)
type alias Paginated a = List (Remote a)

type alias Page = Maybe Int
type alias Caps = String
type Route
    = Personnal Page
    | Major Page
    | Year Page
    | Values Page
    | Info Caps
    | Login
    | Transaction
    | NotFound

routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Personnal (top <?> Query.int "page")
        , map Major (s "filieres" <?> Query.int "page")
        , map Year (s "annee" <?> Query.int "page")
        , map Values (s "caps" <?> Query.int "page")
        , map Info (s "caps" </> string)
        , map Login (s "login")
        , map Transaction (s "transaction")
        ]

toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse routeParser url)


getPage : Int -> Paginated a -> (a -> Html Msg) -> Html Msg
getPage num pages display =
    case Maybe.andThen (\x -> x) (Array.get num (Array.fromList pages)) of
        Just page -> case page of
            Err _ -> H.div [ A.class "error" ] [ H.p [] [ text "Erreur :((" ] ]
            Ok data -> display data
        Nothing -> H.div [ A.class "spinner" ] [ H.p [] [ text "Chargement…" ] ]


---- MODEL ----

type alias Cap =
    { uuid : String
    , name : String
    }

type alias BoardItem =
    { name : String
    , email : String
    , value : Int 
    }

type alias GroupItem =
    { name : String
    , icon : String
    , value : Int
    }

type alias CapValue =
    { id : String
    , name : String
    , value : Float
    , delta : Float
    }

type alias CapInfo =
    { name : String
    , value : Float
    , delta : Float
    , history : List Float
    }

type alias Model =
    { key : Nav.Key
    , page : Route
    , referenceCap : Cap
    , leaderboard : Paginated (List BoardItem)
    , majorBoard : Paginated (List GroupItem)
    , yearBoard : Paginated (List GroupItem)
    , values : Paginated (List CapValue)
    , infos : Dict String (Result Http.Error CapInfo)
    , loginMail : String
    , loginPwd : String
    }


cmdForPage : Route -> Model -> Cmd Msg
cmdForPage route model =
    case route of
        Personnal p -> if loaded model.leaderboard p then Cmd.none else fetchPersonnal p
        Major p -> fetchMajor p
        Year p -> fetchYear p
        Values p -> fetchCharts p
        Info id -> fetchChart id
        _ -> Cmd.none

init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    let page = (toRoute url) in
    let model = { key = key
                , page = page 
                , referenceCap = { name = "Leffe", uuid = "aaaa-bbbb-cccc-dddd" }
                , leaderboard = []
                , majorBoard = []
                , yearBoard = []
                , values = []
                , infos = Dict.empty
                , loginMail = ""
                , loginPwd = ""
                }
    in update (UrlChanged url) model

paginate : Page -> String
paginate page =
    case page of
        Nothing -> ""
        Just p -> "?page=" ++ (String.fromInt p)

fetchPersonnal : Page -> Cmd Msg
fetchPersonnal page =
    apiGet
        ("leaderboard/personnal" ++ paginate page)
        (FetchedBoard page)
        (D.list (D.map3 BoardItem 
            (D.field "name" D.string)
            (D.field "email" D.string)
            (D.field "value" D.int)
        ))

groupDecoder : D.Decoder (List GroupItem)
groupDecoder = 
    D.list (D.map3 GroupItem 
        (D.field "name" D.string)
        (D.field "icon" D.string)
        (D.field "value" D.int)
    )

fetchMajor : Page -> Cmd Msg
fetchMajor page =
    apiGet
        ("leaderboard/major" ++ paginate page)
        (FetchedMajor page)
        groupDecoder

fetchYear : Page -> Cmd Msg
fetchYear page =
    apiGet
        ("leaderboard/year" ++ paginate page)
        (FetchedYear page)
        groupDecoder

fetchCharts : Page -> Cmd Msg
fetchCharts page =
    apiGet
        ("values/_" ++ paginate page) -- TODO: real URL
        (FetchedCharts page)
        (D.list (D.map4 CapValue
            (D.field "id" D.string)
            (D.field "name" D.string)
            (D.field "value" D.float)
            (D.field "delta" D.float)
        ))

fetchChart : String -> Cmd Msg
fetchChart id =
    apiGet
        ("values/" ++ id)
        (FetchedInfo id)
        (D.map4 CapInfo
            (D.field "name" D.string)
            (D.field "value" D.float)
            (D.field "delta" D.float)
            (D.field "history" (D.list D.float))
        )

---- UPDATE ----

type alias HttpResult a = Result Http.Error a
type alias HttpList a = HttpResult (List a)
type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | FetchedBoard Page (HttpList BoardItem)
    | FetchedMajor Page (HttpList GroupItem)
    | FetchedYear Page (HttpList GroupItem)
    | FetchedCharts Page (HttpList CapValue)
    | FetchedInfo String (HttpResult CapInfo)
    | LoginPwd String
    | LoginMail String

setPage : Page -> (Paginated a) -> Remote a -> Paginated a
setPage maybePage pages item =
    let page = Maybe.withDefault 0 maybePage in
    if page == 0 then
        [item]
    else
        case pages of
            [] -> Nothing :: (setPage (Just (page - 1)) [] item)
            p :: ps -> p :: (setPage (Just (page - 1)) ps item)

apiGet : String -> (Result Http.Error a -> Msg) -> D.Decoder a -> Cmd Msg
apiGet url expectation decoder =
    Http.get
        { url = apiUrl ++ url
        , expect = Http.expectJson expectation decoder 
        }

loaded : Paginated a -> Page -> Bool
loaded list page =
    case page of
        Nothing -> case list of
            (Just (Ok _)) :: xs -> True
            _ -> False
        Just 0 -> case list of
            (Just (Ok _)) :: xs -> True
            _ -> False
        Just p -> loaded list (Just (p - 1))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LinkClicked req ->
            case req of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )
                Browser.External href ->
                    ( model, Nav.load href )
        UrlChanged url ->
            let page = (toRoute url) in
            ({ model | page = page }, cmdForPage page model)
        FetchedBoard page res ->
            ( { model | leaderboard = setPage page model.leaderboard (Just res) }
            , Cmd.none
            )
        FetchedMajor page res ->
            ( { model | majorBoard = setPage page model.majorBoard (Just res) }
            , Cmd.none
            )
        FetchedYear page res ->
            ( { model | yearBoard = setPage page model.yearBoard (Just res) }
            , Cmd.none
            )
        FetchedCharts page res ->
            ( { model | values = setPage page model.values (Just res) }
            , Cmd.none
            )
        FetchedInfo id res ->
            ( { model | infos = Dict.update id (\_ -> Just res) model.infos }
            , Cmd.none
            )
        LoginMail mail -> ({ model | loginMail = mail }, Cmd.none)
        LoginPwd pwd -> ({ model | loginPwd = pwd }, Cmd.none)



---- VIEW ----

pageTitle : Model -> String
pageTitle model =
    case model.page of
        Personnal _ -> "Classement"
        Major _ -> "Classement des filières"
        Year _ -> "Classement des années"
        Values _ -> "Valeur des capsules"
        Info id ->
            case Dict.get id model.infos of
                Just (Ok info) -> "À propos de la capsule de " ++ info.name
                _ -> "Capsule inconnue"
        Login -> "Connexion"
        Transaction -> "Transaction"
        NotFound -> "Quatre cent quatre"

gravOptions : Gravatar.Options
gravOptions =
    { size = Just 200
    , default = Gravatar.Identicon
    , rating = Gravatar.RatedG
    , forceDefault = False
    }

delta : Float -> Html Msg
delta x = 
    H.p
        [ A.class (if x > 0 then "up" else if x < 0 then "down" else "stable") ]
        [ text ((if x > 0 then "⇗ +" else if x < 0 then "⇘ " else "⇒ ") ++ (String.fromFloat x) ++ "%") ]

mainView : Model -> Html Msg
mainView model =
    case model.page of
        Personnal page -> H.main_ []
            [ H.h1 [] [ text "Classement" ]
            , H.p [] [ text "Qui a le plus de CryptoCapsules ?" ]
            , H.p []
                [ text "Les valeurs sont exprimés en fonction de la capsule de référence : la "
                , H.a
                    [ A.href ("/caps/" ++ model.referenceCap.uuid) ]
                    [ text model.referenceCap.name ]
                ]
            , getPage (Maybe.withDefault 0 page) model.leaderboard (\p -> H.div [ A.class "table" ]
                (List.map (\x -> H.div [ A.class "row" ]
                    [ Gravatar.img gravOptions x.email
                    , H.p [] [ text x.name ]
                    , H.p [] [ text ((String.fromInt x.value) ++ " caps") ]
                    ]
                ) p))
            ]
        Major page -> H.main_ []
            [ H.h1 [] [ text "Classement des filières" ]
            , H.p [] [ text "Quelle filière a le plus de CryptoCapsules ?" ]
            , H.p [] [ text "La richesse d'une filière est calculé en additionnant les capsules de tous les gens dans cette filière." ]
            , H.p []
                [ text "Les valeurs sont exprimés en fonction de la capsule de référence : la "
                , H.a
                    [ A.href ("/caps/" ++ model.referenceCap.uuid) ]
                    [ text model.referenceCap.name ]
                ]
            , getPage (Maybe.withDefault 0 page) model.majorBoard (\p -> H.div [ A.class "table" ]
                (List.map (\x -> H.div [ A.class "row" ]
                    [ H.img [ A.alt ("Logo de la filière " ++ x.name), A.src x.icon ] []
                    , H.p [] [ text x.name ]
                    , H.p [] [ text ((String.fromInt x.value) ++ " caps") ]
                    ]
                ) p))
            ]
        Year page -> H.main_ []
            [ H.h1 [] [ text "Classement des années" ]
            , H.p [] [ text "Quelle année a le plus de CryptoCapsules ?" ]
            , H.p [] [ text "La richesse d'une année est calculé en additionnant les capsules de tous les gens de cette année." ]
            , H.p []
                [ text "Les valeurs sont exprimés en fonction de la capsule de référence : la "
                , H.a
                    [ A.href ("/caps/" ++ model.referenceCap.uuid) ]
                    [ text model.referenceCap.name ]
                ]
            , getPage (Maybe.withDefault 0 page) model.yearBoard (\p -> H.div [ A.class "table" ]
                (List.map (\x -> H.div [ A.class "row" ]
                    [ H.img [ A.alt ("Logo des" ++ x.name), A.src x.icon ] []
                    , H.p [] [ text x.name ]
                    , H.p [] [ text ((String.fromInt x.value) ++ " caps") ]
                    ]
                ) p))
            ]
        Values page -> H.main_ []
            [ H.h1 [] [ text "Valeur des capsules" ]
            , H.p [] [ text "La valeur des capsules varient en fonction de leur quantité : plus un type de capsule est rare plus il vaut cher." ]
            , H.p []
                [ text "Les valeurs sont exprimés en fonction de la capsule de référence : la "
                , H.a
                    [ A.href ("/caps/" ++ model.referenceCap.uuid) ]
                    [ text model.referenceCap.name ]
                ]
            , getPage (Maybe.withDefault 0 page) model.values (\p -> H.div [ A.class "table" ]
                (List.map (\x -> H.div [ A.class "row" ]
                    [ H.img [ A.alt ("Logo des" ++ x.name), A.src "/icons/capsule.png" ] []
                    , H.p [] [ H.a 
                        [ A.href ("/caps/" ++ x.id) ]
                        ([ text x.name ] ++
                        (if model.referenceCap.name == x.name then [] else [ delta x.delta ])
                        )
                    ]
                    , H.p [] [ text ((String.fromFloat x.value) ++ " " ++ model.referenceCap.name) ]
                    ]
                ) p))
            ]
        Info id -> 
            case Dict.get id model.infos of
                Just (Ok info) -> H.main_ []
                    [ H.h1 [] [ text ("À propos de la capsule de " ++ info.name) 
                    , delta info.delta ]
                    , H.p [] [ text "La valeur des capsules varient en fonction de leur quantité : plus un type de capsule est rare plus il vaut cher." ]
                    , H.p []
                        [ text "Les valeurs sont exprimés en fonction de la capsule de référence : la "
                        , H.a
                            [ A.href ("/caps/" ++ model.referenceCap.uuid) ]
                            [ text model.referenceCap.name ]
                        ]
                    , H.p [] [ text ("Cette capsule vaut actuellement " ++ (String.fromFloat info.value) ++ " " ++ model.referenceCap.name) ]
                    , H.h2 [] [ text "Valeur au cours des 24 dernières heures" ]
                    , H.div [ A.class "chart"] [ LineChart.viewCustom
                        chartConfig
                        [ LineChart.line Colors.blue Dot.plus "Valeur" (List.indexedMap (\i -> \val -> { x = toFloat i, y = Just val }) info.history) ] ]
                    ]
                _ -> H.p [ A.class "error" ] [ text "Pas d'informations sur cette capsule" ]

        Login -> H.main_ []
            [ H.h1 [] [ text "Connexion" ]
            , input "email" "Adresse mail" model.loginMail LoginMail
            , input "password" "Mot de passe" model.loginPwd LoginPwd
            , H.input [ A.value "Lezgo", A.type_ "submit" ] []
            ]
        _ -> H.main_ [ A.class "error" ] [ H.h1 [] [ text "Erreur quatre cent quatre" ], H.p [] [ text "Cette page n'existe pas" ] ]

input : String -> String -> String -> (String -> Msg) -> Html Msg
input ty label val msg =
    H.div [ A.class "field" ]
        [ H.label [ A.for label ] [ text label ]
        , H.input
            [ A.type_ ty, A.id label, A.name label, A.value val, E.onInput msg ]
            []
        ]

chartConfig : LineChart.Config { a | x : Float, y : Maybe Float } Msg
chartConfig =
    { x = Axis.default "" Unit.none .x
    , y = Axis.default "" Unit.none .y
    , container = Container.default "line-chart-1" 700 400
    , intersection = Intersection.default
    , interpolation = Interpolation.default
    , legends = Legends.none
    , events = Events.default
    , area = Area.default
    , grid = Grid.default
    , line = Line.default
    , dots = Dot.default
    , junk = Junk.default
    }

menu : String -> String -> Html Msg
menu name link =
    H.li [] [ H.a [ A.href link ] [ text name ] ]

view : Model -> Document Msg
view model =
    { title = (pageTitle model) ++ " ⋅ CryptoCapsules"
    , body =
        [ H.nav []
            [ H.h1 [] [ text "CryptoCapsules" ]
            , H.ul []
                [ menu "Classement" "/"
                , menu "Classement des filières" "/filieres"
                , menu "Classement des années" "/annee"
                , menu "Valeur des capsules" "/caps"
                , menu "Connexion" "/login"
                ]
            ]
        , mainView model
        ]
    }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = LinkClicked 
        , onUrlChange = UrlChanged
        }
