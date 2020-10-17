module Main exposing (..)

import Canvas as C
import Canvas.Settings as CS
import Canvas.Settings.Text as CT
import Color -- elm install avh4/elm-color
import Html as H
import Html.Attributes as A
import Html.Events as E
import Browser
import Browser.Events as BrowserEvents
import Random
import Json.Decode as JsonDecode
import Math.Vector2 exposing (..)
import Platform.Cmd
import FormatNumber as FmtNum
import FormatNumber.Locales as FmtNumL

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type alias Bird =
    { pos: Vec2
    , vel: Vec2
    , acc: Vec2
    }

steerTo : Float -> Vec2 -> Bird -> Bird
steerTo effect target b =
    let
        acc = add b.acc (sub target b.vel |> scale effect)
        accl = if length acc > 5.0
               then scale 5.0 (normalize acc)
               else acc
    in { b | acc = accl }

randomUnitVec2 : Random.Generator Vec2
randomUnitVec2 =
    Random.map
        (\t -> vec2 (cos t) (sin t))
        (Random.float 0 (2*pi))

vec2zero = vec2 0 0

randomBird : Screen -> Random.Generator Bird
randomBird screen =
    let
        randvec = Random.map2
            vec2
            (Random.float 0 screen.width)
            (Random.float 0 screen.height)
    in
        Random.map2
            (\p v -> { pos = p, vel = v, acc = vec2zero })
            randvec
            randomUnitVec2

randomBirds : Screen -> Int -> Random.Generator (List Bird)
randomBirds screen n = Random.list n (randomBird screen)

type alias Screen =
    { height : Float
    , width : Float
    }

type alias Model =
    { deltas : List Float
    , birds : List Bird
    , birdParams : BirdParams
    , screen : Screen
    }


init : () -> (Model, Cmd Msg)
init _ =
    let
        model =
            { deltas = []
            , birds = []
            , birdParams =
                { separation = { radius = 0, effect = 0 }
                , alignment = { radius = 0, effect = 0 }
                , cohesion = { radius = 0, effect = 0 }
                }
            , screen =
                { height = 300
                , width = 600
                }
            }
    in
        (model, Random.generate SetBirds (randomBirds model.screen 100))



type Msg = Tick Float
    | SetBirds (List Bird)
    | NewBirds Int
    | UpdateSeparation SeparationParams
    | UpdateAlignment AlignmentParams
    | UpdateCohesion CohesionParams

type alias BirdParams =
    { separation : SeparationParams
    , alignment : AlignmentParams
    , cohesion : CohesionParams
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick delta -> updateTick delta model
        SetBirds birds -> ({ model | birds = birds }, Cmd.none)
        NewBirds n -> (model, Random.generate SetBirds (randomBirds model.screen n))
        UpdateSeparation s ->
            let
                bp = model.birdParams
                nm = { model | birdParams = {bp | separation = s }}
            in (nm, Cmd.none)
        UpdateAlignment a ->
            let
                bp = model.birdParams
                nm = { model | birdParams = {bp | alignment = a }}
            in (nm, Cmd.none)
        UpdateCohesion c ->
            let
                bp = model.birdParams
                nm = { model | birdParams = {bp | cohesion = c }}
            in (nm, Cmd.none)


updateTick : Float -> Model -> (Model, Cmd Msg)
updateTick delta model =
    let
        deltas = delta :: (if List.length model.deltas > 60
                 then List.take 60 model.deltas
                 else model.deltas)
        birds = updateBirds model.screen model.birdParams model.birds
    in
        ( { model | deltas = deltas, birds = birds }, Cmd.none )


updateBirds : Screen -> BirdParams -> List Bird -> List Bird
updateBirds screen bp birds =
    birds
    |> List.map (forward screen)
    |> rules screen bp

forward : Screen -> Bird -> Bird
forward screen b =
    let
        npos = add b.pos (normalize b.vel)
        nposx = frem (getX npos) screen.width
        nposy = frem (getY npos) screen.height
        vel = add b.vel b.acc
    in
        { pos = vec2 nposx nposy
        , vel = add b.vel b.acc
        , acc = vec2zero
        }

rules : Screen -> BirdParams -> List Bird -> List Bird
rules screen bp birds =
    let
        maxradius = max bp.separation.radius bp.alignment.radius
            |> max bp.cohesion.radius
        within_ r b1 b2 = within screen r b1.pos b2.pos
        filter r b1 b2 = within_ r b1 b2 && b1 /= b2
        --neighbors b = List.filter (filter b) birds
        separate_ = separate screen bp.separation
        align_ = align bp.alignment
        cohese_ = cohese screen bp.cohesion
    in
        List.map (\b ->
            let
                neighbors_ = List.filter (filter maxradius b) birds
                neighbors__ r = if r == maxradius
                                then neighbors_
                                else List.filter (filter r b) birds
            in
                separate_ b (neighbors__ bp.separation.radius)
                |> (\bb -> align_ bb (neighbors__ bp.alignment.radius))
                |> (\bb -> cohese_ bb (neighbors__ bp.cohesion.radius))
            )
            birds

type alias SeparationParams =
    { radius : Float
    , effect : Float
    }

defaultSeparationParams =
    { radius = 100
    , effect = 0.5
    }

separation : Screen -> SeparationParams -> List Bird -> List Bird
separation screen sp birds =
    let
        within_ b1 b2 = within screen sp.radius b1.pos b2.pos
        filter b bb = within_ b bb && b /= bb
        neighbors b = List.filter (filter b) birds
    in
        List.map (\b -> separate screen sp b (neighbors b)) birds

separate : Screen -> SeparationParams -> Bird -> List Bird -> Bird
separate screen sp b bs =
    let
        bslen = List.length bs |> toFloat
        inv v = scale (-1.0 / (lengthSquared v)) v
        dv = bs
            |> List.map ((\bb -> tsub screen bb.pos b.pos ))
            |> List.map inv
            |> List.foldl add vec2zero
            |> scale (1/bslen)
    in
        if (bslen) > 0 then steerTo sp.effect dv b
        else b

--n * 1/(n^2) = 1/n

within : Screen -> Float -> Vec2 -> Vec2 -> Bool
within screen d v1 v2 =
    ((tdist screen v1 v2)) <= d

-- toroidal subtraction
tsub : Screen -> Vec2 -> Vec2 -> Vec2
tsub screen v1 v2 =
    let
        w = screen.width
        h = screen.height
        dx = (getX v1) - (getX v2)
        dy = (getY v1) - (getY v2)
        dxx = if dx > w/2 then w - dx
              else if dx < -w/2 then w + dx
              else dx
        dyy = if dy > h/2 then w - dy
              else if dx < -w/2 then h + dy
              else dy
    in vec2 dxx dyy

-- toroidal distance
tdist : Screen -> Vec2 -> Vec2 -> Float
tdist screen v1 v2 =
    let
        w = screen.width
        h = screen.height
        dx = (getX v1) - (getX v2) |> abs
        dy = (getY v1) - (getY v2) |> abs
        dxx = if dx > w/2 then w - dx
              else dx
        dyy = if dy > h/2 then w - dy
              else dy
    in
        sqrt (dxx*dxx+dyy*dyy)

type alias AlignmentParams =
    { radius : Float
    , effect : Float
    }

defaultAlignmentParams =
    { radius = 20
    , effect = 0.1
    }

alignment : Screen -> AlignmentParams -> List Bird -> List Bird
alignment screen ap birds =
    let
        within_ b1 b2 = within screen ap.radius b1.pos b2.pos
        neighbors b = List.filter (within_ b) birds
    in
        List.map (\b -> align ap b (neighbors b)) birds

align : AlignmentParams -> Bird -> List Bird -> Bird
align ap b bs =
    let
        bslen = List.length bs |> toFloat
        dv = bs
            |> List.map (\bb -> bb.vel)
            |> List.foldl add vec2zero
            |> scale (1/bslen)
    in
        if bslen > 0
        then steerTo ap.effect dv b -- (normalize dv) b
        else b

type alias CohesionParams =
    { radius : Float
    , effect : Float
    }

defaultCohesionParams =
    { radius = 20
    , effect = 0.01
    }

cohesion : Screen -> CohesionParams -> List Bird -> List Bird
cohesion screen cp birds =
    let
        within_ b1 b2 = within screen cp.radius b1.pos b2.pos
        filter b1 b2 = within_ b1 b2 && b1 /= b2
        neighbors b = List.filter (filter b) birds
    in
        List.map (\b -> cohese screen cp b (neighbors b)) birds

cohese : Screen -> CohesionParams -> Bird -> List Bird -> Bird
cohese screen cp b bs =
    let
        bslen = List.length bs |> toFloat
        dv = bs
            --|> List.map (\bb -> tsub screen bb.pos b.pos)
            |> List.map (\bb -> bb.pos)
            |> List.foldl add vec2zero
            |> scale (1/bslen)
            |> \bbb -> tsub screen bbb b.pos
    in
        if bslen > 0 then steerTo cp.effect dv b
        else b


subscriptions : Model -> Sub Msg
subscriptions _ =
    BrowserEvents.onAnimationFrameDelta Tick


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.h1 [] [ H.text "birds flocking" ]
        , paramSections model
        , canvasElement model
        ]


avg l = if List.length l > 0
        then (List.foldl (+) 0.0 l) / (List.length l |> toFloat)
        else 0

canvasElement model =
    let
        height = model.screen.height
        width = model.screen.width
    in
        C.toHtml (floor width, floor height)
            [ A.style "display" "inline-block"
            , A.style "vertical-align" "top"
            ]
            [ C.clear (0, 0) width height
            , C.shapes
                [ CS.fill Color.black ]
                (List.map (bird model.screen) model.birds)
            , C.text
                [ CT.font { size = 18, family = "sans-serif" }
                , CT.align CT.Center
                , CS.fill (Color.black)
                ]
                (30, 30)
                (1 / ((avg model.deltas) /1000) |> floor |> String.fromInt)
            ]

paramSections model =
    H.div [ A.style "display" "inline-block" ]
        [ separationSection model.birdParams.separation
        , alignmentSection model.birdParams.alignment
        , cohesionSection model.birdParams.cohesion
        , miscSection model
        ]

onRangeChange : (Float -> msg) -> H.Attribute msg
onRangeChange message =
    let
        dec = (JsonDecode.at ["target", "valueAsNumber"] JsonDecode.float)
    in
        E.on "input" (JsonDecode.map message dec)
        --(JsonDecode.map message (JsonDecode.succeed 0.0))

slider: String -> Float -> Float -> Float -> (Float -> Msg) -> H.Html Msg
slider id min max value lst =
    H.input
        [ A.id id
        , A.type_ "range"
        , A.min (String.fromFloat min)
        , A.max (String.fromFloat max)
        , A.value (String.fromFloat value)
        , A.step "0.001"
        , onRangeChange lst
        ] []

floatLabelText s f =
    let
        base = FmtNumL.base
        l = { base | decimals = FmtNumL.Exact 5 }
    in s ++ FmtNum.format l f |> H.text

miscSection params =
    let
        count = (List.length params.birds)
        countLst f = NewBirds (floor f)
    in
        H.div []
            [ H.h2 [] [ H.text "Misc" ]
            , H.label
                [ A.for "misc-count" ]
                [ "bird count: " ++ String.fromInt count |> H.text  ]
            , H.br [] []
            , slider "misc-count" 0 300 (toFloat count) countLst
            , H.br [] []
            ]

separationSection params =
    let
        radiusLst f = UpdateSeparation { params | radius = f }
        effectLst f = UpdateSeparation { params | effect = f ^ 2}
    in
        H.div []
            [ H.h2 [] [ H.text "Separation" ]
            , H.label
                [ A.for "separation-radius" ]
                [ "radius: " ++ String.fromFloat params.radius |> H.text ]
            , H.br [] []
            , slider "separation-radius" 0 50 params.radius radiusLst
            , H.br [] []
            , H.label
                [ A.for "alignment-effect" ]
                [ floatLabelText "effect: " params.effect ]
            , H.br [] []
            , slider "alignment-effect" 0 0.5 (sqrt params.effect) effectLst
            ]

alignmentSection params =
    let
        radiusLst f = UpdateAlignment { params | radius = f }
        effectLst f = UpdateAlignment { params | effect = f ^ 2 }
    in
        H.div []
            [ H.h2 [] [ H.text "Alignment" ]
            , H.label
                [ A.for "alignment-radius" ]
                [ "radius: " ++ String.fromFloat params.radius |> H.text ]
            , H.br [] []
            , slider "alignment-radius" 0 50 params.radius radiusLst
            , H.br [] []
            , H.label
                [ A.for "alignment-effect" ]
                [ floatLabelText "effect: " params.effect ]
            , H.br [] []
            , slider "alignment-effect" 0 1.0 (sqrt params.effect) effectLst
            ]

cohesionSection params =
    let
        radiusLst f = UpdateCohesion { params | radius = f }
        effectLst f = UpdateCohesion { params | effect = f ^ 2}
    in
        H.div []
            [ H.h2 [] [ H.text "Cohesion" ]
            , H.label
                [ A.for "cohesion-radius" ]
                [ "radius: " ++ (String.fromFloat params.radius) |> H.text ]
            , H.br [] []
            , slider "cohesion-radius" 0 50 params.radius radiusLst
            , H.br [] []
            , H.label
                [ A.for "cohesion-effect" ]
                [ floatLabelText "effect: " params.effect ]
            , H.br [] []
            , slider "alignment-effect" 0 0.5 (sqrt params.effect) effectLst
            ]

bird : Screen -> Bird -> C.Shape
bird screen b =
    let
        x = frem (getX b.pos) screen.width
        y = frem (getY b.pos) screen.height
        theta = atan2 -(getY b.vel) (getX b.vel)
        bird1 = [(0, 3), (10, 0), (0, -3), (0, 3)]
            |> List.map (rotate theta)
            |> List.map (translate (x, y))
        head = Maybe.withDefault (0,0) (List.head bird1)
        tail = Maybe.withDefault [] (List.tail bird1)
    in
        C.path head (List.map (\pt -> C.lineTo pt) tail)


frem : Float -> Float -> Float
frem x y = x/y
    |> floor
    |> toFloat
    |> (*) y
    |> (-) x

fmod : Float -> Float -> Float
fmod x y = frem x y
    |> \r -> if x*y < 0 then -r else r

translate : (Float, Float) -> (Float, Float) -> (Float, Float)
translate (x, y) (xx, yy) = (xx + x, yy + y)

-- rotate around the origin
rotate : Float -> (Float, Float) -> (Float, Float)
rotate theta (x, y) =
    let
        ct = theta |> cos
        st = theta |> sin |> Basics.negate
    in
        (x*ct-y*st, x*st+y*ct)


