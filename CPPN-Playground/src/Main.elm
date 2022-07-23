port module Main exposing (..)

import Printf
import Browser
import Svg exposing (svg)
import Html.Attributes exposing (id
                                , class
                                , for
                                , value
                                , width
                                , height
                                , selected
                                , disabled
                                , src)
import Html exposing (Html, h1, button, div, text, p, label, select, option, h4, span, canvas, sub, img)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (decodeValue)
import Json.Decode.Pipeline exposing (required, hardcoded)
import Json.Encode as Encode
import Random exposing (Generator)
import Json.Encode.Extra as Encode
import Result exposing (toMaybe)

{-
Entry point for elm where a HTML element is created to be managed by elm
We tell the element the initial state (init), view function, and update function, as well as a batch of event sources called subscriptions
The batch of subscriptions are to set the latent vector, epochs, and can train values from decoded JSON list, float, int, and bool values in the case that they exist
-}
main =
  Browser.element 
    { init = init
    , view = view
    , update = updateWithStorage
    , subscriptions = \_ -> Sub.batch 
      [ setLatentVector (MaybeSetLatentVector << toMaybe << decodeValue (Decode.list Decode.float))
      , setEpochs       (MaybeSetEpochs       << toMaybe << decodeValue (Decode.int))
      , setCanTrain     (MaybeSetCanTrain     << toMaybe << decodeValue (Decode.bool))
      ]
    }

--creating a type alias Layer to save from rewriting the record type
type alias Layer =
  { units : Int
  , activationFunction : String
  , seed : Float
  }

--random vector function that creates the vector from an int and a randomly generated list of floats
randomVector : Int -> Generator (List Float)
randomVector n =
  Random.list n (Random.float (-1) 1)

--random layer function that creates the layer from a random int from 1 to 20, a random String, and a random float from 0 to 1
randomLayer : Generator Layer
randomLayer =
  Random.map3 Layer
    (Random.int 1 20)
    (Random.uniform "tanh" allActivationFunctions)
    (Random.float 0 1)

--creating a type alias for Network where layers are stored in a Dictionary and retrieved with int keys
type alias Network =
  { layers : Dict Int Layer
  }

--creating a Colour type that can be either R, G, or B
type Colour = Red | Green | Blue

--creating a Model type alias to save from rewriting, a model will contain values for all of the below, except possibly a String, a float, and a colour to represent the currentImage, latentVector, and currentlySelecting
type alias Model =
  { network            : Network
  , outputWidth        : Int
  , outputHeight       : Int
  , walking            : Bool
  , training           : Bool
  , currentEpoch       : Int
  , currentImage       : Maybe String
  , latentDimensions   : Int
  , latentVector       : Maybe (List Float)
  , redNode            : String
  , blueNode           : String
  , greenNode          : String
  , currentlySelecting : Maybe Colour
  , canTrain           : Bool
  }

--encoding function that takes a layer and encodes it into a JSON object
encodeLayer : Layer -> Decode.Value
encodeLayer l =
  Encode.object
    [ ("units", Encode.int l.units)
    , ("activationFunction", Encode.string l.activationFunction)
    , ("seed", Encode.float l.seed)
    ]

--encoding function that takes a network and encodes it into a JSON object
encodeNetwork : Network -> Decode.Value
encodeNetwork net =
    Encode.object
      [ ("layers", Encode.list encodeLayer (Dict.values net.layers))
      ]

--encoding en
encodeModel : Model -> Decode.Value
encodeModel model =
  Encode.object 
    [ ("network", encodeNetwork model.network)
    , ("outputWidth", Encode.int model.outputWidth)
    , ("outputHeight", Encode.int model.outputHeight)
    , ("latentDimensions", Encode.int model.latentDimensions)
    , ("latentVector", Encode.maybe (Encode.list Encode.float) model.latentVector)
    , ("redNode", Encode.string model.redNode)
    , ("greenNode", Encode.string model.greenNode)
    , ("blueNode", Encode.string model.blueNode)
    ]

--creating a record with determined values (for layers and network) that make up an 'empty' model
emptyModel =
  { network = { layers = 
    Dict.fromList
      -- [ (0, Layer 1 "tanh" 0.2)
      -- [ (0, Layer 5 "tanh" 0.2)
      -- , (1, Layer 5 "tanh" 0.3)
      -- , (2, Layer 3 "selu" 0.3)
      [ (0, Layer 20 "tanh" 0.2)
      , (1, Layer 20 "tanh" 0.3)
      , (2, Layer 20 "tanh" 0.4)
      , (3, Layer 20 "tanh" 0.5)
      , (4, Layer 20 "tanh" 0.6)
      , (5, Layer 20 "softsign" 0.7)
      -- Simple
      -- , (5, Layer 10 "relu" 0.2)
      -- , (6, Layer 10 "tanh" 0.3)
      -- , (7, Layer 10 "selu" 0.3)
      -- , (9, Layer 10 "tanh" 0.3)
      ]
    }
  , outputWidth        = 100
  , outputHeight       = 100
  , walking            = False
  , training           = False
  , currentEpoch       = 0
  , latentDimensions   = 10
  , currentImage       = Nothing
  , latentVector       = Nothing
  , redNode            = "final-1"
  , greenNode          = "final-2"
  , blueNode           = "final-3"
  , currentlySelecting = Nothing
  , canTrain           = False
  }

--determining values for the 'basic' layer (Int 5, String "tanh", Float 0.4)
basicLayer = Layer 5 "tanh" 0.4

{-method that takes an JSON value if it exists and returns a Model with a command message
function describes how to initialize our program
the model that is returned is the emptyModel above
a batch of commands are made that set the latent vector with a list of floats randomly generated with the emptyModels latentDimensions Int value-}
init : Maybe Decode.Value -> ( Model, Cmd Msg )
init maybeModel =
  -- TODO: Load the model from JSON
  --  Maybe.withDefault emptyModel maybeModel
  ( emptyModel
  , Cmd.batch [ Random.generate SetLatentVector (randomVector emptyModel.latentDimensions)
              -- , resetModel <| encodeModel emptyModel
              ]
  )


{-communication between JS and Elm that takes a decoded JSON value and makes a command message to set the storage-}
port setStorage : Decode.Value -> Cmd msg

--takes a decoded JSON value and makes a command to reset the model
port resetModel : Decode.Value -> Cmd msg

--takes a decoded JSON value and makes a command to start a random walk 
port startRandomWalk : Decode.Value -> Cmd msg

--takes a decoded JSON value and makes a command to start the training
port startTraining : Decode.Value -> Cmd msg

--takes a decoded JSON value and makes a command to stop the training
port stopTraining : Decode.Value -> Cmd msg

--takes a decoded JSON value and makes a command to stop the random walk
port stopRandomWalk  : Decode.Value -> Cmd msg

--takes a JSON value and converts to a message that makes a subscription to set the latent vector
port setLatentVector : (Encode.Value -> msg) -> Sub msg

--takes a JSON value and converts to a message that makes a subscription to set the Epochs
port setEpochs : (Encode.Value -> msg) -> Sub msg

--takes a JSON value and converts to a message that makes a subscription to set the CanTrain
port setCanTrain : (Encode.Value -> msg) -> Sub msg

--returns a command that downloads big
port downloadBig : () -> Cmd msg

--returns a command that clears the image
port clearImage : () -> Cmd msg

--takes a decoded JSON value and makes a command to rerender
port rerender : Decode.Value -> Cmd msg



{-| We want to `setStorage` on every update. This function adds the setStorage
    command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage (encodeModel model), cmds ]
        )

--creating message and defining its variants (NoOp, ModifyLayerUnits, ...)
type Msg = NoOp
         | ModifyLayerUnits      Int Int
         | AddLayer              
         | AddLayerR             Layer
         | ModifyLayerActivation Int String
         | RemoveLayer
         | SetLatentVector       (List Float)
         | MaybeSetLatentVector  (Maybe (List Float))
         | ToggleRandomWalk
         | ToggleTraining
         | MaybeSetEpochs        (Maybe Int)
         | DownloadBig
         | ClearImage
         | SelectRed
         | SelectGreen
         | SelectBlue
         | SetChannel            (Maybe Colour, String)
         | MaybeSetCanTrain      (Maybe Bool)

{-Update function that takes a message and the old model, and spits out the updated model and a command for Elm to make
There is a case considered for each variant of the Msg type defined above and either a new model or the original is returned, along with a command that can be none (Cmd.none)-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
    NoOp -> (model, Cmd.none)

    SetChannel (currentlySelecting, ourId) ->
      let
          tempModel = case currentlySelecting of
            Just Red   -> { model | redNode   = ourId }
            Just Green -> { model | greenNode = ourId }
            Just Blue  -> { model | blueNode  = ourId }
            _           -> model
          newModel = { tempModel | currentlySelecting = Nothing } 
      in
        ( newModel , rerender <| encodeModel newModel)

    SelectRed ->
      ( { model | currentlySelecting = Just Red   }, Cmd.none )

    SelectGreen ->
      ( { model | currentlySelecting = Just Green }, Cmd.none )

    SelectBlue ->
      ( { model | currentlySelecting = Just Blue  }, Cmd.none )

    ClearImage ->
      (  { model | canTrain = False } , clearImage () )

    DownloadBig ->
      ( model, downloadBig () )

    MaybeSetCanTrain mct ->
      let
          newModel = { model | canTrain = Maybe.withDefault False mct }
      in
          ( newModel, Cmd.none )

    MaybeSetEpochs me ->
      let
          newModel = Maybe.withDefault model (Maybe.map (\e -> ({ model | currentEpoch = e })) me)
      in
          ( newModel, Cmd.none )

    ToggleTraining ->
      let
       newModel = { model | training = not model.training }
       cmd = if newModel.training then
              startTraining
             else
              stopTraining
      in
        ( newModel, cmd <| encodeModel newModel )

    ToggleRandomWalk ->
      let
       newModel = { model | walking = not model.walking }
       cmd = if newModel.walking then
              startRandomWalk
             else
              stopRandomWalk
      in
        ( newModel, cmd <| encodeModel newModel )

    MaybeSetLatentVector mv ->
      let
          newModel = { model | latentVector = mv }
      in
        ( newModel, Cmd.none )

    SetLatentVector v ->
      let
          newModel = { model | latentVector = Just v }
      in
        ( newModel, resetModel <| encodeModel newModel )

    ModifyLayerActivation index newActivation ->
      let
          f         = Maybe.map (\l -> { l | activationFunction = newActivation })
          newLayers = Dict.update index f model.network.layers
          newModel  = { model | network = { layers = newLayers } }
      in
          ( newModel, resetModel <| encodeModel newModel )
           

    RemoveLayer ->
      let
          minLayers    = 1
          currentCount = Dict.size model.network.layers
          layers       = if currentCount == minLayers then
                           model.network.layers
                         else
                           Dict.remove (currentCount - 1) model.network.layers 
          newModel  = { model | network = { layers = layers } }
      in
          ( newModel, resetModel <| encodeModel newModel )


    AddLayerR newLayer ->
      let
          currentCount = Dict.size model.network.layers
          layers       = Dict.insert currentCount newLayer model.network.layers
          newModel     = { model | network = { layers = layers } }
      in
          ( newModel, resetModel <| encodeModel newModel )


    AddLayer ->
      let
          maxLayers    = 10
          currentCount = Dict.size model.network.layers
          cmd = if currentCount < maxLayers then
                  Random.generate AddLayerR randomLayer
                else
                  Cmd.none
      in
          ( model, cmd )


    ModifyLayerUnits step index ->
      let
          maxNeurons = 20
          minNeurons = 1
          f          = Maybe.map (\l -> { l | units = min (max minNeurons (l.units + step)) maxNeurons })
          newLayers  = Dict.update index f model.network.layers
          newModel   = { model | network = { layers = newLayers } }
      in
          (newModel, resetModel <| encodeModel newModel)

{-View function that displays the information - heading, topControls, and network in a divider of id attribute "app"-}
view model =
  div [ id "app" ]
    [ heading
    , topControls model
    , network     model model.network
    ]

--Heading function that displays the String "CPPN Playground" in a divider of class attribute "header"
heading = 
  div [ class "header" ]
      [ h1 [] [ text "CPPN Playground" ]
      ]

{-TopControls function that displays a divider of class attribute "buttons" and class attribute "item" within a divider of id attribute "top-controls"
The buttons toggle the training and the random walk, displaying different Strings depending on the model's training and walking elements
The buttons are disabled depending on the model's canTrain and training elements
The item is a label that pads the current Epoch with 0's on the left until it is 10 digits before displaying the value-}
topControls model =
  div [ id "top-controls" ]
    [ div [ class "buttons" ]
          -- [ button [ class "ctl reset" ] [ text "reset" ]
          [ button [ class "ctl train"
                   , onClick ToggleTraining
                   , disabled (not model.canTrain)
                   ]
                  [ text <| if model.training then "stop-training" else "train"  ]

          , button [ class "ctl random-walk"
                  , onClick ToggleRandomWalk
                  , disabled (model.training)
                  ]
                  [ text <| if model.walking then "stop-random-walk" else "random-walk"  ]

          -- , button [ class "ctl random" ] [ text "random network"  ]
          ]
    -- Epoch
    , div [ class "item" ]
          [ label [] [ text "Epoch" ]
          , p [ class "big" ] [ text <| (String.padLeft 10 '0' <| String.fromInt model.currentEpoch)  ]
          ]
    ]

{-Defining a network called 'net' with a divider of id attribute "network" that contains subdividers of class attribute "item" each with a different instance of model that corresponds to the inputSection, hiddenNetwork, and output-}
network : Model -> b -> Html Msg
network model net =
  div [ id "network" ]
    [ div [ class "item" ]
          [ inputSection model ]
    -- Hidden Network
    , div [ class "item" ]
          [ hiddenNetwork model ]
    -- Output 
    , div [ class "item" ]
          [ output model ]
    ]

{-InputSection function that displays a divider of class attribute "inputs" which displays the text "Inputs" 
and a divider of class attribute "layer" with two dividers of class attributes "layer-top" and "z-input"
The layer-top contains a label with text "Latent vector. Try dragging the circles!"
The z-input contains a label of id "z" and text "z" as well as a SVG scene with id "latent-vector" that makes up the square with draggable colored circles
-}
inputSection model =
  let
  -- setting xInputs inputting a function to divide as well as a divider of class attribute "input" with a & b for content c
      xInputs = List.map2 (\a b -> div [ class "input" ] [ a, b ]) 
                  [ span [ id "x1" ] [ text "x", sub [] [ text "1" ] ]
                  , span [ id "x2" ] [ text "x", sub [] [ text "2" ] ]
                  ]
                  (List.map (neuron model (-1)) (List.range 1 2))
      zInput = [ label [ id "z" ] [ text "z" ] -- sub [] [ text "1" ] ]
               , div [] [ svg  [ id "latent-vector" ] [ ] ]
               ]
  in
    div [ class "inputs" ]
      [ h4 [] [ text "Inputs" ]
      , div [ class "layer" ]
        [ div [ class "layer-top" ] [ label [] [ text "Latent vector. Try dragging the circles!" ] ]
        , div [ class "z-input" ] zInput
        ]
      ]

{-HiddenNetwork function that displays a divider containing two dividers of class attribute "row" and id attribute "hidden-network"
The "row" divider displays two buttons that add or subtract layers upon clicked and are unable to be clicked whenever the model is training
The "row" divider also displays the number of layers in the network within the model conjoined with " Layers"
The "hidden-network" divider displays the mapped out version of each layer 
-}
hiddenNetwork model =
  div []
    [ div [ class "row" ]
          [ button [ onClick AddLayer,    class "ctl plus",  disabled model.training ] [ text "+" ]
          , button [ onClick RemoveLayer, class "ctl minus", disabled model.training ] [ text "-" ]
          , h4 [] [ text <| String.fromInt (Dict.size model.network.layers) ++ " Layers" ]
          -- TODO: ???
          -- , button [ class "ctl" ] [ text "randomise network" ]
          ]
    , div [ id "hidden-network" ]
         <| List.map (layer model) (Dict.toList model.network.layers)
    ]
{-Output function that displays a divider containing the text Output as well as a divider of class attribute "layer" 
The "layer" divider contains four additional dividers of class attributes "layer-top" and "neurons" as well as id attributes "paste" and "input-image"
The "layer-top" divider contains the text "Final output"
The "neurons" divider contains the final output model (the recreated image or final neuron) with a button to download a larger version
The "paste" and "neurons" divider contains text giving instructions on how to upload your desired image
The "input-image" divider contains a label with text "Input Image" as well as a divider of class attribute "img-thingy" in addition to a button that clears the uploaded image
The "img-thingy" divider contains the uploaded image along with text label "Image to match!" and a divider contining the cropped uploaded image
-}
output model =
  let
      last = Dict.size model.network.layers
      selectingClass = 
        case model.currentlySelecting of
          Just Red    -> "red"
          Just Green  -> "green"
          Just Blue   -> "blue"
          Nothing     -> ""
  in
    div []
      [ h4 [] [ text "Output" ]
      , div [ class "layer" ]
            [ div [ class "layer-top" ]
                  [ label [] [ text "Final output"]
                  -- , div [ class "row" ]
                  --       [ neuron_ model "final-1" [ onClick SelectRed   ]
                  --       , neuron_ model "final-2" [ onClick SelectGreen ]
                  --       , neuron_ model "final-3" [ onClick SelectBlue  ]
                  --       ]
                  ]
            --
            , div [ class "neurons"   ]
                  [ finalNeuron model
                  ,  button [ class "ctl", onClick DownloadBig ] [ text "download big version" ] ]
            , div [ id "paste", class "neurons" ] [ label [] [ text "Drag an image here, or just paste from your clipboard." ] ]
            , div [ id "input-image" ]
              --
              [ label [] [ text "Input image" ] 
              , div [ class "img-thingy"] 
                  [ img [ id "uploaded-image"
                        , width model.outputWidth
                        , height model.outputHeight
                        ] [] 

                  , label [] [ text "Image to match!" ]
                  , div [ id "actual-image-container" ] []
                  ]
              , button [ class "ctl", onClick ClearImage, disabled model.training ] [ text "clear" ]
              ]
            ]
      ]
{-AllActivationFunctions function that compiles all the activation functions into an alphabetized list
-}
allActivationFunctions = List.sort 
              [ "tanh"
              , "relu"
              , "selu"
              , "elu"
              , "relu6"
              , "softplus"
              , "softsign"
              ]
{-ActivationFunctions function that allows selection of an activation function within each layer
The selection is disabled when the model is training
-}
activationFunctions model current c
  = let
      mkOpt f = option [ value f, selected (current == f)] [ text f ]
    in
      select [ onInput c
             , class "select-css"
             , disabled model.training
             ] <| List.map mkOpt allActivationFunctions

{-Layer function that displays a divider of class attribute "layer" that contains two dividers of class attributes "layer-top" and "neurons"
The "layer-top" divider contains a divider of class attribute "row" as well as a label that displays the number of filters as a String and the box for selecting the activation function (allActivationFunctions function)
The "row" divider contains two buttons for adding/subtracting one unit from the specified layer, they are disabled when the model is training
The "neurons" divider contains each neuron from index 1 to the number of units in the layer
-}
layer : Model -> (Int, Layer) -> Html Msg
layer model (index, l) =
  div [ class "layer" ]
      [ div [ class "layer-top" ]
            [ div [ class "row" ]
                [ button [ onClick (ModifyLayerUnits ( 1) index), class "ctl plus",  disabled model.training ] [ text "+" ]
                , button [ onClick (ModifyLayerUnits (-1) index), class "ctl minus", disabled model.training ] [ text "-" ]
                ]
            , label [] [text <| String.fromInt (l.units) ++ " filters" ]
            , activationFunctions model l.activationFunction (ModifyLayerActivation index)
            ]
      , div [ class "neurons" ]
            <| List.map (neuron model index) (List.range 1 l.units)
      ]

{-FinalNeuron function that displays the final neuron after all the units in all the layers (the best recreation of the uploaded image the program can create aka the output) the bitmap area has the width and height necessary for the output
-}
finalNeuron : Model -> Html Msg
finalNeuron  model
  = canvas
      [ width  model.outputWidth
      , height model.outputHeight
      , id <| "final-neuron" ] []

{-Neuron_ function that displays the neuron in a bitmap area with a specific size of 20, rgb value for ourClass, and picking-rgb value for selectingClass 
-}
neuron_ : Model -> String -> List (Html.Attribute Msg) -> Html Msg
neuron_ model ourId attrs 
  = let
      size = 20

      ourClass =
        if ourId == model.redNode then
          "red"
        else
          if ourId == model.greenNode then
            "green"
          else
            if ourId == model.blueNode then
              "blue"
            else ""

      selectingClass = 
        case model.currentlySelecting of
          Just Red    -> "pick-red"
          Just Green  -> "pick-green"
          Just Blue   -> "pick-blue"
          Nothing     -> ""

      attr = onClick <| SetChannel (model.currentlySelecting, ourId)
      --
      -- If we're currently selecting; then we have a _click_ action to
      -- pick the current one.
      --
      ourAttrs = Maybe.withDefault
                  attrs 
                  (Maybe.map (\_ -> attr :: attrs) model.currentlySelecting)
    in
      canvas (
        [ width  size
        , height size
        , class selectingClass
        , class ourClass
        , id ourId
        ] ++ ourAttrs)
        []

{-Neuron function that invokes the neuron_ function with an id created from hyphenating the layerNumber and neuronNumber
-}
neuron : Model -> Int -> Int -> Html Msg
neuron model layerNumber neuronNumber
  = neuron_
      model
      (String.fromInt layerNumber ++ "-" ++ String.fromInt neuronNumber)
      []
