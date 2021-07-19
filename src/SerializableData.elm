module SerializableData exposing (..)
import ListExtensions exposing (first)

type Carriage
    = Theoretical DragConnection
    | Concrete DragConnection

type Intensity
    = Exhibiting
    | Elaborating
    | Linking
    | Creating

type alias ChainPosition = Int

type DragConnection
    = Intensity Intensity
    | NoPreviousCarriage

type alias Dimensions =
    { boxSize : Int
    , interCarriageSpace : Int
    , interTrainSpace : Int
    , marginSize : Int
    }

type InsertionLocation
    = Before ChainPosition
    | After ChainPosition

type Interactable
    = IntensityHandles ChainPosition
    | Controls ChainPosition

type alias Diagram =
    { dim : Dimensions
    , segments : List Carriage
    , ux : Maybe Interactable
    }

-- What kind of interactions?
-- Add a new carriage
-- Delete a carriage
-- Flip between theoretical/concrete
-- Increase/decrease Connection intensity