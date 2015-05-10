module Html.Decoder
  ( MouseEvent
  , mouseEvent
  , WheelEvent
  , wheelEvent
  , dom_delta_pixel
  , dom_delta_line
  , dom_delta_page
  , KeyboardEvent
  , keyboardEvent
  , dom_key_location_standard
  , dom_key_location_left
  , dom_key_location_right
  , dom_key_location_numpad
  , CompositionEvent
  , compositionEvent
  , Gamepad
  , gamepad
  , GamepadButton
  , gamepadButton
  , GamepadEvent
  , gamepadEvent
  , AnimationEvent
  , animationEvent
  , DeviceMotionEvent
  , deviceMotionEvent
  , DeviceOrientationEvent
  , deviceOrientationEvent
  , DeviceAcceleration
  , deviceAcceleration
  , DeviceRotationRate
  , deviceRotationRate
  ) where
{-| List of Decoders made to capture DOM events. The types match their
equivalent DOM interfaces.

# Mouse Events
@docs MouseEvent, mouseEvent

# Wheel Events
@docs WheelEvent, wheelEvent, dom_delta_pixel, dom_delta_line, dom_delta_page

# Keyboard Events
@docs KeyboardEvent, keyboardEvent, dom_key_location_standard, dom_key_location_left, dom_key_location_right, dom_key_location_right, dom_key_location_numpad

# Composition Events
@docs CompositionEvent, compositionEvent

# Gamepad Events
@docs GamepadEvent, gamepadEvent, Gamepad, gamepad, GamepadButton, gamepadButton

# Animation Events
@docs AnimationEvent, animationEvent

# Device Motion / Orientation Events
@docs DeviceMotionEvent, deviceMotionEvent, DeviceOrientationEvent, deviceOrientationEvent, deviceAcceleration, deviceAcceleration, DeviceRotationRate, deviceRotationRate

-}


import Json.Decode exposing (Decoder, map, object2, int, bool, float, string, (:=), list)


-- helper --
andMap =
  object2 (<|)


infixl 0 `map`
infixl 0 `andMap`
-----------------
-- Mouse Event --
-----------------

type alias MouseEvent =
  { screenX   : Int
  , screenY   : Int
  , clientX   : Int
  , clientY   : Int
  , ctrlKey   : Bool
  , shiftKey  : Bool
  , altKey    : Bool
  , metaKey   : Bool
  , button    : Int
  }


mouseEvent : Decoder MouseEvent
mouseEvent =
  MouseEvent
    `map`    "screenX"  := int
    `andMap` "screenY"  := int
    `andMap` "clientX"  := int
    `andMap` "clientY"  := int
    `andMap` "ctrlKey"  := bool
    `andMap` "shiftKey" := bool
    `andMap` "altKey"   := bool
    `andMap` "metaKey"  := bool
    `andMap` "button"   := int


-----------------
-- Wheel Event --
-----------------

type alias WheelEvent =
  { screenX   : Int
  , screenY   : Int
  , clientX   : Int
  , clientY   : Int
  , ctrlKey   : Bool
  , shiftKey  : Bool
  , altKey    : Bool
  , metaKey   : Bool
  , button    : Int
  , deltaX    : Float
  , deltaY    : Float
  , deltaZ    : Float
  , deltaMode : Int
  }

wheelEvent : Decoder WheelEvent
wheelEvent =
  WheelEvent
    `map`    "screenX"    := int
    `andMap` "screenY"    := int
    `andMap` "clientX"    := int
    `andMap` "clientY"    := int
    `andMap` "ctrlKey"    := bool
    `andMap` "shiftKey"   := bool
    `andMap` "altKey"     := bool
    `andMap` "metaKey"    := bool
    `andMap` "button"     := int
    `andMap` "deltaX"     := float
    `andMap` "deltaY"     := float
    `andMap` "deltaZ"     := float
    `andMap` "deltaMode"  := int


-- Delta Mode Code
dom_delta_pixel : Int
dom_delta_pixel = 0

dom_delta_line : Int
dom_delta_line = 1

dom_delta_page : Int
dom_delta_page = 2


--------------------
-- Keyboard Event --
--------------------

type alias KeyboardEvent =
  { key         : String
  , code        : String
  , location    : Int
  , ctrlKey     : Bool
  , shiftKey    : Bool
  , altKey      : Bool
  , metaKey     : Bool
  , repeat      : Bool
  , isComposing : Bool
  }


keyboardEvent : Decoder KeyboardEvent
keyboardEvent =
  KeyboardEvent
    `map`    "key"          := string
    `andMap` "code"         := string
    `andMap` "location"     := int
    `andMap` "ctrlKey"      := bool
    `andMap` "shiftKey"     := bool
    `andMap` "altKey"       := bool
    `andMap` "metaKey"      := bool
    `andMap` "repeat"       := bool
    `andMap` "isComposing"  := bool

-- Key location code
dom_key_location_standard : Int
dom_key_location_standard = 0

dom_key_location_left : Int
dom_key_location_left = 1

dom_key_location_right : Int
dom_key_location_right = 2

dom_key_location_numpad : Int
dom_key_location_numpad = 3


-- Composition Event

type alias CompositionEvent =
  { data : String }


compositionEvent : Decoder CompositionEvent
compositionEvent =
  CompositionEvent
    `map` "data" := string

-----------------
-- Touch Event --
-----------------

-- TODO: Figure out getter problem for touch lists
{-}
type alias Touch =
  { identifier  : Int
  , screenX     : Int
  , screenY     : Int
  , clientX     : Int
  , clientY     : Int
  , pageX       : Int
  , pageY       : Int
  }

touch : Decoder Touch
touch =
  Touch
    `map`    "identifier" := int
    `andMap` "screenX"    := int
    `andMap` "screenY"    := int
    `andMap` "clientX"    := int
    `andMap` "clientY"    := int
    `andMap` "pageX"      := int
    `andMap` "pageY"      := int


type alias TouchEvent =
  { touches         : List Touch
  , targetTouches   : List Touch
  , changedTouches  : List Touch
  , altKey    : Bool
  , metaKey   : Bool
  , ctrlKey   : Bool
  , shiftKey  : Bool
  }


touchEvent : Decoder TouchEvent
touchEvent =
  TouchEvent
    `map`    "touches"        := list touch
    `andMap` "targetTouches"  := list touch
    `andMap` "changedTouches" := list touch
    `andMap` "altKey"         := bool
    `andMap` "metaKey"        := bool
    `andMap` "ctrlKey"        := bool
    `andMap` "shiftKey"       := bool
-}
-------------------
-- Gamepad Event --
-------------------

type alias Gamepad =
  { id        : String
  , index     : Int
  , connected : Bool
  , timestamp : Float
  , mapping   : String
  , axes      : List Float
  , buttons   : List GamepadButton
  }

gamepad : Decoder Gamepad
gamepad =
  Gamepad
    `map`    "id"         := string
    `andMap` "index"      := int
    `andMap` "connected"  := bool
    `andMap` "timestamp"  := float
    `andMap` "mapping"    := string
    `andMap` "axes"       := list float
    `andMap` "buttons"    := list gamepadButton


type alias GamepadButton =
  { pressed : Bool
  , value   : Float
  }

gamepadButton : Decoder GamepadButton
gamepadButton =
  GamepadButton
    `map`    "pressed"  := bool
    `andMap` "value"    := float

type alias GamepadEvent =
  { gamepad : Gamepad }

gamepadEvent : Decoder GamepadEvent
gamepadEvent =
  GamepadEvent
    `map` "gamepad" := gamepad

---------------------
-- Animation Event --
---------------------

type alias AnimationEvent =
  { animationName : String
  , elapsedTime   : Float
  , pseudoElement : String
  }

animationEvent : Decoder AnimationEvent
animationEvent =
  AnimationEvent
    `map`    "animationName" := string
    `andMap` "elapsedTime"   := float
    `andMap` "pseudoElement" := string

-------------------------
-- Device Motion Event --
-------------------------

type alias DeviceMotionEvent =
  { acceleration                  : DeviceAcceleration
  , accelerationIncludingGravity  : DeviceAcceleration
  , rotationRate                  : DeviceRotationRate
  , interval                      : Float
  }

deviceMotionEvent : Decoder DeviceMotionEvent
deviceMotionEvent =
  DeviceMotionEvent
    `map`    "acceleration"                 := deviceAcceleration
    `andMap` "accelerationIncludingGravity" := deviceAcceleration
    `andMap` "rotationRate"                 := deviceRotationRate
    `andMap` "interval"                     := float

type alias DeviceOrientationEvent =
  { alpha     : Float
  , beta      : Float
  , gamma     : Float
  , absolute  : Bool
  }

deviceOrientationEvent : Decoder DeviceOrientationEvent
deviceOrientationEvent =
  DeviceOrientationEvent
    `map`    "alpha"    := float
    `andMap` "beta"     := float
    `andMap` "gamma"    := float
    `andMap` "absolute" := bool


type alias DeviceAcceleration =
  { x : Float
  , y : Float
  , z : Float
  }


deviceAcceleration : Decoder DeviceAcceleration
deviceAcceleration =
  DeviceAcceleration
    `map`    "x" := float
    `andMap` "y" := float
    `andMap` "z" := float



type alias DeviceRotationRate =
  { alpha : Float
  , beta  : Float
  , gamma : Float
  }


deviceRotationRate : Decoder DeviceRotationRate
deviceRotationRate =
  DeviceRotationRate
    `map`    "alpha" := float
    `andMap` "beta"  := float
    `andMap` "gamma" := float
