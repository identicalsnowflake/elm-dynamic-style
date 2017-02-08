module DynamicStyle exposing (..)

{-|
# Library for simple, dynamic style effects
@docs hover, hover_, pressure, pressure_, focus, focus_, cssStateEffect, CSSKey, CSSValue, JSEventAttribute
The goal of this library is make locally stateful, declarative CSS effects
(e.g., :hover, :focus, etc.) as easy in Elm as they are with stylesheets.
For example, the following achieves the same effect as setting the
:hover attribute in your stylesheet:
    div
      ( hover
          [ ("color","blue","lightblue") ]
      )
      [ text "so cool!" ]
You may user the primed versions as shorthand to provide a base list of
styles, like this:
    div
      ( hover_
          [ ("font-size","20px")
          , ("font-face","Droid Sans Mono")
          ]
          [ ("color","blue","lightblue") ]
      )
      [ text "wow" ]
Completely painless!
* Note: the effects in this library consume whatever JS hooks are needed
to achieve the effect on that element (e.g., hover consumes onmouseover and
onmouseout, but you are still free to use onclick). If for some reason you
do want to do both a CSS transition and have your application respond to
an event, you can simply make a wrapper element and hook its events.
-}

import Html exposing (Attribute)
import Html.Attributes exposing (style, attribute)
import String


{-|
The key for a CSS property.
-}
type alias CSSKey =
    String


{-|
The value of a CSS property.
-}
type alias CSSValue =
    String


{-|
The name of a JavaScript event attribute.
-}
type alias JSEventAttribute =
    String


{-|
Change styles when the user hovers over an element. For example,
    hover [("color","black","blue")]
will render black text normally, but
blue text when the user hovers over the element.
-}
hover : List ( CSSKey, CSSValue, CSSValue ) -> List (Attribute msg)
hover =
    hover_ []


{-|
Shorthand to add a list of static base styles.
-}
hover_ : List ( CSSKey, CSSValue ) -> List ( CSSKey, CSSValue, CSSValue ) -> List (Attribute msg)
hover_ =
    cssStateEffect [ "onmouseout" ] "onmouseover"


{-|
Change styles when the user pushes on element. For example,
    pressure [("color","black","blue")]
will render black text normally, but
blue text when the user pushes the mouse down on the element.
-}
pressure : List ( CSSKey, CSSValue, CSSValue ) -> List (Attribute msg)
pressure =
    pressure_ []


{-|
Shorthand to add a list of static base styles.
-}
pressure_ : List ( CSSKey, CSSValue ) -> List ( CSSKey, CSSValue, CSSValue ) -> List (Attribute msg)
pressure_ =
    cssStateEffect [ "onmouseup", "onmouseout" ] "onmousedown"


{-|
Change styles when the user focuses on element. For example,
    pressure [("border-color","black","blue")]
will render a black border
normally, but a blue border when the user focuses on the element.
-}
focus : List ( CSSKey, CSSValue, CSSValue ) -> List (Attribute msg)
focus =
    focus_ []


{-|
Shorthand to add a list of static base styles.
-}
focus_ : List ( CSSKey, CSSValue ) -> List ( CSSKey, CSSValue, CSSValue ) -> List (Attribute msg)
focus_ =
    cssStateEffect [ "onblur" ] "onfocus"


{-|
Construct your own stateful effects.
Provide:
- A list of events that deactivate your effect
- One event that activates your effect
- A list of styles (key, value) to apply constantly (the inactive event styles are added to these)
- A list of styles (key, valueIfInactive, valueIfActive) that depend on the stateful effect
A list of attributes will be generated to implement the effect, using inline js and css
-}
cssStateEffect :
    List JSEventAttribute
    -> JSEventAttribute
    -> List ( CSSKey, CSSValue )
    -> List ( CSSKey, CSSValue, CSSValue )
    -> List (Attribute msg)
cssStateEffect jsEventInactives jsEventActive constantStyles dynamicStyles =
    let
        applyToFirstChar f s =
            f (String.left 1 s) ++ String.dropLeft 1 s

        -- takes css property to js equivalent
        --     jsName "border-bottom-width" == "borderBottomWidth"
        jsName : CSSKey -> String
        jsName =
            applyToFirstChar String.toLower
                << String.join ""
                << List.map (applyToFirstChar String.toUpper)
                << String.split "-"

        toJS : List ( CSSKey, CSSValue ) -> String
        toJS =
            List.foldl (\( k, v ) x -> x ++ "this.style." ++ jsName k ++ "='" ++ v ++ "';")
                ""

        inactiveStyles =
            List.map (\( a, b, _ ) -> ( a, b )) dynamicStyles

        activeStyles =
            List.map (\( a, _, c ) -> ( a, c )) dynamicStyles

        styleUpdater : List ( CSSKey, CSSValue ) -> JSEventAttribute -> Attribute msg
        styleUpdater styles event =
            attribute event (toJS styles)
    in
        [ style (constantStyles ++ inactiveStyles)
        , styleUpdater activeStyles jsEventActive
        ]
            ++ List.map (styleUpdater inactiveStyles) jsEventInactives
