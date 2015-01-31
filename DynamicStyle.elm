module DynamicStyle where

{-|
# Library for simple, dynamic style effects

The goal of this library is make locally stateful, declarative CSS effects
(e.g., :hover, :focus, etc.) as easy in Elm as they are with stylesheets.

For example, the following achieves the same effect as setting the
:hover attribute in your stylesheet:

    div (hover [("color","blue","lightblue")]) [text "so cool!"]

You may user the primed versions as shorthand to provide a base list of
styles, like this:

    div (hover'
        [ ("font-size","20px")
        , ("font-face","Droid Sans Mono")]
        [("color","blue","lightblue")])
        [text "wow"]

Completely painless!

* Note: the effects in this library consume whatever JS hooks are needed
to achieve the effect on that element (e.g., hover consumes onmouseover and
onmouseout, but you are still free to use onclick). If for some reason you
do want to do both a CSS transition and have your application respond to
an event, you can simply make a wrapper element and hook its events.

-}

import Html(Attribute)
import Html.Attributes(style,attribute)
import String(toUpper,toLower,left,dropLeft,join,split)
import List(map,foldl)

type alias CSSKey = String
type alias CSSValue = String

type alias JSEventAttribute = String

{-|
Change styles when the user hovers over an element. For example,
hover [("color","black","blue")] will render black text normally, but
blue text when the user hovers over the element.
-}
hover : List (CSSKey,CSSValue,CSSValue) -> List Attribute
hover = hover' []

{-|
Shorthand to add a list of static base styles.
-}
hover' : List (CSSKey,CSSValue) -> List (CSSKey,CSSValue,CSSValue) -> List Attribute
hover' = cssStateEffect ["onmouseout"] "onmouseover"

{-|
Change styles when the user pushes on element. For example,
pressure [("color","black","blue")] will render black text normally, but
blue text when the user pushes the mouse down on the element.
-}
pressure : List (CSSKey,CSSValue,CSSValue) -> List Attribute
pressure = pressure' []

{-|
Shorthand to add a list of static base styles.
-}
pressure' : List (CSSKey,CSSValue) -> List (CSSKey,CSSValue,CSSValue) -> List Attribute
pressure' = cssStateEffect ["onmouseup","onmouseout"] "onmousedown"

{-|
Change styles when the user focuses on element. For example,
pressure [("border-color","black","blue")] will render a black border
normally, but a blue border when the user focuses on the element.
-}
focus : List (CSSKey,CSSValue,CSSValue) -> List Attribute
focus = focus' []

{-|
Shorthand to add a list of static base styles.
-}
focus' : List (CSSKey,CSSValue) -> List (CSSKey,CSSValue,CSSValue) -> List Attribute
focus' = cssStateEffect ["onblur"] "onfocus"

{-|
Construct your own stateful effects by providing a list of JavaScript hooks
to indicate an inactive state, a hook to indicate the active state, static
styles, and a tuple-map for your dynamic styles.
-}
cssStateEffect : List JSEventAttribute -> JSEventAttribute -> List (CSSKey,CSSValue) -> List (CSSKey,CSSValue,CSSValue) -> List Attribute
cssStateEffect jsEventInactives jsEventActive baseState hoverState =
    let jsName cssAttr = (\s -> toLower (left 1 s) ++ dropLeft 1 s)
        <| join ""
        <| map (\s -> toUpper (left 1 s) ++ dropLeft 1 s)
        <| split "-" cssAttr in
    let toJS attrs = foldl (\(a,b) x -> x ++ "this.style." ++ jsName a ++ "='"++b++"';") "" attrs in
    [ style (baseState ++ map (\(a,b,_) -> (a,b)) hoverState)
    , attribute jsEventActive <| toJS <| map (\(a,_,c) -> (a,c)) hoverState]
    ++ map (\inactive -> attribute inactive <| toJS <| map (\(a,b,_) -> (a,b)) hoverState) jsEventInactives
