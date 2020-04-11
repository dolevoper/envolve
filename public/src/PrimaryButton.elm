module PrimaryButton exposing (primaryButton)

import Color.OneDark as Colors
import Element as El exposing (Element)
import Element.Background as Bg
import Element.Border as Border
import Element.Input as Input


primaryButton : Maybe msg -> String -> Element msg
primaryButton msg text =
    Input.button
        [ El.centerX
        , Bg.gradient
            { angle = 0
            , steps = [ Colors.white, El.rgb 1 1 1 ]
            }
        , El.padding 10
        , Border.rounded 5
        , Border.shadow
            { blur = 3
            , color = Colors.commentGrey
            , offset = ( 0, 0 )
            , size = 1
            }
        ]
        { label = El.text text
        , onPress = msg
        }
