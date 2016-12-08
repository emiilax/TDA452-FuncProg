import Haste
import Haste.DOM

main = do
    text <- newTextElem "This is some text."
    appendChild documentBody text
