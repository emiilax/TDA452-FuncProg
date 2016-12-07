import Haste
import Haste.Events
import Haste.DOM


main = do text <- newTextElem "You have not pressed any arrow yet"
          input1 <- newElem "input"
          input2 <- newElem "input"
          setProp input2 "value" (show 1)
          row documentBody [input1, input2]


          documentBody `onEvent` KeyDown $ \k -> do
            let value = getDirection k
            case (length value) of  0 -> return ()
                                    _ -> set input1 [prop "value" =: value]

          incInput2 input2

          --onEvent txt KeyUp $ \keycode -> do
          --  alert keycode
-- inspo from FallingBalls
incInput2 :: Elem -> IO()
incInput2 input2 = do s <- getProp input2 "value"
                      let n = read s
                      setProp input2 "value" (show (n+1))
                      setTimer (Once 1000) (incInput2 input2) >> return ()

getDirection :: KeyData -> String
getDirection (KeyData keyCode _ _ _ _) | keyCode == 38 = "UP"
                                       | keyCode == 39 = "RIGHT"
                                       | keyCode == 40 = "DOWN"
                                       | keyCode == 37 = "LEFT"
                                       | otherwise = ""

appendChildren :: Elem -> [Elem] -> IO ()
appendChildren parent children = sequence_ [appendChild parent c | c <- children]

row :: Elem -> [Elem] -> IO ()
row = appendChildren


wrapDiv :: Elem -> IO Elem
wrapDiv e = do
    d <- newElem "div"
    appendChild d e
    return d

column :: Elem -> [Elem] -> IO ()
column parent children = do
    cs <- sequence [wrapDiv c | c <- children]
    appendChildren parent cs
