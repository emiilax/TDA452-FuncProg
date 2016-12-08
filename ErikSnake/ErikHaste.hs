import Snake
import Haste
import Haste.Events
import Haste.DOM
import Haste.Graphics.Canvas
import Control.Concurrent

main = do text <- newTextElem "You have not pressed any arrow yet"
          canvas <- mkCanvas 300 300
          appendChild documentBody canvas
          Just can <- getCanvas canvas

          --render can (drawGrid grid 0)

          input1 <- newElem "input"
          setProp input1 "value" "right"
          input2 <- newElem "input"
          setProp input2 "value" (show 1)
          row documentBody [input1, input2]

          let renderGrid snake = do
              s <- getProp input1 "value"
              let n = toString s
              let newSnake = moveSnake snake n
              let newGrid = refreshGrid grid theSnake
              render can $ drawGrid (refreshGrid grid newSnake) 0
              setTimer (Once 500) (renderGrid newSnake) >> return ()

          documentBody `onEvent` KeyDown $ \k -> do
            let value = getDirection k
            let currDir = value
            case (length value) of  0 -> return ()
                                    _ -> set input1 [prop "value" =: value]

          renderGrid theSnake
          incInput2 input2

          --onEvent txt KeyUp $ \keycode -> do
          --  alert keycode


hold :: String
hold = "Waiting"

mkCanvas :: Int -> Int -> IO Elem
mkCanvas width height = do
    canvas <- newElem "canvas"
    setStyle canvas "border" "1px solid black"
    setStyle canvas "backgroundColor" "white"
    set canvas
        [ prop "width"  =: show width
        , prop "height" =: show height
        ]
    return canvas


{-grid :: [[Int]]
grid = [[0,0,0,0,0,0],[1,1,1,0,0,0],[0,0,0,0,0,0]]

grid1 :: [[Int]]
grid1 = [[0,0,0,0,0,0],[1,1,1,1,1,1],[0,0,0,0,0,0]]-}

tile :: Double -> Shape ()
tile x = rect (x,x) (x+20, x+20)

drawGrid :: Grid -> Int -> Picture ()
drawGrid (Grid []) _     = return ()
drawGrid (Grid (x:xs)) n = do fillTiles x n
                              drawGrid (Grid xs) (n+20)


fillTiles :: [Tile] -> Int -> Picture ()
fillTiles list n = fillTiles' list 0 n

fillTiles' :: [Tile] -> Int -> Int-> Picture ()
fillTiles' [] _  _   = return ()
fillTiles' (x:xs) row col = do
    let drow = fromIntegral row
    let dcol = fromIntegral col
    case x of Empty  -> stroke $ rect (drow, dcol) (drow+20, dcol+20)
              SnakeBody -> color (RGB 255 215 0) $ fill $ circle (drow+10, dcol+10) 10
              Coin -> color (RGB 255 215 0) $ fill $ circle (drow+10, dcol+10) 10
    fillTiles' xs (row+20) col




theSnake :: Snake
theSnake = (Add (3,3) (Add (3,2) (Add (3,1) End)))

------------- BÖÖÖÖÖÖÖÖS ---------------

twoSnowMenInABox :: Picture ()
twoSnowMenInABox = do
    fill   $ tile 100
    stroke $ tile 200
    stroke $ rect (50,10) (250,150)

{-
incInput1 :: Elem -> IO()
incInput1 input2 = do s <- getProp input1 "value"
                      setProp input2 "value" (s)
                      setTimer (Once 1000) (incInput2 input2) >> return ()-}


-- inspo from FallingBalls
incInput2 :: Elem -> IO()
incInput2 input2 = do s <- getProp input2 "value"
                      let n = read s
                      setProp input2 "value" (show (n+1))
                      setTimer (Once 1000) (incInput2 input2) >> return ()

getDirection :: KeyData -> String
getDirection (KeyData keyCode _ _ _ _) | keyCode == 38 = "down"
                                       | keyCode == 39 = "right"
                                       | keyCode == 40 = "up"
                                       | keyCode == 37 = "left"
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
