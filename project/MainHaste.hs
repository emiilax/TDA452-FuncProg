import Snake
import Haste
import Haste.Events
import Haste.DOM
import Haste.Graphics.Canvas
import System.Random
import Haste.Concurrent


-- Main method that runs the program.
main = do canvas <- mkCanvas (20*20) -- Creates a canvas (the snake-field)
          appendChild documentBody canvas
          Just can <- getCanvas canvas



          -- inputBox that contains the last pressed direction. Used becouse
          -- you can not have global variables. This box is hided in the html
          keyInput <- newElem "input"
          setProp keyInput "value" ""
          scoreText <- newElem "scoreText"

          sizeButton <- newElem "button"
          set sizeButton [ prop "innerHTML" =: "Change size" ]


          column documentBody [scoreText, sizeButton]

          let (Grid grid) = createGrid 20
          g<-newStdGen
          let coinpos = ranPos g (length grid - 1) startSnake
          render can $ drawGrid (Grid grid) 0


--          onEvent sizeButton Click $ \_ -> do s <- prompt "How many tiles (20-40)"
--                                              let n = read s :: Int



          documentBody `onEvent` KeyDown $ \k -> do
            let value = getDirection k
            let currDir = value
            case length value of  0 -> return ()
                                  _ -> set keyInput [prop "value" =: value]




          renderGrid can keyInput (Grid grid) startSnake coinpos scoreText

--Method that renders the grid. "This is where the magic happens". It calls on
--itself when the timer-event is fired.
--
--Inputs:
--  canvas = this is where it all is drawn
--  elem   = the inutfield that holds the direction
--  grid   = the grid that the snake is drawn on
--  snake  = the snake
--  pos    = the position of the coin
--  elem   = score text element that shows the current score
renderGrid :: Canvas -> Elem -> Grid -> Snake -> Pos -> Elem -> IO()
renderGrid can input grid snake coinpos scoreelem = do
    n <- getProp input "value"
    let dir = toString n

    writeLog (toString (dir == ""))
    case dir of "" -> do let newGrid = refreshGrid grid snake coinpos
                         render can $ drawGrid newGrid 0
                         setTimer (Once 200) (renderGrid can input newGrid snake coinpos scoreelem) >> return ()
                _  -> do

                        -- moves the snake in a given direction
                        let newSnake = moveSnake snake dir

                        -- used to know where to add a snake-part if a coin is collected
                        let snakeTail = getSnakeTail snake

                        -- new grid with the new snake.
                        let newGrid = refreshGrid grid newSnake coinpos

                        -- Get what kind of collision and do action depending on what the snake
                        -- collides with.
                        let collisionState = collision newGrid newSnake

                        set scoreelem [ prop "innerHTML" =: ("Score " ++ show (score newSnake))]
                        case collisionState of CWall    -> do clearChildren documentBody
                                                              alert "You lost"
                                                              main

                                               CSnake   -> do clearChildren documentBody
                                                              alert "You lost"
                                                              main

                                                              -- calculate new snake if coin collected
                                                              -- adds to the tail
                                               CCoin    -> do let gSnake = growSnake newSnake snakeTail

                                                              -- calculate new random pos for the new coin
                                                              g <- newStdGen
                                                              let (Grid g1) = grid
                                                              let coinpos = ranPos g (length g1 - 1) gSnake

                                                              -- new grid with the new snake.
                                                              let newGrid = refreshGrid grid gSnake coinpos
                                                              -- render the cancas with the new grid
                                                              render can $ drawGrid newGrid 0

                                                              -- set a new timer, wait for xxx milliseconds
                                                              -- and then call renderGrid again. (Inspo from "fallingballs" example)
                                                              setTimer (Once 200) (renderGrid can input newGrid gSnake coinpos scoreelem) >> return ()

                                               CNothing -> do let newGrid = refreshGrid grid newSnake coinpos
                                                              -- set a new timer, wait for xxx milliseconds
                                                              -- and then call renderGrid again. (Inspo from "fallingballs" example)
                                                              render can $ drawGrid newGrid 0
                                                              setTimer (Once 200) (renderGrid can input grid newSnake coinpos scoreelem) >> return ()



-- String containing the default direction
defaultDir :: String
defaultDir = "up"

-- method used for creating the canvas (Copied from examples given on homepage).
--Creates a square-canvas with a given sidelenght
mkCanvas :: Int -> IO Elem
mkCanvas sideLength = do
    canvas <- newElem "canvas"
    setStyle canvas "border" "1px solid black"
    setStyle canvas "backgroundColor" "white"
    set canvas
        [ prop "width"  =: show sideLength
        , prop "height" =: show sideLength
        ]

    return canvas


-- Method used to draw the grid on the canvas. uses a helpfuntion that that
-- fill tiles in a list.
drawGrid :: Grid -> Int -> Picture ()
drawGrid (Grid []) _     = return ()
drawGrid (Grid (x:xs)) n = do fillTiles x n
                              drawGrid (Grid xs) (n+20)


-- Method used to fill a tile in a given row and column. Uses a helpfuntion
-- filltiles' to draw the row. (We were trying to use map, but since we needed
-- the column and row we didn't find a way.)
fillTiles :: [Tile] -> Int -> Picture ()
fillTiles list = fillTiles' list 0

-- fills tiles in a row. fills with different colors an shapes depending on the
-- tile
fillTiles' :: [Tile] -> Int -> Int-> Picture ()
fillTiles' [] _  _   = return ()
fillTiles' (x:xs) row col = do
    let drow = fromIntegral row
    let dcol = fromIntegral col
    case x of Empty     -> color (RGB 255 255 255) $ stroke $ rect (drow, dcol) (drow+20, dcol+20)
              SnakeBody -> color (RGB 255 0 0)     $ fill   $ circle (drow+10, dcol+10) 10
              Coin      -> color (RGB 255 215 0)   $ fill   $ circle (drow+10, dcol+10) 10
    fillTiles' xs (row+20) col


-- Method used to parse the key pressed. If the key pressed is not "up", "down",
-- "right" or "left" it just returns an empty string.
getDirection :: KeyData -> String
getDirection (KeyData keyCode _ _ _ _) | keyCode == 38 = "up"
                                       | keyCode == 39 = "right"
                                       | keyCode == 40 = "down"
                                       | keyCode == 37 = "left"
                                       | otherwise = ""

--copied from examples
appendChildren :: Elem -> [Elem] -> IO ()
appendChildren parent children = sequence_ [appendChild parent c | c <- children]

--copied from examples
wrapDiv :: Elem -> IO Elem
wrapDiv e = do
   d <- newElem "div"
   appendChild d e
   return d

--Copied from given examples
row :: Elem -> [Elem] -> IO ()
row = appendChildren

--Copied from given examples
column :: Elem -> [Elem] -> IO ()
column parent children = do
   cs <- sequence [wrapDiv c | c <- children]
   appendChildren parent cs
