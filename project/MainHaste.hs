import Snake
import Haste
import Haste.Events
import Haste.DOM
import Haste.Graphics.Canvas
import System.Random


-- Main method that runs the program.
main = do canvas <- mkCanvas 300 -- Creates a canvas (the snake-field)
          appendChild documentBody canvas
          Just can <- getCanvas canvas

          -- inputBox that contains the last pressed direction. Used becouse
          -- you can not have global variables. This box is hided in the html
          keyInput <- newElem "input"
          setProp keyInput "value" defaultDir

          documentBody `onEvent` KeyDown $ \k -> do
            let value = getDirection k
            let currDir = value
            case length value of  0 -> return ()
                                  _ -> set keyInput [prop "value" =: value]

          g<-newStdGen
          renderGrid can keyInput grid startSnake (ranPos g 14 startSnake)

--Method that renders the grid. "This is where the magic happens". It calls on
--itself when the timer-event is fired.
--
--Inputs:
--  canvas = this is where it all is drawn
--  elem   = the inutfield that holds the direction
--  grid   = the grid that the snake is drawn on
--  snake  = the snake
--  pos    = the position of the coin
renderGrid :: Canvas -> Elem -> Grid -> Snake -> Pos -> IO()
renderGrid can input grid snake coinpos = do
    n <- getProp input "value"
    let dir = toString n

    -- moves the snake in a given direction
    let newSnake = moveSnake snake dir

    -- used to know where to add a snake-part if a coin is collected
    let snakeTail = getSnakeTail snake

    -- new grid with the new snake.
    let newGrid = refreshGrid grid newSnake coinpos

    -- Get what kind of collision and do action depending on what the snake
    -- collides with.
    let collisionState = collision newGrid newSnake
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
                                          let coinpos = ranPos g 14 gSnake

                                          -- render the cancas with the new grid
                                          render can $ drawGrid newGrid 0

                                          -- set a new timer, wait for xxx milliseconds
                                          -- and then call renderGrid again. (Inspo from "fallingballs" example)
                                          setTimer (Once 200) (renderGrid can input grid gSnake coinpos) >> return ()

                           CNothing -> do let newGrid = refreshGrid grid newSnake coinpos
                                          -- set a new timer, wait for xxx milliseconds
                                          -- and then call renderGrid again. (Inspo from "fallingballs" example)
                                          render can $ drawGrid newGrid 0
                                          setTimer (Once 200) (renderGrid can input grid newSnake coinpos) >> return ()



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
