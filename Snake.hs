import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Control.Concurrent


type Coord = (Int, Int)

data Dir = DirUp | DirDown | DirLeft | DirRight

data State = State { 
	 stSnake :: IORef [Coord]
	,stDir :: IORef Dir
}

mkState :: IO State
mkState = do
	snake <- newIORef $ reverse [(0,0), (1,0), (2,0), (2,1), (2,2), (2,3)]
	dir <- newIORef DirUp
	return $ State {stSnake = snake, stDir = dir}


main :: IO ()
main = do
	(progName,_) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered]
	createWindow "Snake"

	state <- mkState

	idleCallback $= Just (idle state)
	displayCallback $= (display state)
	keyboardMouseCallback $= Just (keyboard state)
	mainLoop


display :: State -> IO ()
display state = do 
	clear [ColorBuffer]
	loadIdentity
	--scale 1 1 (1::GLfloat)
	snakeCoords <- get (stSnake state)
	mapM_ displaySnakeCoord snakeCoords

	swapBuffers

displaySnakeCoord :: Coord -> IO ()
displaySnakeCoord (x,y) = preservingMatrix $ do
	translate $ Vector3 xx yy (0::GLfloat)
	color $ Color3 ((xx+1)/2) ((yy+1)/2) (1::GLfloat)
	cube (1/bpw::GLfloat)
		where
			bpw = 20
			scale = (\a -> (-1.0) + ((a/bpw) * 2) + 1/(bpw*2/2))
			xx = scale $ fromIntegral x
			yy = scale $ fromIntegral y


idle :: State -> IO ()
idle state = do
	dir <- get (stDir state)
	let (x, y) = dirToCoord dir
	updateSnake state x y
	postRedisplay Nothing
	threadDelay delay
		where
			mps = 10
			delay = 1000000 `div` mps

-- This should use Data.Sequence or Data.Deque
updateSnake :: State -> Int -> Int -> IO ()
updateSnake state dx dy = do
	let snake = stSnake state
	coords <- get snake
	let ((lx,ly):_) = coords
	snake $=! ((lx+dx,ly+dy):(take ((length coords)-1) coords))

dirToCoord :: Dir -> Coord
dirToCoord DirUp = (0, 1)
dirToCoord DirDown = (0, -1)
dirToCoord DirLeft = (-1, 0)
dirToCoord DirRight = (1, 0)


keyboard :: State -> Key -> KeyState -> t1 -> t2 -> IO ()
keyboard state (Char 'w') Down _ _ = (stDir state) $=! (DirUp)
keyboard state (Char 'a') Down _ _ = (stDir state) $=! (DirLeft)
keyboard state (Char 's') Down _ _ = (stDir state) $=! (DirDown)
keyboard state (Char 'd') Down _ _ = (stDir state) $=! (DirRight)
keyboard _ _ _ _ _ = return ()


cube :: GLfloat -> IO ()
cube w = do 
	renderPrimitive Quads $ do
		vertex $ Vertex3 w w w
		vertex $ Vertex3 w w (-w)
		vertex $ Vertex3 w (-w) (-w)
		vertex $ Vertex3 w (-w) w
		vertex $ Vertex3 w w w
		vertex $ Vertex3 w w (-w)
		vertex $ Vertex3 (-w) w (-w)
		vertex $ Vertex3 (-w) w w
		vertex $ Vertex3 w w w
		vertex $ Vertex3 w (-w) w
		vertex $ Vertex3 (-w) (-w) w
		vertex $ Vertex3 (-w) w w
		vertex $ Vertex3 (-w) w w
		vertex $ Vertex3 (-w) w (-w)
		vertex $ Vertex3 (-w) (-w) (-w)
		vertex $ Vertex3 (-w) (-w) w
		vertex $ Vertex3 w (-w) w
		vertex $ Vertex3 w (-w) (-w)
		vertex $ Vertex3 (-w) (-w) (-w)
		vertex $ Vertex3 (-w) (-w) w
		vertex $ Vertex3 w w (-w)
		vertex $ Vertex3 w (-w) (-w)
		vertex $ Vertex3 (-w) (-w) (-w)
		vertex $ Vertex3 (-w) w (-w)