case collisionState of CWall -> do clearChildren documentBody
                                   alert ("You lost. Score " ++ show (score newSnake))
                                   main
                       CSnake -> do clearChildren documentBody
                                    alert ("Stop hitting yourself! Score " ++ show (score newSnake))
                                    main
                       CCoin    -> do render can $ drawGrid newGrid 0
                                      g <- newStdGen
                                      let grownSnake = growSnake newSnake coinpos
                                      let random = ranPos g 14 grownSnake
                                      let newerGrid = refreshGrid newGrid grownSnake random
                                      render can $ drawGrid newerGrid 0
                                      setTimer (Once 300) (renderGrid grownSnake random)
                                      >> return ()
                       CNothing -> do render can $ drawGrid newGrid 0
                                      setTimer (Once 300) (renderGrid newSnake coinpos) >> return ()
