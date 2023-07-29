myPutStr :: String -> IO ()
myPutStr [] = return ()
myPutStr (x:xs) = do
    putChar x
    putStr xs