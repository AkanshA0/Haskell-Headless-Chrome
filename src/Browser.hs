module Browser
   ( main
   ) where
import System.Process
import System.Directory

startBrowser :: String -> IO ()
startBrowser path = do
    setCurrentDirectory path
    system "chrome --remote-debugging-port=9222 --disable-gpu --headless https://www.google.com" >>= \exitCode -> print exitCode

main ::IO ()
main = do
    let p = "C:/Program Files/Google/Chrome/Application"
    startBrowser p
     