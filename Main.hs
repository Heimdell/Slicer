
import System.IO

----

import ConsoleInteraction

main = do
    stdin `hSetBuffering` NoBuffering
    stdin `hSetEcho`      False
    
    runInteraction interact'