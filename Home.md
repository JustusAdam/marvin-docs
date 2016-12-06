Welcome to marvins documentation.

Marvin is a Haskell framework for creating chat bots, inspired by [Hubot](https://hubot.github.com).
Marvin aims to recreate the ease of use and straightforward API of Hubot, and combine it with the safety guarantees and purity of Haskell and the higher efficiency.

If you are new to marvin you may want to begin with the [getting started](getting-started) section or the [how to script with marvin](scripting) section to get a sense of how scripting works with marvin.

## A quick snippet of code

```Haskell
module MyScript where

import Marvin.Prelude

script :: IsAdapter a => ScriptInit a
script = defineScript "my-script" $ do
    hear "sudo (.+)" $ do
        match <- getMatch

        let thing = match !! 1

        reply $ "All right, i'll do " ++ thing
    
    respond "open the (\\w+) door" $ do
        match <- getMatch
        let door = match !! 1
        openDoor door
        send $ printf "Door %s opened" door
    
    respond "what is in file (\\w+)\\??" $ do
        match <- getMatch 
        let file = match !! 1

        contents <- liftIO $ readFile file

        send contents
```