Welcome to marvins documentation.

Marvin is a Haskell framework for creating chat bots, inspired by [Hubot](https://hubot.github.com).
Marvin aims to recreate the ease of use and straightforward API of Hubot, and combine it with the safety guarantees and purity of Haskell and the higher efficiency.

If you are new to marvin you may want to get started with the [installation instructions](installing) and/or [how to script with marvin](scripting).

## A quick snippet of code

```Haskell
{-# LANGUAGE NoImplicitPrelude #-}
module MyScript where

import Marvin.Prelude

script :: IsAdapter a => ScriptInit a
script = defineScript "my-script" $ do
    hear "I|i can't stand this (\w+)" $ do
        match <- getMatch

        let thing = match `indexEx` 1

        reply $ "I'm sorry to tell you but you'll have to do " ++ thing
    
    respond "open the (\w+) door" $ do
        match <- getMatch
        let door = match `indexEx` 1
        openDoor door
        send $ format "Door {} opened" [door]
    
    respond "what is in file (\w+)" $ do
        match <- getMatch 
        let file = match `indexEx` 1

        liftIO $ readFile file

        send file
```