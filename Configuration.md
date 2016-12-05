Configuration for marvin is written in the [configurator](https://hackage.haskell.com/package/configurator) syntax.

Configuration pertaining to the bot is stored under the "bot" key.

```
bot {
    name = "my-bot"
    logging = "INFO"
    adapter = "slack-rtm"
}
```

By default each script has access to a configuration stored under `script.<script-id>`.
And of course these scripts can have nested config groups.

```
bot {
    name = "my-bot"
}

script {
    script-1 {
        some-string = "foo"
        some-int = 1337
        bome-bool = true
    }
    script 2 {
        nested-group {
            val = false
        }
        name = "Trump"
        capable = false
    }
}
```

Configuration pertaining to a particular adapter is stored under `adapter.<adapter-name>`

```
bot {
    name = "my-bot"
    logging = "INFO"
}
adapter {
    slack-rtm {
        token = "eofk"
    }
}
``` 