Configuration for marvin is written in the [configurator](https://hackage.haskell.com/package/configurator) syntax.

## System config

Configuration pertaining to the system itself is stored under the "bot" key.

```
bot {
    name = "my-bot"
    logging = "INFO"
    adapter = "slack-rtm"
}
```

| Key | default | Usage |
|-|-|
| `name` | `"marvin"` | Bot name (for logging and which string `respond` triggers on) |
| `logging` | `ERROR` | Verbosity of the logging |
| `adapter` | optional, defaults to `"slack-rtm"` | Adapter to use in he main file. (Only used by preprocesor, see [marvin-pp](../marvin-pp))

## Script config

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

## Adapter config

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