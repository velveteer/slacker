<h1 align="left">
<img src="./logo.png" height=25 width=35 />
slacker
</h1>

A Haskell library for building Slack apps particularly in [Socket Mode](https://api.slack.com/apis/connections/socket). It takes a little inspiration from Slack's own Bolt SDK family.

The official [JavaScript Bolt SDK](https://github.com/slackapi/bolt-js/tree/main/src/types) is a better place to find up-to-date static types for the hundreds of Slack API JSON objects. This library provides some Haskell record types for commonly used objects, otherwise users are encouraged to handle `Data.Aeson.Value` payloads using lenses (e.g. `microlens-aeson` or `lens-aeson`).

## Features

* Concurrent socket connections
* Automatic connection refresh
* Structured logging
* Graceful shutdown
* Web API JSON requests
* File uploading
* Block Kit DSL

## Usage

```haskell
import Slacker

main :: IO ()
main = do
  let cfg = defaultSlackConfig
          & setApiToken "YOUR_API_TOKEN"
          & setAppToken "YOUR_APP_TOKEN"
  runSocketMode cfg handler

handler :: SlackConfig -> SocketModeEvent -> IO ()
handler cfg = \case
  Event AppMention{..} -> postMessage cfg $ toChannel channel "yes?"
  _ -> pure ()
```

See the [example app](./example-app) for more.

## Roadmap

- [x] Socket Mode Daemon
- [x] Post Messages and Responses via Web API
- [x] Interactive events in socket mode
- [x] Slash commands in socket mode
- [ ] Support ack response payloads
- [ ] Implement More Event Types
- [ ] Implement More Block Types
- [ ] Hackage release

## Slides

Slides for a talk I once gave about this library: https://gist.github.com/velveteer/4e28900021a31c8e75f1a7bb813f0e5f
