<h1 align="left">
<img src="./logo.png" height=25 width=35 />
slacker
</h1>

A Haskell library for working with the Slack API, particularly in [Socket Mode](https://api.slack.com/apis/connections/socket). It takes a little inspiration from Slack's own Bolt SDK family.

This is a work-in-progress, notably it's missing comprehensive types for the large amount of objects from the Slack Events API, Web API, and Block Kit. The base framework for working with socket mode is available, but users currently have to deal with `Data.Aeson.Value` event payloads until more types with `Data.Aeson.FromJSON` instances are available. The roadmap should give some idea of what the finished state could look like.

## Features

* Supports concurrent socket connections
* Handles Slack connection refreshes automatically
* Structured logging
* Graceful shutdown of socket mode
* Format content using Block Kit
* Basic support for Web API calls
* Follows the latest Slack API conventions and avoids deprecated objects

## Usage

```haskell
module Main where
import Slacker

main :: IO ()
main = do
  let cfg = defaultSlackConfig
          & setApiToken "YOUR_API_TOKEN"
          & setAppToken "YOUR_APP_TOKEN"
  runSocketMode cfg handler

handler :: SlackConfig -> SocketModeEvent -> IO ()
handler cfg = \case
  Event "app_mention" evt -> print evt
  _ -> pure ()
```

See the example app for more.

## Roadmap

- [x] Socket Mode Daemon
- [x] Post Messages via Web API
- [ ] Interactive events in socket mode
- [ ] Slash commands in socket mode
- [ ] Implement More Event Types
- [ ] Implement More Block Types
- [ ] Hackage release
