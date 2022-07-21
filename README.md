<h1 align="left">
<img src="./logo.png" height=25 width=35 />
slacker
</h1>

A Haskell library for building Slack apps particularly in [Socket Mode](https://api.slack.com/apis/connections/socket). It takes a little inspiration from Slack's own Bolt SDK family.

`slacker` is currently lacking comprehensive types for the large amount of objects from the Slack Events API, Web API, and Block Kit. The official Bolt SDK for TypeScript is a better place to find up-to-date static types for Slack objects. This library provides Haskell record types for common use cases, otherwise users are encouraged to parse what they need from `Data.Aeson.Value` payloads using lenses (like with `microlens-aeson`).

## Features

* Supports concurrent socket connections
* Handles Slack connection refreshes automatically
* Structured logging
* Graceful shutdown of socket mode
* Support for Web API JSON requests
* File uploading
* Format interactive content using Block Kit
* Follows the latest Slack API conventions and avoids deprecated objects

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
