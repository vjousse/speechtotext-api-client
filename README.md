# Speech to text API client


A frontend client to the [Speech to text API backend](https://github.com/vjousse/speechtotext-api).

> 🌳  built with [elm-spa](https://elm-spa.dev)

## dependencies

This project requires the latest LTS version of [Node.js](https://nodejs.org/)

```bash
npm install -g elm elm-spa
```

## running locally

```bash
npm install
npm run dev  # starts this app at http:/localhost:1234
```

### other commands

```bash
elm-spa add    # add a new page to the application
elm-spa build  # production build
elm-spa watch  # runs build as you code (without the server)
```

## Proxy calls to the API

    mitmproxy --mode reverse:http://localhost:8000 -p 5001

## Screenshots

![File list](/screenshots/speechtotext_screenshot.png)
![Upload File](/screenshots/speechtotext_screenshot_2.png)
