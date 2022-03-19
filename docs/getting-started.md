# Getting Started

I assume you're here because you want to write a React app using PureScript! Good, good, because that's what this guide is all about.

Before we begin you'll need to install [Node.js](https://nodejs.org/). If you aren't sure what editor to use, try [VSCode](https://code.visualstudio.com/). It's got great PureScript language support via the [PureScript IDE](https://marketplace.visualstudio.com/items?itemName=nwolverson.ide-purescript) extension.

Done? Ok, we're going to set up this project from scratch so you have a basic idea of how all the parts work together. Don't worry, it won't take long!

## Creating a PureScript web project

Let's create a directory for the project.

```sh
mkdir purs-react-app && cd purs-react-app
```

Initialize `npm` (comes with Node.js). The command below creates a `package.json` file (and a `package-lock.json` file, but that's for `npm` to manage) to track our JavaScript dependencies (i.e. React) and dev tooling (PureScript, etc). We'll also put some useful scripts in here later.

```sh
npm init -y
```

Install PureScript and Spago (project management tool for PureScript).

```sh
npm i -D purescript spago
```

Next, use Spago to set up the PureScript files we need to get started. Spago is installed locally via `npm`, so we need to prefix this command with `npx` to run it.

```sh
npx spago init
```

You should now have a few new directories and files. `packages.dhall` defines the PureScript package source the project will use. The default will be fine. `spago.dhall` is where our PureScript dependencies are defined. Let's add `react-basic-hooks` to that list using Spago. We'll grab `react-basic-dom` too, since we'll need it soon.

```sh
npx spago install react-basic-hooks react-basic-dom
```

We also need to install React and ReactDOM, since `react-basic-hooks` and `react-basic-dom` rely on them, respectively.

```sh
npm i -S react react-dom
```

Ok, we're almost done.. We're now able to write React code using PureScript, but the only thing we can do with it is compile it to JavaScript and stare at it! Which is actually a good time and you'll learn a lot about PureScript in doing so, but let's save that for a future guide.

What we really want next is to start a server with a basic HTML skeleton so we can actually run our React app!

We can set this up pretty quick with existing bundlers like Parcel. Let's install it.

```sh
npm i -D parcel
```

Parcel's a bit picky about project setup. We're building an app, not a JavaScript library. Remove the following line from the `package.json` file to appease it.. :pray:

```json
  "main": "index.js",
```

Now we need an entry-point HTML file to point Parcel at. Create the file `src/index.html` with the following content.

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>PureScript React App</title>
  <script async type="module" src="index.js"></script>
</head>
<body>
  <div id="root"></div>
</body>
</html>
```

Now that HTML file is pointing to a mysterious `index.js` file! Create `src/index.js` with this content.

```js
import { main } from "../output/Main";

main();
```

More mysteries! What's this "Main" file with a "main" function inside!?

It's our PureScript! Let's take a look at the `src/Main.purs` file Spago created for us earlier. It probably looks like this.

```purs
module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"
```

Let's change that `log` call at the bottom so it logs something with deep, personal significance. We'll be that much more proud of our work once we see it in action.

```purs
main :: Effect Unit
main = do
  log "guuu"
```

## Building and running the app

Building is a two-step process. The PureScript compiler (via Spago) handles the PureScript, emitting it as JavaScript in the `output/` folder. Parcel handles everything from that point on, serving (in dev mode), bundling, and minifying the JavaScript side.

The first thing you should always do is build your PureScript once, like so.

```sh
npx spago build
```

Then you can run Parcel in "serve" mode to see it all running in your browser (`ctrl + c` to quit).

```sh
npx parcel serve src/index.html
```

Parcel will give you a URL for your dev server. Open it and take a look at the console. If everything went as planned, you should feel an inspiring sense of accomplishment! Good job! What? You wanted more than an empty white web page? We're getting there, soon, soon!

If you change your HTML or JavaScript files (including by rebuilding the PureScript code), you'll see those changes live-reload in your browser. Nice.

Let's create a production build.

```sh
npx parcel build --dist-dir dist src/index.html
```

This command directs the bundled output to the `dist/` folder. Add this folder to the `.gitignore` Spago made for us earlier!

```sh
echo "dist/" >> .gitignore
```

Ignore Parcel's cache dir as well.

```sh
echo ".parcel-cache/" >> .gitignore
```

You can deploy that `dist/` directory to a server, or upload it to S3 or GitHub Pages. The possibilities are endless!

For example, you can use `npx` to install and run the `serve` tool, a quick and easy way to spin up a server in a given directory (`ctrl + c` to quit).

```sh
npx serve dist
```

Parcel can do quite a bit for you. I recommend [learning more about it](https://parceljs.org/docs/) (or whatever bundler or dev server you choose) when you have a chance.

## Making "future you's" life easier

Remembering all these commands is a pain. Let's make some shortcuts.

You can add scripts to your `package.json` and invoke them with `npm`. These scripts don't need the `npx` prefix.

```json
{
  "name": "purs-react-app",
  "version": "1.0.0",
  "description": "",
  "scripts": {
    "test": "spago test",
    "start": "spago build && parcel serve src/index.html",
    "build": "spago build && parcel build --dist-dir dist src/index.html"
  },
  "keywords": [],
  "author": "",
  "license": "ISC",
  "devDependencies": {
    "parcel": "^2.0.1",
    "purescript": "^0.14.5",
    "spago": "^0.20.3"
  },
  "dependencies": {
    "react": "^17.0.2",
    "react-dom": "^17.0.2"
  }
}
```

The `start` and `test` scripts are special, you can run them like this: `npm start` or `npm test`

Any other commands you add need the "run" prefix: `npm run build`

We haven't talked about `spago test` yet. You can learn more about general PureScript testing elsewhere. Testing PureScript React components will be a separate guide at some point.

## Rendering your first component

Since this is your first component, I'll write it for you. Don't worry, there's an entire guide on how to write your own components. I'll send you there next!

Copy this content into `src/App/Pages/Home.purs`. You can read it if you like, but don't sweat the details.

```purs
module App.Pages.Home where

import Prelude

import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React

type HomeProps = Unit

mkHome :: Component HomeProps
mkHome = do
  component "Home" \_props -> React.do

    counter /\ setCounter <- useState 0

    pure $ DOM.div
      { children:
          [ DOM.h1_ [ DOM.text "Home" ]
          , DOM.p_ [ DOM.text "Try clicking the button!" ]
          , DOM.button
              { onClick: capture_ do
                  setCounter (_ + 1)
              , children:
                  [ DOM.text "Clicks: "
                  , DOM.text (show counter)
                  ]
              }
          ]
      }
```

If you're already familiar with writing React components in JavaScript and you squint this will probably look familiar!

Now we need to replace the content of `src/Main.purs` so it renders our new component.

```purs
module Main where

import Prelude

import App.Pages.Home (mkHome)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  root <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  case root of
    Nothing ->
      throw "Root element not found."
    Just r  -> do
      home <- mkHome
      render (home unit) r
```

Once you're done, run `npm start`. Spago's going to complain that we're now using additional modules we didn't explicitly depend on. Go ahead and run the command it suggests. It should look like this (don't forget to prefix with `npx`).

```sh
npx spago install exceptions maybe web-dom web-html
```

Now run `npm start` again and open the link.

If you see the click counting button you've succeeded! You're ready to head to the [beginner guide](beginner.md) to learn how to build your own components! Good luck!
