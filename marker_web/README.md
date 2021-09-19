# MarkerWeb

**Important note**: as for now `marker_web` imports `marker` as
a dependency using a relative path to the `marker` directory,
so make sure you've downloaded the whole repository.

To start the `marker_web`:

  * Install dependencies with `mix deps.get`
  * Install Node.js dependencies with `npm install` inside the `assets` directory
  * Start the server with `mix phx.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.

The site contains a text area, where you can write Markdown documents,
which will get transformed into a HTML displayed below. Enjoy!
