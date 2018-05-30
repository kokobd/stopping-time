# stopping-time

This web app demostrates a specific case of optimal stopping problem.

Suppose there are n cards, you can randomly select ONE card from
those cards each turn, and put it back before the next turn.
Each card has a value. You should find a set, so that when we stop
at its elements, we can get maximum average profit.
In other words, each time we get a card, we check whether it's
in the set. If it's in, we stop now. Otherwise, we continue to
the next turn.

For example, If the values are `[1, 2, 3, 4, 5, 0]`, with a cost
`[1, 1, 1, 1, 1, 1]`, we should stop at `[2, 3, 4, 5, 0]`. Note
that `0` in values means whenever we get this card, we lose all
previos profit and must quit now.

## Build and Run

You need to install the latest version of [`npm`][2] and [`stack`][1].
Also a Linux-like environment is assumed, as we use some simple shell
scripts.

It may take a long time if it's your first time to run the
following commands, as a bunch of tools and dozens of packages
will be downloaded and compiled. Don't worry, That's all automatic,
just keep an active network connection and have a cup of coffee.

*Special Note If You are In China* GFW (Great Firewall) sometimes blocks Stackage, a
public package repository of Haskell. You may need to connect to
VPN or some sort of proxies, etc, if you receive network errors.

### server
```sh
cd server
# If you want a regular build
stack build
# Or, a production build
stack build --flag stopping-time-server:production
# Now we can run the server
stack exec server
# The server executable is actually located at
echo $(stack path --local-install-root)/bin/server
```

### client
```sh
cd client
npm install
# Production build
npm run build
# Or, if you want to run webpack-dev-server
npm run
```
You may visit `localhost:9000` if the dev server is running.

[1]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
[2]: https://www.npmjs.com/get-npm
