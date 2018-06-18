# stopping-time

This web app demostrates a specific case of the optimal stopping problem.

## Build and Run

### Prerequisites

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

There are some configuration files **YOU MUST** provide. See [wiki][3]
for more details.

### Server
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

### Client
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
[3]: https://github.com/zelinf/stopping-time/wiki/Missing-Files
