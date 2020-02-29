## Description
The program uses parser combinators to parse a dpkg status file and builds a dictionary containing the name, description, dependencies and reverse dependencies of the packages listed in the file. [Scotty](https://hackage.haskell.org/package/scotty) is used to make it a web-application. The index page of the web-application shows a list of all packages where every entry in the list is a link to a page containing additional information about that package. The program takes the path to which status file to read as a command line argument.

## Building and running
The program can be built using Cabal or Stack by running:

> cabal new-build

or

> stack build

The program can also be built and run in one command by running:

> cabal new-run PreAssignment path/to/status/file

or

> stack run PreAssignment path/to/status/file

### On Windows
To build on Windows using the Haskell Platform make sure you completed step 2 of the [installation instructions](https://www.haskell.org/platform/).

## Heroku
The program is deployed on Heroku at: http://preassignment-vm.herokuapp.com/

I used [this](https://github.com/mfine/heroku-buildpack-stack) buildpack to deploy the project to Heroku.
