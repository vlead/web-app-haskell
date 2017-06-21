#!/bin/bash

# to build and run web-app-haskell
# with initialised database

stack build
rm sqlite.db
stack exec web-app-haskell
