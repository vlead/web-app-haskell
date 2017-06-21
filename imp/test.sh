#!/bin/bash


# to setup and build web-app-haskell
# and to run test suite

stack setup
stack build
rm sqlite.db
stack test
