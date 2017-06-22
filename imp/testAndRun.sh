#!/bin/bash

stack setup 
rm sqlite.db
stack test
stack exec web-app-haskell
