#!/bin/bash

gq https://user-prefs-development.herokuapp.com/v1/graphql --introspect >common-queries/src/clue/resources/UserPreferencesDB.graphql
