#!/bin/bash

gq https://user-prefs-master.herokuapp.com/v1/graphql --introspect >explore/src/clue/resources/UserPreferencesDB.graphql
