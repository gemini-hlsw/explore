#!/bin/bash

gq https://user-prefs-master.herokuapp.com/v1/graphql --introspect >common/src/clue/resources/UserPreferencesDB.graphql
