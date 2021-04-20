#!/bin/bash

gq https://userprefs-hasura-staging.herokuapp.com/v1/graphql --introspect >common-graphql/src/main/resources/graphql/schemas/UserPreferencesDB.graphql
