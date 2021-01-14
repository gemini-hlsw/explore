#!/bin/bash

gq https://userprefs-hasura-staging.herokuapp.com/v1/graphql --introspect >common/src/main/resources/graphql/schemas/UserPreferencesDB.graphql
