#!/bin/bash

gq https://lucuma-odb-development.herokuapp.com/odb --introspect >common-graphql/src/main/resources/graphql/schemas/ObservationDB.graphql
