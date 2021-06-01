#!/bin/bash

gq https://lucuma-odb-no-sharing.herokuapp.com/odb --introspect >common-graphql/src/main/resources/graphql/schemas/ObservationDB.graphql
