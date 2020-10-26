#!/bin/bash

gq https://lucuma-odb-staging.herokuapp.com/odb --introspect >common/src/main/resources/graphql/schemas/ObservationDB.graphql