# User preferences

explore stores a set of user preferences outside of the main odb api server. It is reserved
for settings which would only impact the UI, e.g. zoom of the aladin display, range of plots, etc.
In general, anything that is of no interest to the API users can go to the user prefs database

For the user preferences db we are using hasura to provide an automatic graphql api for simple
odb tables.

This is not considered critical and we would take the freedom of deleting the preferences if it
makes the transition simpler.

We use the graphql naming convention for hasura, thus we need to set the following enn variables:

- HASURA_GRAPHQL_EXPERIMENTAL_FEATURES naming_convention
- HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION graphql-default

For more details see:
https://hasura.io/docs/latest/schema/postgres/naming-convention/

## Environments

We have four environments with a respective db and heroku app:

- master
- development
- staging
- production

## Console

You can spin up a server that servers a console app for an environment by using the CLI from the `hasura/user-prefs` directory. For example:

```
hasura console --endpoint https://user-prefs-master.herokuapp.com
```

## Copy the database

To copy the database between instances you can create a backup on the postgress page under the
durability tab. From there pick the backup id you want to copy and then upload to the destination
environment with the command:

```
heroku pg:backups:restore {original_env}::{backup_id} {DATABASE_URL} --app {new_env}
```

Where:

    * original_env is the name of the environment you are pulling the backup from
    * backup_id is the ID of the backup, found on the Durability tab of the Heroku Postgres database page
    * DATABASE_URL is the env var that Heroku generated for the new database
    * new_env is the new application ID that has the DATABASE_URL attached to it

## Update hasura

Checkout the original code from:
https://git.heroku.com/user-prefs-staging.git

then push this via git to the url of each environment

## Migrations

Migrations are being handled via hasura tools with the migration and metadata files at

[hasura](hasura/user-prefs)

_Note_ unset NODE_OPTIONS before running `hasura console`

Changes should be first tested on user-prefs-master to be later applied to

- user-prefs-development
- user-prefs-staging
- user-prefs

```
hasura migrate apply --endpoint https://gpp-prefs-dev.lucuma.xyz
hasura metadata apply --endpoint https://gpp-prefs-dev.lucuma.xyz
```

If you update a table that is used as a GraphQL Enum, such as `lucumaGridLayoutPositions`,
the GraphQL enums might be out of sync. So, it is probably safest to always run:

```
hasura metadata reload --endpoint https://gpp-prefs-dev.lucuma.xyz
```

For more information visit:
https://hasura.io/docs/latest/migrations-metadata-seeds/migrations-metadata-setup/
