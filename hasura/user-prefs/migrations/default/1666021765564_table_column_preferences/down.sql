
alter table "public"."lucumaTableColumnPreferences" drop constraint "lucumaTableColumnPreferences_sorting_fkey";

ALTER TABLE "public"."lucumaTableColumnPreferences" ALTER COLUMN "sorting" TYPE boolean;

ALTER TABLE "public"."lucumaSortDirection" ALTER COLUMN "id" TYPE character varying;

DELETE FROM "public"."lucumaSortDirection" WHERE "id" = 'asc';

DELETE FROM "public"."lucumaSortDirection" WHERE "id" = 'desc';

DROP TABLE "public"."lucumaSortDirection";

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaTableColumnPreferences" add column "updated_at" timestamptz
--  null default now();
--
-- CREATE OR REPLACE FUNCTION "public"."set_current_timestamp_updated_at"()
-- RETURNS TRIGGER AS $$
-- DECLARE
--   _new record;
-- BEGIN
--   _new := NEW;
--   _new."updated_at" = NOW();
--   RETURN _new;
-- END;
-- $$ LANGUAGE plpgsql;
-- CREATE TRIGGER "set_public_lucumaTableColumnPreferences_updated_at"
-- BEFORE UPDATE ON "public"."lucumaTableColumnPreferences"
-- FOR EACH ROW
-- EXECUTE PROCEDURE "public"."set_current_timestamp_updated_at"();
-- COMMENT ON TRIGGER "set_public_lucumaTableColumnPreferences_updated_at" ON "public"."lucumaTableColumnPreferences"
-- IS 'trigger to set value of column "updated_at" to current timestamp on row update';

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaTableColumnPreferences" add column "created_at" timestamptz
--  null default now();

ALTER TABLE "public"."lucumaTableColumnPreferences" ALTER COLUMN "columnId" TYPE text;

ALTER TABLE "public"."lucumaTableColumnPreferences" ALTER COLUMN "tableId" TYPE text;

ALTER TABLE "public"."lucumaTableColumnPreferences" ALTER COLUMN "userId" TYPE text;

alter table "public"."lucumaTableColumnPreferences" drop constraint "lucumaTableColumnPreferences_userId_fkey";

alter table "public"."lucumaTableColumnPreferences" rename column "userId" to "user_id";

alter table "public"."lucumaTableColumnPreferences" drop constraint "lucumaTableColumnPreferences_pkey";
alter table "public"."lucumaTableColumnPreferences"
    add constraint "lucumaTableColumnPreferences_pkey"
    primary key ("columnId", "tableId");

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaTableColumnPreferences" add column "user_id" text
--  not null;

DROP TABLE "public"."lucumaTableColumnPreferences";

alter table "public"."lucumaTableIds" rename to "lucumaTargetIds";

DELETE FROM "public"."lucumaTargetIds" WHERE "id" = 'spectroscopy_modes';

DELETE FROM "public"."lucumaTargetIds" WHERE "id" = 'asterism_targets';

DELETE FROM "public"."lucumaTargetIds" WHERE "id" = 'constraints_summary';

DELETE FROM "public"."lucumaTargetIds" WHERE "id" = 'target_summary';

DROP TABLE "public"."lucumaTargetIds";
