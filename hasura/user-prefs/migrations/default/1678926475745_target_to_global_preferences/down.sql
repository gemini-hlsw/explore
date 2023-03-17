
-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "fullScreen" boolean
--  null default 'false';

alter table "public"."exploreTargetPreferences" alter column "fullScreen" set default false;
alter table "public"."exploreTargetPreferences" alter column "fullScreen" drop not null;
alter table "public"."exploreTargetPreferences" add column "fullScreen" bool;

alter table "public"."exploreTargetPreferences" alter column "acquisitionOffsets" set default true;
alter table "public"."exploreTargetPreferences" alter column "acquisitionOffsets" drop not null;
alter table "public"."exploreTargetPreferences" add column "acquisitionOffsets" bool;

alter table "public"."exploreTargetPreferences" alter column "scienceOffsets" set default true;
alter table "public"."exploreTargetPreferences" alter column "scienceOffsets" drop not null;
alter table "public"."exploreTargetPreferences" add column "scienceOffsets" bool;

alter table "public"."exploreTargetPreferences" alter column "agsOverlay" set default true;
alter table "public"."exploreTargetPreferences" alter column "agsOverlay" drop not null;
alter table "public"."exploreTargetPreferences" add column "agsOverlay" bool;

alter table "public"."exploreTargetPreferences" alter column "agsCandidates" set default true;
alter table "public"."exploreTargetPreferences" alter column "agsCandidates" drop not null;
alter table "public"."exploreTargetPreferences" add column "agsCandidates" bool;

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "updated_at" timestamptz
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
-- CREATE TRIGGER "set_public_lucumaUserPreferences_updated_at"
-- BEFORE UPDATE ON "public"."lucumaUserPreferences"
-- FOR EACH ROW
-- EXECUTE PROCEDURE "public"."set_current_timestamp_updated_at"();
-- COMMENT ON TRIGGER "set_public_lucumaUserPreferences_updated_at" ON "public"."lucumaUserPreferences"
-- IS 'trigger to set value of column "updated_at" to current timestamp on row update';

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "created_at" timestamptz
--  null default now();

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "acquisitionOffsets" boolean
--  null default 'true';

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "scienceOffsets" boolean
--  null default 'true';

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "agsOverlay" boolean
--  null default 'true';

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "showCatalog" boolean
--  null default 'true';
