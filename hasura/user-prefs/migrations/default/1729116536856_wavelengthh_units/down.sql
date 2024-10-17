
alter table "public"."lucumaUserPreferences" drop constraint "lucumaUserPreferences_wavelengthUnits_fkey";

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "wavelengthUnits" text
--  not null default 'nanometers';

DELETE FROM "public"."lucumaWavelengthUnits" WHERE "id" = 'nanometers';

DELETE FROM "public"."lucumaWavelengthUnits" WHERE "id" = 'micrometers';

DROP TABLE "public"."lucumaWavelengthUnits";
