alter table "public"."lucumaUserPreferences" alter column "elevationPlotRange" drop not null;
ALTER TABLE "public"."lucumaUserPreferences" ALTER COLUMN "elevationPlotRange" drop default;
