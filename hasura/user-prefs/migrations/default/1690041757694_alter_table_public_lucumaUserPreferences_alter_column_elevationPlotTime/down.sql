alter table "public"."lucumaUserPreferences" alter column "elevationPlotTime" drop not null;
ALTER TABLE "public"."lucumaUserPreferences" ALTER COLUMN "elevationPlotTime" drop default;
