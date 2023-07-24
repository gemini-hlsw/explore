
alter table "public"."lucumaUserPreferences" alter column "elevationPlotTime" drop not null;
ALTER TABLE "public"."lucumaUserPreferences" ALTER COLUMN "elevationPlotTime" drop default;

alter table "public"."lucumaUserPreferences" alter column "elevationPlotRange" drop not null;
ALTER TABLE "public"."lucumaUserPreferences" ALTER COLUMN "elevationPlotRange" drop default;

alter table "public"."lucumaUserPreferences" alter column "showCatalog" drop not null;

alter table "public"."lucumaUserPreferences" alter column "agsOverlay" drop not null;

alter table "public"."lucumaUserPreferences" alter column "scienceOffsets" drop not null;

alter table "public"."lucumaUserPreferences" alter column "acquisitionOffsets" drop not null;

alter table "public"."lucumaUserPreferences" alter column "fullScreen" drop not null;

alter table "public"."lucumaUserPreferences" alter column "aladinMouseScroll" drop not null;
