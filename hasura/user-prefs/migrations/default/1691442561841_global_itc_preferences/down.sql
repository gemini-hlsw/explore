
alter table "public"."ItcChartType" rename to "exploreChartType";

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- DROP table "public"."lucumaItcPlotPreferences";

alter table "public"."lucumaUserPreferences" drop constraint "lucumaUserPreferences_itcChartType_fkey";

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "itcChartType" text
--  not null default 's2n_chart';

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "itcDetailsOpen" boolean
--  not null default 'false';
