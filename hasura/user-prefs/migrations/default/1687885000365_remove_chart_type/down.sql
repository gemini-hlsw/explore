
-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- drop type itc_chart_type;

alter table "public"."lucumaItcPlotPreferences" drop constraint "lucumaItcPlotPreferences_chartType_fkey";

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- update "lucumaItcPlotPreferences" set "chartType"='signal_chart' where "chartType"='SIGNAL_CHART';

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- update "lucumaItcPlotPreferences" set "chartType"='s2n_chart' where "chartType"='S2N_CHART';

alter table "public"."lucumaItcPlotPreferences" alter column "chartType" set default 'S2N_CHART'::itc_chart_type;
ALTER TABLE "public"."lucumaItcPlotPreferences" ALTER COLUMN "chartType" TYPE USER-DEFINED;

DELETE FROM "public"."exploreChartType" WHERE "id" = 's2n_chart';

DELETE FROM "public"."exploreChartType" WHERE "id" = 'signal_chart';

DROP TABLE "public"."exploreChartType";
