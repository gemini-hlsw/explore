
-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- drop type elevation_plot_range;
-- drop type elevation_plot_time;

alter table "public"."lucumaUserPreferences" drop constraint "lucumaUserPreferences_elevationPlotTime_fkey";

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- update "lucumaUserPreferences" set "elevationPlotTime"='ut' where "elevationPlotTime"='UT';
-- update "lucumaUserPreferences" set "elevationPlotTime"='sidereal' where "elevationPlotTime"='SIDEREAL';
-- update "lucumaUserPreferences" set "elevationPlotTime"='site' where "elevationPlotTime"='SITE';

DELETE FROM "public"."explorePlotTime" WHERE "id" = 'site';

DELETE FROM "public"."explorePlotTime" WHERE "id" = 'sidereal';

DELETE FROM "public"."explorePlotTime" WHERE "id" = 'ut';

DROP TABLE "public"."explorePlotTime";

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- update "lucumaUserPreferences" set "elevationPlotTime"='ut' where "elevationPlotTime"='UT';
-- update "lucumaUserPreferences" set "elevationPlotTime"='sidereal' where "elevationPlotRange"='SIDEREAL';
-- update "lucumaUserPreferences" set "elevationPlotTime"='site' where "elevationPlotRange"='SITE';

ALTER TABLE "public"."lucumaUserPreferences" ALTER COLUMN "elevationPlotTime" TYPE USER-DEFINED;

alter table "public"."lucumaUserPreferences" drop constraint "lucumaUserPreferences_elevationPlotRange_fkey";

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- update "lucumaUserPreferences" set "elevationPlotRange"='night' where "elevationPlotRange"='NIGHT';
-- update "lucumaUserPreferences" set "elevationPlotRange"='semester' where "elevationPlotRange"='SEMESTER';

ALTER TABLE "public"."lucumaUserPreferences" ALTER COLUMN "elevationPlotRange" TYPE USER-DEFINED;

DELETE FROM "public"."explorePlotRange" WHERE "id" = 'semester';

DELETE FROM "public"."explorePlotRange" WHERE "id" = 'night';

DROP TABLE "public"."explorePlotRange";
