
alter table "public"."lucumaUserPreferences" rename column "elevationPlotLunarElevationVisible" to "elevationPlotLunarElevationVisiblee";

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "elevationPlotLunarElevationVisiblee" boolean
--  not null default 'false';

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "elevationPlotSkyBrightnessVisible" boolean
--  not null default 'true';

alter table "public"."lucumaUserPreferences" rename column "elevationPlotParallacticAngleVisible" to "elevationPlotParalacticAngleVisible";

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "elevationPlotParalacticAngleVisible" boolean
--  not null default 'false';

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaUserPreferences" add column "elevationPlotElevationVisible" boolean
--  not null default 'true';
