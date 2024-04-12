
alter table "public"."lucumaUserPreferences" add column "elevationPlotElevationVisible" boolean
 not null default 'true';

alter table "public"."lucumaUserPreferences" add column "elevationPlotParalacticAngleVisible" boolean
 not null default 'false';

alter table "public"."lucumaUserPreferences" rename column "elevationPlotParalacticAngleVisible" to "elevationPlotParallacticAngleVisible";

alter table "public"."lucumaUserPreferences" add column "elevationPlotSkyBrightnessVisible" boolean
 not null default 'true';

alter table "public"."lucumaUserPreferences" add column "elevationPlotLunarElevationVisiblee" boolean
 not null default 'false';

alter table "public"."lucumaUserPreferences" rename column "elevationPlotLunarElevationVisiblee" to "elevationPlotLunarElevationVisible";
