
alter table "public"."lucumaUserPreferences" add column "itcDetailsOpen" boolean
 not null default 'false';

alter table "public"."lucumaUserPreferences" add column "itcChartType" text
 not null default 's2n_chart';

alter table "public"."lucumaUserPreferences"
  add constraint "lucumaUserPreferences_itcChartType_fkey"
  foreign key ("itcChartType")
  references "public"."exploreChartType"
  ("id") on update cascade on delete cascade;

DROP table "public"."lucumaItcPlotPreferences";

alter table "public"."exploreChartType" rename to "ItcChartType";
