
CREATE TABLE "public"."exploreChartType" ("id" text NOT NULL, PRIMARY KEY ("id") , UNIQUE ("id"));

INSERT INTO "public"."exploreChartType"("id") VALUES (E'signal_chart');

INSERT INTO "public"."exploreChartType"("id") VALUES (E's2n_chart');

ALTER TABLE "public"."lucumaItcPlotPreferences" ALTER COLUMN "chartType" TYPE text;
alter table "public"."lucumaItcPlotPreferences" alter column "chartType" set default 's2n_chart';

update "lucumaItcPlotPreferences" set "chartType"='s2n_chart' where "chartType"='S2N_CHART';

update "lucumaItcPlotPreferences" set "chartType"='signal_chart' where "chartType"='SIGNAL_CHART';

alter table "public"."lucumaItcPlotPreferences"
  add constraint "lucumaItcPlotPreferences_chartType_fkey"
  foreign key ("chartType")
  references "public"."exploreChartType"
  ("id") on update cascade on delete cascade;

drop type itc_chart_type;
