
CREATE TABLE "public"."explorePlotRange" ("id" text NOT NULL, PRIMARY KEY ("id") , UNIQUE ("id"));

INSERT INTO "public"."explorePlotRange"("id") VALUES (E'night');

INSERT INTO "public"."explorePlotRange"("id") VALUES (E'semester');

ALTER TABLE "public"."lucumaUserPreferences" ALTER COLUMN "elevationPlotRange" TYPE text;

update "lucumaUserPreferences" set "elevationPlotRange"='night' where "elevationPlotRange"='NIGHT';
update "lucumaUserPreferences" set "elevationPlotRange"='semester' where "elevationPlotRange"='SEMESTER';

alter table "public"."lucumaUserPreferences"
  add constraint "lucumaUserPreferences_elevationPlotRange_fkey"
  foreign key ("elevationPlotRange")
  references "public"."explorePlotRange"
  ("id") on update cascade on delete cascade;

ALTER TABLE "public"."lucumaUserPreferences" ALTER COLUMN "elevationPlotTime" TYPE text;

update "lucumaUserPreferences" set "elevationPlotTime"='ut' where "elevationPlotTime"='UT';
update "lucumaUserPreferences" set "elevationPlotTime"='sidereal' where "elevationPlotRange"='SIDEREAL';
update "lucumaUserPreferences" set "elevationPlotTime"='site' where "elevationPlotRange"='SITE';

CREATE TABLE "public"."explorePlotTime" ("id" text NOT NULL, PRIMARY KEY ("id") , UNIQUE ("id"));

INSERT INTO "public"."explorePlotTime"("id") VALUES (E'ut');

INSERT INTO "public"."explorePlotTime"("id") VALUES (E'sidereal');

INSERT INTO "public"."explorePlotTime"("id") VALUES (E'site');

update "lucumaUserPreferences" set "elevationPlotTime"='ut' where "elevationPlotTime"='UT';
update "lucumaUserPreferences" set "elevationPlotTime"='sidereal' where "elevationPlotTime"='SIDEREAL';
update "lucumaUserPreferences" set "elevationPlotTime"='site' where "elevationPlotTime"='SITE';

alter table "public"."lucumaUserPreferences"
  add constraint "lucumaUserPreferences_elevationPlotTime_fkey"
  foreign key ("elevationPlotTime")
  references "public"."explorePlotTime"
  ("id") on update cascade on delete cascade;

drop type elevation_plot_range;
drop type elevation_plot_time;
