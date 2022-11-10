
CREATE TABLE "public"."tmpTimingWindows" ("id" serial NOT NULL, "startsOn" timestamptz NOT NULL, "forever" boolean, "closeOn" timestamptz, "remainOpenFor" int4, "repeatPeriod" integer, "repeatForever" boolean, "repatTimes" integer, PRIMARY KEY ("id") );COMMENT ON TABLE "public"."tmpTimingWindows" IS E'Temporary';

alter table "public"."tmpTimingWindows" rename column "repatTimes" to "repeatTimes";

INSERT INTO "public"."lucumaGridLayoutId"("id") VALUES (E'constraints');

SET timezone TO 'UTC';
