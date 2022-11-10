
-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- SET timezone TO 'UTC';

DELETE FROM "public"."lucumaGridLayoutId" WHERE "id" = 'constraints';

alter table "public"."tmpTimingWindows" rename column "repeatTimes" to "repatTimes";

DROP TABLE "public"."tmpTimingWindows";
