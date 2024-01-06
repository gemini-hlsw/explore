
-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- DROP table "public"."lucumaTarget";

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- DROP table "public"."exploreTargetPreferences";

ALTER TABLE "public"."exploreAsterismPreferences" ALTER COLUMN "viewOffsetQ" TYPE integer;

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."exploreAsterismPreferences" add column "viewOffsetQ" integer
--  null default '0';

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."exploreAsterismPreferences" add column "viewOffsetP" bigint
--  null default '0';

ALTER TABLE "public"."exploreAsterismPreferences" ALTER COLUMN "saturation" drop default;

ALTER TABLE "public"."exploreAsterismPreferences" ALTER COLUMN "brightness" drop default;

alter table "public"."lucumaAsterism" drop constraint "lucumaAsterism_pkey";
alter table "public"."lucumaAsterism"
    add constraint "lucumaAsterism_pkey"
    primary key ("prefId");

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaAsterism" add column "targetId" text
--  not null;

alter table "public"."lucumaAsterism" alter column "targetId" drop not null;
alter table "public"."lucumaAsterism" add column "targetId" int4;

alter table "public"."lucumaAsterism" drop constraint "lucumaAsterism_pkey";
alter table "public"."lucumaAsterism"
    add constraint "lucumaAsterism_pkey"
    primary key ("prefId", "targetId");

alter table "public"."exploreAsterismPreferences" drop constraint "exploreAsterismPreferences_userId_fkey";

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."exploreAsterismPreferences" add column "userId" text
--  not null;

alter table "public"."lucumaAsterism" drop constraint "lucumaAsterism_prefId_fkey";

DROP TABLE "public"."exploreAsterismPreferences";

DROP TABLE "public"."lucumaAsterism";
