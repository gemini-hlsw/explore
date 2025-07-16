
alter table "public"."lucumaTarget" drop constraint "lucumaTarget_userId_fkey";

alter table "public"."lucumaTarget" drop constraint "lucumaTarget_pkey";
alter table "public"."lucumaTarget"
    add constraint "lucumaTarget_pkey"
    primary key ("targetId");

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."lucumaTarget" add column "userId" text
--  not null;

alter table "public"."lucumaTarget" drop constraint "lucumaTarget_lineOfSightMotion_fkey";

alter table "public"."lucumaLineOfSightMotion" rename to "lucumaLineOfSight";

DROP TABLE "public"."lucumaTarget";

DELETE FROM "public"."lucumaLineOfSight" WHERE "id" = 'cz';

DELETE FROM "public"."lucumaLineOfSight" WHERE "id" = 'z';

DELETE FROM "public"."lucumaLineOfSight" WHERE "id" = 'rv';

DROP TABLE "public"."lucumaLineOfSight";

