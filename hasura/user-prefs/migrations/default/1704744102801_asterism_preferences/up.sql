
CREATE TABLE "public"."lucumaAsterism" ("prefId" integer NOT NULL, "targetId" integer NOT NULL, PRIMARY KEY ("prefId","targetId") );

CREATE TABLE "public"."exploreAsterismPreferences" ("id" serial NOT NULL, "brightness" integer, "saturation" integer, "fovRA" bigint DEFAULT 900000000, "fovDec" bigint DEFAULT 900000000, PRIMARY KEY ("id") );

alter table "public"."lucumaAsterism"
  add constraint "lucumaAsterism_prefId_fkey"
  foreign key ("prefId")
  references "public"."exploreAsterismPreferences"
  ("id") on update cascade on delete cascade;

alter table "public"."exploreAsterismPreferences" add column "userId" text
 not null;

alter table "public"."exploreAsterismPreferences"
  add constraint "exploreAsterismPreferences_userId_fkey"
  foreign key ("userId")
  references "public"."lucumaUser"
  ("userId") on update cascade on delete cascade;

BEGIN TRANSACTION;
ALTER TABLE "public"."lucumaAsterism" DROP CONSTRAINT "lucumaAsterism_pkey";

ALTER TABLE "public"."lucumaAsterism"
    ADD CONSTRAINT "lucumaAsterism_pkey" PRIMARY KEY ("prefId");
COMMIT TRANSACTION;

alter table "public"."lucumaAsterism" drop column "targetId" cascade;

alter table "public"."lucumaAsterism" add column "targetId" text
 not null;

BEGIN TRANSACTION;
ALTER TABLE "public"."lucumaAsterism" DROP CONSTRAINT "lucumaAsterism_pkey";

ALTER TABLE "public"."lucumaAsterism"
    ADD CONSTRAINT "lucumaAsterism_pkey" PRIMARY KEY ("prefId", "targetId");
COMMIT TRANSACTION;

alter table "public"."exploreAsterismPreferences" alter column "brightness" set default '100';

alter table "public"."exploreAsterismPreferences" alter column "saturation" set default '100';

alter table "public"."exploreAsterismPreferences" add column "viewOffsetP" bigint
 null default '0';

alter table "public"."exploreAsterismPreferences" add column "viewOffsetQ" integer
 null default '0';

ALTER TABLE "public"."exploreAsterismPreferences" ALTER COLUMN "viewOffsetQ" TYPE int8;

DROP table "public"."exploreTargetPreferences";

DROP table "public"."lucumaTarget";
