
CREATE TABLE "public"."lucumaLineOfSight" ("id" text NOT NULL, PRIMARY KEY ("id") , UNIQUE ("id"));

INSERT INTO "public"."lucumaLineOfSight"("id") VALUES (E'rv');

INSERT INTO "public"."lucumaLineOfSight"("id") VALUES (E'z');

INSERT INTO "public"."lucumaLineOfSight"("id") VALUES (E'cz');

CREATE TABLE "public"."lucumaTarget" ("targetId" text NOT NULL, "lineOfSightMotion" text NOT NULL, PRIMARY KEY ("targetId") , UNIQUE ("targetId"));

alter table "public"."lucumaLineOfSight" rename to "lucumaLineOfSightMotion";

alter table "public"."lucumaTarget"
  add constraint "lucumaTarget_lineOfSightMotion_fkey"
  foreign key ("lineOfSightMotion")
  references "public"."lucumaLineOfSightMotion"
  ("id") on update restrict on delete restrict;

alter table "public"."lucumaTarget" add column "userId" text
 not null;

BEGIN TRANSACTION;
ALTER TABLE "public"."lucumaTarget" DROP CONSTRAINT "lucumaTarget_pkey";

ALTER TABLE "public"."lucumaTarget"
    ADD CONSTRAINT "lucumaTarget_pkey" PRIMARY KEY ("targetId", "userId");
COMMIT TRANSACTION;

alter table "public"."lucumaTarget"
  add constraint "lucumaTarget_userId_fkey"
  foreign key ("userId")
  references "public"."lucumaUser"
  ("userId") on update restrict on delete restrict;
