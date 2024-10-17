
CREATE TABLE "public"."lucumaWavelengthUnits" ("id" text NOT NULL, PRIMARY KEY ("id") , UNIQUE ("id"));

INSERT INTO "public"."lucumaWavelengthUnits"("id") VALUES (E'micrometers');

INSERT INTO "public"."lucumaWavelengthUnits"("id") VALUES (E'nanometers');

alter table "public"."lucumaUserPreferences" add column "wavelengthUnits" text
 not null default 'nanometers';

alter table "public"."lucumaUserPreferences"
  add constraint "lucumaUserPreferences_wavelengthUnits_fkey"
  foreign key ("wavelengthUnits")
  references "public"."lucumaWavelengthUnits"
  ("id") on update restrict on delete restrict;
