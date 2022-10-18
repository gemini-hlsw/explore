
CREATE TABLE "public"."lucumaTargetIds" ("id" text NOT NULL, PRIMARY KEY ("id") , UNIQUE ("id"));

INSERT INTO "public"."lucumaTargetIds"("id") VALUES (E'target_summary');

INSERT INTO "public"."lucumaTargetIds"("id") VALUES (E'constraints_summary');

INSERT INTO "public"."lucumaTargetIds"("id") VALUES (E'asterism_targets');

INSERT INTO "public"."lucumaTargetIds"("id") VALUES (E'spectroscopy_modes');

alter table "public"."lucumaTargetIds" rename to "lucumaTableIds";

CREATE TABLE "public"."lucumaTableColumnPreferences" ("tableId" text NOT NULL, "columnId" text NOT NULL, "visible" boolean NOT NULL, "sorting" boolean, PRIMARY KEY ("tableId","columnId") , FOREIGN KEY ("tableId") REFERENCES "public"."lucumaTableIds"("id") ON UPDATE cascade ON DELETE cascade);

alter table "public"."lucumaTableColumnPreferences" add column "user_id" text
 not null;

BEGIN TRANSACTION;
ALTER TABLE "public"."lucumaTableColumnPreferences" DROP CONSTRAINT "lucumaTableColumnPreferences_pkey";

ALTER TABLE "public"."lucumaTableColumnPreferences"
    ADD CONSTRAINT "lucumaTableColumnPreferences_pkey" PRIMARY KEY ("columnId", "tableId", "user_id");
COMMIT TRANSACTION;

alter table "public"."lucumaTableColumnPreferences" rename column "user_id" to "userId";

alter table "public"."lucumaTableColumnPreferences"
  add constraint "lucumaTableColumnPreferences_userId_fkey"
  foreign key ("userId")
  references "public"."lucumaUser"
  ("userId") on update cascade on delete cascade;

ALTER TABLE "public"."lucumaTableColumnPreferences" ALTER COLUMN "userId" TYPE varchar;

ALTER TABLE "public"."lucumaTableColumnPreferences" ALTER COLUMN "tableId" TYPE varchar;

ALTER TABLE "public"."lucumaTableColumnPreferences" ALTER COLUMN "columnId" TYPE varchar;

alter table "public"."lucumaTableColumnPreferences" add column "created_at" timestamptz
 null default now();

alter table "public"."lucumaTableColumnPreferences" add column "updated_at" timestamptz
 null default now();

CREATE OR REPLACE FUNCTION "public"."set_current_timestamp_updated_at"()
RETURNS TRIGGER AS $$
DECLARE
  _new record;
BEGIN
  _new := NEW;
  _new."updated_at" = NOW();
  RETURN _new;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER "set_public_lucumaTableColumnPreferences_updated_at"
BEFORE UPDATE ON "public"."lucumaTableColumnPreferences"
FOR EACH ROW
EXECUTE PROCEDURE "public"."set_current_timestamp_updated_at"();
COMMENT ON TRIGGER "set_public_lucumaTableColumnPreferences_updated_at" ON "public"."lucumaTableColumnPreferences" 
IS 'trigger to set value of column "updated_at" to current timestamp on row update';

CREATE TABLE "public"."lucumaSortDirection" ("id" varchar NOT NULL, PRIMARY KEY ("id") , UNIQUE ("id"));

INSERT INTO "public"."lucumaSortDirection"("id") VALUES (E'desc');

INSERT INTO "public"."lucumaSortDirection"("id") VALUES (E'asc');

ALTER TABLE "public"."lucumaSortDirection" ALTER COLUMN "id" TYPE text;

ALTER TABLE "public"."lucumaTableColumnPreferences" ALTER COLUMN "sorting" TYPE text;

alter table "public"."lucumaTableColumnPreferences"
  add constraint "lucumaTableColumnPreferences_sorting_fkey"
  foreign key ("sorting")
  references "public"."lucumaSortDirection"
  ("id") on update cascade on delete cascade;
