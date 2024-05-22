
alter table "public"."lucumaGridLayoutPositions" add column "created_at" timestamptz
 null default now();

alter table "public"."lucumaGridLayoutPositions" add column "updated_at" timestamptz
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
CREATE TRIGGER "set_public_lucumaGridLayoutPositions_updated_at"
BEFORE UPDATE ON "public"."lucumaGridLayoutPositions"
FOR EACH ROW
EXECUTE PROCEDURE "public"."set_current_timestamp_updated_at"();
COMMENT ON TRIGGER "set_public_lucumaGridLayoutPositions_updated_at" ON "public"."lucumaGridLayoutPositions" 
IS 'trigger to set value of column "updated_at" to current timestamp on row update';

ALTER TABLE "public"."lucumaGridLayoutPositions" ALTER COLUMN "updated_at" TYPE timestamp;

ALTER TABLE "public"."lucumaGridLayoutPositions" ALTER COLUMN "created_at" TYPE timestamp;
