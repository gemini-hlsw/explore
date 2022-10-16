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
