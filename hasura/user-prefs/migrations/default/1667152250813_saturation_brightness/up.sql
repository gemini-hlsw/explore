
alter table "public"."lucumaTargetPreferences" add column "saturation" integer
 null default '100';

alter table "public"."lucumaTargetPreferences" add column "brightness" integer
 null default '100';

alter table "public"."lucumaTargetPreferences" add constraint "sat_brightness_range" check (saturation >= 0 AND saturation <= 100 AND brightness >= 0 AND brightness <= 100);

alter table "public"."lucumaTargetPreferences" add column "created_at" timestamptz
 null default now();

alter table "public"."lucumaTargetPreferences" add column "updated_at" timestamptz
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
CREATE TRIGGER "set_public_lucumaTargetPreferences_updated_at"
BEFORE UPDATE ON "public"."lucumaTargetPreferences"
FOR EACH ROW
EXECUTE PROCEDURE "public"."set_current_timestamp_updated_at"();
COMMENT ON TRIGGER "set_public_lucumaTargetPreferences_updated_at" ON "public"."lucumaTargetPreferences" 
IS 'trigger to set value of column "updated_at" to current timestamp on row update';

alter table "public"."lucumaTargetPreferences" rename to "exploreTargetPreferences";
