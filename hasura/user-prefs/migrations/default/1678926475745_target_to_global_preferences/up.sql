
alter table "public"."lucumaUserPreferences" add column "showCatalog" boolean
 null default 'true';

alter table "public"."lucumaUserPreferences" add column "agsOverlay" boolean
 null default 'true';

alter table "public"."lucumaUserPreferences" add column "scienceOffsets" boolean
 null default 'true';

alter table "public"."lucumaUserPreferences" add column "acquisitionOffsets" boolean
 null default 'true';

alter table "public"."lucumaUserPreferences" add column "created_at" timestamptz
 null default now();

alter table "public"."lucumaUserPreferences" add column "updated_at" timestamptz
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
CREATE TRIGGER "set_public_lucumaUserPreferences_updated_at"
BEFORE UPDATE ON "public"."lucumaUserPreferences"
FOR EACH ROW
EXECUTE PROCEDURE "public"."set_current_timestamp_updated_at"();
COMMENT ON TRIGGER "set_public_lucumaUserPreferences_updated_at" ON "public"."lucumaUserPreferences" 
IS 'trigger to set value of column "updated_at" to current timestamp on row update';

alter table "public"."exploreTargetPreferences" drop column "agsCandidates" cascade;

alter table "public"."exploreTargetPreferences" drop column "agsOverlay" cascade;

alter table "public"."exploreTargetPreferences" drop column "scienceOffsets" cascade;

alter table "public"."exploreTargetPreferences" drop column "acquisitionOffsets" cascade;

alter table "public"."exploreTargetPreferences" drop column "fullScreen" cascade;

alter table "public"."lucumaUserPreferences" add column "fullScreen" boolean
 null default 'false';
