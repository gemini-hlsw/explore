
alter table "public"."lucuma_target_preferences" alter column "agsCandidates" drop not null;

alter table "public"."lucuma_target_preferences" alter column "agsOverlay" drop not null;

alter table "public"."lucuma_target_preferences" alter column "fullScreen" drop not null;

alter table "public"."lucuma_target_preferences" alter column "agsCandidates" set default 'true';

alter table "public"."lucuma_target_preferences" alter column "agsCandidates" set default 'true';

alter table "public"."lucuma_target_preferences" alter column "agsOverlay" set default 'true';

alter table "public"."lucuma_target_preferences" alter column "fovRA" set default '900000000'::bigint;
alter table "public"."lucuma_target_preferences" alter column "fovRA" drop not null;

alter table "public"."lucuma_target_preferences" alter column "fovDec" set default '900000000'::bigint;
alter table "public"."lucuma_target_preferences" alter column "fovDec" drop not null;

alter table "public"."lucuma_target_preferences" alter column "viewOffsetP" drop not null;

alter table "public"."lucuma_target_preferences" alter column "viewOffsetQ" drop not null;
