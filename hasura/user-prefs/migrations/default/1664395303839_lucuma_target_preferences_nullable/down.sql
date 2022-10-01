
alter table "public"."lucuma_target_preferences" alter column "viewOffsetQ" set not null;

alter table "public"."lucuma_target_preferences" alter column "viewOffsetP" set not null;

alter table "public"."lucuma_target_preferences" alter column "fovDec" set not null;
alter table "public"."lucuma_target_preferences" alter column "fovDec" set default '1000000'::bigint;

alter table "public"."lucuma_target_preferences" alter column "fovRA" set not null;
alter table "public"."lucuma_target_preferences" alter column "fovRA" set default '1000000'::bigint;

alter table "public"."lucuma_target_preferences" alter column "agsOverlay" set default 'false';

alter table "public"."lucuma_target_preferences" alter column "agsCandidates" set default 'false';

alter table "public"."lucuma_target_preferences" alter column "agsCandidates" set default 'false';

alter table "public"."lucuma_target_preferences" alter column "fullScreen" set not null;

alter table "public"."lucuma_target_preferences" alter column "agsOverlay" set not null;

alter table "public"."lucuma_target_preferences" alter column "agsCandidates" set not null;
