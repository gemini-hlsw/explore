
alter table "public"."exploreTargetPreferences" add column "scienceOffsets" boolean
 null default 'true';

alter table "public"."exploreTargetPreferences" add column "acquisitionOffsets" boolean
 null default 'true';
