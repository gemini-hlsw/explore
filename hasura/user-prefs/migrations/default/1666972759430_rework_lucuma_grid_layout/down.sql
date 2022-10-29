
CREATE TYPE public.grid_layout_area AS ENUM (
    'observations',
    'targets'
);

alter table "public"."lucumaGridLayoutPositions" drop constraint "lucumaGridLayoutPositions_section_fkey";

ALTER TABLE "public"."lucumaGridLayoutPositions" ALTER COLUMN "section" TYPE USER-DEFINED;

DELETE FROM "public"."lucumaGridLayoutId" WHERE "id" = 'targets';

DELETE FROM "public"."lucumaGridLayoutId" WHERE "id" = 'observations';

DROP TABLE "public"."lucumaGridLayoutId";
