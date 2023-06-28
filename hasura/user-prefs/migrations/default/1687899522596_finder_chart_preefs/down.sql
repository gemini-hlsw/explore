
-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- alter table "public"."exploreFinderChart" add column "inverted" boolean
--  not null default 'false';

alter table "public"."exploreFinderChart" drop constraint "exploreFinderChart_observationId_fkey";

DROP TABLE "public"."exploreFinderChart";
