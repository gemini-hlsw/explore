
CREATE TABLE "public"."exploreFinderChart" ("observationId" text NOT NULL, "attachmentId" text NOT NULL, "rotate" int4 NOT NULL, "scaleX" integer NOT NULL, "scaleY" integer NOT NULL, "flipX" boolean NOT NULL, "flipY" boolean NOT NULL, PRIMARY KEY ("observationId","attachmentId") );

alter table "public"."exploreFinderChart"
  add constraint "exploreFinderChart_observationId_fkey"
  foreign key ("observationId")
  references "public"."lucumaObservation"
  ("observationId") on update restrict on delete restrict;

alter table "public"."exploreFinderChart" add column "inverted" boolean
 not null default 'false';
