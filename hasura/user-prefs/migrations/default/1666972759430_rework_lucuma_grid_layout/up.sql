
CREATE TABLE "public"."lucumaGridLayoutId" ("id" text NOT NULL, PRIMARY KEY ("id") , UNIQUE ("id"));

INSERT INTO "public"."lucumaGridLayoutId"("id") VALUES (E'observations');

INSERT INTO "public"."lucumaGridLayoutId"("id") VALUES (E'targets');

ALTER TABLE "public"."lucumaGridLayoutPositions" ALTER COLUMN "section" TYPE text;

alter table "public"."lucumaGridLayoutPositions"
  add constraint "lucumaGridLayoutPositions_section_fkey"
  foreign key ("section")
  references "public"."lucumaGridLayoutId"
  ("id") on update cascade on delete cascade;

drop type grid_layout_area cascade;
