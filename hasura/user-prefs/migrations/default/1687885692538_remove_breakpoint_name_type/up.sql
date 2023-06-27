
ALTER TABLE "public"."lucumaGridLayoutPositions" ALTER COLUMN "breakpointName" TYPE text;

CREATE TABLE "public"."lucumaGridBreakpointName" ("id" text NOT NULL, PRIMARY KEY ("id") );

INSERT INTO "public"."lucumaGridBreakpointName"("id") VALUES (E'xss');

INSERT INTO "public"."lucumaGridBreakpointName"("id") VALUES (E'xs');

INSERT INTO "public"."lucumaGridBreakpointName"("id") VALUES (E'sm');

INSERT INTO "public"."lucumaGridBreakpointName"("id") VALUES (E'md');

INSERT INTO "public"."lucumaGridBreakpointName"("id") VALUES (E'lg');

INSERT INTO "public"."lucumaGridBreakpointName"("id") VALUES (E'xl');

INSERT INTO "public"."lucumaGridBreakpointName"("id") VALUES (E'xxl');

alter table "public"."lucumaGridLayoutPositions"
  add constraint "lucumaGridLayoutPositions_breakpointName_fkey"
  foreign key ("breakpointName")
  references "public"."lucumaGridBreakpointName"
  ("id") on update cascade on delete cascade;

drop type breakpoint_name;

drop type site;
