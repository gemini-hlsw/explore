
-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- drop type site;

-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:
-- drop type breakpoint_name;

alter table "public"."lucumaGridLayoutPositions" drop constraint "lucumaGridLayoutPositions_breakpointName_fkey";

DELETE FROM "public"."lucumaGridBreakpointName" WHERE "id" = 'xxl';

DELETE FROM "public"."lucumaGridBreakpointName" WHERE "id" = 'xl';

DELETE FROM "public"."lucumaGridBreakpointName" WHERE "id" = 'lg';

DELETE FROM "public"."lucumaGridBreakpointName" WHERE "id" = 'md';

DELETE FROM "public"."lucumaGridBreakpointName" WHERE "id" = 'sm';

DELETE FROM "public"."lucumaGridBreakpointName" WHERE "id" = 'xs';

DELETE FROM "public"."lucumaGridBreakpointName" WHERE "id" = 'xss';

DROP TABLE "public"."lucumaGridBreakpointName";

ALTER TABLE "public"."lucumaGridLayoutPositions" ALTER COLUMN "breakpointName" TYPE USER-DEFINED;
