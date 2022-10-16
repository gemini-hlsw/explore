BEGIN TRANSACTION;
ALTER TABLE "public"."lucumaTableColumnPreferences" DROP CONSTRAINT "lucumaTableColumnPreferences_pkey";

ALTER TABLE "public"."lucumaTableColumnPreferences"
    ADD CONSTRAINT "lucumaTableColumnPreferences_pkey" PRIMARY KEY ("columnId", "tableId", "user_id");
COMMIT TRANSACTION;
