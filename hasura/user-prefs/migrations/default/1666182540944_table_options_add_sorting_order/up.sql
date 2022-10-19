ALTER TABLE "lucumaTableColumnPreferences" ADD "sortingOrder" INT;

UPDATE "lucumaTableColumnPreferences" SET sorting=null;
