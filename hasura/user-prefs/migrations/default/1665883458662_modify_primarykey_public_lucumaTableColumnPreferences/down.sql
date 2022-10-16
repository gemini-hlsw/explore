alter table "public"."lucumaTableColumnPreferences" drop constraint "lucumaTableColumnPreferences_pkey";
alter table "public"."lucumaTableColumnPreferences"
    add constraint "lucumaTableColumnPreferences_pkey"
    primary key ("columnId", "tableId");
