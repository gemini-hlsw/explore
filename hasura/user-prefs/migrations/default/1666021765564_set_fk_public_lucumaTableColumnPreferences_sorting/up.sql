alter table "public"."lucumaTableColumnPreferences"
  add constraint "lucumaTableColumnPreferences_sorting_fkey"
  foreign key ("sorting")
  references "public"."lucumaSortDirection"
  ("id") on update cascade on delete cascade;
