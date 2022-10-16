alter table "public"."lucumaTableColumnPreferences"
  add constraint "lucumaTableColumnPreferences_userId_fkey"
  foreign key ("userId")
  references "public"."lucumaUser"
  ("userId") on update cascade on delete cascade;
