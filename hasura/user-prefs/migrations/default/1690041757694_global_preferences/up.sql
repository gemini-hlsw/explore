
alter table "public"."lucumaUserPreferences" alter column "aladinMouseScroll" set not null;

alter table "public"."lucumaUserPreferences" alter column "fullScreen" set not null;

alter table "public"."lucumaUserPreferences" alter column "acquisitionOffsets" set not null;

alter table "public"."lucumaUserPreferences" alter column "scienceOffsets" set not null;

alter table "public"."lucumaUserPreferences" alter column "agsOverlay" set not null;

alter table "public"."lucumaUserPreferences" alter column "showCatalog" set not null;

alter table "public"."lucumaUserPreferences" alter column "elevationPlotRange" set default 'night';
alter table "public"."lucumaUserPreferences" alter column "elevationPlotRange" set not null;

alter table "public"."lucumaUserPreferences" alter column "elevationPlotTime" set default 'ut';
alter table "public"."lucumaUserPreferences" alter column "elevationPlotTime" set not null;
