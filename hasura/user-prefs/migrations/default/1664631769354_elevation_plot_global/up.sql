

alter table "public"."lucuma_user_preferences" alter column "aladinMouseScroll" drop not null;

alter table "public"."lucuma_user_preferences" add column "elevation_plot_range" elevation_plot_range
 null;

alter table "public"."lucuma_user_preferences" add column "elevation_plot_time" elevation_plot_time
 null;

alter table "public"."lucuma_user_preferences" rename column "aladinMouseScroll" to "aladin_mouse_scroll";

DROP table "public"."lucuma_elevation_plot_preferences";

alter table "public"."lucuma_user_preferences" alter column "aladin_mouse_scroll" set default 'false';

