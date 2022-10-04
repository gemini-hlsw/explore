
alter table "public"."lucumaGridLayoutPositions" rename to "gridLayoutPositions";

alter table "public"."lucumaResizableWidth" rename to "exploreResizableWidth";

alter table "public"."exploreResizableWidth" rename to "explore_resizable_width";

alter table "public"."explore_resizable_width" rename column "userId" to "user_id";

alter table "public"."gridLayoutPositions" rename to "grid_layout_positions";

alter table "public"."grid_layout_positions" rename column "userId" to "user_id";

alter table "public"."grid_layout_positions" rename column "breakpointName" to "breakpoint_name";

alter table "public"."lucumaUser" rename to "lucuma_user";

alter table "public"."lucuma_user" rename column "userId" to "user_id";

alter table "public"."lucumaObservation" rename to "lucuma_observation";

alter table "public"."lucuma_observation" rename column "observationId" to "observation_id";

alter table "public"."lucumaTarget" rename to "lucuma_target";

alter table "public"."lucuma_target" rename column "targetId" to "target_id";

alter table "public"."lucumaUserPreferences" rename to "lucuma_user_preferences";

alter table "public"."lucuma_user_preferences" rename column "userId" to "user_id";

alter table "public"."lucuma_user_preferences" rename column "elevationPlotTime" to "elevation_plot_time";

alter table "public"."lucuma_user_preferences" rename column "elevationPlotRange" to "elevation_plot_range";

alter table "public"."lucuma_user_preferences" rename column "aladinMouseScroll" to "aladin_mouse_scroll";

alter table "public"."lucumaTargetPreferences" rename column "userId" to "user_id";

alter table "public"."lucumaTargetPreferences" rename column "targetId" to "target_id";

alter table "public"."lucumaTargetPreferences" rename to "lucuma_target_preferences";

alter table "public"."lucumaItcPlotPreferences" rename to "lucuma_itc_plot_preferences";

alter table "public"."lucuma_itc_plot_preferences" rename column "userId" to "user_id";

alter table "public"."lucuma_itc_plot_preferences" rename column "observationId" to "observation_id";

alter table "public"."lucuma_itc_plot_preferences" rename column "chartType" to "chart_type";

alter table "public"."lucuma_itc_plot_preferences" rename column "detailsOpen" to "details_open";
