
alter table "public"."lucuma_itc_plot_preferences" rename column "details_open" to "detailsOpen";

alter table "public"."lucuma_itc_plot_preferences" rename column "chart_type" to "chartType";

alter table "public"."lucuma_itc_plot_preferences" rename column "observation_id" to "observationId";

alter table "public"."lucuma_itc_plot_preferences" rename column "user_id" to "userId";

alter table "public"."lucuma_itc_plot_preferences" rename to "lucumaItcPlotPreferences";

alter table "public"."lucuma_target_preferences" rename to "lucumaTargetPreferences";

alter table "public"."lucumaTargetPreferences" rename column "target_id" to "targetId";

alter table "public"."lucumaTargetPreferences" rename column "user_id" to "userId";

alter table "public"."lucuma_user_preferences" rename column "aladin_mouse_scroll" to "aladinMouseScroll";

alter table "public"."lucuma_user_preferences" rename column "elevation_plot_range" to "elevationPlotRange";

alter table "public"."lucuma_user_preferences" rename column "elevation_plot_time" to "elevationPlotTime";

alter table "public"."lucuma_user_preferences" rename column "user_id" to "userId";

alter table "public"."lucuma_user_preferences" rename to "lucumaUserPreferences";

alter table "public"."lucuma_target" rename column "target_id" to "targetId";

alter table "public"."lucuma_target" rename to "lucumaTarget";

alter table "public"."lucuma_observation" rename column "observation_id" to "observationId";

alter table "public"."lucuma_observation" rename to "lucumaObservation";

alter table "public"."lucuma_user" rename column "user_id" to "userId";

alter table "public"."lucuma_user" rename to "lucumaUser";

alter table "public"."grid_layout_positions" rename column "breakpoint_name" to "breakpointName";

alter table "public"."grid_layout_positions" rename column "user_id" to "userId";

alter table "public"."grid_layout_positions" rename to "gridLayoutPositions";

alter table "public"."explore_resizable_width" rename column "user_id" to "userId";

alter table "public"."explore_resizable_width" rename to "exploreResizableWidth";

alter table "public"."exploreResizableWidth" rename to "lucumaResizableWidth";

alter table "public"."gridLayoutPositions" rename to "lucumaGridLayoutPositions";
