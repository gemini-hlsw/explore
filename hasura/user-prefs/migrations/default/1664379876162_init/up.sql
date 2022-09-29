SET check_function_bodies = false;
CREATE TYPE public.breakpoint_name AS ENUM (
    'xxl',
    'xl',
    'lg',
    'md',
    'sm',
    'xs',
    'xxs'
);
CREATE TYPE public.elevation_plot_range AS ENUM (
    'NIGHT',
    'SEMESTER'
);
CREATE TYPE public.elevation_plot_time AS ENUM (
    'UT',
    'SIDEREAL',
    'SITE'
);
CREATE TYPE public.grid_layout_area AS ENUM (
    'observations',
    'targets'
);
CREATE TYPE public.itc_chart_type AS ENUM (
    'S2N_CHART',
    'SIGNAL_CHART'
);
CREATE DOMAIN public.lucuma_target_id AS character varying;
CREATE DOMAIN public.lucuma_user_id AS character varying;
CREATE TYPE public.resizable_area AS ENUM (
    'targets_tree',
    'observations_tree',
    'constraintsets_tree'
);
CREATE TYPE public.site AS ENUM (
    'GS',
    'GN'
);
CREATE TABLE public.explore_resizable_width (
    user_id public.lucuma_user_id NOT NULL,
    section public.resizable_area NOT NULL,
    width integer NOT NULL
);
CREATE TABLE public.grid_layout_positions (
    user_id public.lucuma_user_id NOT NULL,
    section public.grid_layout_area NOT NULL,
    breakpoint_name public.breakpoint_name NOT NULL,
    tile character varying NOT NULL,
    width integer NOT NULL,
    height integer NOT NULL,
    x integer NOT NULL,
    y integer NOT NULL
);
CREATE TABLE public.lucuma_elevation_plot_preferences (
    user_id public.lucuma_user_id NOT NULL,
    target_id public.lucuma_target_id NOT NULL,
    site public.site NOT NULL,
    range public.elevation_plot_range NOT NULL,
    "time" public.elevation_plot_time NOT NULL
);
CREATE TABLE public.lucuma_itc_plot_preferences (
    user_id character varying NOT NULL,
    observation_id character varying NOT NULL,
    details_open boolean DEFAULT false NOT NULL,
    chart_type public.itc_chart_type DEFAULT 'S2N_CHART'::public.itc_chart_type NOT NULL
);
CREATE TABLE public.lucuma_observation (
    observation_id character varying NOT NULL
);
CREATE TABLE public.lucuma_target (
    target_id public.lucuma_target_id NOT NULL
);
CREATE TABLE public.lucuma_target_preferences (
    user_id public.lucuma_user_id NOT NULL,
    target_id public.lucuma_target_id NOT NULL,
    "viewOffsetP" bigint DEFAULT 0 NOT NULL,
    "viewOffsetQ" bigint DEFAULT 0 NOT NULL,
    "agsCandidates" boolean DEFAULT false NOT NULL,
    "agsOverlay" boolean DEFAULT false NOT NULL,
    "fullScreen" boolean DEFAULT false NOT NULL,
    "fovRA" bigint DEFAULT '1000000'::bigint NOT NULL,
    "fovDec" bigint DEFAULT '1000000'::bigint NOT NULL
);
CREATE TABLE public.lucuma_user (
    user_id public.lucuma_user_id NOT NULL
);
CREATE TABLE public.lucuma_user_preferences (
    user_id character varying NOT NULL,
    "aladinMouseScroll" boolean DEFAULT true NOT NULL
);
ALTER TABLE ONLY public.explore_resizable_width
    ADD CONSTRAINT explore_resizable_width_pkey PRIMARY KEY (user_id, section);
ALTER TABLE ONLY public.grid_layout_positions
    ADD CONSTRAINT grid_layout_positions_pkey PRIMARY KEY (user_id, section, breakpoint_name, tile);
ALTER TABLE ONLY public.lucuma_elevation_plot_preferences
    ADD CONSTRAINT lucuma_elevation_plot_preferences_pkey PRIMARY KEY (user_id, target_id);
ALTER TABLE ONLY public.lucuma_itc_plot_preferences
    ADD CONSTRAINT lucuma_itc_plot_preferences_pkey PRIMARY KEY (user_id, observation_id);
ALTER TABLE ONLY public.lucuma_observation
    ADD CONSTRAINT lucuma_observation_pkey PRIMARY KEY (observation_id);
ALTER TABLE ONLY public.lucuma_target
    ADD CONSTRAINT lucuma_target_pkey PRIMARY KEY (target_id);
ALTER TABLE ONLY public.lucuma_target_preferences
    ADD CONSTRAINT lucuma_target_preferences_pkey PRIMARY KEY (user_id, target_id);
ALTER TABLE ONLY public.lucuma_user
    ADD CONSTRAINT lucuma_user_pkey PRIMARY KEY (user_id);
ALTER TABLE ONLY public.lucuma_user_preferences
    ADD CONSTRAINT lucuma_user_preferences_pkey PRIMARY KEY (user_id);
ALTER TABLE ONLY public.lucuma_target_preferences
    ADD CONSTRAINT fk_targetid FOREIGN KEY (target_id) REFERENCES public.lucuma_target(target_id) MATCH FULL ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.lucuma_elevation_plot_preferences
    ADD CONSTRAINT fk_targetid FOREIGN KEY (target_id) REFERENCES public.lucuma_target(target_id) MATCH FULL ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.grid_layout_positions
    ADD CONSTRAINT fk_userid FOREIGN KEY (user_id) REFERENCES public.lucuma_user(user_id) MATCH FULL ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.explore_resizable_width
    ADD CONSTRAINT fk_userid FOREIGN KEY (user_id) REFERENCES public.lucuma_user(user_id) MATCH FULL ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.lucuma_target_preferences
    ADD CONSTRAINT fk_userid FOREIGN KEY (user_id) REFERENCES public.lucuma_user(user_id) MATCH FULL ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.lucuma_elevation_plot_preferences
    ADD CONSTRAINT fk_userid FOREIGN KEY (user_id) REFERENCES public.lucuma_user(user_id) MATCH FULL ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.lucuma_itc_plot_preferences
    ADD CONSTRAINT lucuma_itc_plot_preferences_observation_id_fkey FOREIGN KEY (observation_id) REFERENCES public.lucuma_observation(observation_id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.lucuma_itc_plot_preferences
    ADD CONSTRAINT lucuma_itc_plot_preferences_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.lucuma_user(user_id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.lucuma_user_preferences
    ADD CONSTRAINT lucuma_user_preferences_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.lucuma_user(user_id) ON UPDATE CASCADE ON DELETE CASCADE;
