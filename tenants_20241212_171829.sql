--
-- PostgreSQL database dump
--

-- Dumped from database version 15.3
-- Dumped by pg_dump version 16.0

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: tenants; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.tenants (
    user_role text NOT NULL,
    user_name text NOT NULL,
    user_password text NOT NULL,
    user_id text NOT NULL,
    status text,
    auth_token text,
    refresh_token text,
    created_at bigint
);


ALTER TABLE public.tenants OWNER TO postgres;

--
-- Data for Name: tenants; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO public.tenants VALUES
	('admin', 'marcos.tirao', '3172AppL', 'mtirao@gmail.com', 'enabled', NULL, NULL, NULL),
	('user', '', '', '1111', 'enabled', NULL, NULL, NULL),
	('admin', 'marcos.tirao@icloud.com', '3172AppL', '92525840', 'enabled', 'eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3MzMzMzY0ODYsInVzZXIiOiI5MjUyNTg0MCJ9.ObCYABnzMXzQ0BQncRUEi5qH0V8JWqxJW_z_mloNjJk', 'eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3MzQyMDA0ODYsInVzZXIiOiI5MjUyNTg0MCJ9.6CE8yl1ba8hMPzSRRS_tnLUXkw-xaQdF6XY7199Kdlc', NULL);


--
-- Name: tenants tenants_user_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.tenants
    ADD CONSTRAINT tenants_user_id_key UNIQUE (user_id);


--
-- PostgreSQL database dump complete
--

