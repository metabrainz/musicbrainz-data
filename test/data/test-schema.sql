--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: musicbrainz; Type: SCHEMA; Schema: -; Owner: musicbrainz
--

CREATE SCHEMA musicbrainz;


ALTER SCHEMA musicbrainz OWNER TO musicbrainz;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


--
-- Name: cube; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS cube WITH SCHEMA public;


--
-- Name: EXTENSION cube; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION cube IS 'data type for multidimensional cubes';


--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


SET search_path = musicbrainz, pg_catalog;

--
-- Name: comment; Type: DOMAIN; Schema: musicbrainz; Owner: musicbrainz
--

CREATE DOMAIN comment AS text;


ALTER DOMAIN musicbrainz.comment OWNER TO musicbrainz;

--
-- Name: cover_art_presence; Type: TYPE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TYPE cover_art_presence AS ENUM (
    'absent',
    'present',
    'darkened'
);


ALTER TYPE musicbrainz.cover_art_presence OWNER TO musicbrainz;

--
-- Name: ipi; Type: DOMAIN; Schema: musicbrainz; Owner: musicbrainz
--

CREATE DOMAIN ipi AS character(11)
	CONSTRAINT ipi_check CHECK ((VALUE ~ '^\d{11}$'::text));


ALTER DOMAIN musicbrainz.ipi OWNER TO musicbrainz;

--
-- Name: isrc_t; Type: DOMAIN; Schema: musicbrainz; Owner: musicbrainz
--

CREATE DOMAIN isrc_t AS character(12)
	CONSTRAINT isrc_t_check CHECK ((VALUE ~ '^[A-Z]{2}[A-Z0-9]{3}[0-9]{7}$'::text));


ALTER DOMAIN musicbrainz.isrc_t OWNER TO musicbrainz;

--
-- Name: iswc_t; Type: DOMAIN; Schema: musicbrainz; Owner: musicbrainz
--

CREATE DOMAIN iswc_t AS character(15)
	CONSTRAINT iswc_t_check CHECK ((VALUE ~ '^T-?\d{3}.?\d{3}.?\d{3}[-.]?\d$'::text));


ALTER DOMAIN musicbrainz.iswc_t OWNER TO musicbrainz;

--
-- Name: label_code; Type: DOMAIN; Schema: musicbrainz; Owner: musicbrainz
--

CREATE DOMAIN label_code AS integer
	CONSTRAINT label_code_check CHECK (((VALUE > 0) AND (VALUE < 100000)));


ALTER DOMAIN musicbrainz.label_code OWNER TO musicbrainz;

--
-- Name: locale; Type: DOMAIN; Schema: musicbrainz; Owner: musicbrainz
--

CREATE DOMAIN locale AS character varying(30)
	CONSTRAINT locale_check CHECK (((VALUE)::text ~ '^[a-zA-Z_]+$'::text));


ALTER DOMAIN musicbrainz.locale OWNER TO musicbrainz;

--
-- Name: natural_integer; Type: DOMAIN; Schema: musicbrainz; Owner: musicbrainz
--

CREATE DOMAIN natural_integer AS integer
	CONSTRAINT natural_integer_check CHECK ((VALUE >= 0));


ALTER DOMAIN musicbrainz.natural_integer OWNER TO musicbrainz;

--
-- Name: single_line; Type: DOMAIN; Schema: musicbrainz; Owner: musicbrainz
--

CREATE DOMAIN single_line AS text;


ALTER DOMAIN musicbrainz.single_line OWNER TO musicbrainz;

--
-- Name: presentational_text; Type: DOMAIN; Schema: musicbrainz; Owner: musicbrainz
--

CREATE DOMAIN presentational_text AS single_line;


ALTER DOMAIN musicbrainz.presentational_text OWNER TO musicbrainz;

--
-- Name: non_empty_presentational_text; Type: DOMAIN; Schema: musicbrainz; Owner: musicbrainz
--

CREATE DOMAIN non_empty_presentational_text AS presentational_text;


ALTER DOMAIN musicbrainz.non_empty_presentational_text OWNER TO musicbrainz;

--
-- Name: positive_integer; Type: DOMAIN; Schema: musicbrainz; Owner: musicbrainz
--

CREATE DOMAIN positive_integer AS integer
	CONSTRAINT positive_integer_check CHECK ((VALUE > 0));


ALTER DOMAIN musicbrainz.positive_integer OWNER TO musicbrainz;

--
-- Name: rating; Type: DOMAIN; Schema: musicbrainz; Owner: musicbrainz
--

CREATE DOMAIN rating AS smallint
	CONSTRAINT rating_check CHECK (((VALUE >= 0) AND (VALUE <= 100)));


ALTER DOMAIN musicbrainz.rating OWNER TO musicbrainz;

--
-- Name: relationship_endpoint; Type: TYPE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TYPE relationship_endpoint AS ENUM (
    'artist',
    'label',
    'recording',
    'release',
    'release_group',
    'url',
    'work'
);


ALTER TYPE musicbrainz.relationship_endpoint OWNER TO musicbrainz;

--
-- Name: find_or_insert_artist_data(text, text, text, integer, integer, integer, integer, integer, integer, boolean, integer, integer, integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_artist_data(in_name text, in_sort_name text, in_comment text, in_b_year integer, in_b_month integer, in_b_day integer, in_e_year integer, in_e_month integer, in_e_day integer, in_ended boolean, in_gender_id integer, in_type_id integer, in_country_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
    name_id INT;
    sort_name_id INT;
  BEGIN
    SELECT find_or_insert_artist_name(in_name) INTO name_id;
    SELECT find_or_insert_artist_name(in_sort_name) INTO sort_name_id;

    SELECT artist_data_id INTO found_id
    FROM artist_data
    WHERE name = name_id AND sort_name = sort_name_id AND comment = in_comment AND
      begin_date_year IS NOT DISTINCT FROM in_b_year AND
      begin_date_month IS NOT DISTINCT FROM in_b_month AND
      begin_date_day IS NOT DISTINCT FROM in_b_day AND
      end_date_year IS NOT DISTINCT FROM in_b_year AND
      end_date_month IS NOT DISTINCT FROM in_b_month AND
      end_date_day IS NOT DISTINCT FROM in_b_day AND
      ended = in_ended AND
      gender_id IS NOT DISTINCT FROM in_gender_id AND
      artist_type_id IS NOT DISTINCT FROM in_type_id AND
      country_id IS NOT DISTINCT FROM in_country_id;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO artist_data (name, sort_name, comment,
        begin_date_year, begin_date_month, begin_date_day,
        end_date_year, end_date_month, end_date_day,
        ended, gender_id, artist_type_id, country_id)
      VALUES (name_id, sort_name_id, in_comment,
        in_b_year, in_b_month, in_b_day,
        in_e_year, in_e_month, in_e_day,
        in_ended, in_gender_id, in_type_id, in_country_id)
      RETURNING artist_data_id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_artist_data(in_name text, in_sort_name text, in_comment text, in_b_year integer, in_b_month integer, in_b_day integer, in_e_year integer, in_e_month integer, in_e_day integer, in_ended boolean, in_gender_id integer, in_type_id integer, in_country_id integer) OWNER TO musicbrainz;

--
-- Name: find_or_insert_artist_name(text); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_artist_name(in_name text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
  BEGIN
    SELECT id INTO found_id
    FROM artist_name WHERE name = in_name;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO artist_name (name)
      VALUES (in_name) RETURNING id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_artist_name(in_name text) OWNER TO musicbrainz;

--
-- Name: find_or_insert_artist_tree(integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_artist_tree(in_data_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
  BEGIN
    SELECT artist_tree_id INTO found_id
    FROM artist_tree WHERE artist_data_id = in_data_id;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO artist_tree (artist_data_id)
      VALUES (in_data_id)
      RETURNING artist_tree_id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_artist_tree(in_data_id integer) OWNER TO musicbrainz;

--
-- Name: find_or_insert_label_data(text, text, text, integer, integer, integer, integer, integer, integer, boolean, integer, integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_label_data(in_name text, in_sort_name text, in_comment text, in_b_year integer, in_b_month integer, in_b_day integer, in_e_year integer, in_e_month integer, in_e_day integer, in_ended boolean, in_type_id integer, in_code integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
    name_id INT;
    sort_name_id INT;
  BEGIN
    SELECT find_or_insert_label_name(in_name) INTO name_id;
    SELECT find_or_insert_label_name(in_sort_name) INTO sort_name_id;

    SELECT label_data_id INTO found_id
    FROM label_data
    WHERE name = name_id AND sort_name = sort_name_id AND comment = in_comment AND
      begin_date_year IS NOT DISTINCT FROM in_b_year AND
      begin_date_month IS NOT DISTINCT FROM in_b_month AND
      begin_date_day IS NOT DISTINCT FROM in_b_day AND
      end_date_year IS NOT DISTINCT FROM in_b_year AND
      end_date_month IS NOT DISTINCT FROM in_b_month AND
      end_date_day IS NOT DISTINCT FROM in_b_day AND
      ended = in_ended AND
      label_type_id IS NOT DISTINCT FROM in_type_id AND
      label_code IS NOT DISTINCT FROM in_code;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO label_data (name, sort_name, comment,
        begin_date_year, begin_date_month, begin_date_day,
        end_date_year, end_date_month, end_date_day,
        ended, label_type_id, label_code)
      VALUES (name_id, sort_name_id, in_comment,
        in_b_year, in_b_month, in_b_day,
        in_e_year, in_e_month, in_e_day,
        in_ended, in_type_id, in_code)
      RETURNING label_data_id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_label_data(in_name text, in_sort_name text, in_comment text, in_b_year integer, in_b_month integer, in_b_day integer, in_e_year integer, in_e_month integer, in_e_day integer, in_ended boolean, in_type_id integer, in_code integer) OWNER TO musicbrainz;

--
-- Name: find_or_insert_label_name(text); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_label_name(in_name text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
  BEGIN
    SELECT id INTO found_id
    FROM label_name WHERE name = in_name;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO label_name (name)
      VALUES (in_name) RETURNING id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_label_name(in_name text) OWNER TO musicbrainz;

--
-- Name: find_or_insert_label_tree(integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_label_tree(in_data_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
  BEGIN
    SELECT label_tree_id INTO found_id
    FROM label_tree WHERE label_data_id = in_data_id;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO label_tree (label_data_id)
      VALUES (in_data_id)
      RETURNING label_tree_id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_label_tree(in_data_id integer) OWNER TO musicbrainz;

--
-- Name: find_or_insert_recording_data(text, text, integer, integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_recording_data(in_name text, in_comment text, in_ac integer, in_length integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
    name_id INT;
  BEGIN
    SELECT find_or_insert_track_name(in_name) INTO name_id;

    SELECT recording_data_id INTO found_id
    FROM recording_data
    WHERE name = name_id AND
      comment = in_comment AND
      artist_credit_id = in_ac AND
      length IS NOT DISTINCT FROM in_length;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO recording_data (name, comment, artist_credit_id, length)
      VALUES (name_id, in_comment, in_ac, in_length)
      RETURNING recording_data_id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_recording_data(in_name text, in_comment text, in_ac integer, in_length integer) OWNER TO musicbrainz;

--
-- Name: find_or_insert_recording_tree(integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_recording_tree(in_data_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
  BEGIN
    SELECT recording_tree_id INTO found_id
    FROM recording_tree WHERE recording_data_id = in_data_id;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO recording_tree (recording_data_id)
      VALUES (in_data_id)
      RETURNING recording_tree_id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_recording_tree(in_data_id integer) OWNER TO musicbrainz;

--
-- Name: find_or_insert_release_data(text, text, integer, integer, integer, integer, integer, integer, integer, integer, integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_release_data(in_name text, in_comment text, in_ac integer, in_date_year integer, in_date_month integer, in_date_day integer, in_country_id integer, in_script_id integer, in_language_id integer, in_packaging_id integer, in_status_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
    name_id INT;
  BEGIN
    SELECT find_or_insert_release_name(in_name) INTO name_id;

    SELECT release_data_id INTO found_id
    FROM release_data
    WHERE name = name_id AND
      comment = in_comment AND
      artist_credit_id = in_ac AND
      date_year IS NOT DISTINCT FROM in_date_year AND
      date_month IS NOT DISTINCT FROM in_date_month AND
      date_day IS NOT DISTINCT FROM in_date_day AND
      country_id IS NOT DISTINCT FROM in_country_id AND
      script_id IS NOT DISTINCT FROM in_script_id AND
      language_id IS NOT DISTINCT FROM in_language_id AND
      release_packaging_id IS NOT DISTINCT FROM in_packaging_id AND
      release_status_id IS NOT DISTINCT FROM in_status_id;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO release_data (name, comment, artist_credit_id, date_year,
        date_month, date_day, country_id, script_id, language_id,
        release_packaging_id, release_status_id)
      VALUES (name_id, in_comment, in_ac, in_date_year, in_date_month,
        in_date_day, in_country_id, in_script_id, in_language_id, in_packaging_id,
        in_status_id)
      RETURNING release_data_id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_release_data(in_name text, in_comment text, in_ac integer, in_date_year integer, in_date_month integer, in_date_day integer, in_country_id integer, in_script_id integer, in_language_id integer, in_packaging_id integer, in_status_id integer) OWNER TO musicbrainz;

--
-- Name: find_or_insert_release_data(text, text, integer, uuid, integer, integer, integer, integer, integer, integer, integer, integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_release_data(in_name text, in_comment text, in_ac integer, in_rg uuid, in_date_year integer, in_date_month integer, in_date_day integer, in_country_id integer, in_script_id integer, in_language_id integer, in_packaging_id integer, in_status_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
    name_id INT;
  BEGIN
    SELECT find_or_insert_release_name(in_name) INTO name_id;

    SELECT release_data_id INTO found_id
    FROM release_data
    WHERE name = name_id AND
      comment = in_comment AND
      artist_credit_id = in_ac AND
      release_group_id = in_rg AND
      date_year IS NOT DISTINCT FROM in_date_year AND
      date_month IS NOT DISTINCT FROM in_date_month AND
      date_day IS NOT DISTINCT FROM in_date_day AND
      country_id IS NOT DISTINCT FROM in_country_id AND
      script_id IS NOT DISTINCT FROM in_script_id AND
      language_id IS NOT DISTINCT FROM in_language_id AND
      release_packaging_id IS NOT DISTINCT FROM in_packaging_id AND
      release_status_id IS NOT DISTINCT FROM in_status_id;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO release_data (name, comment, artist_credit_id, release_group_id,
        date_year, date_month, date_day, country_id, script_id, language_id,
        release_packaging_id, release_status_id)
      VALUES (name_id, in_comment, in_ac, in_rg, in_date_year, in_date_month,
        in_date_day, in_country_id, in_script_id, in_language_id, in_packaging_id,
        in_status_id)
      RETURNING release_data_id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_release_data(in_name text, in_comment text, in_ac integer, in_rg uuid, in_date_year integer, in_date_month integer, in_date_day integer, in_country_id integer, in_script_id integer, in_language_id integer, in_packaging_id integer, in_status_id integer) OWNER TO musicbrainz;

--
-- Name: find_or_insert_release_group_data(text, text, integer, integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_release_group_data(in_name text, in_comment text, in_artist_credit_id integer, in_p_type_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
    name_id INT;
    sort_name_id INT;
  BEGIN
    SELECT find_or_insert_release_name(in_name) INTO name_id;

    SELECT release_group_data_id INTO found_id
    FROM release_group_data
    WHERE name = name_id AND
      comment = in_comment AND
      artist_credit_id = in_artist_credit_id AND
      release_group_primary_type_id IS NOT DISTINCT FROM in_p_type_id;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO release_group_data (name, comment, artist_credit_id,
        release_group_primary_type_id)
      VALUES (name_id, in_comment, in_artist_credit_id, in_p_type_id)
      RETURNING release_group_data_id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_release_group_data(in_name text, in_comment text, in_artist_credit_id integer, in_p_type_id integer) OWNER TO musicbrainz;

--
-- Name: find_or_insert_release_group_tree(integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_release_group_tree(in_data_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
  BEGIN
    SELECT release_group_tree_id INTO found_id
    FROM release_group_tree WHERE release_group_data_id = in_data_id;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO release_group_tree (release_group_data_id)
      VALUES (in_data_id)
      RETURNING release_group_tree_id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_release_group_tree(in_data_id integer) OWNER TO musicbrainz;

--
-- Name: find_or_insert_release_name(text); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_release_name(in_name text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
  BEGIN
    SELECT id INTO found_id
    FROM release_name WHERE name = in_name;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO release_name (name)
      VALUES (in_name) RETURNING id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_release_name(in_name text) OWNER TO musicbrainz;

--
-- Name: find_or_insert_release_tree(integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_release_tree(in_data_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
  BEGIN
    SELECT release_tree_id INTO found_id
    FROM release_tree WHERE release_data_id = in_data_id;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO release_tree (release_data_id)
      VALUES (in_data_id)
      RETURNING release_tree_id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_release_tree(in_data_id integer) OWNER TO musicbrainz;

--
-- Name: find_or_insert_release_tree(integer, uuid); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_release_tree(in_data_id integer, in_rg uuid) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
  BEGIN
    SELECT release_tree_id INTO found_id
    FROM release_tree WHERE release_data_id = in_data_id AND release_group_id = in_rg;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO release_tree (release_data_id, release_group_id)
      VALUES (in_data_id, in_rg)
      RETURNING release_tree_id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_release_tree(in_data_id integer, in_rg uuid) OWNER TO musicbrainz;

--
-- Name: find_or_insert_track_name(text); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION find_or_insert_track_name(in_name text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
  DECLARE
    found_id INT;
  BEGIN
    SELECT id INTO found_id
    FROM track_name WHERE name = in_name;

    IF FOUND
    THEN
      RETURN found_id;
    ELSE
      INSERT INTO track_name (name)
      VALUES (in_name) RETURNING id INTO found_id;
      RETURN found_id;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.find_or_insert_track_name(in_name text) OWNER TO musicbrainz;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: artist; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist (
    artist_id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    master_revision_id integer NOT NULL,
    merged_into uuid
);


ALTER TABLE musicbrainz.artist OWNER TO musicbrainz;

--
-- Name: artist_alias; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_alias (
    artist_tree_id integer NOT NULL,
    name integer NOT NULL,
    sort_name integer NOT NULL,
    locale locale,
    artist_alias_type_id integer,
    begin_date_year smallint,
    begin_date_month smallint,
    begin_date_day smallint,
    end_date_year smallint,
    end_date_month smallint,
    end_date_day smallint,
    primary_for_locale boolean DEFAULT false NOT NULL
);


ALTER TABLE musicbrainz.artist_alias OWNER TO musicbrainz;

--
-- Name: artist_alias_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_alias_type (
    id integer NOT NULL,
    name non_empty_presentational_text NOT NULL
);


ALTER TABLE musicbrainz.artist_alias_type OWNER TO musicbrainz;

--
-- Name: artist_alias_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE artist_alias_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.artist_alias_type_id_seq OWNER TO musicbrainz;

--
-- Name: artist_alias_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE artist_alias_type_id_seq OWNED BY artist_alias_type.id;


--
-- Name: artist_credit; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_credit (
    artist_credit_id integer NOT NULL
);


ALTER TABLE musicbrainz.artist_credit OWNER TO musicbrainz;

--
-- Name: artist_credit_artist_credit_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE artist_credit_artist_credit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.artist_credit_artist_credit_id_seq OWNER TO musicbrainz;

--
-- Name: artist_credit_artist_credit_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE artist_credit_artist_credit_id_seq OWNED BY artist_credit.artist_credit_id;


--
-- Name: artist_credit_name; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_credit_name (
    artist_credit_id integer NOT NULL,
    "position" natural_integer NOT NULL,
    artist_id uuid NOT NULL,
    name integer NOT NULL,
    join_phrase single_line DEFAULT ''::text NOT NULL
);


ALTER TABLE musicbrainz.artist_credit_name OWNER TO musicbrainz;

--
-- Name: artist_data; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_data (
    artist_data_id integer NOT NULL,
    name integer NOT NULL,
    sort_name integer NOT NULL,
    begin_date_year smallint,
    begin_date_month smallint,
    begin_date_day smallint,
    end_date_year smallint,
    end_date_month smallint,
    end_date_day smallint,
    artist_type_id integer,
    country_id integer,
    gender_id integer,
    comment presentational_text DEFAULT ''::text NOT NULL,
    ended boolean DEFAULT false NOT NULL
);


ALTER TABLE musicbrainz.artist_data OWNER TO musicbrainz;

--
-- Name: artist_data_artist_data_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE artist_data_artist_data_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.artist_data_artist_data_id_seq OWNER TO musicbrainz;

--
-- Name: artist_data_artist_data_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE artist_data_artist_data_id_seq OWNED BY artist_data.artist_data_id;


--
-- Name: artist_ipi; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_ipi (
    artist_tree_id integer NOT NULL,
    ipi ipi NOT NULL
);


ALTER TABLE musicbrainz.artist_ipi OWNER TO musicbrainz;

--
-- Name: artist_meta; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_meta (
    artist_id uuid NOT NULL,
    rating rating,
    rating_count natural_integer NOT NULL
);


ALTER TABLE musicbrainz.artist_meta OWNER TO musicbrainz;

--
-- Name: artist_name; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_name (
    id integer NOT NULL,
    name non_empty_presentational_text NOT NULL
);


ALTER TABLE musicbrainz.artist_name OWNER TO musicbrainz;

--
-- Name: artist_name_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE artist_name_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.artist_name_id_seq OWNER TO musicbrainz;

--
-- Name: artist_name_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE artist_name_id_seq OWNED BY artist_name.id;


--
-- Name: artist_rating_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_rating_raw (
    artist_id uuid NOT NULL,
    rating rating NOT NULL,
    editor_id integer NOT NULL
);


ALTER TABLE musicbrainz.artist_rating_raw OWNER TO musicbrainz;

--
-- Name: artist_revision; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_revision (
    revision_id integer NOT NULL,
    artist_id uuid NOT NULL,
    artist_tree_id integer NOT NULL
);


ALTER TABLE musicbrainz.artist_revision OWNER TO musicbrainz;

--
-- Name: artist_tag; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_tag (
    artist_id uuid NOT NULL,
    tag_id integer NOT NULL,
    count positive_integer NOT NULL
);


ALTER TABLE musicbrainz.artist_tag OWNER TO musicbrainz;

--
-- Name: artist_tag_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_tag_raw (
    artist_id uuid NOT NULL,
    tag_id integer NOT NULL,
    editor_id integer NOT NULL
);


ALTER TABLE musicbrainz.artist_tag_raw OWNER TO musicbrainz;

--
-- Name: artist_tree; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_tree (
    artist_tree_id integer NOT NULL,
    artist_data_id integer NOT NULL,
    annotation text
);


ALTER TABLE musicbrainz.artist_tree OWNER TO musicbrainz;

--
-- Name: artist_tree_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE artist_tree_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.artist_tree_id_seq OWNER TO musicbrainz;

--
-- Name: artist_tree_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE artist_tree_id_seq OWNED BY artist_tree.artist_tree_id;


--
-- Name: artist_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_type (
    id integer NOT NULL,
    name presentational_text NOT NULL
);


ALTER TABLE musicbrainz.artist_type OWNER TO musicbrainz;

--
-- Name: artist_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE artist_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.artist_type_id_seq OWNER TO musicbrainz;

--
-- Name: artist_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE artist_type_id_seq OWNED BY artist_type.id;


--
-- Name: cdtoc; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE cdtoc (
    id integer NOT NULL,
    discid character(28) NOT NULL,
    freedb_id character(8) NOT NULL,
    track_count positive_integer NOT NULL,
    leadout_offset positive_integer NOT NULL,
    track_offset integer[] NOT NULL,
    degraded boolean DEFAULT false NOT NULL,
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.cdtoc OWNER TO musicbrainz;

--
-- Name: cdtoc_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE cdtoc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.cdtoc_id_seq OWNER TO musicbrainz;

--
-- Name: cdtoc_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE cdtoc_id_seq OWNED BY cdtoc.id;


--
-- Name: clientversion; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE clientversion (
    id integer NOT NULL,
    version character varying(64) NOT NULL,
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.clientversion OWNER TO musicbrainz;

--
-- Name: clientversion_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE clientversion_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.clientversion_id_seq OWNER TO musicbrainz;

--
-- Name: clientversion_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE clientversion_id_seq OWNED BY clientversion.id;


--
-- Name: country; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE country (
    id integer NOT NULL,
    iso_code character varying(2) NOT NULL,
    name character varying(255) NOT NULL
);


ALTER TABLE musicbrainz.country OWNER TO musicbrainz;

--
-- Name: country_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE country_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.country_id_seq OWNER TO musicbrainz;

--
-- Name: country_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE country_id_seq OWNED BY country.id;


--
-- Name: edit; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE edit (
    edit_id integer NOT NULL,
    status smallint
);


ALTER TABLE musicbrainz.edit OWNER TO musicbrainz;

--
-- Name: edit_artist; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE edit_artist (
    edit_id integer NOT NULL,
    revision_id integer NOT NULL
);


ALTER TABLE musicbrainz.edit_artist OWNER TO musicbrainz;

--
-- Name: edit_edit_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE edit_edit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.edit_edit_id_seq OWNER TO musicbrainz;

--
-- Name: edit_edit_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE edit_edit_id_seq OWNED BY edit.edit_id;


--
-- Name: edit_note; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE edit_note (
    editor_id integer NOT NULL,
    edit_id integer NOT NULL,
    text text,
    edit_note_id integer NOT NULL
);


ALTER TABLE musicbrainz.edit_note OWNER TO musicbrainz;

--
-- Name: edit_note_edit_note_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE edit_note_edit_note_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.edit_note_edit_note_id_seq OWNER TO musicbrainz;

--
-- Name: edit_note_edit_note_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE edit_note_edit_note_id_seq OWNED BY edit_note.edit_note_id;


--
-- Name: editor; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor (
    editor_id integer NOT NULL,
    name character varying(64) NOT NULL,
    privs integer DEFAULT 0,
    email character varying(64) DEFAULT NULL::character varying,
    website character varying(255) DEFAULT NULL::character varying,
    bio text,
    member_since timestamp with time zone DEFAULT now(),
    email_confirm_date timestamp with time zone,
    last_login_date timestamp with time zone,
    last_updated timestamp with time zone DEFAULT now(),
    birth_date date,
    gender_id integer,
    country_id integer
);


ALTER TABLE musicbrainz.editor OWNER TO musicbrainz;

--
-- Name: editor_editor_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE editor_editor_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.editor_editor_id_seq OWNER TO musicbrainz;

--
-- Name: editor_editor_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE editor_editor_id_seq OWNED BY editor.editor_id;


--
-- Name: gender; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE gender (
    id integer NOT NULL,
    name character varying(255) NOT NULL
);


ALTER TABLE musicbrainz.gender OWNER TO musicbrainz;

--
-- Name: gender_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE gender_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.gender_id_seq OWNER TO musicbrainz;

--
-- Name: gender_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE gender_id_seq OWNED BY gender.id;


--
-- Name: isrc; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE isrc (
    id integer NOT NULL,
    recording_tree_id integer NOT NULL,
    isrc isrc_t NOT NULL,
    source smallint
);


ALTER TABLE musicbrainz.isrc OWNER TO musicbrainz;

--
-- Name: isrc_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE isrc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.isrc_id_seq OWNER TO musicbrainz;

--
-- Name: isrc_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE isrc_id_seq OWNED BY isrc.id;


--
-- Name: iswc; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE iswc (
    id integer NOT NULL,
    work_tree_id integer NOT NULL,
    iswc iswc_t NOT NULL,
    source smallint
);


ALTER TABLE musicbrainz.iswc OWNER TO musicbrainz;

--
-- Name: iswc_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE iswc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.iswc_id_seq OWNER TO musicbrainz;

--
-- Name: iswc_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE iswc_id_seq OWNED BY iswc.id;


--
-- Name: label; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label (
    label_id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    master_revision_id integer NOT NULL,
    merged_into uuid
);


ALTER TABLE musicbrainz.label OWNER TO musicbrainz;

--
-- Name: label_alias; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_alias (
    label_tree_id integer NOT NULL,
    name integer NOT NULL,
    sort_name integer NOT NULL,
    locale locale,
    label_alias_type_id integer,
    begin_date partial_date,
    end_date partial_date,
    primary_for_locale boolean DEFAULT false NOT NULL
);


ALTER TABLE musicbrainz.label_alias OWNER TO musicbrainz;

--
-- Name: label_alias_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_alias_type (
    id integer NOT NULL,
    name non_empty_presentational_text NOT NULL
);


ALTER TABLE musicbrainz.label_alias_type OWNER TO musicbrainz;

--
-- Name: label_alias_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE label_alias_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.label_alias_type_id_seq OWNER TO musicbrainz;

--
-- Name: label_alias_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE label_alias_type_id_seq OWNED BY label_alias_type.id;


--
-- Name: label_data; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_data (
    label_data_id integer NOT NULL,
    name integer NOT NULL,
    sort_name integer NOT NULL,
    begin_date_year smallint,
    begin_date_month smallint,
    begin_date_day smallint,
    end_date_year smallint,
    end_date_month smallint,
    end_date_day smallint,
    label_type_id integer,
    label_code label_code,
    country_id integer,
    comment text DEFAULT ''::text NOT NULL,
    ended boolean DEFAULT false NOT NULL
);


ALTER TABLE musicbrainz.label_data OWNER TO musicbrainz;

--
-- Name: label_data_label_data_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE label_data_label_data_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.label_data_label_data_id_seq OWNER TO musicbrainz;

--
-- Name: label_data_label_data_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE label_data_label_data_id_seq OWNED BY label_data.label_data_id;


--
-- Name: label_ipi; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_ipi (
    label_tree_id integer NOT NULL,
    ipi ipi NOT NULL
);


ALTER TABLE musicbrainz.label_ipi OWNER TO musicbrainz;

--
-- Name: label_meta; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_meta (
    label_id uuid NOT NULL,
    rating rating,
    rating_count natural_integer NOT NULL
);


ALTER TABLE musicbrainz.label_meta OWNER TO musicbrainz;

--
-- Name: label_name; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_name (
    id integer NOT NULL,
    name non_empty_presentational_text NOT NULL
);


ALTER TABLE musicbrainz.label_name OWNER TO musicbrainz;

--
-- Name: label_name_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE label_name_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.label_name_id_seq OWNER TO musicbrainz;

--
-- Name: label_name_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE label_name_id_seq OWNED BY label_name.id;


--
-- Name: label_rating_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_rating_raw (
    label_id uuid NOT NULL,
    rating rating NOT NULL,
    editor_id integer NOT NULL
);


ALTER TABLE musicbrainz.label_rating_raw OWNER TO musicbrainz;

--
-- Name: label_revision; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_revision (
    revision_id integer NOT NULL,
    label_id uuid NOT NULL,
    label_tree_id integer NOT NULL
);


ALTER TABLE musicbrainz.label_revision OWNER TO musicbrainz;

--
-- Name: label_tag; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_tag (
    label_id uuid NOT NULL,
    tag_id integer NOT NULL,
    count positive_integer NOT NULL
);


ALTER TABLE musicbrainz.label_tag OWNER TO musicbrainz;

--
-- Name: label_tag_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_tag_raw (
    label_id uuid NOT NULL,
    tag_id integer NOT NULL,
    editor_id integer NOT NULL
);


ALTER TABLE musicbrainz.label_tag_raw OWNER TO musicbrainz;

--
-- Name: label_tree; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_tree (
    label_tree_id integer NOT NULL,
    label_data_id integer NOT NULL,
    annotation text
);


ALTER TABLE musicbrainz.label_tree OWNER TO musicbrainz;

--
-- Name: label_tree_label_tree_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE label_tree_label_tree_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.label_tree_label_tree_id_seq OWNER TO musicbrainz;

--
-- Name: label_tree_label_tree_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE label_tree_label_tree_id_seq OWNED BY label_tree.label_tree_id;


--
-- Name: label_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_type (
    id integer NOT NULL,
    name presentational_text NOT NULL
);


ALTER TABLE musicbrainz.label_type OWNER TO musicbrainz;

--
-- Name: label_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE label_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.label_type_id_seq OWNER TO musicbrainz;

--
-- Name: label_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE label_type_id_seq OWNED BY label_type.id;


--
-- Name: language; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE language (
    id integer NOT NULL,
    iso_code_2t character(3),
    iso_code_2b character(3),
    iso_code_1 character(2),
    iso_code_3 character(3),
    name non_empty_presentational_text NOT NULL,
    frequency natural_integer DEFAULT 0 NOT NULL
);


ALTER TABLE musicbrainz.language OWNER TO musicbrainz;

--
-- Name: language_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE language_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.language_id_seq OWNER TO musicbrainz;

--
-- Name: language_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE language_id_seq OWNED BY language.id;


--
-- Name: link; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE link (
    id integer NOT NULL,
    link_type integer NOT NULL,
    begin_date partial_date,
    end_date partial_date,
    attribute_count natural_integer DEFAULT 0 NOT NULL,
    created timestamp with time zone DEFAULT now(),
    ended boolean DEFAULT false NOT NULL
);


ALTER TABLE musicbrainz.link OWNER TO musicbrainz;

--
-- Name: link_attribute; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE link_attribute (
    link integer NOT NULL,
    attribute_type integer NOT NULL,
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.link_attribute OWNER TO musicbrainz;

--
-- Name: link_attribute_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE link_attribute_type (
    id integer NOT NULL,
    parent integer,
    root integer NOT NULL,
    child_order natural_integer DEFAULT 0 NOT NULL,
    gid uuid NOT NULL,
    name character varying(255) NOT NULL,
    description text,
    last_updated timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.link_attribute_type OWNER TO musicbrainz;

--
-- Name: link_attribute_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE link_attribute_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.link_attribute_type_id_seq OWNER TO musicbrainz;

--
-- Name: link_attribute_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE link_attribute_type_id_seq OWNED BY link_attribute_type.id;


--
-- Name: link_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE link_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.link_id_seq OWNER TO musicbrainz;

--
-- Name: link_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE link_id_seq OWNED BY link.id;


--
-- Name: link_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE link_type (
    id integer NOT NULL,
    parent integer,
    child_order natural_integer DEFAULT 0 NOT NULL,
    gid uuid NOT NULL,
    entity_type0 relationship_endpoint NOT NULL,
    entity_type1 relationship_endpoint NOT NULL,
    name non_empty_presentational_text NOT NULL,
    description non_empty_presentational_text,
    link_phrase non_empty_presentational_text NOT NULL,
    reverse_link_phrase non_empty_presentational_text NOT NULL,
    short_link_phrase non_empty_presentational_text NOT NULL,
    priority natural_integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.link_type OWNER TO musicbrainz;

--
-- Name: link_type_attribute_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE link_type_attribute_type (
    link_type integer NOT NULL,
    attribute_type integer NOT NULL,
    min natural_integer,
    max natural_integer,
    last_updated timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.link_type_attribute_type OWNER TO musicbrainz;

--
-- Name: link_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE link_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.link_type_id_seq OWNER TO musicbrainz;

--
-- Name: link_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE link_type_id_seq OWNED BY link_type.id;


--
-- Name: medium; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE medium (
    tracklist_id integer NOT NULL,
    release_tree_id integer NOT NULL,
    "position" positive_integer NOT NULL,
    medium_format_id integer,
    name non_empty_presentational_text
);


ALTER TABLE musicbrainz.medium OWNER TO musicbrainz;

--
-- Name: medium_cdtoc; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE medium_cdtoc (
    id integer NOT NULL,
    release_tree_id integer NOT NULL,
    "position" positive_integer NOT NULL,
    cdtoc integer NOT NULL
);


ALTER TABLE musicbrainz.medium_cdtoc OWNER TO musicbrainz;

--
-- Name: medium_cdtoc_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE medium_cdtoc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.medium_cdtoc_id_seq OWNER TO musicbrainz;

--
-- Name: medium_cdtoc_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE medium_cdtoc_id_seq OWNED BY medium_cdtoc.id;


--
-- Name: medium_format; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE medium_format (
    id integer NOT NULL,
    name non_empty_presentational_text NOT NULL,
    parent integer,
    child_order natural_integer DEFAULT 0 NOT NULL,
    year smallint,
    has_discids boolean DEFAULT false NOT NULL
);


ALTER TABLE musicbrainz.medium_format OWNER TO musicbrainz;

--
-- Name: medium_format_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE medium_format_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.medium_format_id_seq OWNER TO musicbrainz;

--
-- Name: medium_format_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE medium_format_id_seq OWNED BY medium_format.id;


--
-- Name: puid; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE puid (
    id integer NOT NULL,
    puid uuid NOT NULL,
    version integer NOT NULL
);


ALTER TABLE musicbrainz.puid OWNER TO musicbrainz;

--
-- Name: puid_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE puid_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.puid_id_seq OWNER TO musicbrainz;

--
-- Name: puid_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE puid_id_seq OWNED BY puid.id;


--
-- Name: recording; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording (
    recording_id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    master_revision_id integer NOT NULL,
    merged_into uuid
);


ALTER TABLE musicbrainz.recording OWNER TO musicbrainz;

--
-- Name: recording_data; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording_data (
    recording_data_id integer NOT NULL,
    name integer NOT NULL,
    artist_credit_id integer NOT NULL,
    length positive_integer,
    comment presentational_text DEFAULT ''::text NOT NULL
);


ALTER TABLE musicbrainz.recording_data OWNER TO musicbrainz;

--
-- Name: recording_data_recording_data_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE recording_data_recording_data_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.recording_data_recording_data_id_seq OWNER TO musicbrainz;

--
-- Name: recording_data_recording_data_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE recording_data_recording_data_id_seq OWNED BY recording_data.recording_data_id;


--
-- Name: recording_meta; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording_meta (
    recording_id uuid NOT NULL,
    rating rating,
    rating_count natural_integer NOT NULL
);


ALTER TABLE musicbrainz.recording_meta OWNER TO musicbrainz;

--
-- Name: recording_puid; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording_puid (
    puid_id integer NOT NULL,
    recording_tree_id integer NOT NULL
);


ALTER TABLE musicbrainz.recording_puid OWNER TO musicbrainz;

--
-- Name: recording_rating_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording_rating_raw (
    recording_id uuid NOT NULL,
    rating rating NOT NULL,
    editor_id integer NOT NULL
);


ALTER TABLE musicbrainz.recording_rating_raw OWNER TO musicbrainz;

--
-- Name: recording_revision; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording_revision (
    revision_id integer NOT NULL,
    recording_id uuid NOT NULL,
    recording_tree_id integer NOT NULL
);


ALTER TABLE musicbrainz.recording_revision OWNER TO musicbrainz;

--
-- Name: recording_tag; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording_tag (
    recording_id uuid NOT NULL,
    tag_id integer NOT NULL,
    count positive_integer NOT NULL
);


ALTER TABLE musicbrainz.recording_tag OWNER TO musicbrainz;

--
-- Name: recording_tag_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording_tag_raw (
    recording_id uuid NOT NULL,
    tag_id integer NOT NULL,
    editor_id integer NOT NULL
);


ALTER TABLE musicbrainz.recording_tag_raw OWNER TO musicbrainz;

--
-- Name: recording_tree; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording_tree (
    recording_tree_id integer NOT NULL,
    recording_data_id integer NOT NULL,
    annotation text
);


ALTER TABLE musicbrainz.recording_tree OWNER TO musicbrainz;

--
-- Name: recording_tree_recording_tree_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE recording_tree_recording_tree_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.recording_tree_recording_tree_id_seq OWNER TO musicbrainz;

--
-- Name: recording_tree_recording_tree_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE recording_tree_recording_tree_id_seq OWNED BY recording_tree.recording_tree_id;


--
-- Name: release; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release (
    release_id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    master_revision_id integer NOT NULL,
    merged_into uuid
);


ALTER TABLE musicbrainz.release OWNER TO musicbrainz;

--
-- Name: release_coverart; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_coverart (
    release_id uuid NOT NULL,
    last_updated timestamp with time zone,
    cover_art_url character varying(255)
);


ALTER TABLE musicbrainz.release_coverart OWNER TO musicbrainz;

--
-- Name: release_data; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_data (
    release_data_id integer NOT NULL,
    name integer NOT NULL,
    artist_credit_id integer NOT NULL,
    release_status_id integer,
    release_packaging_id integer,
    country_id integer,
    language_id integer,
    script_id integer,
    date_year smallint,
    date_month smallint,
    date_day smallint,
    barcode character varying(255),
    comment presentational_text DEFAULT ''::text NOT NULL
);


ALTER TABLE musicbrainz.release_data OWNER TO musicbrainz;

--
-- Name: release_data_release_data_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_data_release_data_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_data_release_data_id_seq OWNER TO musicbrainz;

--
-- Name: release_data_release_data_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_data_release_data_id_seq OWNED BY release_data.release_data_id;


--
-- Name: release_group; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group (
    release_group_id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    master_revision_id integer NOT NULL,
    merged_into uuid
);


ALTER TABLE musicbrainz.release_group OWNER TO musicbrainz;

--
-- Name: release_group_data; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_data (
    release_group_data_id integer NOT NULL,
    name integer NOT NULL,
    artist_credit_id integer NOT NULL,
    release_group_primary_type_id integer,
    comment presentational_text DEFAULT ''::text NOT NULL
);


ALTER TABLE musicbrainz.release_group_data OWNER TO musicbrainz;

--
-- Name: release_group_data_release_group_data_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_group_data_release_group_data_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_group_data_release_group_data_id_seq OWNER TO musicbrainz;

--
-- Name: release_group_data_release_group_data_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_group_data_release_group_data_id_seq OWNED BY release_group_data.release_group_data_id;


--
-- Name: release_group_meta; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_meta (
    release_group_id uuid NOT NULL,
    release_count natural_integer DEFAULT 0 NOT NULL,
    first_release_date partial_date,
    rating rating,
    rating_count natural_integer NOT NULL
);


ALTER TABLE musicbrainz.release_group_meta OWNER TO musicbrainz;

--
-- Name: release_group_primary_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_primary_type (
    id integer NOT NULL,
    name non_empty_presentational_text NOT NULL
);


ALTER TABLE musicbrainz.release_group_primary_type OWNER TO musicbrainz;

--
-- Name: release_group_primary_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_group_primary_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_group_primary_type_id_seq OWNER TO musicbrainz;

--
-- Name: release_group_primary_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_group_primary_type_id_seq OWNED BY release_group_primary_type.id;


--
-- Name: release_group_rating_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_rating_raw (
    release_group_id uuid NOT NULL,
    rating rating NOT NULL,
    editor_id integer NOT NULL
);


ALTER TABLE musicbrainz.release_group_rating_raw OWNER TO musicbrainz;

--
-- Name: release_group_revision; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_revision (
    revision_id integer NOT NULL,
    release_group_id uuid NOT NULL,
    release_group_tree_id integer NOT NULL
);


ALTER TABLE musicbrainz.release_group_revision OWNER TO musicbrainz;

--
-- Name: release_group_secondary_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_secondary_type (
    id integer NOT NULL,
    name non_empty_presentational_text NOT NULL
);


ALTER TABLE musicbrainz.release_group_secondary_type OWNER TO musicbrainz;

--
-- Name: release_group_secondary_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_group_secondary_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_group_secondary_type_id_seq OWNER TO musicbrainz;

--
-- Name: release_group_secondary_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_group_secondary_type_id_seq OWNED BY release_group_secondary_type.id;


--
-- Name: release_group_tag; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_tag (
    release_group_id uuid NOT NULL,
    tag_id integer NOT NULL,
    count positive_integer NOT NULL
);


ALTER TABLE musicbrainz.release_group_tag OWNER TO musicbrainz;

--
-- Name: release_group_tag_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_tag_raw (
    release_group_id uuid NOT NULL,
    tag_id integer NOT NULL,
    editor_id integer NOT NULL
);


ALTER TABLE musicbrainz.release_group_tag_raw OWNER TO musicbrainz;

--
-- Name: release_group_tree_release_group_tree_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_group_tree_release_group_tree_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_group_tree_release_group_tree_id_seq OWNER TO musicbrainz;

--
-- Name: release_group_tree; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_tree (
    release_group_tree_id integer DEFAULT nextval('release_group_tree_release_group_tree_id_seq'::regclass) NOT NULL,
    release_group_data_id integer NOT NULL,
    annotation text
);


ALTER TABLE musicbrainz.release_group_tree OWNER TO musicbrainz;

--
-- Name: release_group_tree_secondary_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_tree_secondary_type (
    release_group_tree_id integer NOT NULL,
    release_group_secondary_type_id integer NOT NULL
);


ALTER TABLE musicbrainz.release_group_tree_secondary_type OWNER TO musicbrainz;

--
-- Name: release_label; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_label (
    release_tree_id integer NOT NULL,
    label_id uuid,
    catalog_number non_empty_presentational_text
);


ALTER TABLE musicbrainz.release_label OWNER TO musicbrainz;

--
-- Name: release_meta; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_meta (
    release_id uuid NOT NULL,
    date_added timestamp with time zone DEFAULT now(),
    info_url character varying(255),
    amazon_asin character varying(10),
    amazon_store character varying(20),
    cover_art_presence cover_art_presence DEFAULT 'absent'::cover_art_presence NOT NULL
);


ALTER TABLE musicbrainz.release_meta OWNER TO musicbrainz;

--
-- Name: release_name; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_name (
    id integer NOT NULL,
    name non_empty_presentational_text NOT NULL
);


ALTER TABLE musicbrainz.release_name OWNER TO musicbrainz;

--
-- Name: release_name_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_name_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_name_id_seq OWNER TO musicbrainz;

--
-- Name: release_name_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_name_id_seq OWNED BY release_name.id;


--
-- Name: release_packaging; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_packaging (
    id integer NOT NULL,
    name non_empty_presentational_text NOT NULL
);


ALTER TABLE musicbrainz.release_packaging OWNER TO musicbrainz;

--
-- Name: release_packaging_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_packaging_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_packaging_id_seq OWNER TO musicbrainz;

--
-- Name: release_packaging_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_packaging_id_seq OWNED BY release_packaging.id;


--
-- Name: release_revision; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_revision (
    revision_id integer NOT NULL,
    release_id uuid NOT NULL,
    release_tree_id integer NOT NULL
);


ALTER TABLE musicbrainz.release_revision OWNER TO musicbrainz;

--
-- Name: release_status; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_status (
    id integer NOT NULL,
    name non_empty_presentational_text NOT NULL
);


ALTER TABLE musicbrainz.release_status OWNER TO musicbrainz;

--
-- Name: release_status_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_status_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_status_id_seq OWNER TO musicbrainz;

--
-- Name: release_status_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_status_id_seq OWNED BY release_status.id;


--
-- Name: release_tag; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_tag (
    release_id uuid NOT NULL,
    tag_id integer NOT NULL,
    count positive_integer NOT NULL
);


ALTER TABLE musicbrainz.release_tag OWNER TO musicbrainz;

--
-- Name: release_tag_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_tag_raw (
    release_id uuid NOT NULL,
    tag_id integer NOT NULL,
    editor_id integer NOT NULL
);


ALTER TABLE musicbrainz.release_tag_raw OWNER TO musicbrainz;

--
-- Name: release_tree; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_tree (
    release_tree_id integer NOT NULL,
    release_data_id integer NOT NULL,
    release_group_id uuid NOT NULL,
    annotation text
);


ALTER TABLE musicbrainz.release_tree OWNER TO musicbrainz;

--
-- Name: release_tree_release_tree_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_tree_release_tree_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_tree_release_tree_id_seq OWNER TO musicbrainz;

--
-- Name: release_tree_release_tree_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_tree_release_tree_id_seq OWNED BY release_tree.release_tree_id;


--
-- Name: revision; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE revision (
    revision_id integer NOT NULL,
    editor_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE musicbrainz.revision OWNER TO musicbrainz;

--
-- Name: revision_parent; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE revision_parent (
    revision_id integer NOT NULL,
    parent_revision_id integer NOT NULL,
    CONSTRAINT revision_parent_check CHECK ((revision_id <> parent_revision_id))
);


ALTER TABLE musicbrainz.revision_parent OWNER TO musicbrainz;

--
-- Name: revision_path; Type: VIEW; Schema: musicbrainz; Owner: musicbrainz
--

CREATE VIEW revision_path AS
    WITH RECURSIVE revision_path(revision_id, parent_revision_id, distance) AS (SELECT revision_parent.revision_id, revision_parent.parent_revision_id, 1 FROM revision_parent UNION SELECT revision_path.revision_id, revision_parent.parent_revision_id, (revision_path.distance + 1) FROM (revision_parent JOIN revision_path ON ((revision_parent.revision_id = revision_path.parent_revision_id)))) SELECT revision_path.revision_id, revision_path.parent_revision_id, revision_path.distance FROM revision_path;


ALTER TABLE musicbrainz.revision_path OWNER TO musicbrainz;

--
-- Name: revision_revision_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE revision_revision_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.revision_revision_id_seq OWNER TO musicbrainz;

--
-- Name: revision_revision_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE revision_revision_id_seq OWNED BY revision.revision_id;


--
-- Name: script; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE script (
    id integer NOT NULL,
    iso_code character(4) NOT NULL,
    iso_number character(3) NOT NULL,
    name non_empty_presentational_text NOT NULL,
    frequency natural_integer DEFAULT 0 NOT NULL
);


ALTER TABLE musicbrainz.script OWNER TO musicbrainz;

--
-- Name: script_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE script_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.script_id_seq OWNER TO musicbrainz;

--
-- Name: script_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE script_id_seq OWNED BY script.id;


--
-- Name: script_language; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE script_language (
    id integer NOT NULL,
    script integer NOT NULL,
    language integer NOT NULL,
    frequency natural_integer DEFAULT 0 NOT NULL
);


ALTER TABLE musicbrainz.script_language OWNER TO musicbrainz;

--
-- Name: script_language_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE script_language_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.script_language_id_seq OWNER TO musicbrainz;

--
-- Name: script_language_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE script_language_id_seq OWNED BY script_language.id;


--
-- Name: tag; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE tag (
    id integer NOT NULL,
    name non_empty_presentational_text NOT NULL,
    ref_count natural_integer DEFAULT 0 NOT NULL
);


ALTER TABLE musicbrainz.tag OWNER TO musicbrainz;

--
-- Name: tag_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.tag_id_seq OWNER TO musicbrainz;

--
-- Name: tag_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE tag_id_seq OWNED BY tag.id;


--
-- Name: tag_relation; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE tag_relation (
    tag1 integer NOT NULL,
    tag2 integer NOT NULL,
    weight integer NOT NULL,
    last_updated timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.tag_relation OWNER TO musicbrainz;

--
-- Name: track; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE track (
    recording_id uuid NOT NULL,
    tracklist_id integer NOT NULL,
    "position" positive_integer NOT NULL,
    name integer NOT NULL,
    artist_credit_id integer NOT NULL,
    length positive_integer,
    number non_empty_presentational_text NOT NULL
);


ALTER TABLE musicbrainz.track OWNER TO musicbrainz;

--
-- Name: track_name; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE track_name (
    id integer NOT NULL,
    name non_empty_presentational_text NOT NULL
);


ALTER TABLE musicbrainz.track_name OWNER TO musicbrainz;

--
-- Name: track_name_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE track_name_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.track_name_id_seq OWNER TO musicbrainz;

--
-- Name: track_name_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE track_name_id_seq OWNED BY track_name.id;


--
-- Name: tracklist; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE tracklist (
    id integer NOT NULL,
    track_count natural_integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.tracklist OWNER TO musicbrainz;

--
-- Name: tracklist_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE tracklist_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.tracklist_id_seq OWNER TO musicbrainz;

--
-- Name: tracklist_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE tracklist_id_seq OWNED BY tracklist.id;


--
-- Name: tracklist_index; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE tracklist_index (
    tracklist integer NOT NULL,
    toc public.cube
);


ALTER TABLE musicbrainz.tracklist_index OWNER TO musicbrainz;

--
-- Name: url; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE url (
    url_id uuid NOT NULL,
    master_revision_id integer NOT NULL,
    merged_into uuid
);


ALTER TABLE musicbrainz.url OWNER TO musicbrainz;

--
-- Name: url_data; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE url_data (
    url_data_id integer NOT NULL,
    url non_empty_presentational_text NOT NULL,
    comment presentational_text DEFAULT ''::text NOT NULL
);


ALTER TABLE musicbrainz.url_data OWNER TO musicbrainz;

--
-- Name: url_data_url_data_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE url_data_url_data_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.url_data_url_data_id_seq OWNER TO musicbrainz;

--
-- Name: url_data_url_data_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE url_data_url_data_id_seq OWNED BY url_data.url_data_id;


--
-- Name: url_revision; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE url_revision (
    revision_id integer NOT NULL,
    url_id uuid NOT NULL,
    url_tree_id integer NOT NULL
);


ALTER TABLE musicbrainz.url_revision OWNER TO musicbrainz;

--
-- Name: url_tree; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE url_tree (
    url_tree_id integer NOT NULL,
    url_data_id integer NOT NULL,
    annotation text
);


ALTER TABLE musicbrainz.url_tree OWNER TO musicbrainz;

--
-- Name: vote; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE vote (
    editor_id integer NOT NULL,
    edit_id integer NOT NULL,
    vote smallint NOT NULL,
    vote_time timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT vote_vote_check CHECK ((vote = ANY (ARRAY[(-1), 0, 1])))
);


ALTER TABLE musicbrainz.vote OWNER TO musicbrainz;

--
-- Name: work; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work (
    work_id uuid NOT NULL,
    master_revision_id integer NOT NULL,
    merged_into uuid
);


ALTER TABLE musicbrainz.work OWNER TO musicbrainz;

--
-- Name: work_alias; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_alias (
    work_tree_id integer NOT NULL,
    name integer NOT NULL,
    sort_name integer NOT NULL,
    locale locale,
    work_alias_type_id integer,
    begin_date partial_date,
    end_date partial_date,
    primary_for_locale boolean DEFAULT false NOT NULL
);


ALTER TABLE musicbrainz.work_alias OWNER TO musicbrainz;

--
-- Name: work_alias_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_alias_type (
    id integer NOT NULL,
    name non_empty_presentational_text NOT NULL
);


ALTER TABLE musicbrainz.work_alias_type OWNER TO musicbrainz;

--
-- Name: work_alias_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE work_alias_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.work_alias_type_id_seq OWNER TO musicbrainz;

--
-- Name: work_alias_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE work_alias_type_id_seq OWNED BY work_alias_type.id;


--
-- Name: work_data; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_data (
    work_data_id integer NOT NULL,
    name integer NOT NULL,
    work_type_id integer,
    comment presentational_text DEFAULT ''::text NOT NULL,
    language_id integer
);


ALTER TABLE musicbrainz.work_data OWNER TO musicbrainz;

--
-- Name: work_data_work_data_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE work_data_work_data_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.work_data_work_data_id_seq OWNER TO musicbrainz;

--
-- Name: work_data_work_data_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE work_data_work_data_id_seq OWNED BY work_data.work_data_id;


--
-- Name: work_meta; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_meta (
    work_id uuid NOT NULL,
    rating rating,
    rating_count natural_integer NOT NULL
);


ALTER TABLE musicbrainz.work_meta OWNER TO musicbrainz;

--
-- Name: work_name; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_name (
    id integer NOT NULL,
    name non_empty_presentational_text NOT NULL
);


ALTER TABLE musicbrainz.work_name OWNER TO musicbrainz;

--
-- Name: work_name_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE work_name_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.work_name_id_seq OWNER TO musicbrainz;

--
-- Name: work_name_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE work_name_id_seq OWNED BY work_name.id;


--
-- Name: work_rating_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_rating_raw (
    work_id uuid NOT NULL,
    rating rating NOT NULL,
    editor_id integer NOT NULL
);


ALTER TABLE musicbrainz.work_rating_raw OWNER TO musicbrainz;

--
-- Name: work_revision; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_revision (
    revision_id integer NOT NULL,
    work_id uuid NOT NULL,
    work_tree_id integer NOT NULL
);


ALTER TABLE musicbrainz.work_revision OWNER TO musicbrainz;

--
-- Name: work_tag; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_tag (
    work_id uuid NOT NULL,
    tag_id integer NOT NULL,
    count positive_integer NOT NULL
);


ALTER TABLE musicbrainz.work_tag OWNER TO musicbrainz;

--
-- Name: work_tag_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_tag_raw (
    work_id uuid NOT NULL,
    tag_id integer NOT NULL,
    editor_id integer NOT NULL
);


ALTER TABLE musicbrainz.work_tag_raw OWNER TO musicbrainz;

--
-- Name: work_tree; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_tree (
    work_tree_id integer NOT NULL,
    work_data_id integer NOT NULL,
    annotation text
);


ALTER TABLE musicbrainz.work_tree OWNER TO musicbrainz;

--
-- Name: work_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_type (
    id integer NOT NULL,
    name non_empty_presentational_text NOT NULL
);


ALTER TABLE musicbrainz.work_type OWNER TO musicbrainz;

--
-- Name: work_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE work_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.work_type_id_seq OWNER TO musicbrainz;

--
-- Name: work_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE work_type_id_seq OWNED BY work_type.id;


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_alias_type ALTER COLUMN id SET DEFAULT nextval('artist_alias_type_id_seq'::regclass);


--
-- Name: artist_credit_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_credit ALTER COLUMN artist_credit_id SET DEFAULT nextval('artist_credit_artist_credit_id_seq'::regclass);


--
-- Name: artist_data_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_data ALTER COLUMN artist_data_id SET DEFAULT nextval('artist_data_artist_data_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_name ALTER COLUMN id SET DEFAULT nextval('artist_name_id_seq'::regclass);


--
-- Name: artist_tree_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_tree ALTER COLUMN artist_tree_id SET DEFAULT nextval('artist_tree_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_type ALTER COLUMN id SET DEFAULT nextval('artist_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY cdtoc ALTER COLUMN id SET DEFAULT nextval('cdtoc_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY clientversion ALTER COLUMN id SET DEFAULT nextval('clientversion_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY country ALTER COLUMN id SET DEFAULT nextval('country_id_seq'::regclass);


--
-- Name: edit_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit ALTER COLUMN edit_id SET DEFAULT nextval('edit_edit_id_seq'::regclass);


--
-- Name: edit_note_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_note ALTER COLUMN edit_note_id SET DEFAULT nextval('edit_note_edit_note_id_seq'::regclass);


--
-- Name: editor_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor ALTER COLUMN editor_id SET DEFAULT nextval('editor_editor_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY gender ALTER COLUMN id SET DEFAULT nextval('gender_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY isrc ALTER COLUMN id SET DEFAULT nextval('isrc_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY iswc ALTER COLUMN id SET DEFAULT nextval('iswc_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_alias_type ALTER COLUMN id SET DEFAULT nextval('label_alias_type_id_seq'::regclass);


--
-- Name: label_data_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_data ALTER COLUMN label_data_id SET DEFAULT nextval('label_data_label_data_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_name ALTER COLUMN id SET DEFAULT nextval('label_name_id_seq'::regclass);


--
-- Name: label_tree_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_tree ALTER COLUMN label_tree_id SET DEFAULT nextval('label_tree_label_tree_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_type ALTER COLUMN id SET DEFAULT nextval('label_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY language ALTER COLUMN id SET DEFAULT nextval('language_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link ALTER COLUMN id SET DEFAULT nextval('link_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_attribute_type ALTER COLUMN id SET DEFAULT nextval('link_attribute_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_type ALTER COLUMN id SET DEFAULT nextval('link_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium_cdtoc ALTER COLUMN id SET DEFAULT nextval('medium_cdtoc_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium_format ALTER COLUMN id SET DEFAULT nextval('medium_format_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY puid ALTER COLUMN id SET DEFAULT nextval('puid_id_seq'::regclass);


--
-- Name: recording_data_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_data ALTER COLUMN recording_data_id SET DEFAULT nextval('recording_data_recording_data_id_seq'::regclass);


--
-- Name: recording_tree_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_tree ALTER COLUMN recording_tree_id SET DEFAULT nextval('recording_tree_recording_tree_id_seq'::regclass);


--
-- Name: release_data_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_data ALTER COLUMN release_data_id SET DEFAULT nextval('release_data_release_data_id_seq'::regclass);


--
-- Name: release_group_data_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_data ALTER COLUMN release_group_data_id SET DEFAULT nextval('release_group_data_release_group_data_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_primary_type ALTER COLUMN id SET DEFAULT nextval('release_group_primary_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_secondary_type ALTER COLUMN id SET DEFAULT nextval('release_group_secondary_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_name ALTER COLUMN id SET DEFAULT nextval('release_name_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_packaging ALTER COLUMN id SET DEFAULT nextval('release_packaging_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_status ALTER COLUMN id SET DEFAULT nextval('release_status_id_seq'::regclass);


--
-- Name: release_tree_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_tree ALTER COLUMN release_tree_id SET DEFAULT nextval('release_tree_release_tree_id_seq'::regclass);


--
-- Name: revision_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY revision ALTER COLUMN revision_id SET DEFAULT nextval('revision_revision_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY script ALTER COLUMN id SET DEFAULT nextval('script_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY script_language ALTER COLUMN id SET DEFAULT nextval('script_language_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY tag ALTER COLUMN id SET DEFAULT nextval('tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY track_name ALTER COLUMN id SET DEFAULT nextval('track_name_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY tracklist ALTER COLUMN id SET DEFAULT nextval('tracklist_id_seq'::regclass);


--
-- Name: url_data_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY url_data ALTER COLUMN url_data_id SET DEFAULT nextval('url_data_url_data_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_alias_type ALTER COLUMN id SET DEFAULT nextval('work_alias_type_id_seq'::regclass);


--
-- Name: work_data_id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_data ALTER COLUMN work_data_id SET DEFAULT nextval('work_data_work_data_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_name ALTER COLUMN id SET DEFAULT nextval('work_name_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_type ALTER COLUMN id SET DEFAULT nextval('work_type_id_seq'::regclass);


--
-- Name: artist_alias_artist_tree_id_name_locale_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_alias
    ADD CONSTRAINT artist_alias_artist_tree_id_name_locale_key UNIQUE (artist_tree_id, name, locale);


--
-- Name: artist_alias_type_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_alias_type
    ADD CONSTRAINT artist_alias_type_id_key UNIQUE (id);


--
-- Name: artist_alias_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_alias_type
    ADD CONSTRAINT artist_alias_type_pkey PRIMARY KEY (name);


--
-- Name: artist_credit_name_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_credit_name
    ADD CONSTRAINT artist_credit_name_pkey PRIMARY KEY (artist_credit_id, "position");


--
-- Name: artist_credit_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_credit
    ADD CONSTRAINT artist_credit_pkey PRIMARY KEY (artist_credit_id);


--
-- Name: artist_data_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_data
    ADD CONSTRAINT artist_data_pkey PRIMARY KEY (artist_data_id);


--
-- Name: artist_ipi_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_ipi
    ADD CONSTRAINT artist_ipi_pkey PRIMARY KEY (artist_tree_id, ipi);


--
-- Name: artist_meta_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_meta
    ADD CONSTRAINT artist_meta_pkey PRIMARY KEY (artist_id);


--
-- Name: artist_name_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_name
    ADD CONSTRAINT artist_name_id_key UNIQUE (id);


--
-- Name: artist_name_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_name
    ADD CONSTRAINT artist_name_pkey PRIMARY KEY (name);


--
-- Name: artist_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist
    ADD CONSTRAINT artist_pkey PRIMARY KEY (artist_id);


--
-- Name: artist_rating_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_rating_raw
    ADD CONSTRAINT artist_rating_raw_pkey PRIMARY KEY (artist_id, editor_id);


--
-- Name: artist_revision_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_revision
    ADD CONSTRAINT artist_revision_pkey PRIMARY KEY (revision_id);


--
-- Name: artist_tag_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_tag
    ADD CONSTRAINT artist_tag_pkey PRIMARY KEY (artist_id, tag_id);


--
-- Name: artist_tag_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_tag_raw
    ADD CONSTRAINT artist_tag_raw_pkey PRIMARY KEY (artist_id, tag_id, editor_id);


--
-- Name: artist_tree_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_tree
    ADD CONSTRAINT artist_tree_pkey PRIMARY KEY (artist_tree_id);


--
-- Name: artist_type_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_type
    ADD CONSTRAINT artist_type_id_key UNIQUE (id);


--
-- Name: artist_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_type
    ADD CONSTRAINT artist_type_pkey PRIMARY KEY (name);


--
-- Name: cdtoc_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY cdtoc
    ADD CONSTRAINT cdtoc_id_key UNIQUE (id);


--
-- Name: cdtoc_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY cdtoc
    ADD CONSTRAINT cdtoc_pkey PRIMARY KEY (discid);


--
-- Name: clientversion_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY clientversion
    ADD CONSTRAINT clientversion_id_key UNIQUE (id);


--
-- Name: clientversion_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY clientversion
    ADD CONSTRAINT clientversion_pkey PRIMARY KEY (version);


--
-- Name: country_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY country
    ADD CONSTRAINT country_id_key UNIQUE (id);


--
-- Name: country_name_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY country
    ADD CONSTRAINT country_name_key UNIQUE (name);


--
-- Name: country_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY country
    ADD CONSTRAINT country_pkey PRIMARY KEY (iso_code);


--
-- Name: edit_artist_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY edit_artist
    ADD CONSTRAINT edit_artist_pkey PRIMARY KEY (edit_id, revision_id);


--
-- Name: edit_note_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY edit_note
    ADD CONSTRAINT edit_note_pkey PRIMARY KEY (edit_note_id);


--
-- Name: edit_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY edit
    ADD CONSTRAINT edit_pkey PRIMARY KEY (edit_id);


--
-- Name: editor_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor
    ADD CONSTRAINT editor_pkey PRIMARY KEY (editor_id);


--
-- Name: gender_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY gender
    ADD CONSTRAINT gender_id_key UNIQUE (id);


--
-- Name: gender_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY gender
    ADD CONSTRAINT gender_pkey PRIMARY KEY (name);


--
-- Name: isrc_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY isrc
    ADD CONSTRAINT isrc_pkey PRIMARY KEY (recording_tree_id, isrc);


--
-- Name: iswc_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY iswc
    ADD CONSTRAINT iswc_pkey PRIMARY KEY (work_tree_id, iswc);


--
-- Name: label_alias_label_tree_id_name_locale_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_alias
    ADD CONSTRAINT label_alias_label_tree_id_name_locale_key UNIQUE (label_tree_id, name, locale);


--
-- Name: label_alias_type_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_alias_type
    ADD CONSTRAINT label_alias_type_id_key UNIQUE (id);


--
-- Name: label_alias_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_alias_type
    ADD CONSTRAINT label_alias_type_pkey PRIMARY KEY (name);


--
-- Name: label_data_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_data
    ADD CONSTRAINT label_data_pkey PRIMARY KEY (label_data_id);


--
-- Name: label_ipi_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_ipi
    ADD CONSTRAINT label_ipi_pkey PRIMARY KEY (label_tree_id, ipi);


--
-- Name: label_meta_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_meta
    ADD CONSTRAINT label_meta_pkey PRIMARY KEY (label_id);


--
-- Name: label_name_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_name
    ADD CONSTRAINT label_name_id_key UNIQUE (id);


--
-- Name: label_name_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_name
    ADD CONSTRAINT label_name_pkey PRIMARY KEY (name);


--
-- Name: label_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label
    ADD CONSTRAINT label_pkey PRIMARY KEY (label_id);


--
-- Name: label_rating_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_rating_raw
    ADD CONSTRAINT label_rating_raw_pkey PRIMARY KEY (label_id, editor_id);


--
-- Name: label_revision_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_revision
    ADD CONSTRAINT label_revision_pkey PRIMARY KEY (revision_id);


--
-- Name: label_tag_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_tag
    ADD CONSTRAINT label_tag_pkey PRIMARY KEY (label_id, tag_id);


--
-- Name: label_tag_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_tag_raw
    ADD CONSTRAINT label_tag_raw_pkey PRIMARY KEY (label_id, tag_id, editor_id);


--
-- Name: label_tree_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_tree
    ADD CONSTRAINT label_tree_pkey PRIMARY KEY (label_tree_id);


--
-- Name: label_type_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_type
    ADD CONSTRAINT label_type_id_key UNIQUE (id);


--
-- Name: label_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_type
    ADD CONSTRAINT label_type_pkey PRIMARY KEY (name);


--
-- Name: language_iso_code_1_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY language
    ADD CONSTRAINT language_iso_code_1_key UNIQUE (iso_code_1);


--
-- Name: language_iso_code_2b_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY language
    ADD CONSTRAINT language_iso_code_2b_key UNIQUE (iso_code_2b);


--
-- Name: language_iso_code_2t_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY language
    ADD CONSTRAINT language_iso_code_2t_key UNIQUE (iso_code_2t);


--
-- Name: language_iso_code_3_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY language
    ADD CONSTRAINT language_iso_code_3_key UNIQUE (iso_code_3);


--
-- Name: language_name_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY language
    ADD CONSTRAINT language_name_key UNIQUE (name);


--
-- Name: language_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY language
    ADD CONSTRAINT language_pkey PRIMARY KEY (id);


--
-- Name: link_attribute_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_attribute
    ADD CONSTRAINT link_attribute_pkey PRIMARY KEY (link, attribute_type);


--
-- Name: link_attribute_type_description_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_attribute_type
    ADD CONSTRAINT link_attribute_type_description_key UNIQUE (description);


--
-- Name: link_attribute_type_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_attribute_type
    ADD CONSTRAINT link_attribute_type_id_key UNIQUE (id);


--
-- Name: link_attribute_type_name_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_attribute_type
    ADD CONSTRAINT link_attribute_type_name_key UNIQUE (name);


--
-- Name: link_attribute_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_attribute_type
    ADD CONSTRAINT link_attribute_type_pkey PRIMARY KEY (gid);


--
-- Name: link_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link
    ADD CONSTRAINT link_pkey PRIMARY KEY (id);


--
-- Name: link_type_attribute_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_type_attribute_type
    ADD CONSTRAINT link_type_attribute_type_pkey PRIMARY KEY (link_type, attribute_type);


--
-- Name: link_type_description_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_type
    ADD CONSTRAINT link_type_description_key UNIQUE (description);


--
-- Name: link_type_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_type
    ADD CONSTRAINT link_type_id_key UNIQUE (id);


--
-- Name: link_type_link_phrase_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_type
    ADD CONSTRAINT link_type_link_phrase_key UNIQUE (link_phrase);


--
-- Name: link_type_name_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_type
    ADD CONSTRAINT link_type_name_key UNIQUE (name);


--
-- Name: link_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_type
    ADD CONSTRAINT link_type_pkey PRIMARY KEY (gid);


--
-- Name: link_type_reverse_link_phrase_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_type
    ADD CONSTRAINT link_type_reverse_link_phrase_key UNIQUE (reverse_link_phrase);


--
-- Name: link_type_short_link_phrase_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_type
    ADD CONSTRAINT link_type_short_link_phrase_key UNIQUE (short_link_phrase);


--
-- Name: medium_cdtoc_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY medium_cdtoc
    ADD CONSTRAINT medium_cdtoc_pkey PRIMARY KEY (release_tree_id, "position", cdtoc);


--
-- Name: medium_format_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY medium_format
    ADD CONSTRAINT medium_format_id_key UNIQUE (id);


--
-- Name: medium_format_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY medium_format
    ADD CONSTRAINT medium_format_pkey PRIMARY KEY (name);


--
-- Name: medium_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY medium
    ADD CONSTRAINT medium_pkey PRIMARY KEY (release_tree_id, "position");


--
-- Name: puid_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY puid
    ADD CONSTRAINT puid_id_key UNIQUE (id);


--
-- Name: puid_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY puid
    ADD CONSTRAINT puid_pkey PRIMARY KEY (puid);


--
-- Name: recording_data_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY recording_data
    ADD CONSTRAINT recording_data_pkey PRIMARY KEY (recording_data_id);


--
-- Name: recording_meta_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY recording_meta
    ADD CONSTRAINT recording_meta_pkey PRIMARY KEY (recording_id);


--
-- Name: recording_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY recording
    ADD CONSTRAINT recording_pkey PRIMARY KEY (recording_id);


--
-- Name: recording_rating_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY recording_rating_raw
    ADD CONSTRAINT recording_rating_raw_pkey PRIMARY KEY (recording_id, editor_id);


--
-- Name: recording_revision_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY recording_revision
    ADD CONSTRAINT recording_revision_pkey PRIMARY KEY (revision_id);


--
-- Name: recording_tag_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY recording_tag
    ADD CONSTRAINT recording_tag_pkey PRIMARY KEY (recording_id, tag_id);


--
-- Name: recording_tag_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY recording_tag_raw
    ADD CONSTRAINT recording_tag_raw_pkey PRIMARY KEY (recording_id, tag_id, editor_id);


--
-- Name: recording_tree_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY recording_tree
    ADD CONSTRAINT recording_tree_pkey PRIMARY KEY (recording_tree_id);


--
-- Name: release_coverart_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_coverart
    ADD CONSTRAINT release_coverart_pkey PRIMARY KEY (release_id);


--
-- Name: release_data_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_data
    ADD CONSTRAINT release_data_pkey PRIMARY KEY (release_data_id);


--
-- Name: release_group_data_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_data
    ADD CONSTRAINT release_group_data_pkey PRIMARY KEY (release_group_data_id);


--
-- Name: release_group_meta_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_meta
    ADD CONSTRAINT release_group_meta_pkey PRIMARY KEY (release_group_id);


--
-- Name: release_group_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group
    ADD CONSTRAINT release_group_pkey PRIMARY KEY (release_group_id);


--
-- Name: release_group_primary_type_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_primary_type
    ADD CONSTRAINT release_group_primary_type_id_key UNIQUE (id);


--
-- Name: release_group_primary_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_primary_type
    ADD CONSTRAINT release_group_primary_type_pkey PRIMARY KEY (name);


--
-- Name: release_group_rating_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_rating_raw
    ADD CONSTRAINT release_group_rating_raw_pkey PRIMARY KEY (release_group_id, editor_id);


--
-- Name: release_group_revision_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_revision
    ADD CONSTRAINT release_group_revision_pkey PRIMARY KEY (revision_id);


--
-- Name: release_group_secondary_type_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_secondary_type
    ADD CONSTRAINT release_group_secondary_type_id_key UNIQUE (id);


--
-- Name: release_group_secondary_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_secondary_type
    ADD CONSTRAINT release_group_secondary_type_pkey PRIMARY KEY (name);


--
-- Name: release_group_tag_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_tag
    ADD CONSTRAINT release_group_tag_pkey PRIMARY KEY (release_group_id, tag_id);


--
-- Name: release_group_tag_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_tag_raw
    ADD CONSTRAINT release_group_tag_raw_pkey PRIMARY KEY (release_group_id, tag_id, editor_id);


--
-- Name: release_group_tree_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_tree
    ADD CONSTRAINT release_group_tree_pkey PRIMARY KEY (release_group_tree_id);


--
-- Name: release_group_tree_secondary_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_tree_secondary_type
    ADD CONSTRAINT release_group_tree_secondary_type_pkey PRIMARY KEY (release_group_tree_id, release_group_secondary_type_id);


--
-- Name: release_label_release_tree_id_label_id_catalog_number_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_label
    ADD CONSTRAINT release_label_release_tree_id_label_id_catalog_number_key UNIQUE (release_tree_id, label_id, catalog_number);


--
-- Name: release_meta_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_meta
    ADD CONSTRAINT release_meta_pkey PRIMARY KEY (release_id);


--
-- Name: release_name_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_name
    ADD CONSTRAINT release_name_id_key UNIQUE (id);


--
-- Name: release_name_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_name
    ADD CONSTRAINT release_name_pkey PRIMARY KEY (name);


--
-- Name: release_packaging_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_packaging
    ADD CONSTRAINT release_packaging_id_key UNIQUE (id);


--
-- Name: release_packaging_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_packaging
    ADD CONSTRAINT release_packaging_pkey PRIMARY KEY (name);


--
-- Name: release_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release
    ADD CONSTRAINT release_pkey PRIMARY KEY (release_id);


--
-- Name: release_revision_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_revision
    ADD CONSTRAINT release_revision_pkey PRIMARY KEY (revision_id);


--
-- Name: release_status_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_status
    ADD CONSTRAINT release_status_id_key UNIQUE (id);


--
-- Name: release_status_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_status
    ADD CONSTRAINT release_status_pkey PRIMARY KEY (name);


--
-- Name: release_tag_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_tag
    ADD CONSTRAINT release_tag_pkey PRIMARY KEY (release_id, tag_id);


--
-- Name: release_tag_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_tag_raw
    ADD CONSTRAINT release_tag_raw_pkey PRIMARY KEY (release_id, tag_id, editor_id);


--
-- Name: release_tree_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_tree
    ADD CONSTRAINT release_tree_pkey PRIMARY KEY (release_tree_id);


--
-- Name: revision_parent_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY revision_parent
    ADD CONSTRAINT revision_parent_pkey PRIMARY KEY (revision_id, parent_revision_id);


--
-- Name: revision_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY revision
    ADD CONSTRAINT revision_pkey PRIMARY KEY (revision_id);


--
-- Name: script_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY script
    ADD CONSTRAINT script_id_key UNIQUE (id);


--
-- Name: script_iso_number_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY script
    ADD CONSTRAINT script_iso_number_key UNIQUE (iso_number);


--
-- Name: script_language_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY script_language
    ADD CONSTRAINT script_language_id_key UNIQUE (id);


--
-- Name: script_language_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY script_language
    ADD CONSTRAINT script_language_pkey PRIMARY KEY (script, language);


--
-- Name: script_name_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY script
    ADD CONSTRAINT script_name_key UNIQUE (name);


--
-- Name: script_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY script
    ADD CONSTRAINT script_pkey PRIMARY KEY (iso_code);


--
-- Name: tag_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY tag
    ADD CONSTRAINT tag_id_key UNIQUE (id);


--
-- Name: tag_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY tag
    ADD CONSTRAINT tag_pkey PRIMARY KEY (name);


--
-- Name: tag_relation_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY tag_relation
    ADD CONSTRAINT tag_relation_pkey PRIMARY KEY (tag1, tag2);


--
-- Name: track_name_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY track_name
    ADD CONSTRAINT track_name_id_key UNIQUE (id);


--
-- Name: track_name_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY track_name
    ADD CONSTRAINT track_name_pkey PRIMARY KEY (name);


--
-- Name: track_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY track
    ADD CONSTRAINT track_pkey PRIMARY KEY (tracklist_id, "position");


--
-- Name: tracklist_index_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY tracklist_index
    ADD CONSTRAINT tracklist_index_pkey PRIMARY KEY (tracklist);


--
-- Name: tracklist_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY tracklist
    ADD CONSTRAINT tracklist_pkey PRIMARY KEY (id);


--
-- Name: url_data_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY url_data
    ADD CONSTRAINT url_data_pkey PRIMARY KEY (url_data_id);


--
-- Name: url_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY url
    ADD CONSTRAINT url_pkey PRIMARY KEY (url_id);


--
-- Name: url_revision_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY url_revision
    ADD CONSTRAINT url_revision_pkey PRIMARY KEY (revision_id);


--
-- Name: url_tree_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY url_tree
    ADD CONSTRAINT url_tree_pkey PRIMARY KEY (url_tree_id);


--
-- Name: work_alias_type_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_alias_type
    ADD CONSTRAINT work_alias_type_id_key UNIQUE (id);


--
-- Name: work_alias_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_alias_type
    ADD CONSTRAINT work_alias_type_pkey PRIMARY KEY (name);


--
-- Name: work_alias_work_tree_id_name_locale_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_alias
    ADD CONSTRAINT work_alias_work_tree_id_name_locale_key UNIQUE (work_tree_id, name, locale);


--
-- Name: work_data_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_data
    ADD CONSTRAINT work_data_pkey PRIMARY KEY (work_data_id);


--
-- Name: work_meta_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_meta
    ADD CONSTRAINT work_meta_pkey PRIMARY KEY (work_id);


--
-- Name: work_name_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_name
    ADD CONSTRAINT work_name_id_key UNIQUE (id);


--
-- Name: work_name_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_name
    ADD CONSTRAINT work_name_pkey PRIMARY KEY (name);


--
-- Name: work_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work
    ADD CONSTRAINT work_pkey PRIMARY KEY (work_id);


--
-- Name: work_rating_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_rating_raw
    ADD CONSTRAINT work_rating_raw_pkey PRIMARY KEY (work_id, editor_id);


--
-- Name: work_revision_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_revision
    ADD CONSTRAINT work_revision_pkey PRIMARY KEY (revision_id);


--
-- Name: work_tag_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_tag
    ADD CONSTRAINT work_tag_pkey PRIMARY KEY (work_id, tag_id);


--
-- Name: work_tag_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_tag_raw
    ADD CONSTRAINT work_tag_raw_pkey PRIMARY KEY (work_id, tag_id, editor_id);


--
-- Name: work_tree_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_tree
    ADD CONSTRAINT work_tree_pkey PRIMARY KEY (work_tree_id);


--
-- Name: work_type_id_key; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_type
    ADD CONSTRAINT work_type_id_key UNIQUE (id);


--
-- Name: work_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_type
    ADD CONSTRAINT work_type_pkey PRIMARY KEY (name);


--
-- Name: artist_alias_artist_tree_id_name_idx; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX artist_alias_artist_tree_id_name_idx ON artist_alias USING btree (artist_tree_id, name) WHERE (locale IS NULL);


--
-- Name: label_alias_label_tree_id_name_idx; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX label_alias_label_tree_id_name_idx ON label_alias USING btree (label_tree_id, name) WHERE (locale IS NULL);


--
-- Name: release_label_release_tree_id_catalog_number_idx; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX release_label_release_tree_id_catalog_number_idx ON release_label USING btree (release_tree_id, catalog_number) WHERE (label_id IS NULL);


--
-- Name: release_label_release_tree_id_label_id_idx; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX release_label_release_tree_id_label_id_idx ON release_label USING btree (release_tree_id, label_id) WHERE (catalog_number IS NULL);


--
-- Name: work_alias_work_tree_id_name_idx; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX work_alias_work_tree_id_name_idx ON work_alias USING btree (work_tree_id, name) WHERE (locale IS NULL);


--
-- Name: artist_alias_artist_alias_type_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_alias
    ADD CONSTRAINT artist_alias_artist_alias_type_id_fkey FOREIGN KEY (artist_alias_type_id) REFERENCES artist_alias_type(id);


--
-- Name: artist_alias_artist_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_alias
    ADD CONSTRAINT artist_alias_artist_tree_id_fkey FOREIGN KEY (artist_tree_id) REFERENCES artist_tree(artist_tree_id);


--
-- Name: artist_alias_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_alias
    ADD CONSTRAINT artist_alias_name_fkey FOREIGN KEY (name) REFERENCES artist_name(id);


--
-- Name: artist_alias_sort_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_alias
    ADD CONSTRAINT artist_alias_sort_name_fkey FOREIGN KEY (sort_name) REFERENCES artist_name(id);


--
-- Name: artist_credit_name_artist_credit_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_credit_name
    ADD CONSTRAINT artist_credit_name_artist_credit_id_fkey FOREIGN KEY (artist_credit_id) REFERENCES artist_credit(artist_credit_id);


--
-- Name: artist_credit_name_artist_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_credit_name
    ADD CONSTRAINT artist_credit_name_artist_id_fkey FOREIGN KEY (artist_id) REFERENCES artist(artist_id);


--
-- Name: artist_credit_name_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_credit_name
    ADD CONSTRAINT artist_credit_name_name_fkey FOREIGN KEY (name) REFERENCES artist_name(id);


--
-- Name: artist_data_artist_type_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_data
    ADD CONSTRAINT artist_data_artist_type_id_fkey FOREIGN KEY (artist_type_id) REFERENCES artist_type(id);


--
-- Name: artist_data_country_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_data
    ADD CONSTRAINT artist_data_country_id_fkey FOREIGN KEY (country_id) REFERENCES country(id);


--
-- Name: artist_data_gender_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_data
    ADD CONSTRAINT artist_data_gender_id_fkey FOREIGN KEY (gender_id) REFERENCES gender(id);


--
-- Name: artist_data_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_data
    ADD CONSTRAINT artist_data_name_fkey FOREIGN KEY (name) REFERENCES artist_name(id);


--
-- Name: artist_data_sort_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_data
    ADD CONSTRAINT artist_data_sort_name_fkey FOREIGN KEY (sort_name) REFERENCES artist_name(id);


--
-- Name: artist_ipi_artist_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_ipi
    ADD CONSTRAINT artist_ipi_artist_tree_id_fkey FOREIGN KEY (artist_tree_id) REFERENCES artist_tree(artist_tree_id);


--
-- Name: artist_master_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist
    ADD CONSTRAINT artist_master_revision_id_fkey FOREIGN KEY (master_revision_id) REFERENCES artist_revision(revision_id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: artist_merged_into_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist
    ADD CONSTRAINT artist_merged_into_fkey FOREIGN KEY (merged_into) REFERENCES artist(artist_id);


--
-- Name: artist_meta_artist_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_meta
    ADD CONSTRAINT artist_meta_artist_id_fkey FOREIGN KEY (artist_id) REFERENCES artist(artist_id);


--
-- Name: artist_revision_artist_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_revision
    ADD CONSTRAINT artist_revision_artist_id_fkey FOREIGN KEY (artist_id) REFERENCES artist(artist_id);


--
-- Name: artist_revision_artist_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_revision
    ADD CONSTRAINT artist_revision_artist_tree_id_fkey FOREIGN KEY (artist_tree_id) REFERENCES artist_tree(artist_tree_id);


--
-- Name: artist_revision_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_revision
    ADD CONSTRAINT artist_revision_revision_id_fkey FOREIGN KEY (revision_id) REFERENCES revision(revision_id);


--
-- Name: artist_tag_artist_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_tag
    ADD CONSTRAINT artist_tag_artist_id_fkey FOREIGN KEY (artist_id) REFERENCES artist(artist_id);


--
-- Name: artist_tag_tag_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_tag
    ADD CONSTRAINT artist_tag_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES tag(id);


--
-- Name: artist_tree_artist_data_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_tree
    ADD CONSTRAINT artist_tree_artist_data_id_fkey FOREIGN KEY (artist_data_id) REFERENCES artist_data(artist_data_id);


--
-- Name: edit_artist_edit_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_artist
    ADD CONSTRAINT edit_artist_edit_id_fkey FOREIGN KEY (edit_id) REFERENCES edit(edit_id);


--
-- Name: edit_artist_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_artist
    ADD CONSTRAINT edit_artist_revision_id_fkey FOREIGN KEY (revision_id) REFERENCES artist_revision(revision_id);


--
-- Name: edit_note_edit_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_note
    ADD CONSTRAINT edit_note_edit_id_fkey FOREIGN KEY (edit_id) REFERENCES edit(edit_id);


--
-- Name: edit_note_editor_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_note
    ADD CONSTRAINT edit_note_editor_id_fkey FOREIGN KEY (editor_id) REFERENCES editor(editor_id);


--
-- Name: isrc_recording_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY isrc
    ADD CONSTRAINT isrc_recording_tree_id_fkey FOREIGN KEY (recording_tree_id) REFERENCES recording_tree(recording_tree_id);


--
-- Name: iswc_work_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY iswc
    ADD CONSTRAINT iswc_work_tree_id_fkey FOREIGN KEY (work_tree_id) REFERENCES work_tree(work_tree_id);


--
-- Name: label_alias_label_alias_type_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_alias
    ADD CONSTRAINT label_alias_label_alias_type_id_fkey FOREIGN KEY (label_alias_type_id) REFERENCES label_alias_type(id);


--
-- Name: label_alias_label_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_alias
    ADD CONSTRAINT label_alias_label_tree_id_fkey FOREIGN KEY (label_tree_id) REFERENCES label_tree(label_tree_id);


--
-- Name: label_alias_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_alias
    ADD CONSTRAINT label_alias_name_fkey FOREIGN KEY (name) REFERENCES label_name(id);


--
-- Name: label_alias_sort_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_alias
    ADD CONSTRAINT label_alias_sort_name_fkey FOREIGN KEY (sort_name) REFERENCES label_name(id);


--
-- Name: label_data_label_type_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_data
    ADD CONSTRAINT label_data_label_type_id_fkey FOREIGN KEY (label_type_id) REFERENCES label_type(id);


--
-- Name: label_data_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_data
    ADD CONSTRAINT label_data_name_fkey FOREIGN KEY (name) REFERENCES label_name(id);


--
-- Name: label_data_sort_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_data
    ADD CONSTRAINT label_data_sort_name_fkey FOREIGN KEY (sort_name) REFERENCES label_name(id);


--
-- Name: label_ipi_label_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_ipi
    ADD CONSTRAINT label_ipi_label_tree_id_fkey FOREIGN KEY (label_tree_id) REFERENCES label_tree(label_tree_id);


--
-- Name: label_master_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label
    ADD CONSTRAINT label_master_revision_id_fkey FOREIGN KEY (master_revision_id) REFERENCES label_revision(revision_id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: label_merged_into_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label
    ADD CONSTRAINT label_merged_into_fkey FOREIGN KEY (merged_into) REFERENCES label(label_id);


--
-- Name: label_meta_label_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_meta
    ADD CONSTRAINT label_meta_label_id_fkey FOREIGN KEY (label_id) REFERENCES label(label_id);


--
-- Name: label_revision_label_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_revision
    ADD CONSTRAINT label_revision_label_id_fkey FOREIGN KEY (label_id) REFERENCES label(label_id);


--
-- Name: label_revision_label_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_revision
    ADD CONSTRAINT label_revision_label_tree_id_fkey FOREIGN KEY (label_tree_id) REFERENCES label_tree(label_tree_id);


--
-- Name: label_revision_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_revision
    ADD CONSTRAINT label_revision_revision_id_fkey FOREIGN KEY (revision_id) REFERENCES revision(revision_id);


--
-- Name: label_tag_label_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_tag
    ADD CONSTRAINT label_tag_label_id_fkey FOREIGN KEY (label_id) REFERENCES label(label_id);


--
-- Name: label_tag_tag_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_tag
    ADD CONSTRAINT label_tag_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES tag(id);


--
-- Name: label_tree_label_data_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_tree
    ADD CONSTRAINT label_tree_label_data_id_fkey FOREIGN KEY (label_data_id) REFERENCES label_data(label_data_id);


--
-- Name: link_attribute_attribute_type_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_attribute
    ADD CONSTRAINT link_attribute_attribute_type_fkey FOREIGN KEY (attribute_type) REFERENCES link_attribute_type(id);


--
-- Name: link_attribute_link_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_attribute
    ADD CONSTRAINT link_attribute_link_fkey FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: link_attribute_type_parent_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_attribute_type
    ADD CONSTRAINT link_attribute_type_parent_fkey FOREIGN KEY (parent) REFERENCES link_attribute_type(id);


--
-- Name: link_attribute_type_root_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_attribute_type
    ADD CONSTRAINT link_attribute_type_root_fkey FOREIGN KEY (root) REFERENCES link_attribute_type(id);


--
-- Name: link_link_type_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link
    ADD CONSTRAINT link_link_type_fkey FOREIGN KEY (link_type) REFERENCES link_type(id);


--
-- Name: link_type_attribute_type_attribute_type_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_type_attribute_type
    ADD CONSTRAINT link_type_attribute_type_attribute_type_fkey FOREIGN KEY (attribute_type) REFERENCES link_attribute_type(id);


--
-- Name: link_type_attribute_type_link_type_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_type_attribute_type
    ADD CONSTRAINT link_type_attribute_type_link_type_fkey FOREIGN KEY (link_type) REFERENCES link_type(id);


--
-- Name: link_type_parent_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_type
    ADD CONSTRAINT link_type_parent_fkey FOREIGN KEY (parent) REFERENCES link_type(id);


--
-- Name: medium_cdtoc_cdtoc_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium_cdtoc
    ADD CONSTRAINT medium_cdtoc_cdtoc_fkey FOREIGN KEY (cdtoc) REFERENCES cdtoc(id);


--
-- Name: medium_cdtoc_release_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium_cdtoc
    ADD CONSTRAINT medium_cdtoc_release_tree_id_fkey FOREIGN KEY (release_tree_id, "position") REFERENCES medium(release_tree_id, "position");


--
-- Name: medium_format_parent_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium_format
    ADD CONSTRAINT medium_format_parent_fkey FOREIGN KEY (parent) REFERENCES medium_format(id);


--
-- Name: medium_medium_format_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium
    ADD CONSTRAINT medium_medium_format_id_fkey FOREIGN KEY (medium_format_id) REFERENCES medium_format(id);


--
-- Name: medium_release_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium
    ADD CONSTRAINT medium_release_tree_id_fkey FOREIGN KEY (release_tree_id) REFERENCES release_tree(release_tree_id);


--
-- Name: medium_tracklist_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium
    ADD CONSTRAINT medium_tracklist_id_fkey FOREIGN KEY (tracklist_id) REFERENCES tracklist(id);


--
-- Name: recording_data_artist_credit_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_data
    ADD CONSTRAINT recording_data_artist_credit_id_fkey FOREIGN KEY (artist_credit_id) REFERENCES artist_credit(artist_credit_id);


--
-- Name: recording_data_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_data
    ADD CONSTRAINT recording_data_name_fkey FOREIGN KEY (name) REFERENCES track_name(id);


--
-- Name: recording_master_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording
    ADD CONSTRAINT recording_master_revision_id_fkey FOREIGN KEY (master_revision_id) REFERENCES recording_revision(revision_id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: recording_merged_into_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording
    ADD CONSTRAINT recording_merged_into_fkey FOREIGN KEY (merged_into) REFERENCES recording(recording_id);


--
-- Name: recording_meta_recording_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_meta
    ADD CONSTRAINT recording_meta_recording_id_fkey FOREIGN KEY (recording_id) REFERENCES recording(recording_id);


--
-- Name: recording_puid_puid_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_puid
    ADD CONSTRAINT recording_puid_puid_id_fkey FOREIGN KEY (puid_id) REFERENCES puid(id);


--
-- Name: recording_puid_recording_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_puid
    ADD CONSTRAINT recording_puid_recording_tree_id_fkey FOREIGN KEY (recording_tree_id) REFERENCES recording_tree(recording_tree_id);


--
-- Name: recording_revision_recording_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_revision
    ADD CONSTRAINT recording_revision_recording_id_fkey FOREIGN KEY (recording_id) REFERENCES recording(recording_id);


--
-- Name: recording_revision_recording_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_revision
    ADD CONSTRAINT recording_revision_recording_tree_id_fkey FOREIGN KEY (recording_tree_id) REFERENCES recording_tree(recording_tree_id);


--
-- Name: recording_revision_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_revision
    ADD CONSTRAINT recording_revision_revision_id_fkey FOREIGN KEY (revision_id) REFERENCES revision(revision_id);


--
-- Name: recording_tag_recording_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_tag
    ADD CONSTRAINT recording_tag_recording_id_fkey FOREIGN KEY (recording_id) REFERENCES recording(recording_id);


--
-- Name: recording_tag_tag_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_tag
    ADD CONSTRAINT recording_tag_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES tag(id);


--
-- Name: recording_tree_recording_data_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_tree
    ADD CONSTRAINT recording_tree_recording_data_id_fkey FOREIGN KEY (recording_data_id) REFERENCES recording_data(recording_data_id);


--
-- Name: release_coverart_release_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_coverart
    ADD CONSTRAINT release_coverart_release_id_fkey FOREIGN KEY (release_id) REFERENCES release(release_id);


--
-- Name: release_data_country_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_data
    ADD CONSTRAINT release_data_country_id_fkey FOREIGN KEY (country_id) REFERENCES country(id);


--
-- Name: release_data_language_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_data
    ADD CONSTRAINT release_data_language_id_fkey FOREIGN KEY (language_id) REFERENCES language(id);


--
-- Name: release_data_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_data
    ADD CONSTRAINT release_data_name_fkey FOREIGN KEY (name) REFERENCES release_name(id);


--
-- Name: release_data_release_packaging_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_data
    ADD CONSTRAINT release_data_release_packaging_id_fkey FOREIGN KEY (release_packaging_id) REFERENCES release_packaging(id);


--
-- Name: release_data_release_status_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_data
    ADD CONSTRAINT release_data_release_status_id_fkey FOREIGN KEY (release_status_id) REFERENCES release_status(id);


--
-- Name: release_data_script_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_data
    ADD CONSTRAINT release_data_script_id_fkey FOREIGN KEY (script_id) REFERENCES script(id);


--
-- Name: release_group_data_artist_credit_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_data
    ADD CONSTRAINT release_group_data_artist_credit_id_fkey FOREIGN KEY (artist_credit_id) REFERENCES artist_credit(artist_credit_id);


--
-- Name: release_group_data_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_data
    ADD CONSTRAINT release_group_data_name_fkey FOREIGN KEY (name) REFERENCES release_name(id);


--
-- Name: release_group_data_release_group_primary_type_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_data
    ADD CONSTRAINT release_group_data_release_group_primary_type_id_fkey FOREIGN KEY (release_group_primary_type_id) REFERENCES release_group_primary_type(id);


--
-- Name: release_group_master_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group
    ADD CONSTRAINT release_group_master_revision_id_fkey FOREIGN KEY (master_revision_id) REFERENCES release_group_revision(revision_id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: release_group_merged_into_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group
    ADD CONSTRAINT release_group_merged_into_fkey FOREIGN KEY (merged_into) REFERENCES release_group(release_group_id);


--
-- Name: release_group_meta_release_group_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_meta
    ADD CONSTRAINT release_group_meta_release_group_id_fkey FOREIGN KEY (release_group_id) REFERENCES release_group(release_group_id);


--
-- Name: release_group_revision_release_group_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_revision
    ADD CONSTRAINT release_group_revision_release_group_id_fkey FOREIGN KEY (release_group_id) REFERENCES release_group(release_group_id);


--
-- Name: release_group_revision_release_group_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_revision
    ADD CONSTRAINT release_group_revision_release_group_tree_id_fkey FOREIGN KEY (release_group_tree_id) REFERENCES release_group_tree(release_group_tree_id);


--
-- Name: release_group_revision_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_revision
    ADD CONSTRAINT release_group_revision_revision_id_fkey FOREIGN KEY (revision_id) REFERENCES revision(revision_id);


--
-- Name: release_group_tag_release_group_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_tag
    ADD CONSTRAINT release_group_tag_release_group_id_fkey FOREIGN KEY (release_group_id) REFERENCES release_group(release_group_id);


--
-- Name: release_group_tag_tag_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_tag
    ADD CONSTRAINT release_group_tag_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES tag(id);


--
-- Name: release_group_tree_release_group_data_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_tree
    ADD CONSTRAINT release_group_tree_release_group_data_id_fkey FOREIGN KEY (release_group_data_id) REFERENCES release_group_data(release_group_data_id);


--
-- Name: release_group_tree_secondary__release_group_secondary_type_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_tree_secondary_type
    ADD CONSTRAINT release_group_tree_secondary__release_group_secondary_type_fkey FOREIGN KEY (release_group_secondary_type_id) REFERENCES release_group_secondary_type(id);


--
-- Name: release_label_label_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_label
    ADD CONSTRAINT release_label_label_id_fkey FOREIGN KEY (label_id) REFERENCES label(label_id);


--
-- Name: release_label_release_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_label
    ADD CONSTRAINT release_label_release_tree_id_fkey FOREIGN KEY (release_tree_id) REFERENCES release_tree(release_tree_id);


--
-- Name: release_master_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release
    ADD CONSTRAINT release_master_revision_id_fkey FOREIGN KEY (master_revision_id) REFERENCES release_revision(revision_id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: release_merged_into_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release
    ADD CONSTRAINT release_merged_into_fkey FOREIGN KEY (merged_into) REFERENCES release(release_id);


--
-- Name: release_meta_release_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_meta
    ADD CONSTRAINT release_meta_release_id_fkey FOREIGN KEY (release_id) REFERENCES release(release_id);


--
-- Name: release_revision_release_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_revision
    ADD CONSTRAINT release_revision_release_id_fkey FOREIGN KEY (release_id) REFERENCES release(release_id);


--
-- Name: release_revision_release_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_revision
    ADD CONSTRAINT release_revision_release_tree_id_fkey FOREIGN KEY (release_tree_id) REFERENCES release_tree(release_tree_id);


--
-- Name: release_revision_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_revision
    ADD CONSTRAINT release_revision_revision_id_fkey FOREIGN KEY (revision_id) REFERENCES revision(revision_id);


--
-- Name: release_tag_release_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_tag
    ADD CONSTRAINT release_tag_release_id_fkey FOREIGN KEY (release_id) REFERENCES release(release_id);


--
-- Name: release_tag_tag_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_tag
    ADD CONSTRAINT release_tag_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES tag(id);


--
-- Name: revision_editor_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY revision
    ADD CONSTRAINT revision_editor_id_fkey FOREIGN KEY (editor_id) REFERENCES editor(editor_id);


--
-- Name: revision_parent_parent_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY revision_parent
    ADD CONSTRAINT revision_parent_parent_revision_id_fkey FOREIGN KEY (parent_revision_id) REFERENCES revision(revision_id);


--
-- Name: revision_parent_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY revision_parent
    ADD CONSTRAINT revision_parent_revision_id_fkey FOREIGN KEY (revision_id) REFERENCES revision(revision_id);


--
-- Name: script_language_language_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY script_language
    ADD CONSTRAINT script_language_language_fkey FOREIGN KEY (language) REFERENCES language(id);


--
-- Name: script_language_script_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY script_language
    ADD CONSTRAINT script_language_script_fkey FOREIGN KEY (script) REFERENCES script(id);


--
-- Name: tag_relation_tag1_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY tag_relation
    ADD CONSTRAINT tag_relation_tag1_fkey FOREIGN KEY (tag1) REFERENCES tag(id);


--
-- Name: tag_relation_tag2_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY tag_relation
    ADD CONSTRAINT tag_relation_tag2_fkey FOREIGN KEY (tag2) REFERENCES tag(id);


--
-- Name: track_artist_credit_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY track
    ADD CONSTRAINT track_artist_credit_id_fkey FOREIGN KEY (artist_credit_id) REFERENCES artist_credit(artist_credit_id);


--
-- Name: track_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY track
    ADD CONSTRAINT track_name_fkey FOREIGN KEY (name) REFERENCES track_name(id);


--
-- Name: track_recording_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY track
    ADD CONSTRAINT track_recording_id_fkey FOREIGN KEY (recording_id) REFERENCES recording(recording_id);


--
-- Name: track_tracklist_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY track
    ADD CONSTRAINT track_tracklist_id_fkey FOREIGN KEY (tracklist_id) REFERENCES tracklist(id);


--
-- Name: tracklist_index_tracklist_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY tracklist_index
    ADD CONSTRAINT tracklist_index_tracklist_fkey FOREIGN KEY (tracklist) REFERENCES tracklist(id);


--
-- Name: url_master_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY url
    ADD CONSTRAINT url_master_revision_id_fkey FOREIGN KEY (master_revision_id) REFERENCES url_revision(revision_id);


--
-- Name: url_merged_into_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY url
    ADD CONSTRAINT url_merged_into_fkey FOREIGN KEY (merged_into) REFERENCES url(url_id);


--
-- Name: url_revision_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY url_revision
    ADD CONSTRAINT url_revision_revision_id_fkey FOREIGN KEY (revision_id) REFERENCES revision(revision_id);


--
-- Name: url_revision_url_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY url_revision
    ADD CONSTRAINT url_revision_url_id_fkey FOREIGN KEY (url_id) REFERENCES url(url_id);


--
-- Name: url_revision_url_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY url_revision
    ADD CONSTRAINT url_revision_url_tree_id_fkey FOREIGN KEY (url_tree_id) REFERENCES url_tree(url_tree_id);


--
-- Name: url_tree_url_data_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY url_tree
    ADD CONSTRAINT url_tree_url_data_id_fkey FOREIGN KEY (url_data_id) REFERENCES url_data(url_data_id);


--
-- Name: vote_edit_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY vote
    ADD CONSTRAINT vote_edit_id_fkey FOREIGN KEY (edit_id) REFERENCES edit(edit_id);


--
-- Name: vote_editor_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY vote
    ADD CONSTRAINT vote_editor_id_fkey FOREIGN KEY (editor_id) REFERENCES editor(editor_id);


--
-- Name: work_alias_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_alias
    ADD CONSTRAINT work_alias_name_fkey FOREIGN KEY (name) REFERENCES work_name(id);


--
-- Name: work_alias_sort_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_alias
    ADD CONSTRAINT work_alias_sort_name_fkey FOREIGN KEY (sort_name) REFERENCES work_name(id);


--
-- Name: work_alias_work_alias_type_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_alias
    ADD CONSTRAINT work_alias_work_alias_type_id_fkey FOREIGN KEY (work_alias_type_id) REFERENCES work_alias_type(id);


--
-- Name: work_alias_work_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_alias
    ADD CONSTRAINT work_alias_work_tree_id_fkey FOREIGN KEY (work_tree_id) REFERENCES work_tree(work_tree_id);


--
-- Name: work_data_language_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_data
    ADD CONSTRAINT work_data_language_id_fkey FOREIGN KEY (language_id) REFERENCES language(id);


--
-- Name: work_data_name_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_data
    ADD CONSTRAINT work_data_name_fkey FOREIGN KEY (name) REFERENCES work_name(id);


--
-- Name: work_data_work_type_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_data
    ADD CONSTRAINT work_data_work_type_id_fkey FOREIGN KEY (work_type_id) REFERENCES work_type(id);


--
-- Name: work_master_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work
    ADD CONSTRAINT work_master_revision_id_fkey FOREIGN KEY (master_revision_id) REFERENCES work_revision(revision_id);


--
-- Name: work_merged_into_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work
    ADD CONSTRAINT work_merged_into_fkey FOREIGN KEY (merged_into) REFERENCES work(work_id);


--
-- Name: work_meta_work_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_meta
    ADD CONSTRAINT work_meta_work_id_fkey FOREIGN KEY (work_id) REFERENCES work(work_id);


--
-- Name: work_revision_revision_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_revision
    ADD CONSTRAINT work_revision_revision_id_fkey FOREIGN KEY (revision_id) REFERENCES revision(revision_id);


--
-- Name: work_revision_work_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_revision
    ADD CONSTRAINT work_revision_work_id_fkey FOREIGN KEY (work_id) REFERENCES work(work_id);


--
-- Name: work_revision_work_tree_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_revision
    ADD CONSTRAINT work_revision_work_tree_id_fkey FOREIGN KEY (work_tree_id) REFERENCES work_tree(work_tree_id);


--
-- Name: work_tag_tag_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_tag
    ADD CONSTRAINT work_tag_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES tag(id);


--
-- Name: work_tag_work_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_tag
    ADD CONSTRAINT work_tag_work_id_fkey FOREIGN KEY (work_id) REFERENCES work(work_id);


--
-- Name: work_tree_work_data_id_fkey; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_tree
    ADD CONSTRAINT work_tree_work_data_id_fkey FOREIGN KEY (work_data_id) REFERENCES work_data(work_data_id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

