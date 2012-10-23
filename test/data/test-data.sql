--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'SQL_ASCII';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

SET search_path = musicbrainz, pg_catalog;

--
-- Name: artist_alias_type_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('artist_alias_type_id_seq', 1, false);


--
-- Name: artist_credit_artist_credit_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('artist_credit_artist_credit_id_seq', 1, true);


--
-- Name: artist_data_artist_data_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('artist_data_artist_data_id_seq', 14, true);


--
-- Name: artist_name_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('artist_name_id_seq', 27, true);


--
-- Name: artist_tree_artist_tree_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('artist_tree_artist_tree_id_seq', 5, true);


--
-- Name: artist_type_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('artist_type_id_seq', 2, true);


--
-- Name: cdtoc_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('cdtoc_id_seq', 1, false);


--
-- Name: clientversion_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('clientversion_id_seq', 1, false);


--
-- Name: country_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('country_id_seq', 1, true);


--
-- Name: editor_editor_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('editor_editor_id_seq', 1, false);


--
-- Name: gender_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('gender_id_seq', 1, true);


--
-- Name: isrc_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('isrc_id_seq', 1, false);


--
-- Name: iswc_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('iswc_id_seq', 1, false);


--
-- Name: label_alias_type_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('label_alias_type_id_seq', 1, false);


--
-- Name: label_data_label_data_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('label_data_label_data_id_seq', 1, true);


--
-- Name: label_name_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('label_name_id_seq', 2, true);


--
-- Name: label_tree_label_tree_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('label_tree_label_tree_id_seq', 1, true);


--
-- Name: label_type_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('label_type_id_seq', 1, false);


--
-- Name: language_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('language_id_seq', 1, true);


--
-- Name: link_attribute_type_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('link_attribute_type_id_seq', 1, false);


--
-- Name: link_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('link_id_seq', 1, false);


--
-- Name: link_type_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('link_type_id_seq', 1, false);


--
-- Name: medium_cdtoc_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('medium_cdtoc_id_seq', 1, false);


--
-- Name: medium_format_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('medium_format_id_seq', 1, false);


--
-- Name: puid_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('puid_id_seq', 1, false);


--
-- Name: recording_data_recording_data_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('recording_data_recording_data_id_seq', 1, true);


--
-- Name: recording_tree_recording_tree_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('recording_tree_recording_tree_id_seq', 1, true);


--
-- Name: release_data_release_data_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('release_data_release_data_id_seq', 7, true);


--
-- Name: release_group_data_release_group_data_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('release_group_data_release_group_data_id_seq', 1, true);


--
-- Name: release_group_primary_type_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('release_group_primary_type_id_seq', 1, true);


--
-- Name: release_group_secondary_type_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('release_group_secondary_type_id_seq', 1, false);


--
-- Name: release_group_tree_release_group_tree_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('release_group_tree_release_group_tree_id_seq', 1, true);


--
-- Name: release_name_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('release_name_id_seq', 1, true);


--
-- Name: release_packaging_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('release_packaging_id_seq', 1, true);


--
-- Name: release_status_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('release_status_id_seq', 1, true);


--
-- Name: release_tree_release_tree_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('release_tree_release_tree_id_seq', 4, true);


--
-- Name: revision_revision_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('revision_revision_id_seq', 10787, true);


--
-- Name: script_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('script_id_seq', 3, true);


--
-- Name: script_language_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('script_language_id_seq', 1, false);


--
-- Name: tag_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('tag_id_seq', 1, false);


--
-- Name: track_name_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('track_name_id_seq', 1, true);


--
-- Name: tracklist_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('tracklist_id_seq', 1, false);


--
-- Name: url_data_url_data_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('url_data_url_data_id_seq', 1, false);


--
-- Name: url_tree_url_tree_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('url_tree_url_tree_id_seq', 1, false);


--
-- Name: work_alias_type_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('work_alias_type_id_seq', 1, false);


--
-- Name: work_data_work_data_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('work_data_work_data_id_seq', 1, false);


--
-- Name: work_name_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('work_name_id_seq', 1, false);


--
-- Name: work_tree_work_tree_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('work_tree_work_tree_id_seq', 1, false);


--
-- Name: work_type_id_seq; Type: SEQUENCE SET; Schema: musicbrainz; Owner: musicbrainz
--

SELECT pg_catalog.setval('work_type_id_seq', 1, false);


--
-- Data for Name: artist_name; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY artist_name (id, name) FROM stdin;
1	Freddie Mercury
2	Mercury, Freddie
23	Darrel Fitton
24	Fitton, Darrel
25	
26	Bola
27	aloB
15	Bob
16	Hi
\.


--
-- Data for Name: artist_type; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY artist_type (id, name) FROM stdin;
1	Group
2	Person
\.


--
-- Data for Name: country; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY country (id, iso_code, name) FROM stdin;
1	GB	United Kingdom
\.


--
-- Data for Name: gender; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY gender (id, name) FROM stdin;
1	Male
\.


--
-- Data for Name: artist_data; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY artist_data (artist_data_id, name, sort_name, begin_date_year, begin_date_month, begin_date_day, end_date_year, end_date_month, end_date_day, artist_type_id, country_id, gender_id, comment, ended) FROM stdin;
1	1	2	1946	9	5	1991	11	24	1	1	1	Of queen	t
\.


--
-- Data for Name: artist_tree; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY artist_tree (artist_tree_id, artist_data_id, annotation) FROM stdin;
1	1	\N
\.


--
-- Data for Name: editor; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY editor (editor_id, name, password, privs, email, website, bio, member_since, email_confirm_date, last_login_date, last_updated, birth_date, gender_id, country_id) FROM stdin;
1	acid2	mb	0	\N	\N	\N	2012-10-12 18:22:42.254033+01	\N	\N	2012-10-12 18:22:42.254033+01	\N	\N	\N
\.


--
-- Data for Name: revision; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY revision (revision_id, editor_id, created_at) FROM stdin;
1	1	2012-10-12 18:27:42.089011+01
30	1	2012-10-17 18:39:03.840042+01
10751	1	2012-10-23 21:22:35.765612+01
10761	1	2012-10-23 21:56:33.903942+01
10762	1	2012-10-23 22:09:11.043502+01
\.


--
-- Data for Name: artist_revision; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY artist_revision (revision_id, artist_id, artist_tree_id) FROM stdin;
1	206094f7-eea0-4f37-a4c2-97c506f5f560	1
\.


--
-- Data for Name: artist; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY artist (artist_id, master_revision_id, merged_into) FROM stdin;
206094f7-eea0-4f37-a4c2-97c506f5f560	1	\N
\.


--
-- Data for Name: artist_alias_type; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY artist_alias_type (id, name) FROM stdin;
\.


--
-- Data for Name: artist_alias; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY artist_alias (artist_tree_id, name, sort_name, locale, artist_alias_type_id, begin_date_year, begin_date_month, begin_date_day, end_date_year, end_date_month, end_date_day, primary_for_locale) FROM stdin;
\.


--
-- Data for Name: artist_credit; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY artist_credit (artist_credit_id) FROM stdin;
1
\.


--
-- Data for Name: artist_credit_name; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY artist_credit_name (artist_credit_id, "position", artist_id, name, join_phrase) FROM stdin;
\.


--
-- Data for Name: artist_ipi; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY artist_ipi (artist_tree_id, ipi) FROM stdin;
\.


--
-- Data for Name: artist_meta; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY artist_meta (artist_id, rating, rating_count) FROM stdin;
\.


--
-- Data for Name: artist_rating_raw; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY artist_rating_raw (artist_id, rating, editor_id) FROM stdin;
\.


--
-- Data for Name: tag; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY tag (id, name, ref_count) FROM stdin;
\.


--
-- Data for Name: artist_tag; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY artist_tag (artist_id, tag_id, count) FROM stdin;
\.


--
-- Data for Name: artist_tag_raw; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY artist_tag_raw (artist_id, tag_id, editor_id) FROM stdin;
\.


--
-- Data for Name: cdtoc; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY cdtoc (id, discid, freedb_id, track_count, leadout_offset, track_offset, degraded, created) FROM stdin;
\.


--
-- Data for Name: clientversion; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY clientversion (id, version, created) FROM stdin;
\.


--
-- Data for Name: track_name; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY track_name (id, name) FROM stdin;
1	I Love Acid
\.


--
-- Data for Name: recording_data; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY recording_data (recording_data_id, name, artist_credit_id, length, comment) FROM stdin;
1	1	1	64936	
\.


--
-- Data for Name: recording_tree; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY recording_tree (recording_tree_id, recording_data_id, annotation) FROM stdin;
1	1	\N
\.


--
-- Data for Name: isrc; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY isrc (id, recording_tree_id, isrc, source) FROM stdin;
\.


--
-- Data for Name: language; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY language (id, iso_code_2t, iso_code_2b, iso_code_1, iso_code_3, name, frequency) FROM stdin;
1	\N	\N	\N	\N	English	0
\.


--
-- Data for Name: work_name; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY work_name (id, name) FROM stdin;
\.


--
-- Data for Name: work_type; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY work_type (id, name) FROM stdin;
\.


--
-- Data for Name: work_data; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY work_data (work_data_id, name, work_type_id, comment, language_id) FROM stdin;
\.


--
-- Data for Name: work_tree; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY work_tree (work_tree_id, work_data_id, annotation) FROM stdin;
\.


--
-- Data for Name: iswc; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY iswc (id, work_tree_id, iswc, source) FROM stdin;
\.


--
-- Data for Name: label; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY label (label_id, master_revision_id, merged_into) FROM stdin;
7de490ac-84e4-4eab-875d-670fee081968	30	\N
\.


--
-- Data for Name: label_alias_type; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY label_alias_type (id, name) FROM stdin;
\.


--
-- Data for Name: label_name; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY label_name (id, name) FROM stdin;
1	Revolution Records
2	Records, Revolution
\.


--
-- Data for Name: label_type; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY label_type (id, name) FROM stdin;
\.


--
-- Data for Name: label_data; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY label_data (label_data_id, name, sort_name, begin_date_year, begin_date_month, begin_date_day, end_date_year, end_date_month, end_date_day, label_type_id, label_code, country_id, comment, ended) FROM stdin;
1	1	2	\N	\N	\N	\N	\N	\N	\N	\N	\N		f
\.


--
-- Data for Name: label_tree; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY label_tree (label_tree_id, label_data_id, annotation) FROM stdin;
1	1	\N
\.


--
-- Data for Name: label_alias; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY label_alias (label_tree_id, name, sort_name, locale, label_alias_type_id, begin_date_year, begin_date_month, begin_date_day, end_date_year, end_date_month, end_date_day, primary_for_locale) FROM stdin;
\.


--
-- Data for Name: label_ipi; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY label_ipi (label_tree_id, ipi) FROM stdin;
\.


--
-- Data for Name: label_meta; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY label_meta (label_id, rating, rating_count) FROM stdin;
\.


--
-- Data for Name: label_rating_raw; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY label_rating_raw (label_id, rating, editor_id) FROM stdin;
\.


--
-- Data for Name: label_revision; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY label_revision (revision_id, label_id, label_tree_id) FROM stdin;
30	7de490ac-84e4-4eab-875d-670fee081968	1
\.


--
-- Data for Name: label_tag; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY label_tag (label_id, tag_id, count) FROM stdin;
\.


--
-- Data for Name: label_tag_raw; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY label_tag_raw (label_id, tag_id, editor_id) FROM stdin;
\.


--
-- Data for Name: link_type; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY link_type (id, parent, child_order, gid, entity_type0, entity_type1, name, description, link_phrase, reverse_link_phrase, short_link_phrase, priority, last_updated) FROM stdin;
\.


--
-- Data for Name: link; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY link (id, link_type, begin_date_year, begin_date_month, begin_date_day, end_date_year, end_date_month, end_date_day, attribute_count, created, ended) FROM stdin;
\.


--
-- Data for Name: link_attribute_type; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY link_attribute_type (id, parent, root, child_order, gid, name, description, last_updated) FROM stdin;
\.


--
-- Data for Name: link_attribute; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY link_attribute (link, attribute_type, created) FROM stdin;
\.


--
-- Data for Name: link_type_attribute_type; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY link_type_attribute_type (link_type, attribute_type, min, max, last_updated) FROM stdin;
\.


--
-- Data for Name: medium_format; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY medium_format (id, name, parent, child_order, year, has_discids) FROM stdin;
\.


--
-- Data for Name: release_name; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_name (id, name) FROM stdin;
1	Portishead
\.


--
-- Data for Name: release_packaging; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_packaging (id, name) FROM stdin;
1	Jewel Case
\.


--
-- Data for Name: release_status; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_status (id, name) FROM stdin;
1	Official
\.


--
-- Data for Name: script; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY script (id, iso_code, iso_number, name, frequency) FROM stdin;
3	foo 	10 	Latin	0
\.


--
-- Data for Name: release_data; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_data (release_data_id, name, artist_credit_id, release_status_id, release_packaging_id, country_id, language_id, script_id, date_year, date_month, date_day, barcode, comment) FROM stdin;
7	1	1	1	1	1	1	3	1997	9	29	731453918924	
\.


--
-- Data for Name: release_tree; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_tree (release_tree_id, release_data_id, release_group_id, annotation) FROM stdin;
4	7	d7ec6175-0891-448d-b9e4-14d007d53d29	\N
\.


--
-- Data for Name: tracklist; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY tracklist (id, track_count, last_updated) FROM stdin;
\.


--
-- Data for Name: medium; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY medium (tracklist_id, release_tree_id, "position", medium_format_id, name) FROM stdin;
\.


--
-- Data for Name: medium_cdtoc; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY medium_cdtoc (id, release_tree_id, "position", cdtoc) FROM stdin;
\.


--
-- Data for Name: puid; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY puid (id, puid, version) FROM stdin;
\.


--
-- Data for Name: recording; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY recording (recording_id, master_revision_id, merged_into) FROM stdin;
c3c2ed6e-7944-4ed5-b597-3d15dc1718dd	10751	\N
\.


--
-- Data for Name: recording_meta; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY recording_meta (recording_id, rating, rating_count) FROM stdin;
\.


--
-- Data for Name: recording_puid; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY recording_puid (puid_id, recording_tree_id) FROM stdin;
\.


--
-- Data for Name: recording_rating_raw; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY recording_rating_raw (recording_id, rating, editor_id) FROM stdin;
\.


--
-- Data for Name: recording_revision; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY recording_revision (revision_id, recording_id, recording_tree_id) FROM stdin;
10751	c3c2ed6e-7944-4ed5-b597-3d15dc1718dd	1
\.


--
-- Data for Name: recording_tag; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY recording_tag (recording_id, tag_id, count) FROM stdin;
\.


--
-- Data for Name: recording_tag_raw; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY recording_tag_raw (recording_id, tag_id, editor_id) FROM stdin;
\.


--
-- Data for Name: release; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release (release_id, master_revision_id, merged_into) FROM stdin;
df907840-9e5a-41cd-a44d-f039cdecdca4	10762	\N
\.


--
-- Data for Name: release_coverart; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_coverart (release_id, last_updated, cover_art_url) FROM stdin;
\.


--
-- Data for Name: release_group; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_group (release_group_id, master_revision_id, merged_into) FROM stdin;
d7ec6175-0891-448d-b9e4-14d007d53d29	10761	\N
\.


--
-- Data for Name: release_group_primary_type; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_group_primary_type (id, name) FROM stdin;
1	Album
\.


--
-- Data for Name: release_group_data; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_group_data (release_group_data_id, name, artist_credit_id, release_group_primary_type_id, comment) FROM stdin;
1	1	1	1	
\.


--
-- Data for Name: release_group_meta; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_group_meta (release_group_id, release_count, first_release_date_year, first_release_date_month, first_release_date_day, rating, rating_count) FROM stdin;
\.


--
-- Data for Name: release_group_rating_raw; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_group_rating_raw (release_group_id, rating, editor_id) FROM stdin;
\.


--
-- Data for Name: release_group_tree; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_group_tree (release_group_tree_id, release_group_data_id, annotation) FROM stdin;
1	1	\N
\.


--
-- Data for Name: release_group_revision; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_group_revision (revision_id, release_group_id, release_group_tree_id) FROM stdin;
10761	d7ec6175-0891-448d-b9e4-14d007d53d29	1
\.


--
-- Data for Name: release_group_secondary_type; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_group_secondary_type (id, name) FROM stdin;
\.


--
-- Data for Name: release_group_tag; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_group_tag (release_group_id, tag_id, count) FROM stdin;
\.


--
-- Data for Name: release_group_tag_raw; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_group_tag_raw (release_group_id, tag_id, editor_id) FROM stdin;
\.


--
-- Data for Name: release_group_tree_secondary_type; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_group_tree_secondary_type (release_group_tree_id, release_group_secondary_type_id) FROM stdin;
\.


--
-- Data for Name: release_label; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_label (release_tree_id, label_id, catalog_number) FROM stdin;
\.


--
-- Data for Name: release_meta; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_meta (release_id, date_added, info_url, amazon_asin, amazon_store, cover_art_presence) FROM stdin;
\.


--
-- Data for Name: release_revision; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_revision (revision_id, release_id, release_tree_id) FROM stdin;
10762	df907840-9e5a-41cd-a44d-f039cdecdca4	4
\.


--
-- Data for Name: release_tag; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_tag (release_id, tag_id, count) FROM stdin;
\.


--
-- Data for Name: release_tag_raw; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY release_tag_raw (release_id, tag_id, editor_id) FROM stdin;
\.


--
-- Data for Name: script_language; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY script_language (id, script, language, frequency) FROM stdin;
\.


--
-- Data for Name: tag_relation; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY tag_relation (tag1, tag2, weight, last_updated) FROM stdin;
\.


--
-- Data for Name: track; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY track (recording_id, tracklist_id, "position", name, artist_credit_id, length, number) FROM stdin;
\.


--
-- Data for Name: tracklist_index; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY tracklist_index (tracklist, toc) FROM stdin;
\.


--
-- Data for Name: url; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY url (url_id, master_revision_id, merged_into) FROM stdin;
\.


--
-- Data for Name: url_data; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY url_data (url_data_id, url, comment) FROM stdin;
\.


--
-- Data for Name: url_tree; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY url_tree (url_tree_id, url_data_id, annotation) FROM stdin;
\.


--
-- Data for Name: url_revision; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY url_revision (revision_id, url_id, url_tree_id) FROM stdin;
\.


--
-- Data for Name: work; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY work (work_id, master_revision_id, merged_into) FROM stdin;
\.


--
-- Data for Name: work_alias_type; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY work_alias_type (id, name) FROM stdin;
\.


--
-- Data for Name: work_alias; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY work_alias (work_tree_id, name, sort_name, locale, work_alias_type_id, begin_date_year, begin_date_month, begin_date_day, end_date_year, end_date_month, end_date_day, primary_for_locale) FROM stdin;
\.


--
-- Data for Name: work_meta; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY work_meta (work_id, rating, rating_count) FROM stdin;
\.


--
-- Data for Name: work_rating_raw; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY work_rating_raw (work_id, rating, editor_id) FROM stdin;
\.


--
-- Data for Name: work_revision; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY work_revision (revision_id, work_id, work_tree_id) FROM stdin;
\.


--
-- Data for Name: work_tag; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY work_tag (work_id, tag_id, count) FROM stdin;
\.


--
-- Data for Name: work_tag_raw; Type: TABLE DATA; Schema: musicbrainz; Owner: musicbrainz
--

COPY work_tag_raw (work_id, tag_id, editor_id) FROM stdin;
\.


--
-- PostgreSQL database dump complete
--

