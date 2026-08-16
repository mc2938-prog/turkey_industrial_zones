README: Spatial Analysis of Organized Industrial Zones in Turkey
================================================================

OVERVIEW
--------
This script constructs a spatial dataset of Turkish Organized Industrial
Zones (OSBs), merges spatial and economic data at the district level, and
compares zone vs. non-zone district characteristics. The analysis is
cross-sectional and descriptive; the proposed causal design requires panel
data not yet collected.


REQUIREMENTS
------------
Run in R. All packages are installed automatically at the top of the script:
tidyverse, sf, terra, tidygeocoder, geodata, osmdata, exactextractr, tmap,
fixest, readxl, blackmarbler, httr, rvest, knitr


IMPORTANT! - All sources but the TIF file "2025avgnl.tif" is included since it is 10 GBs. It needs to be downloaded and renamed "2025avgnl" for the code to run. It can be downloaded from https://eogdata.mines.edu/nighttime_light/annual/v22/2025/VNL_npp_2025_global_vcmslcfg_v2_c202604011200.average_masked.dat.tif.gz 

The caches are included because otherwise the code runs for too long.

INPUT FILES
-----------
Place all of the following in your working directory before running:

firms.xlsx
  OSBUK zone registry, manually copied from website
  Source: https://osbuk.org/view/osb/osbliste.php

socioeconomic_scores_district_2023.xls
  TUIK district SES scores
  Source: https://veriportali.tuik.gov.tr/en/press/57942

2025avgnl.tif
  VIIRS VNL v2 2025 annual masked composite
  Source: https://eogdata.mines.edu/products/vnl/

Motorway data is downloaded automatically from OpenStreetMap via osmdata
and cached as motorways_sf.rds.


OUTPUT FILES
------------
zones.csv
  One row per OSB zone with coordinates, establishment year, and road distance

analysis_units.csv
  One row per district (zone + neighboring non-zone) with outcome variables

maps/zones_district.png
  Map 1: OSB locations on district boundaries

maps/zones_motorways.png
  Map 2: OSB locations overlaid on motorway network

maps/nightlights_districts.png
  Map 3: Night light radiance by district with OSB overlay


SCRIPT STRUCTURE
----------------

Section 0 - Packages
  Auto-installs and loads all required packages.

Section 1 - Load Datasets
  Loads four raw data sources. Motorway data and geocoded zones are cached
  as RDS files (motorways_sf.rds, osb_geocoded.rds, osb_sf_scraped.rds)
  because downloading and geocoding are time-intensive. OSM geocoding takes
  ~5 minutes for 297 zones; web scraping for establishment years takes
  30-40 minutes. Once cached, subsequent runs load instantly.

Section 2 - Cleaning
  Turkish character normalization. All text joins are done on ASCII strings.
  The fix_turkish() function converts Turkish special characters to ASCII
  equivalents (s, i, g, u, o, c) and is applied to every character column
  before any merge. Without this, joins fail silently — a district named
  "Sanliurfa" will never match "Sanlurfa" without normalization.

Section 3 - Geocoding
  Zones are geocoded via OpenStreetMap Nominatim using tidygeocoder. Each
  zone is queried as "<zone_name> Organize Sanayi Bolgesi <province> Turkey".
  116 of 297 zones were matched (39.1%), covering 54 of 81 provinces.
  Unmatched zones are dropped. Results are cached as osb_geocoded.rds.

Section 4 - Spatial Data Construction
  Downloads GADM level-2 district boundaries and constructs three district-
  level variables. Distance to nearest motorway uses district centroids
  throughout for consistency in the district-level comparison (see Section 6).
  Exact zone coordinates are used only for zones.csv. Night light radiance
  is extracted from the 2025 VIIRS composite using exact_extract().

  IMPORTANT LIMITATION ON NIGHT LIGHTS: The 2025 raster provides a single
  cross-sectional snapshot. The proposed causal design requires VIIRS annual
  composites (2012-2023) and DMSP-OLS composites (1992-2013) to construct a
  panel. These were not downloaded due to time constraints. Without panel
  data, before-after comparisons are not possible and zone effects cannot be
  separated from pre-existing district differences.

Section 5 - Maps
  Defines three ggplot map objects (map1, map2, map3). Maps are printed and
  saved in Section 10.

Section 6 - Zone vs. Non-Zone District Comparison
  WHY DISTRICTS, NOT PROVINCES:
  OSBs are sub-provincial investments. Comparing zone provinces to non-zone
  provinces conflates the zone effect with broader regional advantages that
  drove placement. Staying within provinces and comparing adjacent districts
  provides a more similar control group.

  WHY ADJACENT DISTRICTS, NOT ALL DISTRICTS:
  The comparison group consists only of districts sharing a border with a
  zone district. This produces a more homogeneous comparison than a province-
  wide sample (CV of night lights: 2.59 vs. 2.87) and makes parallel trends
  more plausible for future panel work.

  GADM VS. TUIK NAME MISMATCH:
  Merging SES scores from TUIK onto GADM boundaries required a manual name
  crosswalk. Claude was used to identify the source of the mismatches. The
  two datasets reflect different administrative boundaries:

  GADM uses pre-2013 boundaries: central districts are called "Merkez" and
  several province names are abbreviated (Afyon, K. Maras, Kinkkale).

  TUIK uses post-2013 boundaries following Turkey's metropolitan municipality
  reforms, which split large Merkez districts into multiple named urban
  districts (e.g. Diyarbakir's Merkez became Baglar, Kayapinar, Sur, and
  Yenisehir) and updated several place names (e.g. Ilica -> Aziziye, Eyup ->
  Eyupsultan, Sincanli -> Sinanpasa).

  The case_when block in Section 6 maps GADM names to TUIK equivalents. For
  provinces where Merkez was split into multiple districts, the most populous
  central urban district is used.

Section 7 - Establishment Year Scraping
  Scrapes individual OSB websites to recover founding years. The scraper
  checks each zone's homepage and six common Turkish subpages (/hakkimizda,
  /tarihce, /kurumsal, etc.), extracting years using founding keywords and a
  fallback to the earliest plausible year on the page. Year range is
  restricted to 1961-2015. Results are cached as osb_sf_scraped.rds —
  scraping 116 zones with Sys.sleep(3) between requests takes 30-40 minutes.
  Years should be treated with caution as they may reflect legal founding
  rather than operational start.

Section 8 - Clean Zone Data
  Builds osb_clean, the zone-level dataset exported as zones.csv. Extracts
  exact lat/lon coordinates from geometry before dropping the spatial object.
  Years below 1961 are flagged and excluded.

Section 9 - Consistency Checks
  Runs stopifnot() checks on all key objects before export: geocoding counts,
  district overlap, zone summary arithmetic, column presence in both output
  files, and missing data. If any check fails the script halts with an
  informative error.

Section 10 - Export
  Saves maps to maps/ and writes zones.csv and analysis_units.csv.
  NAs are written as empty strings (na = "").


KEY DESIGN DECISIONS
--------------------
Distance measure consistency:
  analysis_units.csv uses centroid-to-road distances for all districts —
  both zone and non-zone — for comparability. Exact zone coordinates are
  retained in zones.csv for future analysis of zone siting decisions but
  are not used in the district-level comparison.

Geocoding coverage:
  116 of 297 zones were matched, likely overrepresenting larger and better-
  documented OSBs with stronger OSM presence. Remaining zones could be
  recovered via manual geocoding using the address field in the OSBUK registry.

Establishment year coverage:
  54 of 116 zones (46.6%) have a scraped year. Of these, only 9 opened after
  2012, the start of VIIRS coverage — severely limiting panel feasibility
  with current data.


AI DISCLOSURE
-------------
Claude was used for debugging, identifying the GADM/TUIK administrative
boundary mismatch, and organizing the name crosswalk. All analytical
decisions are my own.