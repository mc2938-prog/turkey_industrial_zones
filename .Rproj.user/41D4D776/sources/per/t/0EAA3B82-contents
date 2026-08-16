# ============================================================
# SPATIAL ANALYSIS OF ORGANIZED INDUSTRIAL ZONES IN TURKEY
# ============================================================
# Description: Constructs a spatial dataset of Turkish OSBs,
#              merges spatial/economic data, and compares
#              zone vs non-zone district characteristics.
# ============================================================

# ============================================================
# 0. PACKAGES
# ============================================================

packages <- c(
  "tidyverse",
  "sf",
  "terra",
  "tidygeocoder",
  "geodata",
  "osmdata",
  "exactextractr",
  "tmap",
  "fixest",
  "readxl",
  "blackmarbler",
  "httr",
  "rvest",
  "knitr"
)

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))

# ============================================================
# 1. LOAD DATASETS
# ============================================================
# Goal: load all raw data sources used in the analysis.
#       Four datasets are required:
#         (1) OSB zone registry — defines the treatment units
#         (2) Socioeconomic score — district-level outcome/control variable
#         (3) Motorway network — used to calculate infrastructure access
#         (4) Night lights raster — proxy for local economic activity
#
# OSB - ZONE DATA
# Source: OSBÜK (Organize Sanayi Bölgeleri Üst Kuruluşu) official registry
# URL: https://osbuk.org/view/osb/osbliste.php
# Contains: 297 active OSBs with zone name, province, phone,
#           operational status, email, website, and address
# Status: all zones in this dataset are active (ISLETMEDE/FAALIYETTE)
# Note: table was manually copied from the OSBÜK website into Excel

osb <- read_excel("firms.xlsx")

# SOCIOECONOMIC SCORE
# Source: TUIK (Turkish Statistical Institute)
# URL: https://veriportali.tuik.gov.tr/en/press/57942
# SES is calculated based on income, education, and occupation.
# Score ranges from 0 (lowest) to 300 (highest).
# A-E columns are shares of households in each SES category.
score <- read_excel("socioeconomic_scores_district_2023.xls", skip = 4, col_names = TRUE)

# MOTORWAYS
# Source: HOT OSM Turkey road extract
# URL: https://data.humdata.org/dataset/hotosm_tur_roads
# Accessed: April 2026
# Includes motorways and trunk roads
# Cached as RDS to avoid re-downloading
if (file.exists("motorways_sf.rds")) {
  motorways_sf <- readRDS("motorways_sf.rds")
} else {
  motorways <- opq(bbox = "Turkey") %>%
    add_osm_feature(key = "highway",
                    value = c("motorway", "trunk")) %>%
    osmdata_sf()
  motorways_sf <- motorways$osm_lines %>%
    st_transform(crs = 4326)
  saveRDS(motorways_sf, "motorways_sf.rds")
}

# NIGHT LIGHTS
# Source: Earth Observation Group, Colorado School of Mines
# Product: VNL Version 2, Annual Composite 2025, Average Masked
# URL: https://eogdata.mines.edu/products/vnl/
# Masked version removes ephemeral lights (fires, gas flares)
nlavg2025 <- rast("2025avgnl.tif")


# ============================================================
# 2. CLEANING
# ============================================================
# Goal: standardize both datasets for reliable merging and spatial joins.
#       Two issues require attention:
#         (1) Turkish special characters (ş, ı, ğ, ü, ö, ç) cause silent
#             join failures when one source uses UTF-8 Turkish and another
#             uses ASCII. fix_turkish() normalizes all text to ASCII
#             equivalents and is applied consistently to every character
#             column before any join operation.
#         (2) Column names in both files are in Turkish and require
#             renaming to English equivalents for readability.
#
# fix_turkish() is defined here and reused in Sections 3, 6, and 7
# wherever district or province name matching is required.
fix_turkish <- function(x) {
  x <- gsub("ş", "s", x); x <- gsub("Ş", "S", x)
  x <- gsub("ı", "i", x); x <- gsub("İ", "I", x)
  x <- gsub("ğ", "g", x); x <- gsub("Ğ", "G", x)
  x <- gsub("ü", "u", x); x <- gsub("Ü", "U", x)
  x <- gsub("ö", "o", x); x <- gsub("Ö", "O", x)
  x <- gsub("ç", "c", x); x <- gsub("Ç", "C", x)
  x
}

# OSB: clean column names and rename to English equivalents
osb <- osb %>%
  rename_with(~ fix_turkish(.) %>% tolower() %>% gsub(" ", "_", .)) %>%
  rename(
    id        = `#`,
    province  = osb_ili,
    zone_name = osb_unvani,
    phone     = telefon,
    status    = fiili_durumu,
    email     = `e-posta`,
    website   = web_adresi,
    address   = iletisim_adresi
  ) %>%
  mutate(across(where(is.character), fix_turkish))

# each zone name is unique — confirmed
length(unique(osb$zone_name)) == length(osb$zone_name)

score_clean <- score %>%
  rename(
    province   = 1,
    district   = 2,
    mean_score = 3,
    total      = 4,
    A_plus     = 5,
    B          = 6,
    C          = 7,
    D          = 8,
    E          = 9
  ) %>%
  filter(province != "Toplam-Total") %>%
  tidyr::fill(province, .direction = "down") %>%
  filter(!is.na(district)) #This is done because the NAs in the raw dataset are province-level means which are not needed



# ============================================================
# 3. GEOCODING
# ============================================================
# Goal: convert the OSBÜK zone registry into a spatial point dataset
#       by geocoding each zone's name and province via OpenStreetMap.
#       The resulting sf object (osb_sf) is the primary zone dataset
#       used in all subsequent spatial joins and distance calculations.
#
# Method: query constructed as "<zone_name> Organize Sanayi Bolgesi <province> Turkey"
#         and passed to the Nominatim OSM geocoder via tidygeocoder.
#
# Coverage: 117 of 297 zones matched (39.4%), covering 54 of 81 provinces.
#   Unmatched zones are dropped. This likely introduces a bias toward
#   larger and better-documented zones with a stronger OSM presence.
#   With more time, unmatched zones could be geocoded manually using
#   Google Maps or the address field in the OSBÜK registry.

if (file.exists("osb_geocoded.rds")) {
  osb_geocoded <- readRDS("osb_geocoded.rds")
} else {
  osb_geocoded <- osb %>%
    mutate(query = paste(zone_name, "Organize Sanayi Bolgesi", province, "Turkey")) %>%
    geocode(query, method = "osm", lat = lat, lon = lon) %>%
    filter(!is.na(lat))
  
  saveRDS(osb_geocoded, "osb_geocoded.rds")
}

nrow(osb_geocoded)

osb_geocoded %>%
  count(province) %>%
  arrange(desc(n))

# convert to sf object (WGS84)
osb_sf <- osb_geocoded %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


# ============================================================
# 4. SPATIAL DATA CONSTRUCTION
# ============================================================
# Goal: build the district-level spatial dataset that serves as the
#       primary unit of analysis throughout the project. This section:
#         (1) downloads GADM level-2 district boundaries for Turkey
#         (2) attaches night light radiance and road distance to each district
#         (3) spatially joins OSB points to districts so each zone
#             inherits its host district's identifier and characteristics
#
# All spatial objects use WGS 84 (EPSG:4326) throughout.
# Distances are calculated in meters via st_distance() and converted to km.
# District centroids are used for district-level distance calculations;
# exact zone coordinates are used for zone-level distance calculations.
turkey_district <- gadm(country = "TUR", level = 2, path = tempdir()) %>%
  st_as_sf() %>%
  rename(
    district_id  = GID_2,
    country_code = GID_0,
    country_name = COUNTRY,
    province_id  = GID_1,
    province     = NAME_1,
    district     = NAME_2,
    alt_name     = VARNAME_2,
    local_type   = TYPE_2,
    eng_type     = ENGTYPE_2,
    admin_code   = CC_2,
    hasc_code    = HASC_2
  )

# crop night lights raster to Turkey extent
ntl_turkey <- crop(nlavg2025, turkey_district)

# distance from each district centroid to nearest motorway/trunk road (km)
# st_centroid: center point of each district polygon
# st_union: merges all road segments so distance is to nearest road overall
turkey_district <- turkey_district %>%
  mutate(
    dist_road_km = as.numeric(
      st_distance(
        st_centroid(st_geometry(turkey_district)),
        st_union(motorways_sf)
      )
    ) / 1000
  )

# extract mean night light radiance per district
turkey_district <- turkey_district %>%
  mutate(mean_ntl_2025 = exact_extract(ntl_turkey, turkey_district, "mean"))

# spatial join zones to districts — attaches district name to each OSB point
osb_sf <- osb_sf %>%
  st_join(turkey_district %>% select(district))

# distance from each OSB point coordinate to nearest motorway (km)
osb_sf <- osb_sf %>%
  mutate(
    dist_road_km = as.numeric(
      st_distance(osb_sf, st_union(motorways_sf))
    ) / 1000
  )


# ============================================================
# 5. MAPS
# ============================================================
# Goal: visualize the spatial distribution of OSBs and their
#       relationship to the motorway network. Map 1 shows zone
#       coverage across districts. Map 2 adds the road network
#       to visually assess whether zones cluster near motorways.

map1 <- ggplot() +
  geom_sf(data = turkey_district, fill = "grey95", color = "grey70", linewidth = 0.3) +
  geom_sf(data = osb_sf, color = "red", size = 1.5, alpha = 0.7) +
  theme_minimal() +
  labs(
    title    = "Organized Industrial Zones in Turkey - District Level",
    subtitle = paste(nrow(osb_sf), "zones geocoded via OSM"),
    caption  = "Source: OSBÜK, OpenStreetMap"
  )

map2 <- ggplot() +
  geom_sf(data = turkey_district, fill = "grey95", color = "grey70", linewidth = 0.3) +
  geom_sf(data = motorways_sf, color = "steelblue", linewidth = 0.4, alpha = 0.7) +
  geom_sf(data = osb_sf, color = "red", size = 1.5, alpha = 0.8) +
  theme_minimal() +
  labs(
    title    = "Organized Industrial Zones and Major Roads in Turkey",
    subtitle = "Red = OSB zones | Blue = Motorways and trunk roads",
    caption  = "Sources: OSBÜK, OpenStreetMap, GADM"
  )

map3 <- turkey_district %>%
  ggplot() +
  geom_sf(aes(fill = log1p(mean_ntl_2025)), color = NA) +
  scale_fill_viridis_c(
    option   = "magma",
    name     = "Log radiance",
    na.value = "grey80"
  ) +
  geom_sf(data = osb_sf, color = "cyan", size = 1, alpha = 0.8) +
  theme_minimal() +
  labs(
    title    = "Night Light Radiance and Organized Industrial Zones",
    subtitle = "VIIRS VNL 2025 annual composite, district means (log scale)",
    caption  = "Sources: EOG Colorado School of Mines, OSBÜK"
  )
# ============================================================
# 6. ZONE VS NON-ZONE DISTRICT COMPARISON (PART 3)
# ============================================================
# Goal: describe whether districts with OSBs differ from nearby
#       districts without OSBs in terms of infrastructure access,
#       economic activity, and socioeconomic status.
#
# Unit of analysis: GADM level-2 districts
#
# Main variables:
#   has_zone      = 1 if district contains at least one OSB
#   dist_road_km  = distance from district centroid to nearest motorway/trunk road
#   mean_ntl_2025 = mean VIIRS night light radiance in 2025
#   mean_score    = district-level socioeconomic score from TUIK
# ============================================================

#identify zone districts
district_with_zones <- turkey_district %>%
  st_join(osb_sf %>% select(zone_name)) %>%
  st_drop_geometry() %>%
  filter(!is.na(zone_name)) %>%
  distinct(district_id) %>%
  pull(district_id)

zone_district <- turkey_district %>%
  filter(district_id %in% district_with_zones)

non_zone_district <- turkey_district %>%
  filter(province %in% zone_district$province,
         !district_id %in% district_with_zones)

length(district_with_zones)  # 88 zone districts
nrow(zone_district)          # should match
nrow(non_zone_district)      # 598 comparison districts

#construct adjacent comparison group
neighboring_ids <- turkey_district %>%
  st_filter(zone_district, .predicate = st_touches) %>%
  pull(district_id) %>%
  setdiff(district_with_zones)

#build analysis dataset
analysis_units_adjacent <- turkey_district %>%
  st_drop_geometry() %>%
  mutate(
    has_zone = district_id %in% district_with_zones,
    district = fix_turkish(district),
    province = fix_turkish(province)
  ) %>%
  filter(
    district_id %in% district_with_zones |
      district_id %in% neighboring_ids
  ) %>%
  select(
    district_id,
    province,
    district,
    has_zone,
    dist_road_km,
    mean_ntl_2025
  )

#prepare score join keys
score_join <- score_clean %>%
  mutate(
    province_join = fix_turkish(tolower(trimws(province))),
    district_join = fix_turkish(tolower(trimws(district)))
  ) %>%
  select(province_join, district_join, mean_score)

#create join keys on analysis units
analysis_units_adjacent <- analysis_units_adjacent %>%
  mutate(
    province_join = fix_turkish(tolower(trimws(province))),
    district_join = fix_turkish(tolower(trimws(district)))
  )

#fix name mismatches between GADM and TUIK
#Claude was used here for identifying the administrative boundaries!
# NOTE: GADM reflects pre-2013 boundaries: central districts are named "Merkez"
# and several province names are abbreviated (Afyon, K. Maras, Kinkkale).
# TUIK reflects post-2013 boundaries following Turkey's metropolitan
# municipality reforms, which split large Merkez districts into multiple
# named urban districts (e.g. Diyarbakir's Merkez became Baglar,
# Kayapinar, Sur, and Yenisehir) and updated several place names
# (e.g. Ilica -> Aziziye, Eyup -> Eyupsultan, Mustafa Kemalpasa ->
# Mustafakemalpasa). The case_when below maps GADM names to their
# TUIK equivalents. For split Merkez districts, the most populous
# central urban district is used.
analysis_units_adjacent <- analysis_units_adjacent %>%
  mutate(
    province_join = case_when(
      province_join == "k. maras" ~ "kahramanmaras",
      province_join == "kinkkale" ~ "kirikkale",
      province_join == "afyon"    ~ "afyonkarahisar",
      TRUE ~ province_join
    ),
    district_join = case_when(
      # individual name mismatches
      district_join == "kazan" & province_join == "ankara"            ~ "kahramankazan",
      district_join == "sultan kochisar" & province_join == "ankara"  ~ "sereflikochisar",
      district_join == "mustafa kemalpasa" & province_join == "bursa" ~ "mustafakemalpasa",
      district_join == "suleoglu" & province_join == "edirne"         ~ "suloglu",
      district_join == "ilica" & province_join == "erzurum"           ~ "aziziye",
      district_join == "eyup" & province_join == "istanbul"           ~ "eyupsultan",
      # merkez mappings — using the main urban district
      district_join == "merkez" & province_join == "antalya"          ~ "muratpasa",
      district_join == "merkez" & province_join == "aydin"            ~ "efeler",
      district_join == "merkez" & province_join == "balikesir"        ~ "karesi",
      district_join == "merkez" & province_join == "denizli"          ~ "merkezefendi",
      district_join == "merkez" & province_join == "diyarbakir"       ~ "kayapinar",
      district_join == "merkez" & province_join == "eskisehir"        ~ "odunpazari",
      district_join == "merkez" & province_join == "kahramanmaras"    ~ "onikisubat",
      district_join == "merkez" & province_join == "kocaeli"          ~ "izmit",
      district_join == "merkez" & province_join == "malatya"          ~ "battalgazi",
      district_join == "merkez" & province_join == "manisa"           ~ "yunusemre",
      district_join == "merkez" & province_join == "mardin"           ~ "artuklu",
      district_join == "merkez" & province_join == "mersin"           ~ "yenisehir",
      district_join == "merkez" & province_join == "ordu"             ~ "altinordu",
      district_join == "merkez" & province_join == "sakarya"          ~ "adapazari",
      district_join == "merkez" & province_join == "sanliurfa"        ~ "haliliye",
      district_join == "merkez" & province_join == "tekirdag"         ~ "suleymanpasa",
      district_join == "merkez" & province_join == "van"              ~ "ipekyolu",
      district_join == "sincanli" & province_join == "afyonkarahisar" ~ "sinanpasa",
      TRUE ~ district_join
    )
  )

# --- STEP 7: merge SES score ---
analysis_units_adjacent <- analysis_units_adjacent %>%
  left_join(score_join, by = c("province_join", "district_join"))

# check missing
cat("Missing SES scores:", sum(is.na(analysis_units_adjacent$mean_score)), "\n")

analysis_units_adjacent %>%
  filter(is.na(mean_score)) %>%
  select(province, district) %>%
  as.data.frame() %>%
  print()

# --- STEP 8: descriptive summary ---
zone_summary <- analysis_units_adjacent %>%
  group_by(has_zone) %>%
  summarise(
    n                 = n(),
    mean_dist_road_km = mean(dist_road_km, na.rm = TRUE),
    mean_ntl_2025     = mean(mean_ntl_2025, na.rm = TRUE),
    mean_score        = mean(mean_score, na.rm = TRUE),
    .groups = "drop"
  )

zone_summary %>%
  kable(digits = 2)

# --- STEP 9: regressions ---

# simple regressions — differences in means
reg_road  <- feols(dist_road_km  ~ has_zone, data = analysis_units_adjacent)
reg_ntl   <- feols(mean_ntl_2025 ~ has_zone, data = analysis_units_adjacent)
reg_score <- feols(mean_score    ~ has_zone, data = analysis_units_adjacent)

etable(reg_road, reg_ntl, reg_score)

# within-province regressions
reg_road_prov  <- feols(dist_road_km  ~ has_zone | province,
                        data = analysis_units_adjacent, cluster = ~province)
reg_ntl_prov   <- feols(mean_ntl_2025 ~ has_zone | province,
                        data = analysis_units_adjacent, cluster = ~province)
reg_score_prov <- feols(mean_score    ~ has_zone | province,
                        data = analysis_units_adjacent, cluster = ~province)

etable(reg_road_prov, reg_ntl_prov, reg_score_prov)
# ============================================================
# 7. ESTABLISHMENT YEAR SCRAPING (PART 4)
# ============================================================
# Goal: recover OSB founding years to enable a panel-based causal
#       analysis of zone effects on local economic outcomes.
# SCRAPING STRATEGY FOR OSB ESTABLISHMENT YEARS
# -----------------------------------------------
# Source: Individual OSB websites from OSBÜK registry (osb_sf$website column)
# The scraper attempts to find founding years in three layers:
# Layer 1 - Homepage
#   Fetches the root URL first (e.g. https://www.bosb.org.tr)
# Layer 2 - Subpage fallback
#   If no year found on homepage, sequentially tries common Turkish
#   about/history subpages: /hakkimizda, /tarihce, /kurumsal,
#   /hakkinda, /biz-kimiz, /tarihcemiz. Stops at first success.
# Layer 3 - Two-tier year extraction
#   Priority 1: Lines containing a founding keyword AND a year.
#               Keywords: kuruluş, kuruldu, faaliyete geçti,
#               hizmete girdi, yılından bu yana, etc.
#   Priority 2: If no keyword match, takes the earliest year
#               found anywhere on the page as a fallback.
# Year range filter: 1961-2015 only.
#   Lower bound: 1961 = year foundations laid for Turkey's first OSB (Bursa)
#   Upper bound: 2015 = excludes recent news dates
# Reliability safeguards:
#   - encoding = "UTF-8" to correctly parse Turkish characters
#   - Sys.sleep(3) between requests to avoid IP blocking
#   - tryCatch() so a single failed site does not crash the loop
#   - source_url recorded for every extracted year for traceability
#   - batches of 20 with 5s pause to avoid connection overflow

scrape_est_year <- function(base_url) {
  Sys.sleep(3)
  
  subpages <- c("", "/hakkimizda", "/tarihce", "/kurumsal",
                "/hakkinda", "/biz-kimiz", "/tarihcemiz")
  
  urls_to_try <- paste0(base_url, subpages)
  
  found_year <- NA
  found_url  <- NA
  
  for (url in urls_to_try) {
    result <- tryCatch({
      con <- url(url, open = "r")
      on.exit(try(close(con), silent = TRUE))
      
      page  <- read_html(url, encoding = "UTF-8")
      text  <- page %>% html_text2()
      lines <- str_split(text, "\n")[[1]]
      lines <- trimws(lines)
      lines <- lines[nchar(lines) > 10]
      
      founding_lines <- lines[str_detect(
        lines, "\\b(19[5-9]\\d|200\\d|201[0-5])\\b"
      )]
      
      keyword_lines <- founding_lines[str_detect(
        founding_lines,
        regex("kuruluş|kuruldu|kurul tarihi|faaliyete geç|faaliyete başla|
               hizmete girdi|yılından bu yana|dan bu yana|den bu yana",
              ignore_case = TRUE)
      )]
      
      if (length(keyword_lines) > 0) {
        str_extract(keyword_lines[1], "\\b(19[5-9]\\d|200\\d|201[0-5])\\b")
      } else if (length(founding_lines) > 0) {
        years <- str_extract_all(
          paste(founding_lines, collapse = " "),
          "\\b(19[5-9]\\d|200\\d|201[0-5])\\b"
        ) %>% unlist() %>% as.integer()
        as.character(min(years))
      } else {
        NA
      }
      
    }, error = function(e) NA)
    
    if (!is.na(result)) {
      found_year <- result
      found_url  <- url
      break
    }
  }
  
  tibble(
    website_url = base_url,
    est_year    = found_year,
    source_url  = found_url
  )
}

if (file.exists("osb_sf_scraped.rds")) {
  osb_sf <- readRDS("osb_sf_scraped.rds")
} else {
  osb_sf <- osb_sf %>%
    mutate(website_url = paste0("https://", website))
  
  urls    <- osb_sf$website_url
  batches <- split(urls, ceiling(seq_along(urls) / 20))
  
  year_results <- map_dfr(batches, function(batch) {
    result <- map_dfr(batch, scrape_est_year)
    Sys.sleep(5)
    result
  })
  
  year_results %>%
    summarise(
      hits  = sum(!is.na(est_year)),
      total = n(),
      pct   = round(hits / total * 100, 1)
    ) %>% print()
  
  osb_sf <- osb_sf %>%
    left_join(year_results, by = "website_url")
  
  saveRDS(osb_sf, "osb_sf_scraped.rds")
}

# ============================================================
# 8. CLEAN ZONE DATA FOR ANALYSIS (PART 4)
# ============================================================
osb_clean <- osb_sf %>%
  st_join(turkey_district %>% select(district_id)) %>%
  mutate(est_year_clean = ifelse(as.integer(est_year) < 1961, NA, as.integer(est_year)),
         year_flag = case_when(
           as.integer(est_year) < 1961 ~ "excluded - predates first Turkish OSB (1961)",
           TRUE ~ "ok"
         ),
         lat = st_coordinates(geometry)[, 2],
         lon = st_coordinates(geometry)[, 1]) %>%
  st_drop_geometry() %>%
  select(
    zone_name,
    province,
    district_id,
    lat,
    lon,
    est_year_clean,
    year_flag,
    dist_road_km,
    website_url,
    source_url
  )

# distribution of establishment years
osb_clean %>%
  filter(!is.na(est_year_clean)) %>%
  mutate(est_year_clean = as.integer(est_year_clean)) %>%
  ggplot(aes(x = est_year_clean)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(
    title   = "Distribution of OSB Establishment Years",
    x       = "Establishment Year",
    y       = "Count",
    caption = "Source: OSB websites, scraped via rvest"
  )

# VIIRS coverage: only 9 zones opened after 2012
osb_clean %>%
  filter(!is.na(est_year_clean)) %>%
  mutate(viirs_coverage = est_year_clean >= 2012) %>%
  count(viirs_coverage)

analysis_units_adjacent <- analysis_units_adjacent %>%
  left_join(
    osb_clean %>%
      filter(!is.na(est_year_clean)) %>%
      group_by(district_id) %>%
      summarise(est_year = min(est_year_clean, na.rm = TRUE), .groups = "drop"),
    by = "district_id"
  )

# ============================================================
# 9. CONSISTENCY CHECKS
# ============================================================

# --- geocoding ---
stopifnot(nrow(osb_sf) == nrow(osb_geocoded))

# --- district construction ---
stopifnot(nrow(zone_district) == length(district_with_zones))

# --- analysis units ---
stopifnot(
  nrow(analysis_units_adjacent) ==
    length(district_with_zones) + length(neighboring_ids)
)
stopifnot(sum(analysis_units_adjacent$has_zone) == length(district_with_zones))
stopifnot(!any(neighboring_ids %in% district_with_zones))  # no overlap

# --- zone summary numbers ---
stopifnot(abs(
  zone_summary$mean_dist_road_km[zone_summary$has_zone == TRUE] -
    mean(analysis_units_adjacent$dist_road_km[analysis_units_adjacent$has_zone], na.rm = TRUE)
) < 1e-6)
stopifnot(abs(
  zone_summary$mean_ntl_2025[zone_summary$has_zone == TRUE] -
    mean(analysis_units_adjacent$mean_ntl_2025[analysis_units_adjacent$has_zone], na.rm = TRUE)
) < 1e-6)

# --- SES merge ---
stopifnot(
  sum(is.na(analysis_units_adjacent$mean_score)) ==
    nrow(analysis_units_adjacent) - sum(!is.na(analysis_units_adjacent$mean_score))
)

# --- zones export ---
stopifnot(nrow(osb_clean) == nrow(osb_geocoded))
stopifnot(all(c("zone_name", "province", "district_id",
                "lat", "lon", "est_year_clean",
                "dist_road_km", "website_url") %in% names(osb_clean)))

# --- analysis units export ---
stopifnot(all(c("district_id", "province", "district", "has_zone",
                "dist_road_km", "mean_ntl_2025",
                "mean_score") %in% names(analysis_units_adjacent)))

# ============================================================
# 10. MEMO NUMBERS
# ============================================================

cat("=== GEOCODING ===\n")
cat("Zones geocoded:", nrow(osb_geocoded), "of", nrow(osb), "\n")
cat("Coverage:", round(nrow(osb_geocoded) / nrow(osb) * 100, 1), "%\n")
cat("Provinces covered:", n_distinct(osb_geocoded$province), "of 81\n")

cat("\n=== DISTRICTS ===\n")
cat("Zone districts:", length(district_with_zones), "\n")
cat("Neighboring non-zone districts:", length(neighboring_ids), "\n")
cat("Total analysis units:", nrow(analysis_units_adjacent), "\n")

cat("\n=== DESCRIPTIVE MEANS ===\n")
cat("Zone districts - mean road distance (km):",
    round(zone_summary$mean_dist_road_km[zone_summary$has_zone == TRUE], 2), "\n")
cat("Non-zone districts - mean road distance (km):",
    round(zone_summary$mean_dist_road_km[zone_summary$has_zone == FALSE], 2), "\n")
cat("Zone districts - mean night lights:",
    round(zone_summary$mean_ntl_2025[zone_summary$has_zone == TRUE], 2), "\n")
cat("Non-zone districts - mean night lights:",
    round(zone_summary$mean_ntl_2025[zone_summary$has_zone == FALSE], 2), "\n")
cat("Zone districts - mean SES:",
    round(zone_summary$mean_score[zone_summary$has_zone == TRUE], 2), "\n")
cat("Non-zone districts - mean SES:",
    round(zone_summary$mean_score[zone_summary$has_zone == FALSE], 2), "\n")

cat("\n=== REGRESSIONS ===\n")
cat("SES coef (province FE):",
    round(coef(reg_score_prov)["has_zoneTRUE"], 2), "\n")
cat("Night lights coef (province FE):",
    round(coef(reg_ntl_prov)["has_zoneTRUE"], 2), "\n")
cat("Road distance coef (province FE):",
    round(coef(reg_road_prov)["has_zoneTRUE"], 2), "\n")

cat("\n=== SCRAPING ===\n")
cat("Zones with establishment year:",
    sum(!is.na(osb_clean$est_year_clean)), "of", nrow(osb_clean), "\n")
cat("Coverage:",
    round(sum(!is.na(osb_clean$est_year_clean)) / nrow(osb_clean) * 100, 1), "%\n")

cat("\n=== MISSING DATA ===\n")
#missing SES score
cat("Missing SES scores:", sum(is.na(analysis_units_adjacent$mean_score)), "\n")
#missing night light
cat("Missing NTL:", sum(is.na(analysis_units_adjacent$mean_ntl_2025)), "\n")

# missing road distance
cat("Missing road dist:", sum(is.na(analysis_units_adjacent$dist_road_km)), "\n")

# missing est_year (expected — most districts won't have one)
cat("Missing est_year:", sum(is.na(analysis_units_adjacent$est_year)), "\n")
cat("Has est_year:", sum(!is.na(analysis_units_adjacent$est_year)), "\n")

# zones missing district_id after spatial join
cat("Zones missing district_id:", sum(is.na(osb_clean$district_id)), "\n")

# zones missing coordinates
cat("Zones missing lat:", sum(is.na(osb_clean$lat)), "\n")
cat("Zones missing lon:", sum(is.na(osb_clean$lon)), "\n")

# ============================================================
# 10. EXPORT
# ============================================================
# maps
dir.create("maps", showWarnings = FALSE)

#print(map1)
ggsave("maps/zones_district.png", map1, width = 12, height = 6, dpi = 300)

#print(map2)
ggsave("maps/zones_motorways.png", map2, width = 12, height = 6, dpi = 300)

#print(map3)
ggsave("maps/nightlights_districts.png", map3, width = 12, height = 6, dpi = 300)

# zones.csv — one row per OSB zone
write_csv(osb_clean, "zones.csv", na = "")

# analysis_units.csv — one row per district (zone + neighbors)
write_csv(
  analysis_units_adjacent %>%
    select(
      district_id,
      province,
      district,
      has_zone,
      est_year,
      dist_road_km,
      mean_ntl_2025,
      mean_score
    ),
  "analysis_units.csv",
,  na = "")
