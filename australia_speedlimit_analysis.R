# ============================================================
#  Australia Capital Cities – OSM Speed Limit Analysis
#  Produces results for 30 km/h AND 40 km/h roads
#
#  Inputs : <city>_australia.osm.pbf  (one per city)
#  Outputs:
#    <city>_<speed>kmh.gpkg               GeoPackage per city × speed
#    <city>_<speed>kmh_map.html           Standalone offline map
#    australia_<speed>kmh_summary.csv     Summary table per speed
#    docs/index.html                      GitHub Pages dashboard
#    docs/data/<city>_<speed>.geojson     Data files for dashboard
# ============================================================


# ── 0. Packages ───────────────────────────────────────────────
required_pkgs <- c("osmextract", "sf", "dplyr", "tibble", "knitr")
invisible(lapply(required_pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org")
}))
library(osmextract)
library(sf)
library(dplyr)
library(tibble)
library(knitr)


# ── 1. Config ─────────────────────────────────────────────────
CITIES <- list(
  list(name = "Melbourne", pbf = "melbourne_australia.osm.pbf", epsg = 7855, color = "#c0392b"),
  list(name = "Sydney",    pbf = "sydney_australia.osm.pbf",    epsg = 7856, color = "#2980b9"),
  list(name = "Brisbane",  pbf = "brisbane_australia.osm.pbf",  epsg = 7856, color = "#27ae60"),
  list(name = "Adelaide",  pbf = "adelaide_australia.osm.pbf",  epsg = 7855, color = "#e67e22"),
  list(name = "Perth",     pbf = "perth_australia.osm.pbf",     epsg = 7850, color = "#8e44ad")
)

SPEEDS <- c(30, 40)

CAR_ROADS <- c(
  "motorway", "motorway_link", "trunk", "trunk_link",
  "primary", "primary_link", "secondary", "secondary_link",
  "tertiary", "tertiary_link", "residential", "living_street",
  "unclassified", "service", "road"
)

PERMITTED  <- c("yes", "public", "permissive", "destination")
EXTRA_TAGS <- c("maxspeed", "access", "motor_vehicle", "motorcar")

# Simplification tolerance (metres, in projected CRS) for web GeoJSON.
# Keeps standalone 40 km/h maps under ~3 MB; has no visible effect at city zoom.
SIMPLIFY_TOL <- 12


# ── 2. Download libraries once (offline embedding) ────────────
fetch_text <- function(url) {
  tmp <- tempfile()
  download.file(url, tmp, quiet = TRUE, mode = "wb")
  paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

cat("[Libraries] Downloading Leaflet + Chart.js...\n")
LEAFLET_CSS <- fetch_text("https://unpkg.com/leaflet@1.9.4/dist/leaflet.css")
LEAFLET_JS  <- fetch_text("https://unpkg.com/leaflet@1.9.4/dist/leaflet.js")
CHART_JS    <- fetch_text("https://cdn.jsdelivr.net/npm/chart.js@4.4.2/dist/chart.umd.min.js")
cat("[Libraries] Done.\n")


# ── 3. Process each city ──────────────────────────────────────
process_city <- function(city) {
  n <- city$name
  cat(sprintf("\n[%s] Reading OSM PBF...\n", n))

  raw <- oe_read(city$pbf, layer = "lines", extra_tags = EXTRA_TAGS, quiet = TRUE)

  # Step 1: All car-accessible public roads (tagged AND untagged) for denominator
  roads_public <- raw %>%
    filter(highway %in% CAR_ROADS) %>%
    filter(is.na(access)        | access        %in% PERMITTED) %>%
    filter(is.na(motor_vehicle) | motor_vehicle %in% PERMITTED) %>%
    filter(is.na(motorcar)      | motorcar      %in% PERMITTED) %>%
    filter(!st_is_empty(geometry))

  total_segs    <- nrow(roads_public)
  untagged_segs <- nrow(filter(roads_public, is.na(maxspeed) | maxspeed == ""))
  tagged_segs   <- total_segs - untagged_segs
  tagged_pct    <- round(tagged_segs / total_segs * 100, 1)
  cat(sprintf("[%s] All car roads: %s segs | tagged: %s (%s%%) | untagged: %s\n",
              n, format(total_segs, big.mark=","),
              format(tagged_segs, big.mark=","), tagged_pct,
              format(untagged_segs, big.mark=",")))

  # Step 2: Filter to tagged roads for speed analysis
  roads <- roads_public %>%
    filter(!is.na(maxspeed), maxspeed != "") %>%
    mutate(
      maxspeed_kmh = case_when(
        grepl("^[0-9]+$",      maxspeed)                      ~ as.numeric(maxspeed),
        grepl("^[0-9]+ km/h$", maxspeed, ignore.case = TRUE)  ~
          as.numeric(sub(" km/h", "", maxspeed, ignore.case = TRUE)),
        grepl("^[0-9]+ mph$",  maxspeed, ignore.case = TRUE)  ~
          round(as.numeric(sub(" mph", "", maxspeed, ignore.case = TRUE)) * 1.60934),
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(maxspeed_kmh))

  # Total km of ALL public car roads (tagged + untagged) — true denominator
  total_km_all <- roads_public %>%
    st_transform(city$epsg) %>%
    mutate(length_km = as.numeric(st_length(geometry)) / 1000) %>%
    pull(length_km) %>%
    sum(na.rm = TRUE)

  roads_proj <- roads %>%
    st_transform(city$epsg) %>%
    mutate(length_km = as.numeric(st_length(geometry)) / 1000)

  total_km <- sum(roads_proj$length_km, na.rm = TRUE)

  # Extract per-speed stats and geometries
  speed_data <- lapply(SPEEDS, function(spd) {
    r   <- filter(roads_proj, maxspeed_kmh == spd)
    km      <- sum(r$length_km, na.rm = TRUE)
    n_r     <- nrow(r)
    pct     <- round(km / total_km     * 100, 2)
    pct_all <- round(km / total_km_all * 100, 3)

    cat(sprintf("[%s] %d km/h: %.1f km (%s segs, %.2f%% of tagged, %.3f%% of all)\n",
                n, spd, km, format(n_r, big.mark = ","), pct, pct_all))

    # Simplified WGS84 for web maps (reduces 40 km/h file size ~70%)
    r_web <- r %>%
      st_simplify(dTolerance = SIMPLIFY_TOL, preserveTopology = TRUE) %>%
      filter(!st_is_empty(geometry)) %>%
      st_transform(4326) %>%
      rename(road_name = name) %>%
      select(osm_id, road_name, highway, maxspeed_kmh, length_km)

    list(speed = spd, km = km, n = n_r, pct = pct, pct_all = pct_all, roads_web = r_web)
  })
  names(speed_data) <- as.character(SPEEDS)

  list(name = n, color = city$color, total_km = total_km,
       total_segs = total_segs, tagged_segs = tagged_segs,
       untagged_segs = untagged_segs, tagged_pct = tagged_pct,
       speeds = speed_data)
}

results <- lapply(CITIES, process_city)
names(results) <- sapply(results, `[[`, "name")


# ── 4. GeoPackages ────────────────────────────────────────────
cat("\n[GeoPackages] Saving...\n")
for (r in results) {
  for (spd in SPEEDS) {
    sd   <- r$speeds[[as.character(spd)]]
    gpkg <- sprintf("%s_%dkmh.gpkg", tolower(r$name), spd)
    st_write(sd$roads_web, dsn = gpkg, layer = "roads",
             delete_dsn = TRUE, quiet = TRUE)
    cat(sprintf("  %s\n", gpkg))
  }
}


# ── 5. Summary tables (one per speed) ─────────────────────────
cat("\n", strrep("=", 62), "\n", sep = "")
for (spd in SPEEDS) {
  tbl <- tibble(
    City              = sapply(results, `[[`, "name"),
    All_car_segs      = sapply(results, `[[`, "total_segs"),
    Untagged_segs     = sapply(results, `[[`, "untagged_segs"),
    Tagged_pct        = sapply(results, `[[`, "tagged_pct"),
    Total_tagged_km   = round(sapply(results, `[[`, "total_km"), 0),
    Segments_at_speed = sapply(results, function(r) r$speeds[[as.character(spd)]]$n),
    Length_km         = round(sapply(results, function(r) r$speeds[[as.character(spd)]]$km), 1),
    Pct_of_tagged     = sapply(results, function(r) r$speeds[[as.character(spd)]]$pct)
  )
  cat(sprintf("  %d km/h STREETS – AUSTRALIAN CAPITAL CITIES\n", spd))
  cat(strrep("=", 62), "\n")
  print(knitr::kable(tbl,
    col.names = c("City", "All car segs", "Untagged segs", "Tagged %",
                  "Tagged (km)", sprintf("%d km/h segs", spd),
                  sprintf("%d km/h (km)", spd), "% of tagged"),
    format = "simple", align = c("l","r","r","r","r","r","r","r")))
  cat("\n")
  csv <- sprintf("australia_%dkmh_summary.csv", spd)
  write.csv(tbl, csv, row.names = FALSE)
  cat(sprintf("Saved: %s\n\n", csv))
}


# ── 6. Helper: sf → compact GeoJSON string ────────────────────
sf_to_geojson <- function(sf_obj) {
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp))
  st_write(sf_obj, tmp, delete_dsn = TRUE, quiet = TRUE)
  paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "")
}


# ── 7. Standalone per-city × per-speed offline HTML maps ──────
make_standalone_map <- function(r, spd, leaflet_css, leaflet_js) {
  sd  <- r$speeds[[as.character(spd)]]
  gj  <- sf_to_geojson(sd$roads_web)
  bb  <- st_bbox(sd$roads_web)
  clat <- round((bb["ymin"] + bb["ymax"]) / 2, 5)
  clon <- round((bb["xmin"] + bb["xmax"]) / 2, 5)

  parts <- c(
    '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
    '<meta charset="utf-8">\n',
    '<meta name="viewport" content="width=device-width,initial-scale=1">\n',
    sprintf('<title>%s – %d km/h Streets (OSM)</title>\n', r$name, spd),
    '<style>\n', leaflet_css, '\n',
    '* { margin:0; padding:0; box-sizing:border-box; }\n',
    'body { font-family: sans-serif; }\n',
    '#hdr { padding:10px 16px; background:', r$color,
    '; color:#fff; font-size:14px; font-weight:bold; line-height:1.5; }\n',
    '#map { width:100%; height:calc(100vh - 44px); }\n',
    '.lgd { position:absolute; bottom:30px; right:10px; z-index:1000;',
    ' background:rgba(255,255,255,.92); padding:8px 12px; border-radius:4px;',
    ' font-size:12px; border:1px solid #ccc; }\n',
    '.sw { display:inline-block; width:22px; height:4px; border-radius:2px;',
    ' background:', r$color, '; margin-right:6px; vertical-align:middle; }\n',
    '</style>\n</head>\n<body>\n',
    '<div id="hdr">', r$name,
    sprintf(' – %d km/h streets &nbsp;·&nbsp; ', spd),
    format(sd$n, big.mark = ","), ' segments &nbsp;·&nbsp; ',
    round(sd$km, 1), ' km &nbsp;·&nbsp; source: OpenStreetMap</div>\n',
    '<div id="map"></div>\n',
    sprintf('<div class="lgd"><span class="sw"></span>%d km/h street</div>\n', spd),
    '<script>\n', leaflet_js, '\n</script>\n',
    '<script>\n',
    'var map=L.map("map").setView([', clat, ',', clon, '],12);\n',
    'L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png",{\n',
    '  maxZoom:19,\n',
    '  attribution:"© <a href=\'https://www.openstreetmap.org/copyright\'>OpenStreetMap</a> contributors"\n',
    '}).addTo(map);\n',
    'var lyr=L.geoJSON(', gj, ',{\n',
    '  style:function(){return{color:"', r$color, '",weight:3,opacity:0.85};},\n',
    '  onEachFeature:function(f,l){\n',
    '    var p=f.properties;\n',
    '    l.bindPopup("<b>"+(p.road_name||"Unnamed")+"</b><br>"\n',
    '      +"Type: "+p.highway+"<br>"\n',
    '      +"Length: "+(p.length_km?p.length_km.toFixed(3)+" km":"?"));\n',
    '  }\n',
    '}).addTo(map);\n',
    'if(lyr.getBounds().isValid()) map.fitBounds(lyr.getBounds(),{padding:[20,20]});\n',
    '</script>\n</body>\n</html>'
  )

  out <- sprintf("%s_%dkmh_map.html", tolower(r$name), spd)
  writeLines(paste(parts, collapse = ""), out)
  cat(sprintf("[Map] Saved: %s  (%.1f MB)\n", out,
              file.size(out) / 1024 / 1024))
}

cat("\n[Maps] Building standalone per-city HTML maps...\n")
for (r in results) {
  for (spd in SPEEDS) {
    make_standalone_map(r, spd, LEAFLET_CSS, LEAFLET_JS)
  }
}


# ── 8. GitHub Pages dashboard ─────────────────────────────────
# Architecture: stats embedded as JS object (tiny); GeoJSON saved as separate
# files in docs/data/ and fetched on demand (avoids 50 MB+ single HTML file).

make_github_page <- function(results, leaflet_css, leaflet_js, chartjs) {

  dir.create("docs/data", recursive = TRUE, showWarnings = FALSE)

  # Save GeoJSON data files + collect stats for embedding
  cat("[GitHub Pages] Writing GeoJSON data files...\n")

  stats_js_parts <- lapply(results, function(r) {
    city_speeds <- lapply(SPEEDS, function(spd) {
      sd  <- r$speeds[[as.character(spd)]]
      bb  <- st_bbox(sd$roads_web)
      gj_file <- sprintf("data/%s_%d.geojson", tolower(r$name), spd)
      gj_path  <- file.path("docs", gj_file)

      # Write GeoJSON file for this city × speed
      st_write(sd$roads_web, gj_path, delete_dsn = TRUE, quiet = TRUE)
      sz <- round(file.size(gj_path) / 1024, 0)
      cat(sprintf("  %s  (%d KB)\n", gj_path, sz))

      sprintf(
        '"%d":{km:%s,segs:%d,pct:%s,pct_all:%s,file:"%s",bounds:[[%s,%s],[%s,%s]]}',
        spd, round(sd$km, 1), sd$n, sd$pct, sd$pct_all,
        gj_file,
        bb["ymin"], bb["xmin"], bb["ymax"], bb["xmax"]
      )
    })
    sprintf('"%s":{color:"%s",totalKm:%d,speeds:{%s}}',
            r$name, r$color,
            round(r$total_km_all),
            paste(city_speeds, collapse = ","))
  })

  stats_js  <- paste(stats_js_parts, collapse = ",\n")
  date_str  <- format(Sys.Date(), "%B %Y")
  city_names_js <- paste(sprintf('"%s"', sapply(results, `[[`, "name")),
                         collapse = ",")

  # ── HTML ───────────────────────────────────────────────────
  html <- paste0(
    '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
    '<meta charset="utf-8">\n',
    '<meta name="viewport" content="width=device-width,initial-scale=1">\n',
    '<title>30 & 40 km/h Streets – Australian Capital Cities</title>\n',
    '<style>\n', leaflet_css, '\n</style>\n',

    # Custom CSS
    '<style>\n',
    ':root{--bg:#f0f2f5;--card:#fff;--text:#1a1a2e;--muted:#6c757d;',
    '--border:#e0e4ea;--shadow:0 1px 4px rgba(0,0,0,.08);}\n',
    '*{box-sizing:border-box;margin:0;padding:0;}\n',
    'body{font-family:"Segoe UI",system-ui,sans-serif;background:var(--bg);',
    'color:var(--text);display:flex;flex-direction:column;height:100vh;overflow:hidden;}\n',

    # Header
    'header{background:linear-gradient(135deg,#1a1a2e 0%,#16213e 100%);color:#fff;',
    'padding:14px 28px;flex-shrink:0;border-bottom:3px solid var(--speed-color,#c0392b);}\n',
    'header h1{font-size:18px;font-weight:700;letter-spacing:-.3px;}\n',
    'header .sub{font-size:12px;color:rgba(255,255,255,.65);margin-top:4px;line-height:1.5;}\n',
    'header .sub a{color:rgba(255,255,255,.8);text-decoration:none;}\n',
    'header .sub a:hover{text-decoration:underline;}\n',

    # Controls bar (speed toggle + city tabs)
    '.controls{display:flex;align-items:center;gap:0;background:var(--card);',
    'border-bottom:1px solid var(--border);flex-shrink:0;box-shadow:var(--shadow);flex-wrap:wrap;}\n',
    '.speed-group{display:flex;align-items:center;gap:6px;padding:9px 16px 9px 20px;',
    'border-right:2px solid var(--border);}\n',
    '.speed-label{font-size:11px;font-weight:700;color:var(--muted);text-transform:uppercase;',
    'letter-spacing:.7px;margin-right:4px;white-space:nowrap;}\n',
    '.speed-btn{padding:6px 16px;border:2px solid var(--border);background:var(--card);',
    'border-radius:20px;cursor:pointer;font-size:13px;font-weight:600;',
    'transition:all .18s;white-space:nowrap;}\n',
    '.speed-btn:hover:not(.active){background:#f0f2f5;}\n',
    '.speed-btn.active{color:#fff;}\n',
    '.city-group{display:flex;flex-wrap:wrap;gap:6px;padding:9px 16px;}\n',
    '.city-label{font-size:11px;font-weight:700;color:var(--muted);text-transform:uppercase;',
    'letter-spacing:.7px;margin-right:4px;align-self:center;white-space:nowrap;}\n',
    '.tab{padding:6px 16px;border:1.5px solid var(--border);background:var(--card);',
    'border-radius:20px;cursor:pointer;font-size:13px;font-weight:500;',
    'transition:all .18s;white-space:nowrap;color:var(--text);}\n',
    '.tab:hover:not(.active){background:#f0f2f5;border-color:#bbb;}\n',

    # Main layout
    '.main{display:grid;grid-template-columns:1fr 380px;flex:1;overflow:hidden;min-height:0;}\n',
    '#map{width:100%;height:100%;}\n',
    '.loading-overlay{position:absolute;top:50%;left:50%;transform:translate(-50%,-50%);',
    'background:rgba(255,255,255,.9);padding:16px 24px;border-radius:8px;',
    'font-size:14px;color:var(--muted);display:none;z-index:500;border:1px solid var(--border);}\n',
    '#map-wrap{position:relative;}\n',

    # Side panel
    '.side{display:flex;flex-direction:column;border-left:1px solid var(--border);',
    'background:var(--card);overflow-y:auto;}\n',
    '.stats{padding:20px 20px 16px;border-bottom:1px solid var(--border);}\n',
    '.stats-heading{font-size:11px;font-weight:700;color:var(--muted);',
    'text-transform:uppercase;letter-spacing:.8px;margin-bottom:14px;}\n',
    '.stat-grid{display:grid;grid-template-columns:1fr 1fr;gap:10px;}\n',
    '.stat{background:var(--bg);border-radius:10px;padding:13px 14px;',
    'border:1px solid var(--border);}\n',
    '.stat.full{grid-column:1/-1;}\n',
    '.stat-val{font-size:26px;font-weight:800;line-height:1;letter-spacing:-.5px;}\n',
    '.stat-lbl{font-size:11px;color:var(--muted);margin-top:5px;line-height:1.3;}\n',
    '.chart-box{padding:20px;}\n',
    '.chart-heading{font-size:11px;font-weight:700;color:var(--muted);',
    'text-transform:uppercase;letter-spacing:.8px;margin-bottom:6px;}\n',
    '.chart-desc{font-size:12px;color:var(--muted);line-height:1.5;margin-bottom:12px;',
    'padding-bottom:12px;border-bottom:1px solid var(--border);}\n',
    '.context-text{font-size:12px;color:var(--muted);line-height:1.6;margin-bottom:16px;',
    'padding-bottom:16px;border-bottom:1px solid var(--border);}\n',
    '.network-total{margin-top:12px;font-size:11px;color:var(--muted);text-align:center;}\n',
    '.network-total strong{color:var(--text);}\n',
    '.chart-toggle{display:flex;gap:4px;margin-bottom:12px;}\n',
    '.mode-btn{padding:4px 12px;border:1.5px solid var(--border);background:var(--card);',
    'border-radius:12px;cursor:pointer;font-size:11px;font-weight:600;',
    'color:var(--muted);transition:all .15s;}\n',
    '.mode-btn.active{background:var(--text);color:#fff;border-color:var(--text);}\n',

    # Footer
    'footer{font-size:11px;color:var(--muted);background:var(--card);',
    'border-top:1px solid var(--border);flex-shrink:0;}\n',
    '.footer-inner{display:flex;flex-wrap:wrap;}\n',
    '.footer-block{padding:9px 20px;border-right:1px solid var(--border);',
    'line-height:1.5;flex:1;min-width:200px;}\n',
    '.footer-block:last-child{border-right:none;}\n',
    '.footer-block a{color:#2980b9;text-decoration:none;}\n',
    '.footer-block a:hover{text-decoration:underline;}\n',
    '.badge{display:inline-block;background:#1a1a2e;color:#fff;font-size:10px;',
    'font-weight:600;padding:2px 7px;border-radius:3px;margin-right:4px;',
    'letter-spacing:.3px;vertical-align:middle;}\n',

    # Responsive
    '@media(max-width:768px){\n',
    '  body{height:auto;overflow:auto;}\n',
    '  .main{grid-template-columns:1fr;height:auto;}\n',
    '  #map{height:65vw;min-height:300px;}\n',
    '  .side{border-left:none;border-top:1px solid var(--border);}\n',
    '  .footer-inner{flex-direction:column;}\n',
    '  .footer-block{border-right:none;border-bottom:1px solid var(--border);}\n',
    '}\n',
    '</style>\n</head>\n<body>\n',

    # Header
    '<header>\n',
    '  <h1>30 &amp; 40 km/h Streets in Australian Capital Cities</h1>\n',
    '  <p class="sub">Publicly accessible, car-permitted roads &nbsp;·&nbsp;',
    ' Source: OpenStreetMap &nbsp;·&nbsp; ', date_str,
    ' &nbsp;·&nbsp; <a href="https://github.com/jafshin/aus-30kmh-streets"',
    ' target="_blank">View code on GitHub</a></p>\n',
    '</header>\n',

    # Controls: speed toggle + city tabs
    '<div class="controls">\n',
    '  <div class="speed-group">\n',
    '    <span class="speed-label">Speed</span>\n',
    '    <button class="speed-btn" id="btn-30" onclick="setSpeed(30)">30 km/h</button>\n',
    '    <button class="speed-btn" id="btn-40" onclick="setSpeed(40)">40 km/h</button>\n',
    '  </div>\n',
    '  <div class="city-group">\n',
    '    <span class="city-label">City</span>\n',
    '    <div id="city-tabs"></div>\n',
    '  </div>\n',
    '</div>\n',

    # Main
    '<div class="main">\n',
    '  <div id="map-wrap" style="position:relative;overflow:hidden;">\n',
    '    <div id="map"></div>\n',
    '    <div class="loading-overlay" id="loading">Loading map data…</div>\n',
    '  </div>\n',
    '  <div class="side">\n',
    '    <div class="stats">\n',
    '      <div class="stats-heading" id="stats-heading">Select a speed &amp; city</div>\n',
    '      <p class="context-text" id="context-text">Select a speed limit and city to explore the data.</p>\n',
    '      <div class="stat-grid">\n',
    '        <div class="stat"><div class="stat-val" id="s-km">–</div>',
    '<div class="stat-lbl">km of roads</div></div>\n',
    '        <div class="stat"><div class="stat-val" id="s-segs">–</div>',
    '<div class="stat-lbl">road segments</div></div>\n',
    '        <div class="stat full"><div class="stat-val" id="s-pct-all">–</div>',
    '<div class="stat-lbl">% of public car road network</div></div>\n',
    '      </div>\n',
    '      <p class="network-total" id="network-total"></p>\n',
    '    </div>\n',
    '    <div class="chart-box">\n',
    '      <div class="chart-heading">City Comparison</div>\n',
    '      <p class="chart-desc">The <strong>km</strong> view shows absolute road length.',
    ' Switch to <strong>% of network</strong> for a proportional comparison',
    ' using the full public car road network as the denominator.</p>\n',
    '      <div class="chart-toggle">\n',
    '        <button class="mode-btn active" id="mode-km" onclick="setChartMode(\'km\')">km</button>\n',
    '        <button class="mode-btn" id="mode-pct" onclick="setChartMode(\'pct\')">% of network</button>\n',
    '      </div>\n',
    '      <canvas id="chart"></canvas>\n',
    '    </div>\n',
    '  </div>\n',
    '</div>\n',

    # Footer
    '<footer>\n',
    '  <div class="footer-inner">\n',
    '    <div class="footer-block"><span class="badge">DISCLAIMER</span>',
    ' All data is sourced from <a href="https://www.openstreetmap.org" target="_blank">OpenStreetMap</a>,',
    ' a volunteer-maintained platform. Speed limit tags may be incomplete or out of date.',
    ' Only roads with an explicit maxspeed tag are included.',
    ' The authors accept no responsibility for the accuracy of this information.',
    ' OSM metropolitan extracts provided by <a href="https://interline.io" target="_blank">Interline.io</a>;',
    ' geographic boundaries reflect Interline metro area definitions.</div>\n',
    '    <div class="footer-block"><span class="badge">LICENCE</span>',
    ' Code: <a href="https://github.com/jafshin/aus-30kmh-streets/blob/main/LICENSE"',
    ' target="_blank">MIT</a> &nbsp;·&nbsp;',
    ' Data &amp; findings: <a href="https://creativecommons.org/licenses/by/4.0/"',
    ' target="_blank">CC BY 4.0</a> &nbsp;·&nbsp;',
    ' Map data &copy; <a href="https://www.openstreetmap.org/copyright"',
    ' target="_blank">OpenStreetMap contributors</a> (ODbL)</div>\n',
    '    <div class="footer-block"><span class="badge">CONTACT</span>',
    ' Questions or feedback?<br>',
    ' <a href="mailto:afshin.jafari@rmit.edu.au">afshin.jafari@rmit.edu.au</a></div>\n',
    '  </div>\n',
    '</footer>\n',

    # Libraries
    '<script>\n', leaflet_js, '\n</script>\n',
    '<script>\n', chartjs, '\n</script>\n',

    # App script
    '<script>\n',
    '// ── Embedded stats (tiny) ─────────────────────────────\n',
    'var STATS={', stats_js, '};\n',
    'var CITY_NAMES=[', city_names_js, '];\n\n',

    # Speed colors
    'var SPEED_COLORS={"30":"#c0392b","40":"#d35400"};\n\n',

    # Map init
    'var map=L.map("map");\n',
    'L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png",{\n',
    '  maxZoom:19,\n',
    '  attribution:"© <a href=\'https://www.openstreetmap.org/copyright\'>OpenStreetMap</a> contributors"\n',
    '}).addTo(map);\n',
    'map.setView([-27, 133], 4);\n\n',

    # Chart init (no data yet)
    'var barChart=new Chart(document.getElementById("chart").getContext("2d"),{\n',
    '  type:"bar",\n',
    '  data:{\n',
    '    labels:CITY_NAMES,\n',
    '    datasets:[{data:CITY_NAMES.map(function(){return 0;}),',
    'backgroundColor:CITY_NAMES.map(function(n){return STATS[n].color+"55";}),',
    'borderColor:CITY_NAMES.map(function(n){return STATS[n].color;}),',
    'borderWidth:2,borderRadius:4,borderSkipped:false}]\n',
    '  },\n',
    '  options:{responsive:true,plugins:{legend:{display:false},\n',
    '    tooltip:{callbacks:{label:function(ctx){\n',
    '      if(!activeSpeed) return "";\n',
    '      var s=STATS[CITY_NAMES[ctx.dataIndex]].speeds[activeSpeed];\n',
    '      return chartMode==="km"\n',
    '        ?[s.km+" km","Segments: "+s.segs.toLocaleString()]\n',
    '        :[s.pct_all+"% of network","Segments: "+s.segs.toLocaleString()];\n',
    '    }}}},\n',
    '    scales:{y:{beginAtZero:true,title:{display:true,text:"km"},ticks:{font:{size:11}}},\n',
    '            x:{grid:{display:false},ticks:{font:{size:11}}}}}\n',
    '});\n\n',

    # City tabs
    'var tabsEl=document.getElementById("city-tabs");\n',
    'CITY_NAMES.forEach(function(name,i){\n',
    '  var b=document.createElement("button");\n',
    '  b.className="tab"; b.textContent=name;\n',
    '  b.addEventListener("click",function(){setCity(i);});\n',
    '  tabsEl.appendChild(b);\n',
    '});\n\n',

    # State
    'var activeSpeed=null, activeCityIdx=-1;\n',
    'var activeLayer=null;\n',
    'var geojsonCache={};\n\n',

    # speed context messages
    'var SPEED_CONTEXT={\n',
    '  "30":"30 km/h zones are explicitly designated lower-speed areas, typically found in activity centres,',
    ' shared zones or residential precincts where pedestrian safety is prioritised.',
    ' In Victoria, councils can apply to trial or permanently designate 30 km/h zones under state speed zoning policy.",\n',
    '  "40":"40 km/h zones include local area traffic management precincts, shopping strips and areas with high pedestrian activity.',
    ' They represent a middle tier between the standard urban default (50 km/h) and the slower 30 km/h zones found in pedestrian-priority areas."\n',
    '};\n\n',

    # chart mode toggle
    'var chartMode="km";\n',
    'function setChartMode(mode){\n',
    '  chartMode=mode;\n',
    '  document.getElementById("mode-km").classList.toggle("active",mode==="km");\n',
    '  document.getElementById("mode-pct").classList.toggle("active",mode==="pct");\n',
    '  updateChart();\n',
    '}\n',
    'function updateChart(){\n',
    '  if(!activeSpeed) return;\n',
    '  barChart.data.datasets[0].data=CITY_NAMES.map(function(n){\n',
    '    var s=STATS[n].speeds[activeSpeed];\n',
    '    return chartMode==="km"?s.km:s.pct_all;\n',
    '  });\n',
    '  barChart.data.datasets[0].backgroundColor=CITY_NAMES.map(function(n,i){\n',
    '    return i===activeCityIdx?STATS[n].color+"cc":STATS[n].color+"33";\n',
    '  });\n',
    '  barChart.options.scales.y.title.text=\n',
    '    chartMode==="km"?"km":"% of network";\n',
    '  barChart.update("none");\n',
    '}\n\n',

    # setSpeed
    'function setSpeed(spd){\n',
    '  activeSpeed=String(spd);\n',
    '  var sc=SPEED_COLORS[activeSpeed];\n',
    '  // header accent\n',
    '  document.querySelector("header").style.borderBottomColor=sc;\n',
    '  var ctxEl=document.getElementById("context-text");\n',
    '  if(ctxEl) ctxEl.innerHTML=SPEED_CONTEXT[activeSpeed]||"";\n',
    '  // speed buttons\n',
    '  ["30","40"].forEach(function(s){\n',
    '    var b=document.getElementById("btn-"+s);\n',
    '    var on=(s===activeSpeed);\n',
    '    b.classList.toggle("active",on);\n',
    '    b.style.background=on?sc:"";\n',
    '    b.style.borderColor=on?sc:"";\n',
    '    b.style.color=on?"#fff":"";\n',
    '  });\n',
    '  updateChart();\n',
    '  // reload map if a city is already selected\n',
    '  if(activeCityIdx>=0) loadCityLayer(activeCityIdx);\n',
    '}\n\n',

    # setCity
    'function setCity(idx){\n',
    '  activeCityIdx=idx;\n',
    '  var cityName=CITY_NAMES[idx];\n',
    '  var cityColor=STATS[cityName].color;\n',
    '  // city tabs\n',
    '  document.querySelectorAll(".tab").forEach(function(el,i){\n',
    '    var on=i===idx;\n',
    '    el.classList.toggle("active",on);\n',
    '    el.style.background=on?cityColor:"";\n',
    '    el.style.borderColor=on?cityColor:"";\n',
    '    el.style.color=on?"#fff":"";\n',
    '  });\n',
    '  updateChart();\n',
    '  if(activeSpeed) loadCityLayer(idx);\n',
    '}\n\n',

    # loadCityLayer (fetch GeoJSON on demand)
    'function loadCityLayer(idx){\n',
    '  if(activeSpeed===null) return;\n',
    '  var cityName=CITY_NAMES[idx];\n',
    '  var sd=STATS[cityName].speeds[activeSpeed];\n',
    '  var cityColor=STATS[cityName].color;\n',
    '  // update stats panel immediately\n',
    '  document.getElementById("stats-heading").textContent=\n',
    '    cityName+" – "+activeSpeed+" km/h Streets";\n',
    '  var ntEl=document.getElementById("network-total");\n',
    '  if(ntEl){\n',
    '    var tkm=STATS[cityName].totalKm;\n',
    '    ntEl.innerHTML="out of <strong>"+tkm.toLocaleString()+"</strong> km total public car road network";\n',
    '  }\n',
    '  document.getElementById("s-km").textContent=sd.km.toFixed(1);\n',
    '  document.getElementById("s-km").style.color=cityColor;\n',
    '  document.getElementById("s-segs").textContent=sd.segs.toLocaleString();\n',
    '  document.getElementById("s-segs").style.color=cityColor;\n',
    '  document.getElementById("s-pct-all").textContent=sd.pct_all+"%";\n',
    '  document.getElementById("s-pct-all").style.color=cityColor;\n',
    '  var cacheKey=cityName+"_"+activeSpeed;\n',
    '  if(geojsonCache[cacheKey]){\n',
    '    renderLayer(geojsonCache[cacheKey],cityColor,sd);\n',
    '  } else {\n',
    '    document.getElementById("loading").style.display="block";\n',
    '    fetch(sd.file)\n',
    '      .then(function(r){return r.json();})\n',
    '      .then(function(gj){\n',
    '        geojsonCache[cacheKey]=gj;\n',
    '        document.getElementById("loading").style.display="none";\n',
    '        renderLayer(gj,cityColor,sd);\n',
    '      })\n',
    '      .catch(function(e){\n',
    '        document.getElementById("loading").textContent="Error loading data.";\n',
    '        console.error(e);\n',
    '      });\n',
    '  }\n',
    '}\n\n',

    # renderLayer
    'function renderLayer(gj,color,sd){\n',
    '  if(activeLayer) map.removeLayer(activeLayer);\n',
    '  activeLayer=L.geoJSON(gj,{\n',
    '    style:function(){return{color:color,weight:activeSpeed==="30"?3:2.5,opacity:0.85};},\n',
    '    onEachFeature:function(f,l){\n',
    '      var p=f.properties;\n',
    '      l.bindPopup("<b>"+(p.road_name||"Unnamed road")+"</b><br>"\n',
    '        +"Type: "+p.highway+"<br>"\n',
    '        +"Length: "+(p.length_km?p.length_km.toFixed(3)+" km":"?"));\n',
    '    }\n',
    '  }).addTo(map);\n',
    '  var b=sd.bounds;\n',
    '  map.fitBounds([[b[0][0],b[0][1]],[b[1][0],b[1][1]]],{padding:[30,30]});\n',
    '}\n\n',

    # Init
    'setSpeed(30);\n',
    'setCity(0);\n',
    '</script>\n</body>\n</html>'
  )

  writeLines(paste(html, collapse = ""), "docs/index.html")
  sz <- round(file.size("docs/index.html") / 1024, 0)
  cat(sprintf("[GitHub Pages] Saved: docs/index.html (%d KB)\n", sz))
}

cat("\n[GitHub Pages] Building dashboard...\n")
make_github_page(results, LEAFLET_CSS, LEAFLET_JS, CHART_JS)


# ── 9. Done ───────────────────────────────────────────────────
cat("\n", strrep("=", 62), "\n", sep = "")
cat("All outputs:\n")
for (r in results) {
  for (spd in SPEEDS) {
    cat(sprintf("  %-12s %d km/h  →  %s_%dkmh.gpkg  |  %s_%dkmh_map.html\n",
                r$name, spd, tolower(r$name), spd, tolower(r$name), spd))
  }
}
cat("  australia_30kmh_summary.csv\n")
cat("  australia_40kmh_summary.csv\n")
cat("  docs/index.html  +  docs/data/<city>_<speed>.geojson\n\n")
cat("GitHub Pages:\n")
cat("  git add docs/\n")
cat("  git commit -m 'Add 30 & 40 km/h dashboard'\n")
cat("  git push\n")
cat("  Settings → Pages → Source: main branch /docs folder\n")
cat(strrep("=", 62), "\n")
