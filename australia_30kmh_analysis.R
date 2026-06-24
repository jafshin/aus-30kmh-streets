# ============================================================
#  Australia Capital Cities – OSM 30 km/h Street Analysis
#  Input  : <city>_australia.osm.pbf  (one per city)
#  Outputs:
#    <city>_30kmh.gpkg          – GeoPackage per city
#    <city>_30kmh_map.html      – Standalone offline map per city
#    australia_30kmh_summary.csv
#    docs/index.html            – GitHub Pages site (fully self-contained)
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


# ── 1. City config ────────────────────────────────────────────
CITIES <- list(
  list(name = "Melbourne", pbf = "melbourne_australia.osm.pbf", epsg = 7855, color = "#c0392b"),
  list(name = "Sydney",    pbf = "sydney_australia.osm.pbf",    epsg = 7856, color = "#2980b9"),
  list(name = "Brisbane",  pbf = "brisbane_australia.osm.pbf",  epsg = 7856, color = "#27ae60"),
  list(name = "Adelaide",  pbf = "adelaide_australia.osm.pbf",  epsg = 7855, color = "#e67e22"),
  list(name = "Perth",     pbf = "perth_australia.osm.pbf",     epsg = 7850, color = "#8e44ad")
)

# Car-accessible public roads only — excludes footways, cycleways, paths, steps
CAR_ROADS <- c(
  "motorway", "motorway_link",
  "trunk", "trunk_link",
  "primary", "primary_link",
  "secondary", "secondary_link",
  "tertiary", "tertiary_link",
  "residential", "living_street",
  "unclassified", "service", "road"
)

EXTRA_TAGS <- c("maxspeed", "access", "motor_vehicle", "motorcar")

PERMITTED <- c("yes", "public", "permissive", "destination")


# ── 2. Download JS/CSS libraries once (for offline embedding) ─
fetch_text <- function(url) {
  tmp <- tempfile()
  download.file(url, tmp, quiet = TRUE, mode = "wb")
  paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

cat("[Libraries] Downloading Leaflet + Chart.js for offline embedding...\n")
LEAFLET_CSS <- fetch_text("https://unpkg.com/leaflet@1.9.4/dist/leaflet.css")
LEAFLET_JS  <- fetch_text("https://unpkg.com/leaflet@1.9.4/dist/leaflet.js")
CHART_JS    <- fetch_text("https://cdn.jsdelivr.net/npm/chart.js@4.4.2/dist/chart.umd.min.js")
cat("[Libraries] Done.\n")


# ── 3. Process each city ──────────────────────────────────────
process_city <- function(city) {
  n <- city$name
  cat(sprintf("\n[%s] Reading OSM PBF...\n", n))
  if (!file.exists(city$pbf)) stop(sprintf("PBF not found: %s", city$pbf))

  raw <- oe_read(city$pbf, layer = "lines", extra_tags = EXTRA_TAGS, quiet = TRUE)

  roads <- raw %>%
    filter(highway %in% CAR_ROADS) %>%
    filter(is.na(access)        | access        %in% PERMITTED) %>%
    filter(is.na(motor_vehicle) | motor_vehicle %in% PERMITTED) %>%
    filter(is.na(motorcar)      | motorcar      %in% PERMITTED) %>%
    filter(!is.na(maxspeed), maxspeed != "") %>%
    filter(!st_is_empty(geometry)) %>%
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

  roads_proj <- roads %>%
    st_transform(city$epsg) %>%
    mutate(length_km = as.numeric(st_length(geometry)) / 1000)

  total_km <- sum(roads_proj$length_km, na.rm = TRUE)
  roads_30 <- filter(roads_proj, maxspeed_kmh == 30)
  km_30    <- sum(roads_30$length_km, na.rm = TRUE)
  n_30     <- nrow(roads_30)
  pct_30   <- round(km_30 / total_km * 100, 2)

  cat(sprintf("[%s] Tagged network: %.0f km | 30 km/h: %.1f km (%s segments, %.2f%%)\n",
              n, total_km, km_30, format(n_30, big.mark = ","), pct_30))

  # WGS84 output layer with clean column names
  roads_30_wgs84 <- roads_30 %>%
    st_transform(4326) %>%
    rename(road_name = name) %>%
    select(osm_id, road_name, highway, maxspeed_kmh, length_km)

  list(
    name        = n,
    color       = city$color,
    total_km    = total_km,
    km_30       = km_30,
    n_30        = n_30,
    pct_30      = pct_30,
    roads_wgs84 = roads_30_wgs84
  )
}

results <- lapply(CITIES, process_city)
names(results) <- sapply(results, `[[`, "name")


# ── 4. GeoPackages ────────────────────────────────────────────
cat("\n[GeoPackages] Saving...\n")
for (r in results) {
  gpkg <- paste0(tolower(r$name), "_30kmh.gpkg")
  st_write(r$roads_wgs84, dsn = gpkg, layer = "roads_30kmh",
           delete_dsn = TRUE, quiet = TRUE)
  cat(sprintf("  %s\n", gpkg))
}


# ── 5. Summary table ──────────────────────────────────────────
summary_tbl <- tibble(
  City            = sapply(results, `[[`, "name"),
  Total_tagged_km = round(sapply(results, `[[`, "total_km"), 0),
  Segments_30kmh  = sapply(results, `[[`, "n_30"),
  Length_30kmh_km = round(sapply(results, `[[`, "km_30"), 1),
  Pct_of_tagged   = sapply(results, `[[`, "pct_30")
)

cat("\n", strrep("=", 62), "\n", sep = "")
cat("  30 km/h STREETS – AUSTRALIAN CAPITAL CITIES (OSM)\n")
cat(strrep("=", 62), "\n")
print(knitr::kable(
  summary_tbl,
  col.names = c("City", "Total tagged (km)", "30 km/h segs",
                "30 km/h (km)", "% of tagged network"),
  format = "simple", align = c("l", "r", "r", "r", "r")
))
write.csv(summary_tbl, "australia_30kmh_summary.csv", row.names = FALSE)
cat("Saved: australia_30kmh_summary.csv\n")


# ── 6. Helper: sf -> compact GeoJSON string ───────────────────
sf_to_geojson <- function(sf_obj) {
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp))
  st_write(sf_obj, tmp, delete_dsn = TRUE, quiet = TRUE)
  paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "")
}


# ── 7. Per-city standalone offline HTML maps ──────────────────
make_standalone_map <- function(r, leaflet_css, leaflet_js) {
  gj  <- sf_to_geojson(r$roads_wgs84)
  bb  <- st_bbox(r$roads_wgs84)
  clat <- round((bb["ymin"] + bb["ymax"]) / 2, 5)
  clon <- round((bb["xmin"] + bb["xmax"]) / 2, 5)

  parts <- c(
    '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
    '<meta charset="utf-8">\n',
    '<meta name="viewport" content="width=device-width,initial-scale=1">\n',
    '<title>', r$name, ' – 30 km/h Streets (OSM)</title>\n',
    '<style>\n', leaflet_css, '\n',
    '* { margin:0; padding:0; box-sizing:border-box; }\n',
    'body { font-family: sans-serif; }\n',
    '#hdr { padding:10px 16px; background:', r$color, '; color:#fff;',
    ' font-size:14px; font-weight:bold; line-height:1.5; }\n',
    '#map { width:100%; height:calc(100vh - 44px); }\n',
    '.lgd { position:absolute; bottom:30px; right:10px; z-index:1000;',
    ' background:rgba(255,255,255,.92); padding:8px 12px; border-radius:4px;',
    ' font-size:12px; border:1px solid #ccc; }\n',
    '.sw { display:inline-block; width:22px; height:4px; border-radius:2px;',
    ' background:', r$color, '; margin-right:6px; vertical-align:middle; }\n',
    '</style>\n</head>\n<body>\n',
    '<div id="hdr">', r$name, ' – 30 km/h streets &nbsp;·&nbsp; ',
    format(r$n_30, big.mark = ","), ' segments &nbsp;·&nbsp; ',
    round(r$km_30, 1), ' km &nbsp;·&nbsp; source: OpenStreetMap</div>\n',
    '<div id="map"></div>\n',
    '<div class="lgd"><span class="sw"></span>30 km/h street</div>\n',
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

  out <- paste0(tolower(r$name), "_30kmh_map.html")
  writeLines(paste(parts, collapse = ""), out)
  cat(sprintf("[Map] Saved: %s\n", out))
}

cat("\n[Maps] Building per-city standalone offline maps...\n")
invisible(lapply(results, make_standalone_map,
                 leaflet_css = LEAFLET_CSS, leaflet_js = LEAFLET_JS))


# ── 8. GitHub Pages site (docs/index.html) ───────────────────
make_github_page <- function(results, leaflet_css, leaflet_js, chartjs) {

  cat("[GitHub Pages] Building per-city GeoJSON (this may take a moment)...\n")

  # Build JS data array embedding each city's GeoJSON and stats
  cities_js <- paste(
    lapply(results, function(r) {
      bb <- st_bbox(r$roads_wgs84)
      gj <- sf_to_geojson(r$roads_wgs84)
      cat(sprintf("  [GitHub Pages] %s GeoJSON embedded.\n", r$name))
      paste0(
        '{\n',
        '  name:"',  r$name,              '",\n',
        '  color:"', r$color,             '",\n',
        '  km:',     round(r$km_30, 1),   ',\n',
        '  segs:',   r$n_30,              ',\n',
        '  pct:',    r$pct_30,            ',\n',
        '  bounds:[[', bb["ymin"], ',', bb["xmin"], '],',
                   '[', bb["ymax"], ',', bb["xmax"], ']],\n',
        '  geojson:', gj,                 '\n',
        '}'
      )
    }),
    collapse = ",\n"
  )

  date_str <- format(Sys.Date(), "%B %Y")

  # ── HTML template ─────────────────────────────────────────────
  html <- paste0(
    '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
    '<meta charset="utf-8">\n',
    '<meta name="viewport" content="width=device-width,initial-scale=1">\n',
    '<title>30 km/h Streets – Australian Capital Cities</title>\n',
    # Leaflet CSS
    '<style>\n', leaflet_css, '\n</style>\n',
    # Custom CSS
    '<style>\n',
    ':root{--bg:#f5f6f7;--card:#fff;--text:#1a1a2e;--muted:#6c757d;--border:#e0e0e0;}\n',
    '*{box-sizing:border-box;margin:0;padding:0;}\n',
    'body{font-family:"Segoe UI",system-ui,sans-serif;background:var(--bg);',
    'color:var(--text);display:flex;flex-direction:column;height:100vh;overflow:hidden;}\n',
    'header{background:#1a1a2e;color:#fff;padding:14px 24px;flex-shrink:0;}\n',
    'header h1{font-size:18px;font-weight:700;}\n',
    'header p{font-size:12px;color:rgba(255,255,255,.6);margin-top:3px;}\n',
    '.tabs{display:flex;gap:6px;padding:10px 24px;background:var(--card);',
    'border-bottom:1px solid var(--border);flex-wrap:wrap;flex-shrink:0;}\n',
    '.tab{padding:7px 18px;border:1px solid var(--border);background:var(--card);',
    'border-radius:20px;cursor:pointer;font-size:13px;font-weight:500;',
    'transition:all .15s;white-space:nowrap;}\n',
    '.tab:hover:not(.active){background:#efefef;}\n',
    '.main{display:grid;grid-template-columns:1fr 300px;flex:1;overflow:hidden;min-height:0;}\n',
    '#map{width:100%;height:100%;}\n',
    '.side{display:flex;flex-direction:column;border-left:1px solid var(--border);',
    'background:var(--card);overflow-y:auto;}\n',
    '.stats{padding:20px;border-bottom:1px solid var(--border);}\n',
    '.stats h2{font-size:12px;font-weight:600;color:var(--muted);text-transform:uppercase;',
    'letter-spacing:.7px;margin-bottom:14px;}\n',
    '.stat-grid{display:grid;grid-template-columns:1fr 1fr;gap:10px;}\n',
    '.stat{background:var(--bg);border-radius:8px;padding:12px;}\n',
    '.stat.full{grid-column:1/-1;}\n',
    '.stat-val{font-size:28px;font-weight:700;line-height:1;}\n',
    '.stat-lbl{font-size:11px;color:var(--muted);margin-top:4px;}\n',
    '.chart-box{padding:20px;flex:1;}\n',
    '.chart-box h2{font-size:12px;font-weight:600;color:var(--muted);text-transform:uppercase;',
    'letter-spacing:.7px;margin-bottom:14px;}\n',
    'footer{font-size:11px;color:var(--muted);padding:7px 24px;background:var(--card);',
    'border-top:1px solid var(--border);flex-shrink:0;}\n',
    '@media(max-width:768px){\n',
    '  body{height:auto;overflow:auto;}\n',
    '  .main{grid-template-columns:1fr;height:auto;}\n',
    '  #map{height:65vw;min-height:280px;}\n',
    '  .side{border-left:none;border-top:1px solid var(--border);}\n',
    '}\n',
    '</style>\n</head>\n<body>\n',

    # Header
    '<header>\n',
    '  <h1>30 km/h Streets in Australian Capital Cities</h1>\n',
    '  <p>Publicly accessible, car-permitted roads with a posted 30 km/h speed limit',
    ' &nbsp;·&nbsp; Source: OpenStreetMap &nbsp;·&nbsp; ', date_str, '</p>\n',
    '</header>\n',

    # City tabs (built by JS)
    '<div class="tabs" id="tabs"></div>\n',

    # Main layout
    '<div class="main">\n',
    '  <div id="map"></div>\n',
    '  <div class="side">\n',
    '    <div class="stats">\n',
    '      <h2 id="city-label">Select a city</h2>\n',
    '      <div class="stat-grid">\n',
    '        <div class="stat">\n',
    '          <div class="stat-val" id="s-km">–</div>\n',
    '          <div class="stat-lbl">km of 30 km/h streets</div>\n',
    '        </div>\n',
    '        <div class="stat">\n',
    '          <div class="stat-val" id="s-segs">–</div>\n',
    '          <div class="stat-lbl">road segments</div>\n',
    '        </div>\n',
    '        <div class="stat full">\n',
    '          <div class="stat-val" id="s-pct">–</div>\n',
    '          <div class="stat-lbl">% of tagged road network</div>\n',
    '        </div>\n',
    '      </div>\n',
    '    </div>\n',
    '    <div class="chart-box">\n',
    '      <h2>City Comparison</h2>\n',
    '      <canvas id="chart"></canvas>\n',
    '    </div>\n',
    '  </div>\n',
    '</div>\n',

    # Footer
    '<footer>Data: OpenStreetMap contributors &nbsp;·&nbsp;',
    ' Only roads with an explicit maxspeed=30 tag are included &nbsp;·&nbsp;',
    ' Percentages are of roads with a recorded speed tag, not the full network</footer>\n',

    # Libraries
    '<script>\n', leaflet_js, '\n</script>\n',
    '<script>\n', chartjs, '\n</script>\n',

    # App script
    '<script>\n',
    'var CITIES=[\n', cities_js, '\n];\n\n',

    # Map
    'var map=L.map("map");\n',
    'L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png",{\n',
    '  maxZoom:19,\n',
    '  attribution:"© <a href=\'https://www.openstreetmap.org/copyright\'>OpenStreetMap</a> contributors"\n',
    '}).addTo(map);\n',
    'map.setView([-33.87, 151.21], 5);\n\n',

    # Chart
    'var barChart=new Chart(document.getElementById("chart").getContext("2d"),{\n',
    '  type:"bar",\n',
    '  data:{\n',
    '    labels:CITIES.map(function(c){return c.name;}),\n',
    '    datasets:[{\n',
    '      data:CITIES.map(function(c){return c.km;}),\n',
    '      backgroundColor:CITIES.map(function(c){return c.color+"55";}),\n',
    '      borderColor:CITIES.map(function(c){return c.color;}),\n',
    '      borderWidth:2,borderRadius:4,borderSkipped:false\n',
    '    }]\n',
    '  },\n',
    '  options:{\n',
    '    responsive:true,\n',
    '    plugins:{\n',
    '      legend:{display:false},\n',
    '      tooltip:{callbacks:{label:function(ctx){\n',
    '        var c=CITIES[ctx.dataIndex];\n',
    '        return [c.km+" km","Segments: "+c.segs.toLocaleString(),c.pct+"% of tagged network"];\n',
    '      }}}\n',
    '    },\n',
    '    scales:{\n',
    '      y:{beginAtZero:true,title:{display:true,text:"km of 30 km/h streets"},ticks:{font:{size:11}}},\n',
    '      x:{grid:{display:false},ticks:{font:{size:11}}}\n',
    '    }\n',
    '  }\n',
    '});\n\n',

    # Tabs
    'var tabsEl=document.getElementById("tabs");\n',
    'CITIES.forEach(function(c,i){\n',
    '  var b=document.createElement("button");\n',
    '  b.className="tab";\n',
    '  b.textContent=c.name;\n',
    '  b.addEventListener("click",function(){selectCity(i);});\n',
    '  tabsEl.appendChild(b);\n',
    '});\n\n',

    # City select function
    'var activeLayer=null, activeIdx=-1;\n',
    'function selectCity(idx){\n',
    '  if(idx===activeIdx) return;\n',
    '  activeIdx=idx;\n',
    '  var c=CITIES[idx];\n',
    '  // tabs\n',
    '  document.querySelectorAll(".tab").forEach(function(el,i){\n',
    '    var on=i===idx;\n',
    '    el.classList.toggle("active",on);\n',
    '    el.style.background  =on?c.color:"";\n',
    '    el.style.borderColor =on?c.color:"";\n',
    '    el.style.color       =on?"#fff":"";\n',
    '  });\n',
    '  // map layer\n',
    '  if(activeLayer) map.removeLayer(activeLayer);\n',
    '  activeLayer=L.geoJSON(c.geojson,{\n',
    '    style:function(){return{color:c.color,weight:3,opacity:0.85};},\n',
    '    onEachFeature:function(f,l){\n',
    '      var p=f.properties;\n',
    '      l.bindPopup("<b>"+(p.road_name||"Unnamed road")+"</b><br>"\n',
    '        +"Type: "+p.highway+"<br>"\n',
    '        +"Length: "+(p.length_km?p.length_km.toFixed(3)+" km":"?"));\n',
    '    }\n',
    '  }).addTo(map);\n',
    '  if(activeLayer.getLayers().length>0&&activeLayer.getBounds().isValid())\n',
    '    map.fitBounds(activeLayer.getBounds(),{padding:[30,30]});\n',
    '  // stats\n',
    '  document.getElementById("city-label").textContent=c.name;\n',
    '  document.getElementById("s-km").textContent=c.km.toFixed(1);\n',
    '  document.getElementById("s-km").style.color=c.color;\n',
    '  document.getElementById("s-segs").textContent=c.segs.toLocaleString();\n',
    '  document.getElementById("s-segs").style.color=c.color;\n',
    '  document.getElementById("s-pct").textContent=c.pct+"%";\n',
    '  document.getElementById("s-pct").style.color=c.color;\n',
    '  // chart highlight\n',
    '  barChart.data.datasets[0].backgroundColor=CITIES.map(function(d,i){\n',
    '    return i===idx?d.color+"cc":d.color+"33";\n',
    '  });\n',
    '  barChart.update("none");\n',
    '}\n\n',
    'selectCity(0);\n',
    '</script>\n</body>\n</html>'
  )

  dir.create("docs", showWarnings = FALSE)
  writeLines(paste(html, collapse = ""), "docs/index.html")
  size_mb <- file.size("docs/index.html") / 1024 / 1024
  cat(sprintf("[GitHub Pages] Saved: docs/index.html (%.1f MB)\n", size_mb))
}

cat("\n[GitHub Pages] Building docs/index.html...\n")
make_github_page(results, LEAFLET_CSS, LEAFLET_JS, CHART_JS)


# ── 9. Done ───────────────────────────────────────────────────
cat("\n", strrep("=", 62), "\n", sep = "")
cat("All outputs:\n")
for (r in results) {
  cat(sprintf("  %-12s  %s_30kmh.gpkg  |  %s_30kmh_map.html\n",
              r$name, tolower(r$name), tolower(r$name)))
}
cat("  australia_30kmh_summary.csv\n")
cat("  docs/index.html\n\n")
cat("To publish on GitHub Pages:\n")
cat("  git add docs/index.html\n")
cat("  git commit -m 'Add 30 km/h web map'\n")
cat("  git push\n")
cat("  Then go to Settings > Pages > Source: main branch /docs folder\n")
cat(strrep("=", 62), "\n")
