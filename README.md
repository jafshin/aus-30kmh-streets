# 30 & 40 km/h Streets in Australia

This project maps Australian capital-city roads that have an explicit
`maxspeed=30` or `maxspeed=40` tag in OpenStreetMap.

## Run locally

The analysis downloads free daily city extracts from BBBike automatically on
the first run:

```sh
Rscript australia_speedlimit_analysis.R
```

Existing downloads are reused. Force a fresh download with:

```sh
UPDATE_OSM=true Rscript australia_speedlimit_analysis.R
```

Extracts are stored under `data/osm/` and are not committed. BBBike's `.poly`
files define the city coverage; the source URLs are kept alongside each city in
the R configuration.

## Automated refreshes

The GitHub Actions workflow runs each Monday morning Australian Eastern time
(Sunday 17:20 UTC), rebuilds `docs/`, and opens or updates a pull request. It
never pushes generated data directly to `main`, so changes can be reviewed.

For pull-request creation, enable **Settings → Actions → General → Workflow
permissions → Allow GitHub Actions to create and approve pull requests**.

## Data limitations and corrections

The dashboard is a view of OpenStreetMap, not an authoritative speed-limit
register. A signed limit that is missing or incorrect in OSM will also be
missing or incorrect here. The map provides links to create an OSM note for a
location, edit the visible area, or open an individual mapped road.

Only add a speed limit to OSM when it can be verified from signs, official
sources, or a local survey. Council-wide announcements are excellent evidence,
but each affected OSM road still needs the appropriate tag.
