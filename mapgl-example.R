library(tibble)
library(sf)
library(mapgl)

state_capitals <- tibble(
  capital = c(
    "Montgomery", "Juneau", "Phoenix", "Little Rock", "Sacramento",
    "Denver", "Hartford", "Dover", "Tallahassee", "Atlanta",
    "Honolulu", "Boise", "Springfield", "Indianapolis", "Des Moines",
    "Topeka", "Frankfort", "Baton Rouge", "Augusta", "Annapolis",
    "Boston", "Lansing", "Saint Paul", "Jackson", "Jefferson City",
    "Helena", "Lincoln", "Carson City", "Concord", "Trenton",
    "Santa Fe", "Albany", "Raleigh", "Bismarck", "Columbus",
    "Oklahoma City", "Salem", "Harrisburg", "Providence", "Columbia",
    "Pierre", "Nashville", "Austin", "Salt Lake City", "Montpelier",
    "Richmond", "Olympia", "Charleston", "Madison", "Cheyenne"
  ),
  state = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California",
    "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
    "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
    "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
    "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
    "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
    "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
  ),
  longitude = c(
    -86.2999, -134.4197, -112.0740, -92.2896, -121.4944,
    -104.9847, -72.6851, -75.5244, -84.2807, -84.3880,
    -157.8583, -116.2023, -89.6501, -86.1581, -93.6091,
    -95.6890, -84.8733, -91.1871, -69.7653, -76.4922,
    -71.0639, -84.5467, -93.1016, -90.1848, -92.1735,
    -112.0270, -96.6852, -119.7674, -71.5372, -74.7597,
    -105.9644, -73.7562, -78.6382, -100.7837, -82.9988,
    -97.5164, -123.0351, -76.8844, -71.4128, -81.0348,
    -100.3463, -86.7844, -97.7431, -111.8910, -72.5778,
    -77.4360, -122.9007, -81.6326, -89.3838, -104.8202
  ),
  latitude = c(
    32.3617, 58.3005, 33.4484, 34.7465, 38.5556,
    39.7392, 41.7640, 39.1582, 30.4383, 33.7490,
    21.3069, 43.6150, 39.7983, 39.7684, 41.5911,
    39.0473, 38.1867, 30.4515, 44.3106, 38.9784,
    42.3601, 42.7325, 44.9537, 32.2988, 38.5767,
    46.5958, 40.8136, 39.1638, 43.2067, 40.2170,
    35.6672, 42.6526, 35.7721, 46.8083, 39.9612,
    35.4676, 44.9429, 40.2732, 41.8240, 34.0007,
    44.3683, 36.1627, 30.2672, 40.7608, 44.2601,
    37.5407, 47.0379, 38.3498, 43.0731, 41.1400
  )
)

capitals_sf <- st_as_sf(
  state_capitals,
  coords = c("longitude", "latitude"),
  crs = 4326
)

mapboxgl(
  style = mapbox_style("streets"),
  center = c(-96, 38),
  zoom = 3
) |>
  add_circle_layer(
    id = "capitals",
    source = capitals_sf,
    circle_color = "#1a6fb5",
    circle_radius = 7,
    circle_stroke_color = "#ffffff",
    circle_stroke_width = 2,
    tooltip = "capital"
  )
