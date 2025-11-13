devtools::load_all()
path_gdb <- "/home/steve/Documents/inSileco/UrgenceAviR/eBird_QC.gdb.zip"

# Attention: la projection de la GDB est en 32198, s'assurer que le polygon que l'on utilise pour filtrer est bien dans la même projection
lac <- sf::st_read("lac-st-pierre.geojson") |>
    sf::st_transform(sf::st_crs(32198))

mapview::mapview(lac)

data_test <- get_ebird(path = path_gdb, extent = lac, year = 2020) 

names(data_test)
