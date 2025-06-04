
rblick_stations <- c(
  "Gisingen", "Diepoldsau", "Kennelbach", "Rekingen", "Brugg", "Mellingen", 
  "Brienzwiler", "Bruegg-Aegerten", "Basel Rheinhalle", "Riegel", "Schwaibach", 
  "Bad Rotenfels", "Maxau", "Rockenau-SKA", "Worms", "Raunheim", "Mainz", 
  "Grolsheim", "Kaub", "Kalkofen", "Cochem", "Andernach", "Menden", "Koeln", 
  "Duesseldorf", "Hattingen", "Schermbeck", "Lobith", "Andelfingen"
  )

# file prefix = c(station names)
knmi_routing_files <- list(
  "Alpenrhein200" = c(
    "Landquart_Felsenbach",    # col 1
    "Landwasser_Davos",        # col 2
    "no_name",                 # col 3
    "Hinterrhein_Fuerstenau",  # col 4
    "Vorderrhein_Ilanz",       # col 5
    "Rhine_Domat_Ems",         # col 6 Rhein_Domat_Ems
    "Gisingen",                # col 7 Ill_Gisingen
    "Diepoldsau",              # col 8 Rhein_Diepolsdau 
    "Rhine_Neuhausen"          # col 9
  ),
  "Birs200" = c(
    "no_name",                 # col 1
    "no_name",                 # col 2
    "Ergolz_Liestal",          # col 3
    "Birs_Muenchenstein"       # col 4
  ),
  "Bielersee200" = c(
    "no_name",                 # col 1
    "Broye_Payerne",           # col 2
    "Suze_Sonceboz",           # col 3
    "no_name",                 # col 4
    "no_name",                 # col 5
    "no_name",                 # col 6
    "no_name",                 # col 7
    "no_name",                 # col 8
    "Bruegg-Aegerten",         # col 9  Aare_Bruegg_Aegerten
    "Aare_Murgenthal",         # col 10
    "Wigger_Zofingen",         # col 11
    "Brugg"                    # col 12 Aare_Brugg
  ),
  "Emme200" = c(
    "no_name",                 # col 1
    "no_name",                 # col 2
    "Emme_Emmenmatt",          # col 3
    "no_name",                 # col 4
    "Emme_Wiler"               # col 5
  ),
  "Limmat200" = c(
    "Sihl_Zurich",             # col 1
    "Seez_Mels",               # col 2
    "no_name",                 # col 3
    "Linth_Mollis",            # col 4
    "Linth_Weesen",            # col 5
    "Limmatt_Unterhard",       # col 6
    "Limmatt_Baden"            # col 7
  ),
  "Swissrhine200" = c(
    "Rekingen",                # col 1 Rhine_Reckingen
    "Aare_Untersiggenthal",    # col 2
    "Rhine_Rheinfelden",       # col 3 Rhine_Rhinefelden
    "Basel Rheinhalle",        # col 4 Rhine_Basel
    "no_name"
  ),
  "Reuss200" = c(
    "no_name",                 # col 1
    "Reuss_Seedorf",           # col 2
    "Reuss_Muelhau",           # col 3
    "Mellingen",               # col 4 Reuss_Mellingen
    "no_name",                 # col 5
    "Reuss_Luzern"             # col 6
  ),
  "Saane200" = c(
    "Sarine_Broc",             # col 1
    "no_name",                 # col 2
    "Sarine_Fribourg",         # col 3
    "no_name",                 # col 4
    "no_name",                 # col 5
    "Aare_Schoenau",           # col 6
    "Sense_Thoerishaus",       # col 7
    "no_name",                 # col 8
    "Aare_Hagneck",            # col 9
    "Guerbe_Buergistein"      # col 10
  ),
  "ToessGlatt200" = c(
    "Toess_Neftenbach",        # col 1
    "Glatt_Rheinsfelden"       # col 2
  ),
  "Thunersee200" = c(
    "Luetschine_Gsteig",       # col 1
    "no_name",                 # col 2
    "Aare_Thun"                # col 3
  ),
  "Thur200" = c(
    "Thur_Halden",             # col 1
    "no_name",                 # col 2
    "no_name",                 # col 3
    "Andelfingen"              # col 4 Thur_Andelfingen
  ),
  "Wigger200" = c(
    "no_name",                 # col 1
    "Wigger_Zofingen"          # col 2
  )
)

# file name = c(station names)
hind_routing_files <- list(
  "Alpenrhein200_CTRL_RUN_WSL_F_2021_g73.dat" = knmi_routing_files$Alpenrhein200,
  "Birs200_CTRL_RUN_WSL_F_2021_g73.dat" = knmi_routing_files$Birs200,
  "Bielersee200_CTRL_RUN_WSL_F_2021_g73.dat" = knmi_routing_files$Bielersee200,
  "Emme200_CTRL_RUN_WSL_F_2021_g73.dat" = knmi_routing_files$Emme200,
  "Limmat200_CTRL_RUN_WSL_F_2021_g73.dat" = knmi_routing_files$Limmat200,
  "Swissrhine200_CTRL_RUN_WSL_F_2021_g73.dat" = knmi_routing_files$Swissrhine200,
  "Reuss200_CTRL_RUN_WSL_F_2021_g73.dat" = knmi_routing_files$Reuss200,
  "Saane200_CTRL_RUN_WSL_F_2021_g73.dat" = knmi_routing_files$Saane200,
  "ToessGlatt200_CTRL_RUN_WSL_F_2021_g73.dat" = knmi_routing_files$ToessGlatt200,
  "Thunersee200_CTRL_RUN_WSL_F_2021_g73.dat" = knmi_routing_files$Thunersee200,
  "Thur200_CTRL_RUN_WSL_F_2021_g73.dat" = knmi_routing_files$Thur200,
  "Wigger200_CTRL_RUN_WSL_F_2021_g73.dat" = knmi_routing_files$Wigger200
)

# file name = c(station names)
observation_files <- list(
  
  # Alpenrhein200
  "9999.daily.mean.dat" = c("Gisingen"),           # from Ill_Gisingen
  "2288.daily.mean.dat" = c("Rhine_Neuhausen"),
  "2150.daily.mean.dat" = c("Landquart_Felsenbach"),
  "2387.daily.mean.dat" = c("Hinterrhein_Fuerstenau"),
  "2355.daily.mean.dat" = c("Landwasser_Davos"),
  "2033.daily.mean.dat" = c("Vorderrhein_Ilanz"),
  "2602.daily.mean.dat" = c("Rhine_Domat_Ems"),    # from Rhein_Domat_Ems
  "2473.daily.mean.dat" = c("Diepoldsau"),          # from Rhein_Diepolsdau
  
  # Birs200
  "2106.daily.mean.dat" = c("Birs_Muenchenstein"),
  "2202.daily.mean.dat" = c("Ergolz_Liestal"),
  
  # Bielersee200
  "2016.daily.mean.dat" = c("Brugg"),              # from Aare_Brugg
  "2450.daily.mean.dat" = c("Wigger_Zofingen"),
  "2063.daily.mean.dat" = c("Aare_Murgenthal"),
  "2029.daily.mean.dat" = c("Bruegg-Aegerten"),    # from Aare_Bruegg_Aegerten
  "2307.daily.mean.dat" = c("Suze_Sonceboz"),
  "2034.daily.mean.dat" = c("Broye_Payerne"),
  
  # Emme200
  "2155.daily.mean.dat" = c("Emme_Wiler"),
  "2070.daily.mean.dat" = c("Emme_Emmenmatt"),
  
  # Limmat200
  "2243.daily.mean.dat" = c("Limmatt_Baden"),
  "2099.daily.mean.dat" = c("Limmatt_Unterhard"),
  "2104.daily.mean.dat" = c("Linth_Weesen"),
  "2372.daily.mean.dat" = c("Linth_Mollis"),
  "2426.daily.mean.dat" = c("Seez_Mels"),
  "2176.daily.mean.dat" = c("Sihl_Zurich"),
  
  # Swissrhine200
  "2205.daily.mean.dat" = c("Aare_Untersiggenthal"),
  "2091.daily.mean.dat" = c("Rhine_Rheinfelden"),
  "2289.daily.mean.dat" = c("Basel Rheinhalle"),   # from Rhine_Basel
  "2143.daily.mean.dat" = c("Rekingen"),           # from Rhine_Reckingen
  
  # Reuss200
  "2152.daily.mean.dat" = c("Reuss_Luzern"),
  "2110.daily.mean.dat" = c("Reuss_Muelhau"),
  "2018.daily.mean.dat" = c("Mellingen"),          # from Reuss_Mellingen
  "2056.daily.mean.dat" = c("Reuss_Seedorf"),
  
  # Saane200
  "2119.daily.mean.dat" = c("Sarine_Fribourg"),
  "2160.daily.mean.dat" = c("Sarine_Broc"),
  "2085.daily.mean.dat" = c("Aare_Hagneck"),
  "2179.daily.mean.dat" = c("Sense_Thoerishaus"),
  "2135.daily.mean.dat" = c("Aare_Schoenau"),
  # does not exist
  # "2472.daily.mean.dat" = c("Guerbe_Buergistein"),
  
  # ToessGlatt200
  "2132.daily.mean.dat" = c("Toess_Neftenbach"),
  "2415.daily.mean.dat" = c("Glatt_Rheinsfelden"),
  
  # Thunersee200
  "2030.daily.mean.dat" = c("Aare_Thun"),
  "2109.daily.mean.dat" = c("Luetschine_Gsteig"),
  
  # Thur200
  "2044.daily.mean.dat" = c("Andelfingen"),
  "2181.daily.mean.dat" = c("Thur_Halden")
)


