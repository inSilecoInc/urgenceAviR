globalVariables(c(
  "Alpha", "Alpha_Code", "An", "Annee", "AnneeDebut", "CODE_ID", "CentroideX", "CentroideY",
  "Code", "Code4_EN", "Code4_FR", "CodeSp", "Count", "Date", "Espece", "Groupe_FR", "Jour",
  "JourDebut", "Lat", "LatDec", "LatStart", "Latin", "Latitude", "Long", "LongDec", "LongStart",
  "Longitude", "Mois", "MoisDebut", "N", "Name_EN", "Name_SC", "NomCol", "NomFR", "Nom_FR",
  "Nom_Scient", "Nombre", "Observateur", "Observateurs", "ObserverName", "Region",
  "STATUT_COS", "SousGroupe", "Species", "Species_ID", "abondance", "alpha_code", "annee",
  "category", "code4_fr", "code_id", "code_sp", "day", "inconnus", "jour", "latitude", "loc_ID",
  "longitude", "methode", "mois", "month", "n_obs", "nb_nicheur", "no_seance", "nomRef", "nom_fr",
  "obs", "read.csv", "read.csv2", "setNames", "species_id", "visuelblancs", "visuelbruns", "year"
))


alpha_to_species_id <- c(
  "UNPH" = "PHAL_UNI",
  "HARD" = "HADU",
  "UNDU" = "DUCK_UNI",
  "UNMB" = "DUCK_UNI",
  "UNLA" = "UNLG",
  "UNWW" = "UNLG",
  "UNGU" = "UNLG",
  "GULL" = "UNLG",
  "GULL SP" = "UNLG",
  "MURA" = "MURR_UNI",
  "UNMU" = "MURR_UNI",
  "ALCI" = "MURR_UNI",
  "MURRE SP" = "MURR_UNI",
  "MURR" = "MURR_UNI",
  "PIGU" = "MURR_UNI",
  "UNLO" = "LOON_UNI",
  "LOON SP" = "LOON_UNI",
  "UNSC" = "SCOT_UNI",
  "SCOTER SP" = "SCOT_UNI",
  "UNME" = "MERG_UNI",
  "UNKI" = "KITT_UNI",
  "UNSH" = "SHOR_UNI",
  "UNCO" = "CORM_UNI",
  "CORM" = "CORM_UNI",
  "CAGO" = "CANG",
  "HEGU" = "HERG",
  "SCAU" = "SCAU_UNI",
  "PEEP" = "PEEP_UNI",
  "SANDPIPER SP" = "PEEP_UNI",
  "TEAL" = "TEAL_UNI",
  "TEAL SP" = "TEAL_UNI",
  "UNTE" = "TERN_UNI",
  "Tern" = "TERN_UNI",
  "TERN" = "TERN_UNI",
  "TERN SP" = "TERN_UNI",
  "UNST" = "TERN_UNI",
  "ACTE" = "TERN_UNI",
  "UNSK" = "JAEG_UNI",
  "UNFJ" = "JAEG_UNI",
  "UNJA" = "JAEG_UNI",
  "UNGE" = "GOLD_UNI",
  "UNSD" = "SEAD_UNI",
  "MEGO" = "SEAD_UNI",
  "MELD" = "SEAD_UNI",
  "SCSC" = "SEAD_UNI",
  "SWAN" = "SWAN_UNI",
  "UNGO" = "GOOS_UNI",
  "COSH" = "CORS",
  "KUGU" = "ICGU",
  "AGWT" = "GWTE",
  "UNEI" = "EIDE_UNI",
  "THGU" = "ICGU_THA"
)

equivalences <- c(
  "canard sp." = "DUCK_UNI",
  "canard de mer sp." = "SEAD_UNI",
  "garrot sp." = "GOLD_UNI",
  "canard barboteur sp." = "GOLD_UNI",
  "goeland sp." = "UNLG_UNI",
  "harle sp." = "MERG_UNI",
  "plongeon sp." = "LOON_UNI",
  "macreuse sp." = "SCOT_UNI",
  "fuligule sp." = "SCAU_UNI",
  "sterne sp." = "TERN_UNI",
  "cormoran sp." = "CORM_UNI"
)


equivalences_garrots <- c(
  "GASP" = "UNGE",
  "PLSP" = "UNLO",
  "GIGO" = "UNGE",
  "HASP" = "UNME",
  "FUSP" = "SCAU",
  "MASP" = "UNSC",
  "CASP" = "UNDU",
  "CNCC" = "UNMB",
  "GOEL" = "UNGU",
  "OIRI" = "GWFG",
  "GARG" = "HEGU",
  "GMAR" = "GBBG",
  "GBCE" = "RBGU",
  "GBLA" = "UNWW",
  "GBOU" = "GLGU",
  "GART" = "ICGU",
  "CAPL" = "UNSD",
  "GOSP" = "UNGU",
  "GOBL" = "UNWW",
  "GAHA" = "MEGO",
  "COSP" = "UNCO",
  "MABJ" = "BLSC",
  "GAIO" = "UNGO",
  "HAGA" = "MEGO",
  "BECO" = "COGO",
  "BESP" = "PEEP",
  "HAHK" = "MELD",
  "BESP" = "PEEP",
  "BUMA" = "NOHA",
  "CABA" = "UNDU",
  "CANC" = "UNMB",
  "CASP" = "UNDU",
  "CHPJ" = "LEYE",
  "EPSP" = "HAWK",
  "FUMP" = "SCAU",
  "FUSP" = "SCAU",
  "GAHA" = "MEGO",
  "GASP" = "UNGE",
  "GOSP" = "UNGU",
  "HASP" = "UNME",
  "MASP" = "UNSC",
  "OIRI" = "GWFG",
  "PLSP" = "UNSD",
  "RAPA" = "RAPT",
  "SASP" = "TEAL",
  "STSP" = "UNTE",
  "BUSE" = "HAWK"
)

# List of external files with paths and required column names
external_files <- list(
  ebird_data = list(
    path = "eBirdQC_juin2024_Extraction2024-11-04/eBirdQC_juin2024_Extraction2024-11-04.gdb",
    check_columns = c("OBSERVATION_DATE", "COMMON_NAME", "OBSERVATION_COUNT")
  ),
  species_codes = list(
    path = "data/CodesEspeces.dbf",
    check_columns = c("Nom_FR", "Nom_Scient", "Name_EN", "Code4_EN", "Code4_FR", "Alpha_Code", "SousGroupe", "STATUT_COS")
  ),
  species_metadata = list(
    path = "data/metadata_species.csv",
    check_columns = c("Name_SC", "Species_ID")
  ),
  canards_de_mer = list(
    path = "/home/steve/Documents/UrgenceAviR_EmeRgencyApp/DonneesSCF/CanardsDeMer_EstuaireStL/ConsultationCanardsDeMer.csv",
    check_columns = c("NomLieu", "LATITUDE", "LONGITUDE", "Annee", "Mois", "Jour", "NombreTotal", "Nom_FR")
  ),
  eider_hiver = list(
    path = "DonneesSCF/Eiders_Hiver/ConsultationEiderHiver.csv",
    check_columns = c("Region", "An", "Mois", "Jour", "Species", "visuelblancs", "visuelbruns", "inconnus", "LatDec", "LongDec")
  ),
  garrot = list(
    path = "DonneesSCF/GarrotIslande_Hiver/ConsultationGarrot.csv",
    check_columns = c("annee", "mois", "jour", "CodeSp", "N", "Observateurs", "Lat", "Long", "loc_ID")
  ),
  macreuse = list(
    path = "DonneesSCF/Macreuses_EstuaireStL/ConsultationMacreuse.csv",
    check_columns = c("Date", "Observateur", "Espece", "Nombre", "Longitude", "Latitude")
  ),
  oies = list(
    path = "DonneesSCF/OieDesNeiges_Printemps/ConsultationOieDesNeigesPrintemps.csv",
    check_columns = c("Date", "Observateur", "Code", "Count", "Longitude", "Latitude")
  ),
  sauvagine_fleuve = list(
    path = "DonneesSCF/Sauvagine_Fleuve/ConsultationSauvagineFleuve.csv",
    check_columns = c("Date", "Latitude", "Longitude", "Nombre", "Observateur")
  ),
  sriv = list(
    path = "DonneesSCF/Sauvagine_Est/ConsultationSRIV.csv",
    check_columns = c("debut", "obslat", "obslong", "total", "obsdro")
  ),
  somec = list(
    path = "DonneesSCF/SOMEC/ConsultationSOMEC.csv",
    check_columns = c("Alpha", "LatStart", "LongStart", "Latin", "Date", "Count", "ObserverName")
  ),
  biomq = list(
    path = "/home/steve/Documents/UrgenceAviR_EmeRgencyApp/DonneesSCF/Biomq/consultationBIOMQ.xlsx", # Replace with the actual path to your BIOMQ file
    check_columns = c(
      "NomCol", "CentroideX", "CentroideY", "NomFR",
      "nb_nicheur", "methode", "nomRef", "AnneeDebut",
      "MoisDebut", "JourDebut"
    )
  )
)

final_cols <- c(
  "latitude",
  "longitude",
  "locality",
  "date",
  "code_id",
  "abondance",
  "obs",
  "inv_type",
  "source",
  "colony",
  "link"
)
