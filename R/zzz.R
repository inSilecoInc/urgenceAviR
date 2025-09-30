globalVariables(c(
  "Alpha", "Alpha_Code", "An", "Annee", "AnneeDebut", "CODE_ID", "CentroideX", "CentroideY",
  "Code", "Code4_EN", "Code4_FR", "CodeSp", "Count", "Date", "Espece", "Groupe_FR", "Jour",
  "JourDebut", "Lat", "LatDec", "LatStart", "Latin", "Latitude", "Long", "LongDec", "LongStart",
  "Longitude", "Mois", "MoisDebut", "N", "Name_EN", "Name_SC", "NomCol", "NomFR", "Nom_FR",
  "Nom_Scient", "Nombre", "Observateur", "Observateurs", "ObserverName", "Region",
  "STATUT_COS", "SousGroupe", "Species", "Species_ID", "abondance", "alpha_code", "annee",
  "category", "code4_fr", "code_id", "code_sp", "day", "inconnus", "jour", "latitude", "loc_ID",
  "longitude", "methode", "mois", "month", "n_obs", "nb_nicheur", "no_seance", "nomRef", "nom_fr",
  "obs", "read.csv", "read.csv2", "setNames", "species_id", "visuelblancs", "visuelbruns", "year",
  "Nom_Ile", "Nom_francais",
  "Nb_compte", "Methode_descriptif","Obs_ID","Nb_total_ind",
  "CruiseID","StartDate","LatStart" ,"LongStart",
  "Alpha","Count","Distance","InTransect",
  "SeaState","FlySwim"
))

### espèces d'intérêt du Québec--------
input <- system.file("exdata/Taxonomy/Taxo_vulnerabilite.xlsx", package = "urgenceAviR") 
type<-readxl::read_excel(input,sheet="Feuil1",guess_max = 1048576) #get nb of columns, possible bug in the number given to shhet in xlsx_col_types compared to read_excel?
tmp <- as.data.frame(readxl::read_excel(input,sheet="Feuil1",col_types=rep("text",length(type))),stringsAsFactors=FALSE)
tmp[]<-lapply(tmp,function(i){type.convert(i,as.is=TRUE)})
spQc<-tmp

### espèces ECCC-----------
input <- system.file("inst/exdata/Taxonomy/ECCC Avian Core 20241025.xlsx", package = "urgenceAviR") 
type<-readxl::read_excel(input,sheet="ECCC Avian Core 20241025",guess_max = 1048576) 
tmp <- as.data.frame(readxl::read_excel(input,sheet="ECCC Avian Core 20241025"))
tmp[]<-lapply(tmp,function(i){type.convert(i,as.is=TRUE)})
taxo<-tmp
rm(tmp)

#ajouter code_id à spQc
spQc$code_id<-taxo$Species_ID[match(spQc$NomFR,taxo$French_Name)]


equivalences <- c(
  "ARL" = "HADU",
  "AROY" = "GOEA",
  "AUTO" = "NOGO",
  "BAS" = "UNDU",
  "BCN" = "CANG",
  "BECA" = "AMWO",
  "BHU" = "CACG",
  "BIHO" = "BCN",
  "BNEI" = "SNBU",
  "BONA" = "BOGU",
  "BPAT" = "RLHA",
  "BUSM" = "NOHA",
  "BUTA" = "AMBI",
  "CBR" = "WODU",
  "CHI" = "GADW",
  "CHJA" = "AMGO",
  "CHPJ" = "GRYE",
  "CNO" = "ABDU",
  "COL" = "MALL",
  "CRA" = "BRAN",
  "CREC" = "AMKE",
  "CSOL" = "SOSA",
  "FLAM" = "NOFL",
  "FMP" = "SCAU_UNI",
  "FOUB" = "NOGA",
  "FSP" = "UNSD",
  "FUC" = "RNDU",
  "FUD" = "CANV",
  "FUM" = "GRSC",
  "GAI" = "BAGO",
  "GAIG" = "GREG",
  "GBLE" = "BLJA",
  "GCOR" = "GRCO",
  "GHA" = "COME",
  "GHER" = "GBHE",
  "GPIC" = "PIWO",
  "GRUC" = "SACR",
  "GSP" = "UNSD",
  "GUIL" = "BLGU",
  "GUNO" = "BLTE",
  "HBIC" = "TRES",
  "HIMA" = "SNOW",
  "HNEI" = "SNOW",
  "HSP" = "UNSD",
  "HUP" = "RBME",
  "HYC" = "UNDU",
  "KAK" = "LTDU",
  "KILD" = "KILL",
  "MAB" = "WWSC",
  "MARR" = "CHSW",
  "MAS" = "SCOT_UNI",
  "MERA" = "AMRO",
  "MTRI" = "BLKI",
  "MXC" = "UNDU",
  "MXN" = "UNDU",
  "NXC" = "UNGO",
  "PBUS" = "BWHA",
  "PCAT" = "RTLO",
  "PEPI" = "RAZO",
  "PFU" = "	LESC",
  "PGA" = "	BUFF",
  "PHUA" = "COLO",
  "PIGE" = "ROPI",
  "PIL" = "NOPI",
  "PSP" = "UNSD",
  "PYGA" = "BALD",
  "QROU" = "RUBL",
  "SAV" = "GWTE",
  "SOU" = "SHOV",
  "STER" = "TERN_UNI",
  "STPI" = "COTE",
  "TOUR" = "MODO",
  "ALSP" = "ALCI_UNI",
  "COSH" = "CORS",
  "GRBH" = "GBHE",
  "LBGU" = "UNGL",
  "MURA" = "ALCI_UNI",
  "SASP" = "PEEP_UNI",
  "SCAU" = "SCAU_UNI",
  "UNCO" = "CORM_UNI",
  "UNEI" = "UNSD",
  "UNGO" = "GOLD_UNI",
  "UNGU" = "UNGL",
  "UNGU" = "UNGL",
  "UNHA" = "ACCI_UNI",
  "UNLA" = "UNGL",
  "UNLI" = "SHOR_UNI",
  "UNLO" = "LOON_UNI",
  "UNME" = "UNME",
  "UNMU" = "MURR_UNI",
  "UNPH" = "PHAL_UNI",
  "UNPL" = "SHOR_UNI",
  "UNSB" = "SHOR_UNI",
  "UNSC" = "SCOT_UNI",
  "UNTE" = "TERN_UNI",
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
  "cormoran sp." = "CORM_UNI",
  "Larus smithsonianus" = "HEGU",
  "Larus sp." = "UNLG",
  "Goélands" = "UNLG",
  "Sternes" = "TERN_UNI",
  "Cormorans" = "CORM_UNI",
  "bernache cravant" ="BRAN",  
  "bernache du canada" = "CANG", 
  "canard noir" = "ABDU",
  "canard pilet" = "NOPI", 
  "oie des neiges" = "SNGE",
  "petit garrot" = "BUFF",
  "ARPL" = "HADU",
  "BECA" = "CANG",
  "BECR" = "BRAN",
  "BESP" = "SHOR_UNI",
  "BEVI" = "PUSA",
  "BRNE" = "SNBU",
  "CACO" = "MALL",
  "CAME" = "AMWI",
  "CANO" = "ABDU",
  "CAPI" = "NOPI",
  "CAPL" = "UNSD",
  "CASP" = "UNDU",
  "CNCC" = "UNDU",
  "COAI" = "DCCO",
  "COSP" = "CORM_UNI",
  "EIDU" = "COEI",
  "EIGO" = "UNSD",
  "EITG" = "KIEI",
  "FAGE" = "GYRF",
  "FOBA" = "NOGA",
  "FUCO" = "RNDU",
  "FUMI" = "GRSC",
  "FUBO" = "NOFU",
  "FUSP" = "SCAU_UNI",
  "GAHA" = "UNSD",
  "GAIO" = "GOLD_UNI",
  "GAIS" = "BAGO",
  "GAOO" = "COGO",
  "GASP" = "GOLD_UNI",
  "GOAC" = "ICGU",
  "GOAR" = "HERG",
  "GOBC" = "RBBG",
  "GOBL" = "UNLG",
  "GOBO" = "BOGU",
  "GOMA" = "GBBG",
  "GOSP" = "UNGL",
  "GRCB" = "CORA",
  "GRCM" = "GRCO",
  "GRES" = "HOGR",
  "GRHA" = "COME",
  "GRJO" = "RNGR",
  "GUMA" = "COMU",
  "GUMI" = "BLGU",
  "GUPP" = "MURR_UNI",
  "HACO" = "HOME",
  "HAGA" = "UNSD",
  "HAHU" = "RBME", 
  "HAKA" = "LTDU",
  "HANE" = "SNOW",
  "HASP" = "UNSD",
  "MAAB" = "WWSC",
  "MABJ" = "BLSC",
  "MABR" = "WWSC",
  "MACN" = "BLSC",
  "MAFB" = "SUSC",
  "MANO" = "BLSC",
  "MASP" = "SCOT_UNI",
  "MPAM" = "BEKI",
  "MOTR" = "BLKI",
  "OINE" = "SNGO",
  "OIRI" = "GWFG",
  "OISP" = "UNKN",
  "PEFU" = "LESC",
  "PEGA" = "BUFF",
  "PEPI" = "RAZO",
  "PHOQ" = "SEAL",
  "PLCA" = "RTLO",
  "PLHU" = "COLO",
  "PLSP" = "LOON_UNI",
  "PYTB" = "BAEA",
  "RERO" = "RFOX",
  "ROCO" = "FBWH",
  "SAHI" = "WITE",
  "BAPE" = "OSPR",
  "BEWI" = "WISN",
  "BIGR" = "BCNH",
  "BUAM" = "AMBI",
  "BUMA" = "NOHA",
  "BUPA" = "RLHA",
  "BUQR" = "RTHA",
  "BUSE" = "ACCI_UNI",
  "CAAM" = "AMWI",
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

# Create an environment to store mutable package variables
.pkg_env <- new.env()

# Base path for datasets folder - must be set before using load_all_datasets()
.pkg_env$datasets_folder <- NULL

# Accessor function for datasets_folder
datasets_folder <- function() {
  .pkg_env$datasets_folder
}

# Initialize external files in the environment
.pkg_env$external_files <- list(
  ebird_data = list(
    path = NULL,  # Will be set by set_datasets_folder()
    check_columns = c("OBSERVATION_DATE", "COMMON_NAME", "OBSERVATION_COUNT")
  ),
  canards_de_mer = list(
    path = NULL,  # Will be set by set_datasets_folder()
    check_columns = c("NomLieu", "LATITUDE", "LONGITUDE", "Annee", "Mois", "Jour", "NombreTotal", "Nom_FR")
  ),
  eider_hiver = list(
    path = NULL,  # Will be set by set_datasets_folder()
    check_columns = c("Region", "An", "Mois", "Jour", "Species", "visuelblancs", "visuelbruns", "inconnus", "LatDec", "LongDec")
  ),
  garrot = list(
    path = NULL,  # Will be set by set_datasets_folder()
    check_columns = c("annee", "mois", "jour", "CodeSp", "N", "Observateurs", "Lat", "Long", "loc_ID")
  ),
  macreuse = list(
    path = NULL,  # Will be set by set_datasets_folder()
    check_columns = c("Date", "Observateur", "Espece", "Nombre", "Longitude", "Latitude")
  ),
  oies = list(
    path = NULL,  # Will be set by set_datasets_folder()
    check_columns = c("Date", "Observateur", "Code", "Count", "Longitude", "Latitude")
  ),
  sauvagine_fleuve = list(
    path = NULL,  # Will be set by set_datasets_folder()
    check_columns = c("Date", "Latitude", "Longitude", "Nombre", "Observateur")
  ),
  sriv = list(
    path = NULL,  # Will be set by set_datasets_folder()
    check_columns = c("debut", "obslat", "obslong", "total", "obsdro")
  ),
  somec = list(
    path = paste0(datasets_folder, "ConsultationSOMEC.csv"),
    check_columns = c("CruiseID","Alpha","StartDate", "LatStart", "LongStart", "Alpha", "Count", "ObserverName")
  ),
  biomq = list(
    path = NULL,  # Will be set by set_datasets_folder()
    check_columns = c(
      "NomCol", "CentroideX", "CentroideY", "NomFR",
      "nb_nicheur", "methode", "nomRef", "AnneeDebut",
      "MoisDebut", "JourDebut"
    )
  ),
  Iles_Nunavik = list(
    path = paste0(datasets_folder, "consultationIles_Nunavik.csv"), 
    check_columns = c(
      "Nom_Ile", "Longitude", "Latitude", "Nom_francais",
      "Nb_compte", "Methode_descriptif", "Annee",
      "Mois", "Jour")
  ),
  Inventaire_aerien_Nunavik = list(
    path = paste0(datasets_folder,"consultationInventaire_aerien_Nunavik.csv"), 
    check_columns = c(
      "Obs_ID", "Nom_français", "Longitude", "Latitude", 
      "Nb_total_ind",  "Date","Observateur")
    ),
  atlantic_colonies = list(
    path = paste0(datasets_folder,"all_atlantic_colonies_obs.csv"), 
    check_columns = c(
      "ColonyId", "Species_code.full", "Long", "Lat", 
      "Colony_size",  "Date","Source","CensusId")
  )
)

# Accessor function for external_files
external_files <- function() {
  .pkg_env$external_files
}

#' Set datasets folder path
#'
#' @param path Path to the datasets folder (must end with "/")
#' @export
set_datasets_folder <- function(path) {
  if (!endsWith(path, "/")) {
    path <- paste0(path, "/")
  }
  .pkg_env$datasets_folder <- path
  
  # Update all file paths with new base path
  file_names <- list(
    ebird_data = "eBird.gdb",
    species_codes = "CodesEspeces.dbf",
    species_metadata = "metadata_species.csv",
    canards_de_mer = "ConsultationCanardsDeMer.csv",
    eider_hiver = "ConsultationEiderHiver.csv",
    garrot = "ConsultationGarrot.csv",
    macreuse = "ConsultationMacreuses.csv",
    oies = "ConsultationOieDesNeigesPrintemps.csv",
    sauvagine_fleuve = "ConsultationSauvagineFleuve.csv",
    sriv = "ConsultationSRIV.csv",
    somec = "ConsultationSOMEC.csv",
    biomq = "consultationBIOMQ.xlsx"
  )
  
  for (i in names(.pkg_env$external_files)) {
    .pkg_env$external_files[[i]]$path <- paste0(path, file_names[[i]])
  }
}

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
