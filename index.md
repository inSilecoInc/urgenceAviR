# urgenceAviR

Un package conçu pour rassembler et intégrer des relevés d’oiseaux. Il
facilite l’exploration interactive des données et l’évaluation des
menaces pesant sur les populations d’oiseaux aquatiques au Québec à la
suite d’un incident de déversement.

## Démarrage

``` r
# Installer devtools si ce n'est pas déjà fait
install.packages("devtools")

# Installer urgenceAviR depuis GitHub
devtools::install_github("insileco/urgenceAviR")
```

## Jeux de données intégrés

Le package intègre les jeux de données suivants :

| Nom du jeu de données             | Fichier source                              | Colonnes requises                                                                                                   |
|-----------------------------------|---------------------------------------------|---------------------------------------------------------------------------------------------------------------------|
| Canards de mer                    | `ConsultationCanardsDeMer.csv`              | `NomLieu`, `LATITUDE`, `LONGITUDE`, `Annee`, `Mois`, `Jour`, `NombreTotal`, `Nom_FR`                                |
| Eider en hiver                    | `ConsultationEiderHiver.csv`                | `Region`, `An`, `Mois`, `Jour`, `Species`, `visuelblancs`, `visuelbruns`, `inconnus`, `LatDec`, `LongDec`           |
| Garrot                            | `ConsultationGarrot.csv`                    | `annee`, `mois`, `jour`, `CodeSp`, `N`, `Observateurs`, `Lat`, `Long`, `loc_ID`                                     |
| Macreuses                         | `ConsultationMacreuses.csv`                 | `Date`, `Observateur`, `Espece`, `Nombre`, `Longitude`, `Latitude`                                                  |
| Oies des neiges au printemps      | `ConsultationOieDesNeigesPrintemps.csv`     | `Date`, `Observateur`, `Code`, `Count`, `Longitude`, `Latitude`                                                     |
| Sauvagine du fleuve Saint-Laurent | `ConsultationSauvagineFleuve.csv`           | `Date`, `Latitude`, `Longitude`, `Nombre`, `Observateur`                                                            |
| SRIV                              | `ConsultationSRIV.csv`                      | `debut`, `obslat`, `obslong`, `total`, `obsdro`                                                                     |
| SOMEC                             | `ConsultationSOMEC.csv`                     | `CruiseID`, `Alpha`, `StartDate`, `LatStart`, `LongStart`, `Count`, `ObserverName`                                  |
| BIOMQ                             | `consultationBIOMQ.xlsx`                    | `NomCol`, `CentroideX`, `CentroideY`, `NomFR`, `nb_nicheur`, `methode`, `nomRef`, `Année`, `MoisDebut`, `JourDebut` |
| Îles du Nunavik                   | `ConsultationIles_Nunavik.csv`              | `Nom_Ile`, `Longitude`, `Latitude`, `Nom_francais`, `Nb_compte`, `Methode_descriptif`, `Annee`, `Mois`, `Jour`      |
| Inventaire aérien du Nunavik      | `ConsultationInventaire_aerien_Nunavik.csv` | `Obs_ID`, `Nom_français`, `Longitude`, `Latitude`, `Nb_total_ind`, `Date`, `Observateur`                            |
| Atlantic colonies                 | `all_atlantic_colonies_obs.csv`             | `ColonyId`, `Species_code.full`, `Long`, `Lat`, `Colony_size`, `Date`, `Source`, `CensusId`                         |

## Lancement de l’application

L’application peut être lancée selon trois modes différents:

### Mode 1: Local avec dossier de données préconfiguré

Utilisez ce mode pour travailler en local avec un dossier de données
spécifique:

``` r
# Définir le dossier contenant les fichiers de données
urgenceAviR::set_datasets_folder("path/to/files")

# Lancer l'application
urgenceAviR::run_app()
```

### Mode 2: Local avec configuration du dossier de données (production)

Utilisez ce mode pour un déploiement local en production où le dossier
de données est configuré dans `inst/golem-config.yml`:

``` r
# Activer le mode production
Sys.setenv(R_CONFIG_ACTIVE = "production")

# Lancer l'application sans téléversement de fichiers
urgenceAviR::run_app(file_upload = FALSE)
```

### Mode 3: Cloud avec téléversement de fichiers

Utilisez ce mode pour un déploiement cloud où les utilisateurs
téléversent leurs propres fichiers:

``` r
# Activer le mode production
Sys.setenv(R_CONFIG_ACTIVE = "production")

# Lancer l'application avec téléversement de fichiers
urgenceAviR::run_app()
```

**Note**: La taille maximale des fichiers téléversés est de 250 MB.
