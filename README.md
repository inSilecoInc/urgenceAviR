# urgenceAviR

<!-- badges: start -->
  [![R-CMD-check](https://github.com/inSilecoInc/urgenceAviR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/inSilecoInc/urgenceAviR/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

Un package conçu pour rassembler et intégrer des relevés d'oiseaux. Il facilite l'exploration interactive des données et l'évaluation des menaces pesant sur les populations d'oiseaux aquatiques au Québec à la suite d'un incident de déversement.

## Démarrage

```r
# Installer devtools si ce n'est pas déjà fait
install.packages("devtools")

# Installer urgenceAviR depuis GitHub
devtools::install_github("insileco/urgenceAviR")
```

## Jeux de données intégrés

Le package intègre les jeux de données suivants :

| Nom du jeu de données | Fichier source |
|----------------------|----------------|
| eBird | `eBird.gdb` |
| Canards de mer | `ConsultationCanardsDeMer.csv` |
| Eider en hiver | `ConsultationEiderHiver.csv` |
| Garrot | `ConsultationGarrot.csv` |
| Macreuses | `ConsultationMacreuses.csv` |
| Oies des neiges au printemps | `ConsultationOieDesNeigesPrintemps.csv` |
| Sauvagine du fleuve Saint-Laurent | `ConsultationSauvagineFleuve.csv` |
| SRIV | `ConsultationSRIV.csv` |
| SOMEC | `ConsultationSOMEC.csv` |
| BIOMQ | `consultationBIOMQ.xlsx` |
| Îles du Nunavik | `consultationIles_Nunavik.csv` |
| Inventaire aérien du Nunavik | `consultationInventaire_aerien_Nunavik.csv` |

## Lancement de l'application

Pour lancer l'application Shiny permettant l'exploration interactive des données:

```r
# Lancer l'application
urgenceAviR::run_app()
```
