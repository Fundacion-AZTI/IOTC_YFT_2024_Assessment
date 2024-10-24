# IOTC Yellowfin Stock Assessment 2024

> :loudspeaker: **Goal**
>
> The main goal of this repository is to replicate the 2024 Indian Ocean yellowfin assessment configurations in R. The assessment is implemented in [Stock Synthesis 3](https://vlab.noaa.gov/web/stock-synthesis).

In order to execute this repository, you will need to have some knowledge on:

- Github (highly desirable but not mandatory)
- Stock Synthesis 3 (SS3, mandatory)
- R programming (mandatory)
- `r4ss` R package (mandatory)

The steps to run this repository are described below. The functioning of this repository was tested on Windows. Some adaptations might be required for MacOS or Linux.

## 1. Clone this repository

If you are a Github user, you can clone this repository by following [these instructions](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository). Make sure to use the `reproducible` branch.

If you are not familiar with Github, you can download this repository as ZIP file by following [these instructions](https://docs.github.com/en/get-started/start-your-journey/downloading-files-from-github#downloading-a-repositorys-files). Save the downloaded folder (called **working folder** hereafter) somewhere locally. 

## 2. Install R packages

Install all R packages listed in `.Rprofile`. You can install the missing CRAN packages automatically by running this line:

```{r}
install.load::install_load("dplyr", "purrr", "readr", "sf", "stringr", "reshape", "readxl", "scales", "tidyr")
```

The `r4ss` package (see <https://github.com/r4ss/r4ss>) is probably the most important one. This repository was built using `r4ss` version 1.50.0, which is currently (October 2024) the latest release. Install it running this line: 

```{r}
remotes::install_github("r4ss/r4ss")
```

The `FishFreqTree` package (see <https://github.com/HaikunXu/FishFreqTree>) can be installed by running:

```{r}
devtools::install_github('HaikunXu/RegressionTree', ref='main')
```

## 3. Open the R project

There is an R project in the working folder called `IOTC_YFT_2024_Assessment.Rproj`. We suggest to open it from RStudio. This will automatically set your R working directory to the working folder and load the R packages listed in `.Rprofile`. 

If you want to use a different visual interface that does not allow to work with R projects, you will need to set the working directory manually to the working folder. Also, you will need to load all the R packages in `.Rprofile`.


## 4. Create the working folder structure

Some folders will need to be created in the working folder. This is automatically done by running `create_subfolders.R`. 

The folder structure in the working folder follows the three spatial configurations evaluated in this assessment: 

- `4A_io`: four areas, main configuration
- `2A_io`: two areas 
- `1A_io`: one area 

The `2A_io` and `1A_io` configurations implement two subconfigurations: aggregated (`agg`) and areas-as-fleets (`aaf`). See assessment report.

## 6. Download the raw data

Download the raw data from the IOTC website. 

* The catch and size data can be found here: <https://iotc.org/documents/WPTT/26AS/Data/01>. You will find *five Excel files*, copy all of them and paste them in `data/raw` in the *working folder*.
* The CPUE data can be found here: <https://iotc.org/documents/standardised-cpue-yft-and-bet>. Copy only the `YFT` **folder** and paste it in `data/raw` in the *working folder*.

## 7. Create SS3 inputs

In this repository, the folder `code/ss3_data_inputs` contains the R scripts to generate the data inputs for SS3. They are organized in three folders and should be created in this order:

- `CE`: R scripts to generate catch input.
- `CPUE`: R scripts to generate CPUE input.
- `LF`: R scripts to generate size frequency input.

The first number of the scripts names indicate the order in which should be run. 

If you are only interested in `4A_io` models, you can ignore the `2A_io` and `1A_io` scripts.

## 8. Age-length and tagging data

The raw age-length and tagging data are not publicly available, so it is not possible to generate them. 

However, you can find the data in SS3 format for each model configuration in the `data/ss3_inputs` folder. 

## 9. Download the SS3 executable

Download `ss3_win.exe` (v3.30.22.1) here: <https://github.com/nmfs-ost/ss3-source-code/releases>. You will need to download a different file if you are using MacOS or Linux. Then, save it in the `code` folder.

## 10. Get the SS3 base files

These are SS3 input files. We start to do the stepwise implementation from this set of input files. For the 4A models, the base files come from the 2021 assessment. You can find the base files in this repository: `models/base`.

## 11. Start the stepwise implementation

This is probably the most important step. Here, you will start creating the SS3 inputs with different configurations. You can find the code to do this in `code/ss3_configurations` of this repository. Read the comments in those scripts carefully.