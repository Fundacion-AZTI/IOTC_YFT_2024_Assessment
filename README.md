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

If you are not familiar with Github, you can download this repository as ZIP file by following [these instructions](https://docs.github.com/en/get-started/start-your-journey/downloading-files-from-github#downloading-a-repositorys-files). Save the downloaded folder somewhere locally. 

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

There is an R project in the repository folder called `IOTC_YFT_2024_Assessment.Rproj`. We suggest to open it from RStudio. This will automatically set your working directory to the repository folder and load the R packages listed in `.Rprofile`. 

If you want to use a different visual interface that does not allow to work with R projects, you will need set the working directory mannually to the repository folder. Also, you will need to load all the R packages in `.Rprofile`.

## 4. Specify the working folder

The *working folder* is a folder where the raw and processed data will be saved, and the SS3 configuration files will be created and run. Note that this is different than the *repository folder*.

Create the *working folder* somewhere on your local computer, and then specify the path in `sharepoint_path.R`. For example:

```{r}
shrpoint_path = 'C:/Use/2024_YFT_IOTC'
```

## 5. Create the working folder structure

Some folders will need to be created in the *working folder*. This is automatically done by running `create_subfolders.R`. After running it, the *working folder* should look like:

``` bash
├───data
│   ├───processed
│   ├───raw
│   └───ss3_inputs
│       ├───1A_io
│       │   ├───aaf
│       │   └───agg
│       ├───2A_io
│       │   ├───aaf
│       │   └───agg
│       └───4A_io
├───models
│   ├───base
│   │   ├───1A_io
│   │   ├───2A_io
│   │   └───4A_io
│   ├───configurations
│   │   ├───1A_io
│   │   │   ├───aaf
│   │   │   └───agg
│   │   ├───2A_io
│   │   │   ├───aaf
│   │   │   └───agg
│   │   └───4A_io
│   │       └───sensitivity
│   ├───diagnostics
│   ├───forecast
│   └───reference_models
└───output
    ├───figures
    └───tables
```

This assessment implements three spatial configurations: 

- `4A_io`: four areas, main configuration
- `2A_io`: two areas 
- `1A_io`: one area 

The `2A_io` and `1A_io` configurations implement two subconfigurations: aggregated (`agg`) and areas-as-fleets (`aaf`). See assessment report.

## 6. Download the raw data

Download the raw data from the IOTC website. 

* The catch and size data can be found here: <https://iotc.org/documents/WPTT/26AS/Data/01>. You will find *five Excel files*, copy all of them and paste them in `data/raw` in the *working folder*.
* The CPUE data can be found here: <https://iotc.org/documents/standardised-cpue-yft-and-bet>. Copy the `YFT` **folder** and paste it in `data/raw` in the *working folder*.

## 7. Create SS3 inputs

In this repository, the folder `code/ss3_data_inputs` contains the R scripts to generate the data inputs for SS3. They are organized in three folders and should be created in that order:

- `CE`: R scripts to generate catch input.
- `CPUE`: R scripts to generate CPUE input.
- `LF`: R scripts to generate size frequency input.

The first number of the scripts names indicate the order in which should be run. 

## 8. Age-length and tagging data

The raw age-length and tagging data are not publicy available, so it is not possible to generate them. 

However, you can find the data in SS3 format for each model configuration in the `data/ss3_inputs` folder of this repository. Copy and paste them in `data/ss3_inputs` of the *working folder*. You could do this manually or by running these lines:

```{r}
source('sharepoint_path.R')
file.copy(from = 'data/ss3_inputs/4A_io', to = file.path(shrpoint_path, 'data/ss3_inputs/4A_io'))
file.copy(from = 'data/ss3_inputs/2A_io/agg', to = file.path(shrpoint_path, 'data/ss3_inputs/2A_io/agg'))
file.copy(from = 'data/ss3_inputs/2A_io/aaf', to = file.path(shrpoint_path, 'data/ss3_inputs/2A_io/aaf'))
file.copy(from = 'data/ss3_inputs/1A_io/agg', to = file.path(shrpoint_path, 'data/ss3_inputs/1A_io/agg'))
file.copy(from = 'data/ss3_inputs/1A_io/aaf', to = file.path(shrpoint_path, 'data/ss3_inputs/1A_io/aaf'))
```
