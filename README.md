# IOTC Yellowfin Stock Assessment 2024

> :loudspeaker: **Goal**
>
> The main goal of this repository is to replicate the 2024 Indian Ocean yellowfin assessment configurations in R. The assessment is implemented in [Stock Synthesis 3](https://vlab.noaa.gov/web/stock-synthesis).

In order to execute this repository, you will need to have some knowledge on:

- Github (highly desirable but not mandatory)
- Stock Synthesis 3 (SS3, mandatory)
- R programming (mandatory)
- `r4ss` R package (mandatory)

The steps to run this repository are described below.

## 1. Clone this repository

If you are a Github user, you can clone this repository by following [these instructions](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository).

If you are not familiar with Github, you can download this repository as ZIP file by following [these instructions](https://docs.github.com/en/get-started/start-your-journey/downloading-files-from-github#downloading-a-repositorys-files). Save the downloaded folder somewhere locally. 

## 2. Open the R project

There is an R project in the repository folder called `IOTC_YFT_2024_Assessment.Rproj`. We suggest to open it from RStudio. This will automatically set your working directory to the repository folder. 

If you want to use a different visual interface that does not allow to work with R projects, you will need set the working directory mannually to the repository folder.

## 3. Specify the working folder

The *working folder* is a folder where the raw and processed data will be saved, and the SS3 configuration files will be created and run. Note that this is different than the *repository folder*.

Create the working folder somewhere on your local computer, and then specify the path in the `sharepoint_path.R`. For example:

```{r}
shrpoint_path = 'C:/Use/2024_YFT_IOTC'
```