# IOTC Yellowfin Stock Assessment 2024

This Github repository is used to conduct the 2024 Yellowfin assessment in the Indian Ocean. **Open it in RStudio as a project**.

> :loudspeaker: **Goal**
>
> The main goal of this Github repository is to replicate all the 2024 SS configurations using R code, which will improve the collaboration among stock assessors and the communication of results. This automatization can save a lot of time during the assessment process.

These following folders can be found in this repository:

-   `code`: store the R scripts used to analyze the data (`code/analyses`), and to create the SS inputs (`code/ss/inputs`) and configurations (`code/ss/configurations`).
-   `presentation`: store Quarto files (`.qmd`) to produce the assessment presentation(s).
-   `report`: store Quarto files (`.qmd`) to produce the assessment report.

## Sharepoint

You also need to have access to the Sharepoint to read the raw data and store the SS files. In the Sharepoint, you can find the following main folders:

-   `data`: store the raw data provided by the IOTC Secretariat (`data/raw`), the processed data to make figures (`data/processed`), and the SS inputs for different SS spatial configurations (`data/ss_inputs`).
-   `models`: store the base SS files (e.g., the 2021 SS files, `models/base`) that will be then modified to create the 2024 configurations. Only include one set of base files (`control.ss`, `data.ss`, `forecast.ss`, `starter.ss`) per spatial configuration. This folder also store the SS configurations for the 2024 assessment by spatial configuration (`models/configurations`). Use the R scripts in `code/ss/configurations` in the Github repository to create the 2024 configurations.
-   `output`: store figures and tables from the data analysis and SS.

> :warning: **Important**
>
> Since the Sharepoint local path might change for AZTI and external collaborators, you will need to locally change the `sharepoint_path.R` script in order to read the data and save the SS configurations properly. DO NOT commit/push changes in `sharepoint_path.R`.

The **folders** in the Sharepoint should look like:

``` bash
└───IOTC_YFT_2024 - General
    ├───data
    │   ├───processed
    │   ├───raw
    │   └───ss_inputs
    │       ├───1A_west
    │       ├───2A_io
    │       └───4A_io
    ├───models
    │   ├───base
    │   │   └───4A_io
    │   └───configurations
    │       ├───1A_west
    │       ├───2A_io
    │       └───4A_io
    └───outputs
        ├───figures
        └───tables
```

## Basic steps to use Github

If you are unfamiliar with Github, these are the first steps that you need to do:

1.  Create a Github account [here](https://github.com).
2.  The simplest way to manage your files (on Windows) is to use [Github Desktop](https://github.com/apps/desktop).
3.  Clone this repository. Follow [these instructions](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository).

If you followed these steps, you will now have all the files of this repository locally (i.e., in your computer). Now you can start making changes to existing files or adding new files.

Every time you want to make a new change (assuming you are already a collaborator in this repository), you must follow the following steps:

1.  `git pull` to have all the updates that other people could have made. It is highly recommended to `git pull` the repository regularly and before making any change. Follow [these instructions](https://docs.github.com/en/desktop/working-with-your-remote-repository-on-github-or-github-enterprise/syncing-your-branch-in-github-desktop#pulling-to-your-local-branch-from-the-remote).
2.  `git commit` to record changes to one or more files, and it is done after you have made the desired change. Write a short description of the change. Follow [these instructions](https://docs.github.com/en/desktop/making-changes-in-a-branch/committing-and-reviewing-changes-to-your-project-in-github-desktop#write-a-commit-message-and-push-your-changes).
3.  After you are done with the `git commit`, you need to `git push` your changes to make them available to all the collaborators. You can check that your changes have been successfully uploaded to the repository by examining the Github repository site.
