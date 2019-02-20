# World Bank - Bangladesh Data Analytics Project

## Introduction

- For any changes in the repository please refer to instructions laid out here and if not available here please email at mgorgens@worldbank.org or edin.hamzic@symphony.is. 
- The README.md/.ipynb document lays out the structure of repository and set of instructions about naming nomenclature and how to keep the repository.
- The README.md/.ipynb contains the following sections:
    - Repository structure
    - General guidelines
    - Description of the repository structure
    - Naming nomenclature
- There are several important aspects why naming nomenc The general idea of naming nomenclature is for everyone to get on board and easily understand the structure of the project,

## Repository structure

- The repository contains: five main directories as listed below. The detailed description is laid out in the following sections:
    - **data**: Raw data. If we do not get large file support (LFS) for our repository. We will keep locally and unchanged. Detailed directory structure is laid out in *Description of the repository structure* as well as for all other directories.
    
    - **docs**: Final reports distributed by data sources (6 data sources) and general documents.
    - **ds**: Python and R scripts and Jupyter Notebooks with models. These scripts must follow the naming nomenclature as described in section "Naming nomenclature".
    - **fe**: Python and R scripts and Jupyter Notebook for data preprocessing and feature engineering that also must follow the naming nomenclature as described in section "Naming nomenclature".
    - **output**: Figures, tables and prediction files following the structure of *data*, *ds* and *fe* directories and naming nomenclature.
    - **README.md/.ipynb**
    - **WORKBOOK.md/.ipynb/**: The daily work log.
    
```
|--data
|--docs
|--ds
|--fe
|--output
|--utils
|--README.ipynb/.md
|--WORKBOOK.ipynb/.md
```

## General guidelines

- The general workflow is following: DATA is read from corresponding directory within the *data* directory 
- The 

## Description of the repository structure

### Data

- **Data** directory contains raw data following the structure laid out below. If we do not get the large file support (LFS) for our repository we will keep **data** directory in the original form on all our local machines.
- The following directories are available in their original form:
    - **bbs**: Data from [Bangladesh Bureau of Statistics](http://bbs.portal.gov.bd): SVRS and Census 2011 data
    - **dgfp**: Data from [Service Statistics](http://www.dgfpmis.org/ss/ss_menu.php) of [Directorate General of Family Planning](http://dgfp.gov.bd/) 
    - **dhis2**: Data from [Bangladesh DHIS2](http://www.dghs.gov.bd/index.php/en/component/content/article?id=456) of [Directorate General of Health Services](http://www.dghs.gov.bd)
    - **dhs**: [Demographic Health Surveys](https://dhsprogram.com/)
    - **general**: General data if appropriate
    - **shapefiles**: Bangladesh shapefiles: [GADM](https://gadm.org/) and [DIVA](http://www.diva-gis.org/gdata)
    - **other**: Other data sources. Not classified.
    - **unicef**: Data from [UNICEF MICS](http://mics.unicef.org/) studies and other survey data produced by [UNICEF Bangladesh](https://www.unicef.org/bangladesh/en).

```
|--data
|----bbs
|----dgfp
|----dhis2
|----dhs
|----general
|----shapefiles
|----other
|----unicef
```

### Docs

- All final or draft documents and presentations should be stored here including literature and scientific papers of interest.


### DS

- This directory contains R and Python scripts and notebooks that are written for performing data analysis.
- Scripts and notebooks must follow the naming nomenclature as laid out in **Naming nomenclature** below.
- Scripts and notebooks must contains descriptions of the analysis whether in the script itself or in accompanying .md or .txt file with the same name as the script or notebook.
- Descriptions must contains exact name of input files and their location, short description of analysis and the output files and their location.

```
|--ds
|----bbs
|----dgfp
|----dhis2
|----dhs
|----general
|----shapefiles
|----other
|----unicef
```

### FE

- This directory contains R and Python scripts and notebooks that are written for performing data preprocessing and feature engineering.
- Scripts and notebooks must follow the naming nomenclature as laid out in **Naming nomenclature** below.
- Scripts and notebooks must contains descriptions of the analysis whether in the script itself or in accompanying .md or .txt file with the same name as the script or notebook.
- Descriptions must contains exact name of input files and their location, short description of analysis and the output files and their location.

```
|--fe
|----bbs
|----dgfp
|----dhis2
|----dhs
|----general
|----shapefiles
|----other
|----unicef
```

### Output
- The output directory follows the same structure as **ds** and **fe** directories and it contains output from **fe** and **ds** scripts and notebooks following the below naming nomenclature.
- Important things is that files and directories are named in the same ways as scripts and notebooks that are used to produce them. 

```
|--output
|----bbs
|----dgfp
|----dhis2
|----dhs
|----general
|----shapefiles
|----other
|----unicef
```

### Utils
- This directory should contains any custom written modules and packages (for both Python and R).

## Naming nomenclature

- Please follow these general instructions regarding the naming the scripts, files and output files
- There will be two general ways of naming your scripts:
    - Naming structure: **[activity]\_[data\_source]\_[meta_information or number].[file\_extension]**
    - List of activities:
        - dd: data download
        - dp: data preprocessing 
        - ds: data science, building models and performing analytics
    - List of data sources:
        - bbs, dgfp, dhis2, dhs, unicef, combined (if data is combined from multiple sources)
    - Meta information or number:
        - Extra information that can be of value and easy to refer to
        - If number is used it is essential to provide a detailed explanation in the script. If not in the script than there must be an .md or .txt file with the same name as script that describes the following things:
            - What is the input and where is stored (a relative path within repository)
            - Short description of what script or notebook is doing
            - Whati is the output and where is stored. It is prefered for script to have one output file or directory only if possible and named the output by the script name. 
    - Example 1: **dd_dhis2_demo.ipynb**: This is a demo Jupyter notebook is used to download dhis2 data.
    - Example 2: **dp_unicef_mics.R**: This is R script used for data preprcoessing of MICS UNICEF data.
    - Exmaple 3: **ds_combined\_dbscan.R**: This is an R script that performs analysis on the combined dataset (specify in the script or accompanied .md or .txt file which explains what data is combined) document 
