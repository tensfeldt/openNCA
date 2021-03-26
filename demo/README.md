
# Table of Contents

-   [Installation from source using the pre-built package](#orgff60bf6)
    -   [Demonstration Installation](#org2b8a163)
    -   [Open R or RStudio](#org3201277)
    -   [Set your default folder for installation in the R session](#org2bcba53)
    -   [Download Testcase scripts, data, and installation files to your default folder](#org23cee21)
    -   [Execute the installation script to install openNCA Computation Engine package library from source](#orgb1ce526)
-   [Installation and building package from source using **devtools**](#org9cacfd5)
    -   [Dependencies for installation and building from source using **devtools**](#orgb920650)
        -   [devtools: Tools to Make Developing R Packages Easier](#org2e9a026)
        -   [curl: A Modern and Flexible Web Client for R](#org0c055ad)
    -   [Demonstration Installation](#orgf738eb9)
    -   [Open R or RStudio](#org7ac88d4)
    -   [Set your default folder for installation in the R session](#org09ba9e8)
    -   [Download Testcase scripts, data, and installation files to your default folder](#org01fed3b)
    -   [Execute the installation script to build the openNCA Computation Engine package library](#org6e44ea9)

These instructions provide brief installation and testcase execution
guidance.

There are two approaches presented:

-   [Installation from source using the pre-built package](#org7c906cc)
-   [Installation and building package from source using **devtools**](#org0855cb0)


<a id="orgff60bf6"></a>

# <a id="org7c906cc"></a>Installation from source using the pre-built package


<a id="org2b8a163"></a>

## Demonstration Installation


<a id="org3201277"></a>

## Open R or RStudio

openNCA Computation Engine v3.0 (commit c3d3f48) has been qualified with R-3.5.1.
So, at the moment, consider R-3.5.1 as a minimum installation requirement.


<a id="org2bcba53"></a>

## Set your default folder for installation in the R session

In the next step you will download a zip file from GitHub and extract to this folder.
The default folder will then have a child "demo" folder containing the sources for
installation and testcase files for demonstration use.


<a id="org23cee21"></a>

## Download Testcase scripts, data, and installation files to your default folder

<https://github.com/tensfeldt/openNCA/raw/master/demo/demo.zip>


<a id="orgb1ce526"></a>

## Execute the installation script to install openNCA Computation Engine package library from source

Load ../demo/install/openNCA<sub>install</sub><sub>package.R</sub> and
execute all of the lines. Alternatively, source the openNCA<sub>install</sub><sub>package.R</sub> file.
This script will directly download and install the openNCA library package from source.

The following illustrates downloading demo.zip and unzipping to your Downloads folder using an
R script:

    home <- Sys.getenv("HOMEPATH")
    setwd(file.path(home, "Downloads"))
    getwd()
    unzip("demo.zip")
    list.files("./demo")
    source("./demo/install/openNCA_install_package.R")


<a id="org9cacfd5"></a>

# <a id="org0855cb0"></a>Installation and building package from source using **devtools**


<a id="orgb920650"></a>

## Dependencies for installation and building from source using **devtools**


<a id="org2e9a026"></a>

### [devtools: Tools to Make Developing R Packages Easier](https://CRAN.R-project.org/package=devtools)


<a id="org0c055ad"></a>

### [curl: A Modern and Flexible Web Client for R](https://CRAN.R-project.org/package=curl)


<a id="orgf738eb9"></a>

## Demonstration Installation


<a id="org7ac88d4"></a>

## Open R or RStudio

openNCA Computation Engine v3.0 (commit c3d3f48) has been qualified with R-3.5.1.
So, at the moment, consider R-3.5.1 as a minimum installation requirement.


<a id="org09ba9e8"></a>

## Set your default folder for installation in the R session

In the next step you will download a zip file from GitHub and extract to this folder.
The default folder will then have a child "demo" folder containing the sources for
installation and testcase files for demonstration use.


<a id="org01fed3b"></a>

## Download Testcase scripts, data, and installation files to your default folder

<https://github.com/tensfeldt/openNCA/raw/master/demo/demo.zip>


<a id="org6e44ea9"></a>

## Execute the installation script to build the openNCA Computation Engine package library

Load ../demo/install/openNCA<sub>build</sub><sub>package.R</sub> and
execute all of the lines. Alternatively, source the openNCA<sub>build</sub><sub>package.R</sub> file.
This script will build the openNCA library package from source.

The following illustrates downloading demo.zip and unzipping to your Downloads folder using an
R script:

    home <- Sys.getenv("HOMEPATH")
    setwd(file.path(home, "Downloads"))
    getwd()
    unzip("demo.zip")
    list.files("./demo")
    source("./demo/install/openNCA_build_package.R")

