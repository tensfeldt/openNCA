
# Table of Contents

-   [Installation from source using the pre-built package](#orgb21de6b)
    -   [Demonstration Installation](#org4b7ea5e)
    -   [Open R or RStudio](#org0b1f5d9)
    -   [Set your default folder for installation in the R session](#org2ab74f1)
    -   [Download Testcase scripts, data, and installation files to your default folder](#org9c3405f)
    -   [Execute the installation script to install openNCA Computation Engine package library from source](#orgf59bd0c)
-   [Installation and building package from source using **devtools**](#org3b43c81)
    -   [Dependencies for installation and building from source using **devtools**](#org128ea0a)
        -   [devtools: Tools to Make Developing R Packages Easier](#org1cacf6c)
        -   [curl: A Modern and Flexible Web Client for R](#org9736814)
    -   [Demonstration Installation](#org062913c)
    -   [Open R or RStudio](#org6892319)
    -   [Set your default folder for installation in the R session](#org73debfc)
    -   [Download Testcase scripts, data, and installation files to your default folder](#org20aca40)
    -   [Execute the installation script to build the openNCA Computation Engine package library](#org98ce135)

These instructions provide brief installation and testcase execution
guidance.

There are two approaches presented:

-   [Installation from source using the pre-built package](#orge9c42b2)
-   [Installation and building package from source using **devtools**](#org62324d9)


<a id="orgb21de6b"></a>

# <a id="orge9c42b2"></a>Installation from source using the pre-built package


<a id="org4b7ea5e"></a>

## Demonstration Installation


<a id="org0b1f5d9"></a>

## Open R or RStudio

openNCA Computation Engine v3.0 (commit c3d3f48) has been qualified with R-3.5.1.
So, at the moment, consider R-3.5.1 as a minimum installation requirement.


<a id="org2ab74f1"></a>

## Set your default folder for installation in the R session

In the next step you will download a zip file from GitHub and extract to this folder.
The default folder will then have a child "demo" folder containing the sources for
installation and testcase files for demonstration use.


<a id="org9c3405f"></a>

## Download Testcase scripts, data, and installation files to your default folder

<https://github.com/tensfeldt/openNCA/raw/master/demo/demo.zip>


<a id="orgf59bd0c"></a>

## Execute the installation script to install openNCA Computation Engine package library from source

Load ./demo/install/openNCA<sub>install</sub><sub>package.R</sub> and
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


<a id="org3b43c81"></a>

# <a id="org62324d9"></a>Installation and building package from source using **devtools**


<a id="org128ea0a"></a>

## Dependencies for installation and building from source using **devtools**


<a id="org1cacf6c"></a>

### [devtools: Tools to Make Developing R Packages Easier](https://CRAN.R-project.org/package=devtools)


<a id="org9736814"></a>

### [curl: A Modern and Flexible Web Client for R](https://CRAN.R-project.org/package=curl)


<a id="org062913c"></a>

## Demonstration Installation


<a id="org6892319"></a>

## Open R or RStudio

openNCA Computation Engine v3.0 (commit c3d3f48) has been qualified with R-3.5.1.
So, at the moment, consider R-3.5.1 as a minimum installation requirement.


<a id="org73debfc"></a>

## Set your default folder for installation in the R session

In the next step you will download a zip file from GitHub and extract to this folder.
The default folder will then have a child "demo" folder containing the sources for
installation and testcase files for demonstration use.


<a id="org20aca40"></a>

## Download Testcase scripts, data, and installation files to your default folder

<https://github.com/tensfeldt/openNCA/raw/master/demo/demo.zip>


<a id="org98ce135"></a>

## Execute the installation script to build the openNCA Computation Engine package library

Load ./demo/install/openNCA<sub>build</sub><sub>package.R</sub> and
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

