
# Table of Contents

-   [Installation using the pre-built package](#org596b4c0)
    -   [Open R or RStudio](#org7e611ac)
    -   [Set your default folder for installation in the R session](#orga134805)
    -   [Download Testcase scripts, data, and installation files to a folder of your choice](#orgec19fed)
    -   [Execute the installation script to install openNCA Computation Engine package library from source](#org239ad3f)
-   [Installation and building package from source using **devtools**](#org6e8a46a)
    -   [Dependencies for installation and building from source](#org9be8556)
        -   [devtools: Tools to Make Developing R Packages Easier](#org0b5c48e)
        -   [curl: A Modern and Flexible Web Client for R](#org987618f)
    -   [Open R or RStudio](#orga4d8275)
    -   [Create/Set up a folder for installation in the R session](#orgecfa5e4)
    -   [Download Testcase scripts, data, and installation files to the install folder](#orge276f14)
    -   [Download the build package script from Github](#org1edabb7)
    -   [Execute the installation script to build the openNCA Computation Engine package library](#org134a035)
-   [Execute Model 1 Single Dose Extravascular Example Testcase](#org3251459)

These instructions provide brief installation and testcase execution
guidance.

There are two approaches presented:

-   [Installation using the pre-built package](#orgeca932b) - **Recommended**
-   [Installation and building package from source using **devtools**](#org22e81d2)
-   [Execute Model 1 Single Dose Extravascular Example Testcase](#orge552e6e)


<a id="org596b4c0"></a>

# <a id="orgeca932b"></a>Installation using the pre-built package


<a id="org7e611ac"></a>

## Open R or RStudio

openNCA Computation Engine v3.0 (commit c3d3f48) has been qualified with R-3.5.1.
So, at the moment, consider R-3.5.1 as a minimum installation requirement.
openNCA CE is being used in a Production Qualified Environment with R-4.0.3.


<a id="orga134805"></a>

## Set your default folder for installation in the R session

In the next step you will download a zip file from GitHub and extract to this folder.
The default folder will then have a child "demo" folder containing the sources for
installation and testcase files for demonstration use.


<a id="orgec19fed"></a>

## Download Testcase scripts, data, and installation files to a folder of your choice

<https://github.com/tensfeldt/openNCA/raw/master/demo/demo.zip>


<a id="org239ad3f"></a>

## Execute the installation script to install openNCA Computation Engine package library from source

Source or load and execute all of the lines for the ./demo/install/openNCA_install_package.R script.
This script will install the openNCA library package from source.

If you do have the package **curl** installed, the following illustrates downloading demo.zip and
unzipping to your Downloads folder using an R script:

    home <- Sys.getenv("HOMEPATH")
    setwd(file.path(home, "Downloads/demonstration"))
    zipfile <- "demo.zip"
    curl::curl_download("https://github.com/tensfeldt/openNCA/raw/master/demo/demo.zip", destfile=zipfile)
    unzip(zipfile, exdir=getwd())
    list.files(recursive=TRUE)
    source("./install/openNCA_install_package.R")
    library(openNCA)

    
    R version 3.5.1 Patched (2018-11-18 r75627) -- "Feather Spray"
    Copyright (C) 2018 The R Foundation for Statistical Computing
    Platform: x86_64-w64-mingw32/x64 (64-bit)
    
    R is free software and comes with ABSOLUTELY NO WARRANTY.
    You are welcome to redistribute it under certain conditions.
    Type 'license()' or 'licence()' for distribution details.
    
      Natural language support but running in an English locale
    
    R is a collaborative project with many contributors.
    Type 'contributors()' for more information and
    'citation()' on how to cite R or R packages in publications.
    
    Type 'demo()' for some demos, 'help()' for on-line help, or
    'help.start()' for an HTML browser interface to help.
    Type 'q()' to quit R.
    
    > home <- Sys.getenv("HOMEPATH")
    > setwd(file.path(home, "Downloads/demonstration"))
    > zipfile <- "demo.zip"
    > curl::curl_download("https://github.com/tensfeldt/openNCA/raw/master/demo/demo.zip", destfile=zipfile)
    > unzip(zipfile, exdir=getwd())
    > list.files(recursive=TRUE)
     [1] "demo.zip"                                                                
     [2] "install/openNCA_build_package.R"                                         
     [3] "install/openNCA_download_demo.R"                                         
     [4] "install/openNCA_install_package.R"                                       
     [5] "openNCA Computation Engine Demonstration Installation and Test Cases.pdf"
     [6] "README.html"                                                             
     [7] "README.md"                                                               
     [8] "release_files/openNCA_c3d3f48_3.0.0.tar.gz"                              
     [9] "tc2001_M1SD.R"                                                           
    [10] "tc2002_M1SD.R"                                                           
    [11] "tc2003_M1SS.R"                                                           
    [12] "tc2004_M2SD.R"                                                           
    [13] "tc2005_M2SS.R"                                                           
    [14] "tc2006_M3SD.R"                                                           
    [15] "tc2008_M4SD.R"                                                           
    [16] "tc2009_M4SS.R"                                                           
    [17] "testcases/DEMO1001_M1SD/DEMO1001-CEST.csv"                               
    [18] "testcases/DEMO1001_M1SD/DEMO1001-DT.csv"                                 
    [19] "testcases/DEMO1001_M1SD/DEMO1001-KEL.csv"                                
    [20] "testcases/DEMO1001_M1SD/DEMO1001-MCT.csv"                                
    [21] "testcases/DEMO1001_M1SD/DEMO1001-PPRM.csv"                               
    [22] "testcases/DEMO1002_M1SS/DEMO1002-CEST.csv"                               
    [23] "testcases/DEMO1002_M1SS/DEMO1002-DT.csv"                                 
    [24] "testcases/DEMO1002_M1SS/DEMO1002-KEL.csv"                                
    [25] "testcases/DEMO1002_M1SS/DEMO1002-MCT.csv"                                
    [26] "testcases/DEMO1002_M1SS/DEMO1002-PPRM.csv"                               
    [27] "testcases/DEMO1003_M2SD/DEMO1003-CEST.csv"                               
    [28] "testcases/DEMO1003_M2SD/DEMO1003-DT.csv"                                 
    [29] "testcases/DEMO1003_M2SD/DEMO1003-KEL.csv"                                
    [30] "testcases/DEMO1003_M2SD/DEMO1003-MCT.csv"                                
    [31] "testcases/DEMO1003_M2SD/DEMO1003-PPRM.csv"                               
    [32] "testcases/DEMO1004_M2SS/DEMO1004-DT.csv"                                 
    [33] "testcases/DEMO1004_M2SS/DEMO1004-KEL.csv"                                
    [34] "testcases/DEMO1004_M2SS/DEMO1004-MCT.csv"                                
    [35] "testcases/DEMO1004_M2SS/DEMO1004-REF.csv"                                
    [36] "testcases/DEMO1005_M3SD/DEMO1005-DT.csv"                                 
    [37] "testcases/DEMO1005_M3SD/DEMO1005-KEL.csv"                                
    [38] "testcases/DEMO1005_M3SD/DEMO1005-MCT.csv"                                
    [39] "testcases/DEMO1005_M3SD/DEMO1005-REF.csv"                                
    [40] "testcases/DEMO1007_M4SD/DEMO1007-DT.csv"                                 
    [41] "testcases/DEMO1007_M4SD/DEMO1007-KEL.csv"                                
    [42] "testcases/DEMO1007_M4SD/DEMO1007-MCT.csv"                                
    [43] "testcases/DEMO1007_M4SD/DEMO1007-REF.csv"                                
    [44] "testcases/DEMO1008_M4SS/DEMO1008-DT.csv"                                 
    [45] "testcases/DEMO1008_M4SS/DEMO1008-KEL.csv"                                
    [46] "testcases/DEMO1008_M4SS/DEMO1008-MCT.csv"                                
    [47] "testcases/DEMO1008_M4SS/DEMO1008-REF.csv"                                
    > source("./install/openNCA_install_package.R")
      converting help for package 'openNCA'
        ae                                      html  
        aepct                                   html  
        aet                                     html  
        aetau                                   html  
        aetpct                                  html  
        at                                      html  
        auc_XbpctO                              html  
        auc_XbpctP                              html  
        auc_XpctO                               html  
        auc_XpctP                               html  
        auc_all                                 html  
        auc_dn                                  html  
        auc_inf_o                               html  
        auc_inf_oc                              html  
        auc_inf_p                               html  
        auc_inf_pc                              html  
        auc_last                                html  
        auc_lastc                               html  
        auc_lin                                 html  
        auc_lin_log                             html  
        auc_lin_up_log_down                     html  
        auc_log                                 html  
        auc_t                                   html  
        auc_t1_t2                               html  
        auc_tau                                 html  
        aumc_XpctO                              html  
        aumc_XpctP                              html  
        aumc_inf_o                              html  
        aumc_inf_p                              html  
        aumc_last                               html  
        aumc_lin                                html  
        aumc_lin_log                            html  
        aumc_lin_up_log_down                    html  
        aumc_log                                html  
        aumc_tau                                html  
        aurc_all                                html  
        c0                                      html  
        cav                                     html  
        cendinf                                 html  
        cendinf_dn                              html  
        cest                                    html  
        clast                                   html  
        clfo                                    html  
        clfow                                   html  
        clfp                                    html  
        clfpw                                   html  
        clftau                                  html  
        clftauw                                 html  
        clo                                     html  
        clow                                    html  
        clp                                     html  
        clpw                                    html  
        clr                                     html  
        clrt                                    html  
        clrtau                                  html  
        cltau                                   html  
        cltauw                                  html  
        cmax                                    html  
        cmax_dn                                 html  
        cmaxc                                   html  
        cmin                                    html  
        cmin_dn                                 html  
        create_dependency_list                  html  
        create_dosing_intervals                 html  
        ctrough                                 html  
        ctroughend                              html  
        dependent_parameters                    html  
        dof                                     html  
        dosec                                   html  
        est_c0                                  html  
        estimate_concentration                  html  
        estimate_missing_concentration          html  
        estimate_told_concentration             html  
        get_told_concentration                  html  
        interpolate_lin                         html  
        interpolate_log                         html  
        kel                                     html  
        kel_r                                   html  
        lasttime                                html  
        makenumeric                             html  
        maxrate                                 html  
        midptlast                               html  
        model_display_parameters                html  
        model_parameters                        html  
        model_spec                              html  
        mr_auc_inf_o                            html  
        mr_auc_inf_p                            html  
        mr_auc_last                             html  
        mr_auc_tau                              html  
        mr_cmax                                 html  
        mrt_evif_o                              html  
        mrt_evif_p                              html  
        mrt_ivif_o                              html  
        mrt_ivif_p                              html  
        mrt_last                                html  
        opennca_version                         html  
        parameter_indices                       html  
        parameter_regex                         html  
        parameter_required                      html  
        parameters_by_class                     html  
        parse.reg                               html  
        ptf                                     html  
        ptr                                     html  
        ptroughr                                html  
        ptroughrend                             html  
        rate                                    html  
        ratelast                                html  
        rmempty                                 html  
        run_M1_SD_computation                   html  
        run_M1_SS_computation                   html  
        run_M2_SD_computation                   html  
        run_M2_SS_computation                   html  
        run_M3_SD_computation                   html  
        run_M3_SS_computation                   html  
        run_M4_SD_computation                   html  
        run_M4_SS_computation                   html  
        run_computation                         html  
        specific_gravity_adjustment             html  
        tendinf                                 html  
        tlag                                    html  
        tlast                                   html  
        tmax                                    html  
        tmaxrate                                html  
        tmin                                    html  
        unit_conversion                         html  
        unitclass_parameters                    html  
        update_mct_data                         html  
        v0                                      html  
        valid_models                            html  
        validate_timeconc_data                  html  
        vol_sum                                 html  
        vsso                                    html  
        vssow                                   html  
        vssp                                    html  
        vsspw                                   html  
        vzfo                                    html  
        vzfow                                   html  
        vzfp                                    html  
        vzfpw                                   html  
        vzftau                                  html  
        vzftauw                                 html  
        vzo                                     html  
        vzow                                    html  
        vzp                                     html  
        vzpw                                    html  
    > 


<a id="org6e8a46a"></a>

# <a id="org22e81d2"></a>Installation and building package from source using **devtools**


<a id="org9be8556"></a>

## Dependencies for installation and building from source


<a id="org0b5c48e"></a>

### [devtools: Tools to Make Developing R Packages Easier](https://CRAN.R-project.org/package=devtools)


<a id="org987618f"></a>

### [curl: A Modern and Flexible Web Client for R](https://CRAN.R-project.org/package=curl)


<a id="orga4d8275"></a>

## Open R or RStudio

openNCA Computation Engine v3.0 (commit c3d3f48) has been qualified with R-3.5.1.
So, at the moment, consider R-3.5.1 as a minimum installation requirement.
openNCA CE is being used in a Production Qualified Environment with R-4.0.3.


<a id="orgecfa5e4"></a>

## Create/Set up a folder for installation in the R session

In the next steps you will download files from GitHub and extract to the installation folder.
Once complete, this folder will have a child "demo" folder containing the sources for
installation and R and testcase dataset files for demonstration use.


<a id="orge276f14"></a>

## Download Testcase scripts, data, and installation files to the install folder

<https://github.com/tensfeldt/openNCA/raw/master/demo/demo.zip>


<a id="org1edabb7"></a>

## Download the build package script from Github

Download from <https://github.com/tensfeldt/openNCA/blob/master/demo/install/openNCA_build_package.R> and
store in your installation folder


<a id="org134a035"></a>

## Execute the installation script to build the openNCA Computation Engine package library

Source or load and execute all of the lines for the ./demo/install/openNCA_build_package.R script.
This script will build the openNCA library package from source.

The following illustrates downloading demo.zip, extracting all the files, and executing the
openNCA_build_package.R script within in the installation **Downloads/demonstration** folder using an R script:

    library(curl)
    home <- Sys.getenv("HOMEPATH")
    setwd(file.path(home, "Downloads/demonstration"))
    curl_download("https://github.com/tensfeldt/openNCA/raw/master/demo/install/openNCA_download_demo.R", destfile="openNCA_download_demo.R")
    source("openNCA_download_demo.R")
    unlink("openNCA_download_demo.R")
    curl_download("https://github.com/tensfeldt/openNCA/raw/master/demo/install/openNCA_build_package.R", destfile="openNCA_build_package.R")
    source("openNCA_build_package.R")
    unlink("openNCA_build_package.R")
    library(openNCA)

    
    R version 3.5.1 Patched (2018-11-18 r75627) -- "Feather Spray"
    Copyright (C) 2018 The R Foundation for Statistical Computing
    Platform: x86_64-w64-mingw32/x64 (64-bit)
    
    R is free software and comes with ABSOLUTELY NO WARRANTY.
    You are welcome to redistribute it under certain conditions.
    Type 'license()' or 'licence()' for distribution details.
    
      Natural language support but running in an English locale
    
    R is a collaborative project with many contributors.
    Type 'contributors()' for more information and
    'citation()' on how to cite R or R packages in publications.
    
    Type 'demo()' for some demos, 'help()' for on-line help, or
    'help.start()' for an HTML browser interface to help.
    Type 'q()' to quit R.
    
    > library(curl)
    > home <- Sys.getenv("HOMEPATH")
    > setwd(file.path(home, "Downloads/demonstration"))
    > curl_download("https://github.com/tensfeldt/openNCA/raw/master/demo/install/openNCA_download_demo.R", destfile="openNCA_download_demo.R")
    > source("openNCA_download_demo.R")
      converting help for package 'openNCA'
        ae                                      html  
        aepct                                   html  
        aet                                     html  
        aetau                                   html  
        aetpct                                  html  
        at                                      html  
        auc_XbpctO                              html  
        auc_XbpctP                              html  
        auc_XpctO                               html  
        auc_XpctP                               html  
        auc_all                                 html  
        auc_dn                                  html  
        auc_inf_o                               html  
        auc_inf_oc                              html  
        auc_inf_p                               html  
        auc_inf_pc                              html  
        auc_last                                html  
        auc_lastc                               html  
        auc_lin                                 html  
        auc_lin_log                             html  
        auc_lin_up_log_down                     html  
        auc_log                                 html  
        auc_t                                   html  
        auc_t1_t2                               html  
        auc_tau                                 html  
        aumc_XpctO                              html  
        aumc_XpctP                              html  
        aumc_inf_o                              html  
        aumc_inf_p                              html  
        aumc_last                               html  
        aumc_lin                                html  
        aumc_lin_log                            html  
        aumc_lin_up_log_down                    html  
        aumc_log                                html  
        aumc_tau                                html  
        aurc_all                                html  
        c0                                      html  
        cav                                     html  
        cendinf                                 html  
        cendinf_dn                              html  
        cest                                    html  
        clast                                   html  
        clfo                                    html  
        clfow                                   html  
        clfp                                    html  
        clfpw                                   html  
        clftau                                  html  
        clftauw                                 html  
        clo                                     html  
        clow                                    html  
        clp                                     html  
        clpw                                    html  
        clr                                     html  
        clrt                                    html  
        clrtau                                  html  
        cltau                                   html  
        cltauw                                  html  
        cmax                                    html  
        cmax_dn                                 html  
        cmaxc                                   html  
        cmin                                    html  
        cmin_dn                                 html  
        create_dependency_list                  html  
        create_dosing_intervals                 html  
        ctrough                                 html  
        ctroughend                              html  
        dependent_parameters                    html  
        dof                                     html  
        dosec                                   html  
        est_c0                                  html  
        estimate_concentration                  html  
        estimate_missing_concentration          html  
        estimate_told_concentration             html  
        get_told_concentration                  html  
        interpolate_lin                         html  
        interpolate_log                         html  
        kel                                     html  
        kel_r                                   html  
        lasttime                                html  
        makenumeric                             html  
        maxrate                                 html  
        midptlast                               html  
        model_display_parameters                html  
        model_parameters                        html  
        model_spec                              html  
        mr_auc_inf_o                            html  
        mr_auc_inf_p                            html  
        mr_auc_last                             html  
        mr_auc_tau                              html  
        mr_cmax                                 html  
        mrt_evif_o                              html  
        mrt_evif_p                              html  
        mrt_ivif_o                              html  
        mrt_ivif_p                              html  
        mrt_last                                html  
        opennca_version                         html  
        parameter_indices                       html  
        parameter_regex                         html  
        parameter_required                      html  
        parameters_by_class                     html  
        parse.reg                               html  
        ptf                                     html  
        ptr                                     html  
        ptroughr                                html  
        ptroughrend                             html  
        rate                                    html  
        ratelast                                html  
        rmempty                                 html  
        run_M1_SD_computation                   html  
        run_M1_SS_computation                   html  
        run_M2_SD_computation                   html  
        run_M2_SS_computation                   html  
        run_M3_SD_computation                   html  
        run_M3_SS_computation                   html  
        run_M4_SD_computation                   html  
        run_M4_SS_computation                   html  
        run_computation                         html  
        specific_gravity_adjustment             html  
        tendinf                                 html  
        tlag                                    html  
        tlast                                   html  
        tmax                                    html  
        tmaxrate                                html  
        tmin                                    html  
        unit_conversion                         html  
        unitclass_parameters                    html  
        update_mct_data                         html  
        v0                                      html  
        valid_models                            html  
        validate_timeconc_data                  html  
        vol_sum                                 html  
        vsso                                    html  
        vssow                                   html  
        vssp                                    html  
        vsspw                                   html  
        vzfo                                    html  
        vzfow                                   html  
        vzfp                                    html  
        vzfpw                                   html  
        vzftau                                  html  
        vzftauw                                 html  
        vzo                                     html  
        vzow                                    html  
        vzp                                     html  
        vzpw                                    html  
    > unlink("openNCA_download_demo.R")
    > curl_download("https://github.com/tensfeldt/openNCA/raw/master/demo/install/openNCA_build_package.R", destfile="openNCA_build_package.R")
    > source("openNCA_build_package.R")
    > unlink("openNCA_build_package.R")
    > library(openNCA)
    > 


<a id="org3251459"></a>

# <a id="orge552e6e"></a>Execute Model 1 Single Dose Extravascular Example Testcase

Once installation is complete, test installation with a sample execution. Note that
this example assumes that the tidyverse package is installed.

    home <- Sys.getenv("HOMEPATH")
    setwd(file.path(home, "Downloads/demonstration"))
    source("tc2001_M1SD.R")

