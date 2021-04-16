### Installation of openNCA v3.0 specific commit c3d3f48 from demo.zip
###  This is the installation procedure to install from packaged demo.zip
### 2021-03-26/TGT/ First Version

### Installation Tools Required
library(devtools)
library(curl)

### Remove any prior installation
if(interactive()) { 
    if("openNCA" %in% rownames(installed.packages())) { 
        detach('package:openNCA', unload=TRUE)
        remove.packages("openNCA")
    }
}

### commit_zip
zipfile <- "demo.zip"
### Download https://github.com/tensfeldt/openNCA/raw/master/demo/demo.zip
curl_download("https://github.com/tensfeldt/openNCA/raw/master/demo/demo.zip", destfile=zipfile)
### unzip the install file  
unzip(zipfile, exdir=getwd())
list.files()
### find unzipped directory
install_taz <- list.files(path=file.path(getwd(), "release_files"), pattern="*.tar.gz")
### Install Package  
install.packages(file.path(getwd(), "release_files", install_taz), repos = NULL, type="source")
### Load Package
library(openNCA)
