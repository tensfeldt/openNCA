### Installation of openNCA v3.0 specific commit c3d3f48 from demo.zip
###  This is the installation procedure to install from packaged demo.zip
### 2021-04-16/TGT/ First Version

### Remove any prior installation
if(interactive()) { 
    if("openNCA" %in% rownames(installed.packages())) { 
        detach('package:openNCA', unload=TRUE)
        remove.packages("openNCA")
    }
}

### find package file in the *release\under{}files* directory
install_taz <- list.files(path=file.path(getwd(), "release_files"), pattern="*.tar.gz")
### Install Package  
install.packages(file.path(getwd(), "release_files", install_taz), repos = NULL, type="source")
### Load Package
library(openNCA)
