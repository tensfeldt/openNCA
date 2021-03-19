### Installation of openNCA v3.0 specific commit c3d3f48
### 2021-03-19/TGT/ First Version

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

### Download and install the interim version of *openNCAreport*  
commit <- "c3d3f48"
### Commit dir
commit_dir <- paste0("openNCA_", commit)
### Commit file
commit_file <- paste0(commit_dir, ".zip")
### Remove any previously downloaded commit  
unlink(commit_file)
### Download commit for installation  
curl_download(paste0("https://github.com/tensfeldt/openNCA/archive/", commit, ".zip"), destfile=commit_file)
### show that the file has been downloaded  
list.files(pattern=commit_file)
### unzip the install file  
unzip(commit_file, exdir=getwd())
### find unzipped directory
install_dir <- list.files(pattern=paste0("openNCA-", commit))
### Load Package  
pkgload::load_all(path=file.path(install_dir, "openNCA"))
library(openNCA)
