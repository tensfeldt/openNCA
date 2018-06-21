#' Generate Table
#'
#' This function reads in a dataframe and creates a PDF document generating
#' a latex table. 
#' 
#' @details 
#' \itemize{
#'  \item 'path' and 'title' by defauly are 'NULL'
#'  \item 'groupBy', 'pageBy', 'orderBy', 'columnToDisplay', 'columnNamesToDisplay', 'columnType' and 'footnote' by default are an empty list ('list()')
#'  \item 'pageNumber' and 'dateGeneration' by defualt are 'TRUE'
#'  \item 'outputFileName' by defualt is 'NULL' and will take the name of 'huxtable-output.pdf'
#'  \item 'oriented' by default is 'FALSE' and will result in portrait
#'  \item 'numOfRows' by default is set to '40'
#'  \item 'tableScale' by default is set to '1'
#'  \item 'tableResize' by default is set to '1'
#'  \item 'leftMargin' by default is set to '0'
#'  \item Both 'tableScale' and 'tableResize' tend to perform similar task but they work better together
#'  \item Both 'tableScale' and 'tableResize' values are multiplied to a 'textwidth' value of 390pt (1pt = 0.3515mm) to determine the final value
#' } 
#' 
#' @note 
#' Erros will be throw if invalid input for 'path', 'numOfRows', 'tableScale', 'tableResize', 'leftMargin' is passed.
#'
#' @param path The path of the XML data file  
#' @param title The title of the table that will appear in the PDF document
#' @param groupBy A list of columns by which the dataset will be grouped by 
#' @param pageBy A list of columns by which the dataset will be paged by
#' @param orderBy A list of columns that will be used to order the dataset
#' @param columnToDisplay A list of columns to display, by default all are displayed (specify the order as well)
#' @param columnNamesToDisplay A list of columns names mappings which will be used instead of the column names of the dataset
#' @param columnType A list of columns names mappings which will be used to select either character or numeric for certain columns, by default this behavior will be auto-generated
#' @param footnote A list of notes that will be appended at the bottom of the table in the PDF document
#' @param outputFileName The file name of the output file (include '.pdf')
#' @param pageNumber A boolean value used to display page numbers
#' @param dateGeneration A boolean value used to generate date
#' @param numOfRows A numeric value that is used to determine the number of rows in each page
#' @param tableScale A numeric value that will be used to scale the table outputted in the PDF document
#' @param tableResize A numeric value that will be used to resize the table outputted in the PDF document
#' @param leftMargin A numeric value that will be used to scale the page's left margin in the PDF document (in inches)
#' @param oriented A boolean value that is used to determine whether PDF will output in portrait or landscape
#' @section Returns:
#' \strong{PDF} \cr 
#' \itemize{
#'  \item Table generated with specified options
#' }
#' @examples 
#' 
#' #TEST CASE ONE
#' 
#' #Arguments
#' Path = "/eNCA/tc00/M1SD/B1261002.in"
#' GroupBy = list('SUBJID', 'TREATXT')                 
#' PageBy = list()                  
#' OrderBy = list('SUBJID')              
#' ColumnType = list(SUBJID = "character", PKDATAROWID = "character", PERIOD = "numeric")            
#' ColumnToDisplay = list("SUBJID", "TREATXT", "PERIOD", "DOSE", "PKDATAROWID", "PKPTMR", "PKPTM", "PKCNC")        
#' ColumnNamesToDisplay = list(SUBJID = "Subject ID", TREATXT = "Treatment Group", PKCNC = "Concentration (NG/ML)")  
#' Footnote = list("this is a test", "this is a second test")              
#' Title = "PKL001"
#' PageNumber = TRUE
#' OutputFileName = NULL
#' DateGeneration = TRUE
#' NumOfRows = 40
#' 
#' #Function call
#' generate_table(
#'   path = Path, 
#'   title = Title, 
#'   pageNumber = PageNumber, 
#'   dateGeneration = DateGeneration, 
#'   numOfRows = NumOfRows, 
#'   groupBy = GroupBy, 
#'   pageBy = PageBy, 
#'   orderBy = OrderBy, 
#'   columnType = ColumnType, 
#'   columnToDisplay = ColumnToDisplay, 
#'   columnNamesToDisplay = ColumnNamesToDisplay, 
#'   footnote = Footnote
#') 
#' 
#' Will add more test cases with outputs soon
#' 
#' @author
#' \itemize{
#'  \item Kevin McConnell \email{kmcconnell@rudrya.com}
#' } 
#' @export
generate_table <- function(path = NULL, title = NULL, pageNumber = TRUE, dateGeneration = TRUE, numOfRows = 40, oriented = FALSE, groupBy = list(), pageBy = list(), orderBy = list(), columnType = list(), columnToDisplay = list(), columnNamesToDisplay = list(), footnote = list(), tableScale = 1, tableResize = 1, leftMargin = 0) {
  if(is.null(path) || !file.exists(path)){
    stop("The path specifed in 'path' is not valid or does not exist")  
  }
  if(is.null(numOfRows) || !is.numeric(numOfRows)){
    stop("The number of rows specifed in 'numOfRows' is not valid")  
  }
  if(is.null(tableScale) || !is.numeric(tableScale)){
    stop("The number specifed in 'tableScale' is not valid")  
  }
  if(is.null(tableResize) || !is.numeric(tableResize)){
    stop("The number specifed in 'tableResize' is not valid")  
  }
  if(is.null(leftMargin) || !is.numeric(leftMargin)){
    stop("The number specifed in 'leftMargin' is not valid")  
  }
  
  ## dependeny function
  huxtableize <- function (obj_list, borders = 0) {
    lapply(obj_list, function (obj) {
      if (! inherits(obj, 'huxtable')) {
        obj <- huxtable::as_huxtable(obj)
        obj <- huxtable::set_all_borders(obj, borders)
      }
      obj
    })
  }
  
  ## this function is needed to parse a XML file
  parseXMLFile <- function(path) {
    #xml_data <- xmlParse("/Users/ronishdesai/Downloads/tc01-ogormanmt_20150528_000229847PM/B7461005.in")
    xml_data <- XML::xmlParse(path)
    
    if(length(XML::getNodeSet(xml_data, "//DataFrame")) != 0) {
      if(length(XML::getNodeSet(xml_data, "//DataFrame/Columns/Column")) != 0) {
        col_names <- list()
        for(i in 1:length(XML::getNodeSet(xml_data, "//DataFrame/Columns/Column"))){
          xml_attr <- XML::xmlAttrs(XML::getNodeSet(xml_data, "//DataFrame/Columns/Column")[[i]])
          col_names[[i]] <- xml_attr[which(names(xml_attr) == "name")]
        }
        
        xml_df <- t(XML::xmlToDataFrame(nodes = XML::getNodeSet(xml_data, "//DataFrame/Columns/Column/Items"), collectNames = FALSE))
        
        if(length(XML::getNodeSet(xml_data, "//DataFrame/RowNames/Items")) != 0) {
          row_names <- as.character(XML::xmlToDataFrame(nodes = XML::getNodeSet(xml_data, "//DataFrame/RowNames/Items"), collectNames = FALSE, stringsAsFactors = FALSE))
          xml_df <- as.data.frame(xml_df, row.names = row_names, stringsAsFactors = FALSE)
          write.csv(xml_df, file = 'tmp_parse_xml_file.csv')
          xml_df <- read.csv('tmp_parse_xml_file.csv', stringsAsFactors = FALSE, row.names = 1)
          invisible(file.remove('tmp_parse_xml_file.csv'))
        } else {
          xml_df <- as.data.frame(xml_df, row.names = nrow(xml_df), stringsAsFactors = FALSE)
          write.csv(xml_df, file = 'tmp_parse_xml_file.csv')
          xml_df <- read.csv('tmp_parse_xml_file.csv', stringsAsFactors = FALSE, row.names = 1)
          invisible(file.remove('tmp_parse_xml_file.csv'))
        }
        colnames(xml_df) <- col_names 
      } else {
        xml_df <- t(xmlToDataFrame(nodes = getNodeSet(xml_data, "//DataFrame/Columns/Column/Items"), collectNames = FALSE))
        
        if(length(XML::getNodeSet(xml_data, "//DataFrame/RowNames/Items")) != 0) {
          row_names <- as.character(XML::xmlToDataFrame(nodes = XML::getNodeSet(xml_data, "//DataFrame/RowNames/Items"), collectNames = FALSE, stringsAsFactors = FALSE))
          xml_df <- as.data.frame(xml_df, row.names = row_names, stringsAsFactors = FALSE)
          write.csv(xml_df, file = 'tmp_parse_xml_file.csv')
          xml_df <- read.csv('tmp_parse_xml_file.csv', stringsAsFactors = FALSE, row.names = 1)
          invisible(file.remove('tmp_parse_xml_file.csv'))
        } else {
          xml_df <- as.data.frame(xml_df, row.names = nrow(xml_df), stringsAsFactors = FALSE, row.names = 1)
          write.csv(xml_df, file = 'tmp_parse_xml_file.csv')
          xml_df <- read.csv('tmp_parse_xml_file.csv', stringsAsFactors = FALSE)
          invisible(file.remove('tmp_parse_xml_file.csv'))
        }
      }
    } else {
      xml_df <- NULL
    }
    return(xml_df)
  }
  
  ## this function is a helper function for the PDF 
  getBrace <- function(x) {
    initBrace <- 0
    lastBrace <- 0
    closingBrace <- 0
    returnBrace <- 0
    for(i in 1:nchar(x)){
      tmp <- substr(x, i, i)
      if(tmp == "{"){
        if(initBrace == 0){
          initBrace = i
        } else {
          lastBrace = i
        }
      } else if(tmp == "}") {
        closingBrace <- i
        if(lastBrace != 0){
          lastBrace = 0
          closingBrace = 0
        } else {
          returnBrace = i
          return(returnBrace)
        }
      }
    }
  }
  
  ## this fucntion creates the PDF
  quick_pdf <- function (... , file = "huxtable-output.pdf", title = NULL, tableScale = 1.2, scale = 1, adjustWidth = -10, n_row = 40, pageNum = TRUE, dateGen = TRUE, orient = FALSE, pageBy = list(), groupBy = list(), orderBy = list(), footnote = list(), columnNamesToDisplay = list()) {
    force(file) # ensures confirm() is called before any other files are created.
    huxtbl <- (...)
    i <- 1
    
    clean_tmp_dir <- normalizePath(getwd(), mustWork = TRUE)
    latex_file <- tempfile(tmpdir = clean_tmp_dir, fileext = ".tex")
    sink(latex_file, append = TRUE)
    
    if(!is.null(dateGen) && isTRUE(dateGen)){
      time_stamp <- paste0("Generated on: ", as.POSIXct(Sys.time()))
    }
    if(!is.null(title)){
      header_title <- paste0(as.character(title))
    } else {
      header_title <- ""
    }
    if(!is.null(pageBy) && length(pageBy) != 0){
      for(p in 1:length(pageBy)){
        page <- ifelse(unlist(pageBy)[p] %in% names(unlist(columnNamesToDisplay)), unlist(columnNamesToDisplay)[(names(unlist(columnNamesToDisplay)) == (unlist(pageBy)[p]))], unlist(pageBy)[p])
        if(nchar(header_title) == 0){
          if(p == 1){
            header_title <- paste0(header_title, "Page by: ", page) 
          } else {
            header_title <- paste0(header_title, page)
          }
        } else {
          if(p == 1){
            header_title <- paste0(header_title, " \\\\ ", "Page by: ", page) 
          } else if(p == length(pageBy)){
            header_title <- paste0(header_title, " and ", page)
          } else {
            header_title <- paste0(header_title, " , ", page)
          }
        }
      }
    }
    if(!is.null(groupBy) && length(groupBy) != 0){
      for(p in 1:length(groupBy)){
        group <- ifelse(unlist(groupBy)[p] %in% names(unlist(columnNamesToDisplay)), unlist(columnNamesToDisplay)[(names(unlist(columnNamesToDisplay)) == (unlist(groupBy)[p]))], unlist(groupBy)[p])
        if(nchar(header_title) == 0){
          if(p == 1){
            header_title <- paste0(header_title, "Grouped by: ", group) 
          } else {
            header_title <- paste0(header_title, group)
          }
        } else {
          if(p == 1){
            header_title <- paste0(header_title, " \\\\ ", "Grouped by: ", group) 
          } else if(p == length(groupBy)){
            header_title <- paste0(header_title, " and ", group)
          } else {
            header_title <- paste0(header_title, " , ", group)
          }
        }
      }
    }
    if(!is.null(orderBy) && length(orderBy) != 0){
      for(p in 1:length(orderBy)){
        order <- ifelse(unlist(orderBy)[p] %in% names(unlist(columnNamesToDisplay)), unlist(columnNamesToDisplay)[(names(unlist(columnNamesToDisplay)) == (unlist(orderBy)[p]))], unlist(orderBy)[p])
        if(nchar(header_title) == 0){
          if(p == 1){
            header_title <- paste0(header_title, "Ordered by: ", order) 
          } else {
            header_title <- paste0(header_title, order)
          }
        } else {
         if(p == 1){
            header_title <- paste0(header_title, " \\\\ ", "Ordered by: ", order) 
          } else if(p == length(orderBy)){
            header_title <- paste0(header_title, " and ", order)
          } else {
            header_title <- paste0(header_title, " , ", order)
          }
        }
      }
    }
    
    if(!is.null(pageBy) && length(pageBy) != 0) {
      for(n in 1:length(huxtbl)){
        seltbl <- huxtable::as_hux(huxtbl[[n]])
        if(nrow(seltbl) < n_row) {
          factor <- nrow(seltbl)
        } else {
          factor <- n_row 
        }
        i <- 1
        tmp <- NULL
        while(i < nrow(seltbl)){
          if(i == 1) {
            tmp <- dplyr::slice(seltbl, i:(factor))
          } else {
            tmp <- dplyr::slice(seltbl, i:(i+factor-1))
          }
          if(!is.null(title)){
            tmp <- huxtable::set_caption(tmp, title)
          }
          if(!is.null(footnote) && length(footnote) != 0) {
            for(j in 1:length(footnote)){
              if(j == 1){
                tmp <- huxtable::add_footnote(tmp, footnote[[j]])
              } else {
                tmp <- huxtable::add_footnote(tmp, footnote[[j]], border = 0) 
              }
            }
          }
          if(!is.null(columnNamesToDisplay) && length(columnNamesToDisplay) != 0){
            for(k in 1:length(names(columnNamesToDisplay))){
              tmp_name <- columnNamesToDisplay[[ names(columnNamesToDisplay)[k] ]]
              names(tmp)[ names(tmp) == names(columnNamesToDisplay)[k] ] <- tmp_name
            }
            tmp <- huxtable::add_colnames(tmp)
            tmp <- huxtable::set_bottom_border(tmp, 1, 1:ncol(tmp), 1)
            tmp <- huxtable::set_top_border(tmp, 1, 1:ncol(tmp), 1)
            tmp <- huxtable::set_bold(tmp, 1, 1:ncol(tmp), TRUE)
            tmp <- huxtable::set_align(tmp, 1:nrow(tmp), 1:ncol(tmp), "center")
          } else {
            tmp <- huxtable::add_colnames(tmp)
            tmp <- huxtable::set_bottom_border(tmp, 1, 1:ncol(tmp), 1)
            tmp <- huxtable::set_top_border(tmp, 1, 1:ncol(tmp), 1)
            tmp <- huxtable::set_bold(tmp, 1, 1:ncol(tmp), TRUE)
            tmp <- huxtable::set_align(tmp, 1:nrow(tmp), 1:ncol(tmp), "center")
          }
          hts <- huxtableize(list(tmp))
          
          # hts <- huxtableize(list(...))
          # on my Mac, tempdir() gets a double slash in the path, which screws up texi2pdf.
          # You can't use normalizePath with a non-existent file, so the below doesn't work:
          # latex_file <- normalizePath(tempfile(fileext = ".tex"), mustWork = TRUE)
          if(i == 1) {
            tryCatch({
              if(n == 1){
                cat('\\documentclass{article}\n')
                huxtable::report_latex_dependencies()
                cat('\n\\usepackage{changepage}')
                cat('\n\\usepackage{lastpage}')
                if(!is.null(orient) && isTRUE(orient)){
                  cat('\n\\usepackage[margin=0.5in,bottom=1in,top=1in,landscape]{geometry}')
                } else {
                  cat('\n\\usepackage[margin=0.5in,bottom=1in,top=1in]{geometry}')
                }
                if(!is.null(dateGen) && isTRUE(dateGen)){
                  cat('\n\\usepackage{fancyhdr}')
                  cat('\n\\pagestyle{fancy}')
                  cat(paste0('\n\\cfoot{', time_stamp,'}'))
                  cat('\n\\renewcommand{\\headrulewidth}{1pt}')
                  cat('\n\\renewcommand{\\footrulewidth}{1pt}')
                } else {
                  cat('\n\\usepackage{fancyhdr}')
                  cat('\n\\pagestyle{fancy}')
                  cat('\n\\cfoot{ }')
                  cat('\n\\renewcommand{\\headrulewidth}{1pt}')
                  cat('\n\\renewcommand{\\footrulewidth}{1pt}')
                }
                cat(paste0('\n\\fancyhead[L]{', header_title,'}'))
                if(!is.null(pageNum) && isTRUE(pageNum)){
                  cat('\n\\fancyhead[R]{\\thepage of \\pageref{LastPage}}')
                }
                cat('\n\\begin{document}')
              } else {
                cat('\n\\clearpage')
              }
              cat('\n\\begin{table}[t]')
              if(length(pageBy) == 1){
                cat('\n\\captionsetup{justification=centering,singlelinecheck=off}')
                page <- ifelse(unlist(pageBy)[[1]] %in% names(unlist(columnNamesToDisplay)), unlist(columnNamesToDisplay)[(names(unlist(columnNamesToDisplay)) == (unlist(pageBy)[[1]]))], unlist(pageBy)[[1]])
                table_caption <- paste0(page, " = ", as.character(lapply(unique(as.data.frame(seltbl)[unlist(pageBy)]), function(x) { x[x != ""] })))
                cat(paste0('\n\\caption*{', table_caption, '}'))
              } else {
                table_caption <- ""
                for(t in 1:length(pageBy)){
                  page <- ifelse(unlist(pageBy)[[t]] %in% names(unlist(columnNamesToDisplay)), unlist(columnNamesToDisplay)[(names(unlist(columnNamesToDisplay)) == (unlist(pageBy)[[t]]))], unlist(pageBy)[[t]])
                  temp_caption <- paste0(page, " = ", as.character(lapply(unique(as.data.frame(seltbl)[(unlist(pageBy)[[t]])]), function(x) { x[x != ""] })))
                  if(t == 1){
                    table_caption <- paste0(table_caption, temp_caption)
                  } else {
                    table_caption <- paste0(table_caption, " and ", temp_caption)
                  }
                }
                cat('\n\\captionsetup{justification=centering,singlelinecheck=off}')
                cat(paste0('\n\\caption*{', table_caption, '}'))
              }
              cat(paste0('\n\n\\begin{adjustwidth}{',as.character(adjustWidth*72.26),'pt}{}'))
              cat(paste0('\n\\resizebox{',as.character(scale),'\\textwidth}{!}{'))
              cat(paste0('\n\\centering\\begin{tabularx}{', as.character(tableScale),'\\textwidth}{', paste0(rep("X", ncol(tmp)), collapse = "") ,'}'))
              lapply(hts, function (ht) {
                cat('\n\n')
                latex <- as.character(huxtable::to_latex(ht))
                if(!is.null(title)){
                  latex <- substr(latex, (129+nchar(as.character(title))), nchar(latex)-14) #133
                } else {
                  latex <- substr(latex, 59, nchar(latex)-14) #59 or #18 
                }
                latex <- substr(latex, getBrace(latex)+1, nchar(latex))
                
                cat(latex)
                cat('\n\n')
              })
              cat('\n}\n\\end{adjustwidth}')
              cat('\n\\end{table}')
              if(factor <= nrow(seltbl) && n == length(huxtbl)){
                cat('\n\\end{document}')
              }
            },
            error = identity
            )
          } else if((i+factor) == nrow(seltbl)) {
            tryCatch({
              cat('\n\n\\clearpage')
              cat('\n\\begin{table}[t]')
              if(!is.null(title)){
                cat('\n\\captionsetup{justification=centering,singlelinecheck=off}')
                cat(paste0('\n\\caption*{', as.character(title), '}'))
              }
              cat(paste0('\n\n\\begin{adjustwidth}{',as.character(adjustWidth*72.26),'pt}{}'))
              cat(paste0('\n\\resizebox{',as.character(scale),'\\textwidth}{!}{'))
              cat(paste0('\n\\centering\\begin{tabularx}{', as.character(tableScale),'\\textwidth}{', paste0(rep("X", ncol(tmp)), collapse = "") ,'}'))
              lapply(hts, function (ht) {
                cat('\n\n')
                latex <- as.character(huxtable::to_latex(ht))
                if(!is.null(title)){
                  latex <- substr(latex, (129+nchar(as.character(title))), nchar(latex)-14) #133
                } else {
                  latex <- substr(latex, 59, nchar(latex)-14) #59 or #18 
                }
                latex <- substr(latex, getBrace(latex)+1, nchar(latex))
                
                cat(latex)
                cat('\n\n')
              })
              cat('\n}\n\\end{adjustwidth}')
              cat('\n\\end{table}')
              if(n == length(huxtbl)){
                cat('\n\\end{document}') 
              }
            },
            error = identity
            #finally = {sink()}
            )
          } else {
            tryCatch({
              cat('\n\n\\clearpage')
              cat('\n\\begin{table}[t]')
              if(!is.null(title)){
                cat('\n\\captionsetup{justification=centering,singlelinecheck=off}')
                cat(paste('\n\\caption*{', as.character(title), '}'))
              }
              cat(paste0('\n\n\\begin{adjustwidth}{',as.character(adjustWidth*72.26),'pt}{}'))
              cat(paste0('\n\\resizebox{',as.character(scale),'\\textwidth}{!}{'))
              cat(paste0('\n\\centering\\begin{tabularx}{', as.character(tableScale),'\\textwidth}{', paste0(rep("X", ncol(tmp)), collapse = "") ,'}'))
              lapply(hts, function (ht) {
                cat('\n\n')
                latex <- as.character(huxtable::to_latex(ht))
                if(!is.null(title)){
                  latex <- substr(latex, (129+nchar(as.character(title))), nchar(latex)-14) #133
                } else {
                  latex <- substr(latex, 59, nchar(latex)-14) #59 or #18 
                }
                latex <- substr(latex, getBrace(latex)+1, nchar(latex))
                
                cat(latex)
                cat('\n\n')
              })
              cat('\n}\n\\end{adjustwidth}')
              cat('\n\\end{table}')
            },
            error = identity
            )
          }
          
          i <- factor+i
          if((i+factor-1) > nrow(seltbl)){
            factor <- nrow(seltbl)-i
          } 
        }
      }
    } else {
      if(nrow(huxtbl) < n_row) {
        factor <- nrow(huxtbl)
      } else {
        factor <- n_row 
      }
      while(i < nrow(huxtbl)){
        if(i == 1) {
          tmp <- huxtable::as_hux(dplyr::slice(huxtbl, i:(factor)))
        } else {
          tmp <- huxtable::as_hux(dplyr::slice(huxtbl, i:(i+factor-1)))
        }
        if(!is.null(title)){
          tmp <- huxtable::set_caption(tmp, title)
        }
        if(!is.null(footnote) && length(footnote) != 0) {
          for(j in 1:length(footnote)){
            if(j == 1){
              tmp <- huxtable::add_footnote(tmp, footnote[[j]])
            } else {
              tmp <- huxtable::add_footnote(tmp, footnote[[j]], border = 0) 
            }
          }
        }
        if(!is.null(columnNamesToDisplay) && length(columnNamesToDisplay) != 0){
          for(k in 1:length(names(columnNamesToDisplay))){
            tmp_name <- columnNamesToDisplay[[ names(columnNamesToDisplay)[k] ]]
            names(tmp)[ names(tmp) == names(columnNamesToDisplay)[k] ] <- tmp_name
          }
          tmp <- huxtable::add_colnames(tmp)
          tmp <- huxtable::set_bottom_border(tmp, 1, 1:ncol(tmp), 1)
          tmp <- huxtable::set_top_border(tmp, 1, 1:ncol(tmp), 1)
          tmp <- huxtable::set_bold(tmp, 1, 1:ncol(tmp), TRUE)
          tmp <- huxtable::set_align(tmp, 1:nrow(tmp), 1:ncol(tmp), "center")
        } else {
          tmp <- huxtable::add_colnames(tmp)
          tmp <- huxtable::set_bottom_border(tmp, 1, 1:ncol(tmp), 1)
          tmp <- huxtable::set_top_border(tmp, 1, 1:ncol(tmp), 1)
          tmp <- huxtable::set_bold(tmp, 1, 1:ncol(tmp), TRUE)
          tmp <- huxtable::set_align(tmp, 1:nrow(tmp), 1:ncol(tmp), "center")
        }
        hts <- huxtableize(list(tmp))
        
        # hts <- huxtableize(list(...))
        # on my Mac, tempdir() gets a double slash in the path, which screws up texi2pdf.
        # You can't use normalizePath with a non-existent file, so the below doesn't work:
        # latex_file <- normalizePath(tempfile(fileext = ".tex"), mustWork = TRUE)
        if(i == 1) {
          tryCatch({
            cat('\\documentclass{article}\n')
            huxtable::report_latex_dependencies()
            cat('\n\\usepackage{changepage}')
            cat('\n\\usepackage{lastpage}')
            if(!is.null(orient) && isTRUE(orient)){
              cat('\n\\usepackage[margin=0.5in,bottom=1in,top=1in,landscape]{geometry}')
            } else {
              cat('\n\\usepackage[margin=0.5in,bottom=1in,top=1in]{geometry}')
            }
            if(!is.null(dateGen) && isTRUE(dateGen)){
              cat('\n\\usepackage{fancyhdr}')
              cat('\n\\pagestyle{fancy}')
              cat(paste0('\n\\cfoot{', time_stamp,'}'))
              cat('\n\\renewcommand{\\headrulewidth}{1pt}')
              cat('\n\\renewcommand{\\footrulewidth}{1pt}')
            } else {
              cat('\n\\usepackage{fancyhdr}')
              cat('\n\\pagestyle{fancy}')
              cat('\n\\cfoot{ }')
              cat('\n\\renewcommand{\\headrulewidth}{1pt}')
              cat('\n\\renewcommand{\\footrulewidth}{1pt}')
            }
            cat(paste0('\n\\fancyhead[L]{', header_title,'}'))
            if(!is.null(pageNum) && isTRUE(pageNum)){
              cat('\n\\fancyhead[R]{\\thepage of \\pageref{LastPage}}')
            }
            cat('\n\\begin{document}')
            cat('\n\\begin{table}[t]')
            cat(paste0('\n\n\\begin{adjustwidth}{',as.character(adjustWidth*72.26),'\\baselineskip}{}'))
            cat(paste0('\n\\resizebox{',as.character(scale),'\\textwidth}{!}{'))
            cat(paste0('\n\\centering\\begin{tabularx}{', as.character(tableScale),'\\textwidth}{', paste0(rep("X", ncol(tmp)), collapse = "") ,'}'))
            lapply(hts, function (ht) {
              cat('\n\n')
              latex <- as.character(huxtable::to_latex(ht))
              if(!is.null(title)){
                latex <- substr(latex, (129+nchar(as.character(title))), nchar(latex)-14) #133
              } else {
                latex <- substr(latex, 59, nchar(latex)-14) #59 or #18 
              }
              latex <- substr(latex, getBrace(latex)+1, nchar(latex))
              
              cat(latex)
              cat('\n\n')
            })
            cat('\n}\n\\end{adjustwidth}')
            cat('\n\\end{table}')
            if(factor == nrow(huxtbl)){
              cat('\n\\end{document}')
            }
          },
          error = identity
          )
        } else if((i+factor) == nrow(huxtbl)) {
          tryCatch({
            cat('\n\n\\clearpage')
            cat('\n\\begin{table}[t]')
            cat(paste0('\n\n\\begin{adjustwidth}{',as.character(adjustWidth*72.26),'\\baselineskip}{}'))
            cat(paste0('\n\\resizebox{',as.character(scale),'\\textwidth}{!}{'))
            cat(paste0('\n\\centering\\begin{tabularx}{', as.character(tableScale),'\\textwidth}{', paste0(rep("X", ncol(tmp)), collapse = "") ,'}'))
            lapply(hts, function (ht) {
              cat('\n\n')
              latex <- as.character(huxtable::to_latex(ht))
              if(!is.null(title)){
                latex <- substr(latex, (129+nchar(as.character(title))), nchar(latex)-14) #133
              } else {
                latex <- substr(latex, 59, nchar(latex)-14) #59 or #18 
              }
              latex <- substr(latex, getBrace(latex)+1, nchar(latex))
              
              cat(latex)
              cat('\n\n')
            })
            cat('\n}\n\\end{adjustwidth}')
            cat('\n\\end{table}')
            cat('\n\\end{document}')
          },
          error = identity,
          finally = {sink()}
          )
        } else {
          tryCatch({
            cat('\n\n\\clearpage')
            cat('\n\\begin{table}[t]')
            cat(paste0('\n\n\\begin{adjustwidth}{',as.character(adjustWidth*72.26),'\\baselineskip}{}'))
            cat(paste0('\n\\resizebox{',as.character(scale),'\\textwidth}{!}{'))
            cat(paste0('\n\\centering\\begin{tabularx}{', as.character(tableScale),'\\textwidth}{', paste0(rep("X", ncol(tmp)), collapse = "") ,'}'))
            lapply(hts, function (ht) {
              cat('\n\n')
              latex <- as.character(huxtable::to_latex(ht))
              if(!is.null(title)){
                latex <- substr(latex, (129+nchar(as.character(title))), nchar(latex)-14) #133
              } else {
                latex <- substr(latex, 59, nchar(latex)-14) #59 or #18 
              }
              latex <- substr(latex, getBrace(latex)+1, nchar(latex))
              
              cat(latex)
              cat('\n\n')
            })
            cat('\n}\n\\end{adjustwidth}')
            cat('\n\\end{table}')
          },
          error = identity
          )
        }
        
        i <- factor+i
        if((i+factor-1) > nrow(huxtbl)){
          factor <- nrow(huxtbl)-i
        } 
      } 
    }
    
    tools::texi2pdf(latex_file, clean = TRUE) # outputs to current working directory
    pdf_file <- sub('\\.tex$', '.pdf', basename(latex_file))
    if(!file.exists(pdf_file)) stop('Could not find texi2pdf output file "', pdf_file, '"')
    #if (! file.remove(latex_file)) warning('Could not remove intermediate TeX file "', latex_file, '"')
    # we overwrite existing files. If no explicit `file` argument was specified, confirm() has
    # already checked if this is OK, or has failed in non-interactive sessions:
    if(file.copy(pdf_file, file, overwrite = TRUE)) {
      file.remove(pdf_file)
    } else {
      stop('Could not copy pdf file to ', file, '. The pdf file remains at "', pdf_file, '"')
    }
    invisible(NULL)
  }
  
  ## this funcion will return a list of dataframes with all unique parameters given by pageBy
  getPageBy <- function(data, pageBy){
    dataList <- list()
    
    if(length(pageBy) == 1){
      page_len <- length(unique(data[,unlist(pageBy)]))
      for(x in 1:page_len){
        if(length(strsplit(as.character((unique(data[ ,unlist(pageBy) ]))[x]), " ")[[1]]) == 1){
          filter <- paste(unlist(pageBy), '==', as.character((unique(data[ ,unlist(pageBy) ]))[x]))
        } else {
          filter <- paste(unlist(pageBy), '==', paste0("'", as.character((unique(data[ ,unlist(pageBy) ]))[x]), "'"))
        }
        filter_tmp <- dplyr::filter_(data, filter)
        dataList[[x]] <- filter_tmp
      }
    } else if(length(pageBy) > 1){
      page_len <- nrow(unique(data[,unlist(pageBy)]))
      for(x in 1:page_len) {
        for(y in 1:length(unlist(pageBy))){
          if(y == 1){
            if(length(strsplit(as.character((unique(data[ ,unlist(pageBy) ])[y])[x,]), " ")[[1]]) == 1){
              filter <- paste(names(unique(data[ ,unlist(pageBy) ]))[y], '==', as.character((unique(data[ ,unlist(pageBy) ])[y])[x,]))
            } else {
              filter <- paste(names(unique(data[ ,unlist(pageBy) ]))[y], '==', paste0("'", as.character((unique(data[ ,unlist(pageBy) ])[y])[x,]), "'"))
            }
          } else {
            if(length(strsplit(as.character((unique(data[ ,unlist(pageBy) ])[y])[x,]), " ")[[1]]) == 1){
              filter <- paste(filter, "&", names(unique(data[ ,unlist(pageBy) ]))[y], '==', as.character((unique(data[ ,unlist(pageBy) ])[y])[x,]))
            } else {
              filter <- paste(filter, "&", names(unique(data[ ,unlist(pageBy) ]))[y], '==', paste0("'", as.character((unique(data[ ,unlist(pageBy) ])[y])[x,]), "'"))
            }
          } 
        }
        filter_tmp <- dplyr::filter_(data, filter)
        dataList[[x]] <- filter_tmp
      }
    }
    return(dataList)
  }
  
  ## this function will return the data frame formatting the values based groupBy parameter
  getGroupBy <- function(data, groupBy, nRows){
    tmp_alt <- data
    tmp_alt$ROWORDER <- 1:nrow(tmp_alt)
    
    if(length(groupBy) == 1){
      grp_len <- nrow(unique(dataOut[ ,unlist(groupBy) ]))
      
      for(l in 1:grp_len) {
        if(length(strsplit(as.character((unique(dataOut[ ,unlist(groupBy) ]))[l]), " ")[[1]]) == 1){
          filter <- paste(unlist(groupBy), '==', as.character((unique(dataOut[ ,unlist(groupBy) ]))[l]))
        } else {
          filter <- paste(unlist(groupBy), '==', paste0("'", as.character((unique(dataOut[ ,unlist(groupBy) ]))[l]), "'"))
        }
        filter_tmp <- dplyr::filter_(tmp_alt, filter)
        if(nrow(tmp_alt[tmp_alt$ROWORDER %in% filter_tmp$ROWORDER,]) >= 2 && nrow(tmp_alt[tmp_alt$ROWORDER %in% filter_tmp$ROWORDER,]) >= nrow(tmp_alt[tmp_alt$ROWORDER %in% filter_tmp$ROWORDER,])) {
          temp <- tmp_alt[tmp_alt$ROWORDER %in% filter_tmp$ROWORDER,][2:nrow(tmp_alt[tmp_alt$ROWORDER %in% filter_tmp$ROWORDER,]),]
          temp[(temp$ROWORDER %% nRows != 1), unlist(groupBy) ] <- rep("", length(temp[temp$ROWORDER %% nRows != 1,unlist(groupBy) ]))
          tmp_alt[tmp_alt$ROWORDER %in% filter_tmp$ROWORDER,][2:nrow(tmp_alt[tmp_alt$ROWORDER %in% filter_tmp$ROWORDER,]), ] <- temp
        }
      }
      dataOut <- tmp_alt[names(tmp_alt) != "ROWORDER"]
    } else if(length(groupBy) > 1){
      grp_len <- nrow(unique(dataOut[ ,unlist(groupBy) ]))
      
      for(l in 1:grp_len) {
        for(m in 1:length(unlist(groupBy))){
          if(m == 1){
            if(length(strsplit(as.character((unique(dataOut[ ,unlist(groupBy) ])[m])[l,]), " ")[[1]]) == 1){
              filter <- paste(names(unique(dataOut[ ,unlist(groupBy) ]))[m], '==', as.character((unique(dataOut[ ,unlist(groupBy) ])[m])[l,]))
            } else {
              filter <- paste(names(unique(dataOut[ ,unlist(groupBy) ]))[m], '==', paste0("'", as.character((unique(dataOut[ ,unlist(groupBy) ])[m])[l,]), "'"))
            }
          } else {
            if(length(strsplit(as.character((unique(dataOut[ ,unlist(groupBy) ])[m])[l,]), " ")[[1]]) == 1){
              filter <- paste(filter, "&", names(unique(dataOut[ ,unlist(groupBy) ]))[m], '==', as.character((unique(dataOut[ ,unlist(groupBy) ])[m])[l,]))
            } else {
              filter <- paste(filter, "&", names(unique(dataOut[ ,unlist(groupBy) ]))[m], '==', paste0("'", as.character((unique(dataOut[ ,unlist(groupBy) ])[m])[l,]), "'"))
            }
          } 
        }
        filter_tmp <- dplyr::filter_(tmp_alt, filter)
        if(nrow(tmp_alt[tmp_alt$ROWORDER %in% filter_tmp$ROWORDER,]) >= 2 && nrow(tmp_alt[tmp_alt$ROWORDER %in% filter_tmp$ROWORDER,]) >= nrow(tmp_alt[tmp_alt$ROWORDER %in% filter_tmp$ROWORDER,])) {
          temp <- tmp_alt[tmp_alt$ROWORDER %in% filter_tmp$ROWORDER,][2:nrow(tmp_alt[tmp_alt$ROWORDER %in% filter_tmp$ROWORDER,]),]
          for(n in 1:length(groupBy)){
            temp[(temp$ROWORDER %% nRows != 1), unlist(groupBy) ][[n]] <- rep("", length(temp[temp$ROWORDER %% nRows != 1,unlist(groupBy) ][[n]]))
          }
          tmp_alt[tmp_alt$ROWORDER %in% filter_tmp$ROWORDER,][2:nrow(tmp_alt[tmp_alt$ROWORDER %in% filter_tmp$ROWORDER,]), ] <- temp
        }
      }
      dataOut <- tmp_alt[names(tmp_alt) != "ROWORDER"]
    }
    return(dataOut)
  }
  
  ## this sets the character types for the huxtable object if appplicable
  setType <- function(data, type) {
    if(length(type) != 0){
      if(is.null(names(type))){
        stop("The column names specified in 'type' do not match the columns of the dataset")  
      } else {
        if(any(!names(type) %in% names(data))){
          stop("The specified columns in the column mappings for 'type' are not found in the the dataset")  
        } else {
          dataHux <- data
          for(i in 1:length(names(type))){
            tmp <- type[[ names(type)[i] ]]
            if(tolower(tmp) != "character" && tolower(tmp) != "numeric") {
              stop("The specified value in the column mappings for 'type' is invalid") 
            } else {
              if(tolower(tmp) == "character") {
                dataHux <- huxtable::set_number_format(dataHux, row = 1:nrow(data), col = names(type)[i], value = as.character(data[[ names(type)[i] ]]))
              } else if(tolower(tmp) == "numeric"){
                dataHux <- huxtable::set_number_format(dataHux, row = 1:nrow(data), col = names(type)[i], value = as.numeric(data[[ names(type)[i] ]]))
              }
            }
          }
          return(dataHux)
        }
      }
    } else {
      #DO NOTHING (Uses auto-generated column types)
      return(data)
    }
  }
  
  dataIn <- parseXMLFile(path)

  if(!is.null(columnNamesToDisplay) && length(columnToDisplay) != 0) {
    if(any(!columnToDisplay %in% names(dataIn))){
      stop("The specified columns in 'columnToDisplay' are not found in the the dataset")  
    } else {
      dataOut <- dataIn[names(dataIn) %in% columnToDisplay]
      dataOut <- dataOut[unlist(columnToDisplay)]
    }
  } else {
    #DO NOTHING (Display all columns)
    dataOut <- dataIn
  }
  
  if(length(columnNamesToDisplay) != 0){
    if(length(columnToDisplay) != 0 && length(columnNamesToDisplay) <= length(columnToDisplay)){
      if(is.null(names(columnNamesToDisplay))){
        stop("The column names specified in 'columnNamesToDisplay' do not match the column of the dataset")  
      } else {
        if(any(!(names(columnNamesToDisplay) %in% names(dataOut)))){
          stop("The specified columns in the column mappings for 'columnNamesToDisplay' are not found in the the dataset")  
        } 
      }
    } else {
      stop("There are more column names specified in 'columnNamesToDisplay' than the subsetted dataset")
    }
  } else {
    #DO NOTHING (Display original column names)
  }
  
  if(!is.null(orderBy) && length(orderBy) != 0) {
    if(orderBy %in% names(dataOut)){
      dataOut[order(dataOut[,unlist(orderBy)]),]
    }
  }
  
  if(!is.null(pageBy) && length(pageBy) != 0) {
    dataList <- getPageBy(dataOut, pageBy)
    dataOut <- lapply(dataList, getGroupBy, groupBy = groupBy, nRows = numOfRows)
  } else {
    if(!is.null(groupBy) && length(groupBy) != 0) {
      dataOut <- getGroupBy(dataOut, groupBy, numOfRows)
    }
  }
  
  if(!is.null(pageBy) && length(pageBy) != 0){
    dataHux <- lapply(dataOut, huxtable::as_hux)
    dataHux <- lapply(dataHux, setType, type = columnType)
  } else {
    dataHux <- huxtable::as_hux(dataOut) 
    dataHux <- setType(dataHux, columnType)
  }

  quick_pdf(dataHux, title = title, footnote = footnote, columnNamesToDisplay = columnNamesToDisplay, pageBy = pageBy, groupBy = groupBy, orderBy = orderBy, pageNum = pageNumber, dateGen = dateGeneration, n_row = numOfRows, orient = oriented, tableScale = tableScale, adjustWidth = leftMargin, scale = tableResize)
  
}  
