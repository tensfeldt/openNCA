#' Generate Plot
#'
#' This function reads in a dataframe and creates a PDF document generating a plot. 
#' 
#' @details 
#' \itemize{
#'  \item 'data', 'x', 'y' and 'arrangeBy' by defauly are 'NULL'
#'  \item 'groupBy', 'pageBy' by default are an empty list ('list()')
#'  \item 'groupMethod' by default is 'NULL', but can be set to either 'mean' or 'median'
#'  \item 'groupType' and 'plotType by default are 'NULL', but can be either 'line' or 'point'
#'  \item 'colorBy', 'title', 'xLab', 'yLab', 'footnote' by default are 'NULL'
#'  \item 'yOrient' by default is 'FALSE' and will result in normal tickmark text
#'  \item 'outputFileName' by defualt is 'NULL' and will take the name of 'generate-plot.pdf'
#'  \item 'oriented' by default is 'FALSE' and will result in portrait
#'  \item Either 'groupType' or 'dataType' needs to be provided to generate the plot.
#'  \item If 'groupBy' is specified then both 'groupMethod' and 'groupType' also needs to be specified.
#' } 
#' 
#' @note 
#' Erros will be throw if invalid input for 'data', 'x', 'y', 'plotType', 'oriented' is passed.
#'
#' @param data The data frame object with the required data 
#' @param x The column that will be used for the x-coordinate 
#' @param y The column that will be used for the y-coordinate 
#' @param arrangeBy The column by which the dataset will be arranged or combined by
#' @param pageBy A list of columns by which the dataset will be paged by
#' @param groupBy A list of columns that will be used to group the dataset by
#' @param groupMethod A method by which the group by calculation should occur ('mean' or 'median')
#' @param groupType A list of the type of group by to perform ('line' and/or 'point')
#' @param plotType A list of the type of plot to display ('line' and/or 'point')
#' @param colorBy A column by which the dataset will be colored by
#' @param yOrient A boolean value that is used to change the orientation of the text of y-axis 
#' @param title The title of the plot 
#' @param xLab The label for the x-axis
#' @param yLab The label for the y-axis
#' @param footnote A string note that will be appended at the bottom of the plot in the PDF document
#' @param outputFileName The file name of the output file (include '.pdf')
#' @param oriented A boolean value that is used to determine whether PDF will output in portrait or landscape
#' @section Returns:
#' \strong{PDF} \cr 
#' \itemize{
#'  \item Plot generated with specified options 
#' }
#' @examples 
#' 
#' #Arguments
#' Data = plot_dataframe
#' X = "PKPTMS"
#' Y = "PKCNC"
#' GroupBy = "TREATXT"
#' GroupMethod = "mean"
#' GroupType = list("line", "point")
#' PlotType = list("line")
#' XLab = "Time"
#' YLab = "Concentration (mg/ml)"
#' Title = "Conc. vs Time"
#' Footnote = "This is a test footnote"
#' YOrient = TRUE
#' PageBy = "SUBJID"
#' LegendTitle = "Treatment"
#' Oriented = TRUE
#' 
#' #TEST CASE ONE
#' 
#' #Function call
#' generate_plot(data = Data,
#'   x = X,
#'   y = Y, 
#'   groupBy = GroupBy, 
#'   groupMethod = GroupMethod, 
#'   groupType = GroupType, 
#'   plotType = PlotType, 
#'   xLab = XLab, 
#'   yLab = YLab, 
#'   title = Title, 
#'   footnote = Footnote, 
#'   yOrient = YOrient, 
#'   pageBy = PageBy, 
#'   legendTitle = LegendTitle, 
#'   oriented = Oriented
#' )
#' 
#' Will add more test cases with outputs soon
#' 
#' @author
#' \itemize{
#'  \item Kevin McConnell \email{kmcconnell@rudrya.com}
#' } 
#' @export
generate_plot <- function(data = NULL, x = NULL, y = NULL, groupBy = NULL, groupMethod = NULL, groupType = list(), arrangeBy = NULL, pageBy = NULL, colorBy = NULL, plotType = list(), yOrient = FALSE, title = NULL, xLab = NULL, yLab = NULL, footnote = NULL, legendTitle = NULL, outputFileName = NULL, oriented = FALSE) {
  if(is.null(data)){
    stop("The dataframe provided for 'data' is not valid. Please try again!")  
  } else {
    dataIn <- data
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
  
  ## this function is needed to get the mean/median data 
  get_calc_data <- function(data = NULL, x = NULL, y = NULL, group = NULL, method = NULL) {
    data$GROUP <- NA 
    
    xVal <- unique(data[x])
    gVal <- unique(data[group])
    for(i in 1:nrow(xVal)){
      for(j in 1:nrow(gVal)){
        yVal <- data[data[x] == xVal[i,],][data[data[x] == xVal[i,],][group] == gVal[j,], y]
        if(tolower(method) == "mean") {
          groupVal <- mean(yVal)
        } else if(tolower(method) == "median") {
          groupVal <- median(yVal)
        }
        data[data[x] == xVal[i,],][data[data[x] == xVal[i,],][group] == gVal[j,],'GROUP'] <- rep(groupVal, length(yVal))
      }
    }
    return(data)
  }
  
  ## this is a list of approved shapes for geom_point 
  shapes <- c(19, 21, 17, 24, 15, 22, 18, 23, 25)
  
  ## this is a list of approved line types for geom_line 
  lines <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "F1", "4C88C488", "12345678")
  
  # set_number_format(test2, row = 1:nrow(test2), col = 'ID', value = as.character(dat1$ID))
  
  if(is.null(x) || !any(names(dataIn) == x)){
    stop("The column value specified in 'x' is not valid or does not exist in the data provided")  
  }
  if(is.null(y) || !any(names(dataIn) == y)){
    stop("The column value specified in 'y' is not valid or does not exist in the data provided")  
  }
  if(!is.null(arrangeBy) && !any(names(dataIn) == arrangeBy)){
    stop("The column value specified in 'arrangeBy' does not exist in the data provided")  
  }
  if(!is.null(pageBy) && !any(names(dataIn) == pageBy)){
    stop("The column value specified in 'pageBy' does not exist in the data provided")  
  }
  if(is.null(oriented) || (!is.null(oriented) && (oriented != TRUE && oriented != FALSE))){
    stop("The value specified in 'oriented' is invalid. Please specify a boolean value")  
  } else {
    if(isTRUE(oriented)){
      pdf_w <- 11
      pdf_h <- 8.5
      pdf_t <- 0.5
      pdf_r <- 0.5
      pdf_b <- 0.75
      pdf_l <- 0.5
    } else {
      pdf_w <- 8.5
      pdf_h <- 11
      pdf_t <- 1.5
      pdf_r <- 0.5
      pdf_b <- 1.75
      pdf_l <- 0.5
    }
  }
  
  if((is.null(plotType) || length(plotType) == 0) && (is.null(groupType) || length(groupType) == 0)) {
    stop("The specified values in 'plotType' or 'groupType are not valid. Please provide valid values for either 'plotType', 'groupType', or both.")  
  }
  
  if((!is.null(groupType) && length(groupType) > 0) && (!is.null(plotType) && length(plotType) > 0)) {
    validType <- list('point', 'line')
    if(any(!plotType %in% validType)){
      stop("The specified values in 'plotType' are not valid. Please provide valid values.")  
    } 
    if(any(!groupType %in% validType)){
      stop("The specified values in 'groupType' are not valid. Please provide valid values.")  
    } 
  } else if((!is.null(groupType) && length(groupType) > 0) || (!is.null(plotType) && length(plotType) > 0)) {
    if(!is.null(groupType) && length(groupType) > 0) {
      validType <- list('point', 'line')
      if(any(!groupType %in% validType)){
        stop("The specified values in 'groupType' are not valid. Please provide valid values.")  
      } 
    } 
    if(!is.null(plotType) && length(plotType) > 0) {
      validType <- list('point', 'line')
      if(any(!plotType %in% validType)){
        stop("The specified values in 'plotType' are not valid. Please provide valid values.")  
      } 
    }
  }
  
  if(!is.null(groupBy) && !any(names(dataIn) == groupBy)){
    stop("The column value specified in 'groupBy' does not exist in the data provided")  
  } else if (!is.null(groupBy)){
    if((is.null(groupMethod) || (!is.null(groupMethod) && ((tolower(groupMethod) != "mean") && (tolower(groupMethod) != "median"))))){
      stop("The value specified in 'groupMethod' is not provided or not valid")  
    } else {
      dataIn <- get_calc_data(dataIn, x = x, y = y, group = groupBy, method = groupMethod)
    }
  }
  
  if(!is.null(pageBy)){
    tmp_page <- unique(dataIn[pageBy])
    plotList <- list()
    for(p in 1:nrow(tmp_page)){
      dataTmp <- dataIn[dataIn[pageBy] == tmp_page[p,], ]
      
      if(!is.null(groupBy)){
        if(!is.null(arrangeBy) && !is.null(colorBy)){
          plot <- ggplot2::ggplot(dataTmp, ggplot2::aes_string(x = x, y = 'GROUP', group = arrangeBy, color = colorBy)) 
        } else if(!is.null(arrangeBy)){
          plot <- ggplot2::ggplot(dataTmp, ggplot2::aes_string(x = x, y = 'GROUP', group = arrangeBy)) 
        } else if(!is.null(colorBy)){
          plot <- ggplot2::ggplot(dataTmp, ggplot2::aes_string(x = x, y = 'GROUP', group = groupBy, color = colorBy)) 
        } else {
          plot <- ggplot2::ggplot(dataTmp, ggplot2::aes_string(x = x, y = 'GROUP', group = groupBy)) 
        }
      } else if(!is.null(arrangeBy) && !is.null(colorBy)){
        plot <- ggplot2::ggplot(dataTmp, ggplot2::aes_string(x = x, y = y, group = arrangeBy, color = colorBy)) 
      } else if(!is.null(arrangeBy)){
        plot <- ggplot2::ggplot(dataTmp, ggplot2::aes_string(x = x, y = y, group = arrangeBy))
      } else if(!is.null(colorBy)){
        plot <- ggplot2::ggplot(dataTmp, ggplot2::aes_string(x = x, y = y))
      }
      
      if('point' %in% groupType){
        plot <- plot + ggplot2::geom_point(ggplot2::aes_string(shape = groupBy), size = 3) + ggplot2::scale_shape_manual(values = shapes[1:nrow(unique(data[groupBy]))])
      } else if('point' %in% plotType){
        plot <- plot + ggplot2::geom_point()
      }
      
      if('line' %in% groupType){
        plot <- plot + ggplot2::geom_line(ggplot2::aes_string(lty = groupBy)) + ggplot2::scale_linetype_manual(values = lines[1:nrow(unique(data[groupBy]))])
      } else if('line' %in% plotType){
        plot <- plot + ggplot2::geom_line()
      } 
      
      if(isTRUE(yOrient)) {
        plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
      }
      if(!is.null(title)){
        plot_t <- as.character(title)
      } else {
        plot_t <- paste0(x, " vs ", y)
      }
      if(!is.null(xLab)){
        plot_x <- as.character(xLab)
      } else {
        plot_x <- x
      }
      if(!is.null(yLab)){
        plot_y <- as.character(yLab)
      } else {
        plot_y <- y
      }
      if(!is.null(footnote)){
        plot_f <- as.character(footnote)
      } else {
        plot_f <- ""
      }
      if(!is.null(legendTitle)){
        plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(title = legendTitle))
      }
      header <- paste0(pageBy, " = ", tmp_page[p,])
      
      plot <- plot + ggplot2::labs(title = plot_t,
                                   subtitle = header,
                                   caption = plot_f,
                                   x = plot_x,
                                   y = plot_y)
      
      plot <- plot +  ggplot2::theme(axis.title.x = ggplot2::element_text(vjust = 1), 
                                     plot.margin = ggplot2::margin(pdf_t, pdf_r, pdf_b, pdf_l, "inch"),
                                     panel.background = ggplot2::element_rect(fill = "white", colour = "white"), 
                                     axis.line = ggplot2::element_line(size = 0.5, colour = "black"),
                                     legend.position="bottom",
                                     legend.box = "horizontal",
                                     legend.background = ggplot2::element_rect(fill="white",
                                                                               size=0.5, linetype="solid", 
                                                                               colour ="black"))
      
      if((max(unique(dataTmp[x]))%%8) == 0){
        max_tick_x <- max(unique(dataTmp[x])) + 8
      } else {
        max_tick_x <- max(unique(dataTmp[x])) + (max(unique(dataTmp[x]))%%8) 
      }
      plot <- plot + ggplot2::scale_x_continuous(breaks = seq(from = 0, to = max_tick_x, by = 8)) + ggplot2::geom_vline(xintercept = max_tick_x, alpha = 0.0)
      
      #max_tick_y <- (max(unique(dataTmp[y]), na.rm = FALSE) + ((max(unique(dataTmp[y]), na.rm = FALSE) - min(unique(dataTmp[y]), na.rm = FALSE))/nrow(unique(dataTmp[y]))))
      #print(nrow(unique(dataTmp[y])))
      #plot <- plot + ggplot2::scale_y_continuous(limits = c(min(unique(dataTmp[y]), na.rm = FALSE), ceiling(max_tick_y)))
      
      plotList[[p]] <- plot
    }
    
    for(l in 1:length(plotList)){
      if(l == 1){
        if(!is.null(outputFileName) &&  grepl("\\.pdf", outputFileName)){
          grDevices::pdf(file = paste0(getwd(),"/", outputFileName), width = pdf_w, height = pdf_h)  
        } else {
          grDevices::pdf(file = paste0(getwd(),"/generate_plot.pdf"), width = pdf_w, height = pdf_h)  
        }
      } else if(l == length(plotList)){
        invisible(grDevices::dev.off())
      } else {
        print(plotList[[l]])
      }
    }
  } else {
    if(!is.null(groupBy)){
      if(!is.null(arrangeBy) && !is.null(colorBy)){
        plot <- ggplot2::ggplot(dataIn, ggplot2::aes_string(x = x, y = 'GROUP', group = arrangeBy, color = colorBy)) 
      } else if(!is.null(arrangeBy)){
        plot <- ggplot2::ggplot(dataIn, ggplot2::aes_string(x = x, y = 'GROUP', group = arrangeBy)) 
      } else if(!is.null(colorBy)){
        plot <- ggplot2::ggplot(dataIn, ggplot2::aes_string(x = x, y = 'GROUP', group = groupBy, color = colorBy)) 
      } else {
        plot <- ggplot2::ggplot(dataIn, ggplot2::aes_string(x = x, y = 'GROUP', group = groupBy)) 
      }
    } else if(!is.null(arrangeBy) && !is.null(colorBy)){
      plot <- ggplot2::ggplot(dataIn, ggplot2::aes_string(x = x, y = y, group = arrangeBy, color = colorBy)) 
    } else if(!is.null(arrangeBy)){
      plot <- ggplot2::ggplot(dataIn, ggplot2::aes_string(x = x, y = y, group = arrangeBy))
    } else if(!is.null(colorBy)){
      plot <- ggplot2::ggplot(dataIn, ggplot2::aes_string(x = x, y = y))
    }
    
    if('point' %in% groupType){
      plot <- plot + ggplot2::geom_point(ggplot2::aes_string(shape = groupBy), size = 3) + ggplot2::scale_shape_manual(values = shapes[1:nrow(unique(data[groupBy]))])
    } else if('point' %in% plotType){
      plot <- plot + ggplot2::geom_point()
    }
    
    if('line' %in% groupType){
      plot <- plot + ggplot2::geom_line(ggplot2::aes_string(lty = groupBy)) + ggplot2::scale_linetype_manual(values = lines[1:nrow(unique(data[groupBy]))])
    } else if('line' %in% plotType){
      plot <- plot + ggplot2::geom_line()
    } 
    
    if(isTRUE(yOrient)) {
      plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    }
    if(!is.null(title)){
      plot_t <- as.character(title)
    } else {
      plot_t <- paste0(x, " vs ", y)
    }
    if(!is.null(xLab)){
      plot_x <- as.character(xLab)
    } else {
      plot_x <- x
    }
    if(!is.null(yLab)){
      plot_y <- as.character(yLab)
    } else {
      plot_y <- y
    }
    if(!is.null(footnote)){
      plot_f <- as.character(footnote)
    } else {
      plot_f <- ""
    }
    if(!is.null(legendTitle)){
      plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(title = legendTitle))
    }
    
    plot <- plot + ggplot2::labs(title = plot_t,
                                 caption = plot_f,
                                 x = plot_x,
                                 y = plot_y)
    
    plot <- plot +  ggplot2::theme(axis.title.x = ggplot2::element_text(vjust = 1), 
                                   plot.margin = ggplot2::margin(pdf_t, pdf_r, pdf_b, pdf_l, "inch"),
                                   panel.background = ggplot2::element_rect(fill = "white", colour = "white"), 
                                   axis.line = ggplot2::element_line(size = 0.5, colour = "black"),
                                   legend.position="bottom",
                                   legend.box = "horizontal",
                                   legend.background = ggplot2::element_rect(fill="white",
                                                                             size=0.5, linetype="solid", 
                                                                             colour ="black")) 
    if((max(unique(dataIn[x]))%%8) == 0){
      max_tick_x <- max(unique(dataIn[x])) + 8
    } else {
      max_tick_x <- max(unique(dataIn[x])) + (max(unique(dataIn[x]))%%8) 
    }
    plot <- plot + ggplot2::scale_x_continuous(breaks = seq(from = 0, to = max_tick_x, by = 8)) + ggplot2::geom_vline(xintercept = max_tick_x, alpha = 0.0)
    
    
    if(!is.null(outputFileName) &&  grepl("\\.pdf", outputFileName)){
      grDevices::pdf(file = paste0(getwd(),"/", outputFileName), width = pdf_w, height = pdf_h)  
    } else {
      grDevices::pdf(file = paste0(getwd(),"/generate_plot.pdf"), width = pdf_w, height = pdf_h)  
    }
    print(plot)
    invisible(grDevices::dev.off())
  }
}
