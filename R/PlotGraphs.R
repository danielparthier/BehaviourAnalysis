#' Colour scheme
#'
#' This function generates colourblind friendly schemes and returns them as hex-string.
#' @param Mode A string for the colour scheme ("dark", "light").
#' @param n An integer indicating length of output.
#'
#' @return Returns a vector string containing colours in hex-code.
#' 
#' @export
CustomColourPalette <- function(Mode = "dark", n = 5) {
  if(is.character(Mode) & length(Mode)==1) {
    switch(Mode,
           dark = {
             OutputCol <- c("#402747", "#903C93", "#FFC857", "#119DA4", "#19647E")
             colFun <- grDevices::colorRampPalette(colors = OutputCol)
           },
           light = {
             OutputCol <- c("#D7191C", "#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6")
             colFun <- grDevices::colorRampPalette(colors = OutputCol)
           },
           {
             warning(paste0("'",Mode, "' ", "is not a valid argument for 'Mode'.\nRevert to default: 'dark'"))
             OutputCol <- c("#402747", "#903C93", "#FFC857", "#119DA4", "#19647E")
             colFun <- grDevices::colorRampPalette(colors = OutputCol)
           }
    )
  } else {
    warning(paste0("'",paste(Mode, collapse = "' '"), "' ", "is not a valid argument for 'Mode'.\nRevert to default: 'dark'"))
    OutputCol <- c("#402747", "#903C93", "#FFC857", "#119DA4", "#19647E")
    colFun <- grDevices::colorRampPalette(colors = OutputCol)
  }
  return(colFun(n))
}


#' Plot Speed
#'
#' This function plots the speed of the object used as speed reference. When adding the x/y 
#' references a 2D-plot is drawn using colour-coding for the speed. If the x/y information 
#' is omitted the speed will be plotted against time.
#' @param CoordTable A data.table used as input.
#' @param Speed A string referencing the speed column.
#' @param CoordRef A string referencing coordinate columns.
#' @param ObjectTable Optional data.table with Objects.
#' @param Unit A string indicating the speed unit (default = "px/s").
#' @param FacetRef A string as reference for facet wrap (for multi-view).
#'
#' @return Returns a ggplot.
#' @export
SpeedPlot <- function(CoordTable,
                      Speed,
                      CoordRef = NULL,
                      ObjectTable = NULL,
                      Unit = "px/s",
                      FacetRef = NULL) {
  ObjectLoc <- NULL
  if(is.null(CoordRef)&is.character(Speed)) {
    OutputPlot <- ggplot2::ggplot(data = CoordTable, ggplot2::aes_string(x = "Time", y = Speed))+
      ggplot2::geom_line()+
      ggplot2::scale_x_continuous(expand = c(0,0))+
      ggplot2::scale_y_continuous(expand = c(0,0))+
      ggplot2::ylab(label = paste0("Speed (", Unit, ")"))+
      ggplot2::xlab(label = "Time (s)")+
      ggplot2::theme_classic()+
      ggplot2::theme(legend.title.align=0.5, plot.margin = ggplot2::margin(4, 10, 4, 4, "pt"))
    if(is.character(FacetRef)) {
      OutputPlot <- OutputPlot+ggplot2::facet_wrap(facets = FacetRef)
    }
    return(OutputPlot)
  } else if(is.character(CoordRef) & is.character(Speed)) {
    x <- paste0(CoordRef, "_x")
    y <- paste0(CoordRef, "_y")
    if(sum(grepl(x = names(CoordTable), pattern = paste0(c(x, y), collapse = "|")))!=2) {
      stop("CoordRef missmatch")
    }
    OutputPlot <- ggplot2::ggplot(data = CoordTable, ggplot2::aes_string(x = x, y = y, colour = Speed))+
      ggplot2::geom_path(size=1, lineend = "round", linejoin = "round", linemitre = 1)+
      ggplot2::scale_color_viridis_c()+
      ggplot2::labs(colour = paste0("Speed\n(", Unit, ")"))+
      ggplot2::theme_void()+
      ggplot2::theme(legend.title.align=0.5)
    if(is.data.table(ObjectTable)) {
      labels <- gsub("[^\\d]+", "", ObjectTable[,ObjectLoc], perl=TRUE)
      if(any(suppressWarnings(expr = is.na(as.integer(labels))))) {
        labels <- ObjectTable[,ObjectLoc,]
      }
        OutputPlot <- OutputPlot+
          ggplot2::geom_point(data = ObjectTable, ggplot2::aes(x = x, y = y), shape = 21, colour = "black", alpha = 0.5, size = 8, stroke = 2)+
          ggplot2::geom_text(data = ObjectTable, inherit.aes = F, ggplot2::aes(x = x, y = y, label = labels))
    }
    if(is.character(FacetRef)) {
      OutputPlot <- OutputPlot+ggplot2::facet_wrap(facets = FacetRef)
    }
    return(OutputPlot)
  } else {
    warning("Missing arguments")
  }
}

#' Plot Location
#'
#' This function plots the speed of the object used as speed reference. When adding the x/y 
#' references a 2D-plot is drawn using colour-coding for the speed. If the x/y information 
#' is omitted the speed will be plotted against time.
#' @param CoordTable A data.table used as input.
#' @param CoordRef A string referencing coordinate columns.
#' @param ObjectTable Optional data.table with Objects.
#' @param Density A bool indicating whether the density should be plotted.
#' @param BinNumber An integer indicating the number of bins for the density (default = 500).
#' @param FacetRef A string as reference for facet wrap (for multi-view).
#'
#' @return Returns a ggplot.
#' @export
LocationPlot <- function(CoordTable,
                         CoordRef = NULL,
                         ObjectTable = NULL,
                         Density = TRUE,
                         BinNumber = 500,
                         FacetRef = NULL) {
  ..density.. <- NULL
  ObjectLoc <- NULL
  if(is.character(CoordRef) & Density) {
    x <- paste0(CoordRef, "_x")
    y <- paste0(CoordRef, "_y")
    if(sum(grepl(x = names(CoordTable), pattern = paste0(c(x, y), collapse = "|")))!=2) {
      stop("CoordRef missmatch")
    }
    OutputPlot <- ggplot2::ggplot(data = CoordTable, ggplot2::aes_string(x = x, y = y))+
      ggplot2::stat_density_2d(geom = "raster", ggplot2::aes(fill = ..density..),contour = FALSE, n = c(BinNumber, BinNumber))+
      ggplot2::scale_fill_viridis_c()+
      ggplot2::labs(fill = "Density")+
      ggplot2::theme_void()+
      ggplot2::theme(legend.title.align=0.5, plot.margin = ggplot2::margin(4, 10, 4, 4, "pt"))
    if(is.data.table(ObjectTable)) {
      labels <- gsub("[^\\d]+", "", ObjectTable[,ObjectLoc], perl=TRUE)
      if(any(suppressWarnings(expr = is.na(as.integer(labels))))) {
        labels <- ObjectTable[,ObjectLoc,]
      }
        OutputPlot <- OutputPlot+
        ggplot2::geom_point(data = ObjectTable, ggplot2::aes(x = x, y = y), shape = 21, colour = "black", alpha = 0.5, size = 8, stroke = 2)+
        ggplot2::geom_text(data = ObjectTable, inherit.aes = F, ggplot2::aes(x = x, y = y, label = labels))
    }
    if(is.character(FacetRef)) {
      OutputPlot <- OutputPlot+ggplot2::facet_wrap(facets = FacetRef)
    }
    return(OutputPlot)
  } else if(is.character(CoordRef) & !Density) {
    x <- paste0(CoordRef, "_x")
    y <- paste0(CoordRef, "_y")
    if(sum(grepl(x = names(CoordTable), pattern = paste0(c(x, y), collapse = "|")))!=2) {
      stop("CoordRef missmatch")
    }
    OutputPlot <- ggplot2::ggplot(data = CoordTable, ggplot2::aes_string(x = x, y = y))+
      ggplot2::geom_path(size=1, lineend = "round", linejoin = "round", linemitre = 1, colour = "black")+
      ggplot2::theme_void()+
      ggplot2::theme(legend.title.align=0.5, plot.margin = ggplot2::margin(4, 10, 4, 4, "pt"))
    if(is.data.table(ObjectTable)) {
      labels <- gsub("[^\\d]+", "", ObjectTable[,ObjectLoc], perl=TRUE)
      if(any(suppressWarnings(expr = is.na(as.integer(labels))))) {
        labels <- ObjectTable[,ObjectLoc,]
      }
        OutputPlot <- OutputPlot+
        ggplot2::geom_point(data = ObjectTable, ggplot2::aes(x = x, y = y), shape = 21, colour = "black", alpha = 0.5, size = 8, stroke = 2)+
        ggplot2::geom_text(data = ObjectTable, inherit.aes = F, ggplot2::aes(x = x, y = y, label = labels))
    }
    if(is.character(FacetRef)) {
      OutputPlot <- OutputPlot+ggplot2::facet_wrap(facets = FacetRef)
    }
    return(OutputPlot)
  } else {
    warning("Missing arguments")
  }
}

#' Plot Distance
#'
#' This function plots the distance of the object used as distance reference. When adding the x/y 
#' references a 2D-plot is drawn using colour-coding for the distance. If the x/y information 
#' is omitted the distance will be plotted against time.
#' @param CoordTable A data.table used as input.
#' @param Distance A string referencing the distance column.
#' @param CoordRef A string referencing coordinate columns.
#' @param ObjectTable Optional data.table with Objects.
#' @param Unit A string indicating the speed unit (default = "px/s").
#' @param ObjectDistance A bool indicating if reference is linked to stationary object(s) (default = FALSE).
#' @param FacetRef A string as reference for facet wrap (for multi-view).
#' 
#' @return Returns a ggplot.
#' @export
DistancePlot <- function(CoordTable,
                      Distance,
                      CoordRef = NULL,
                      ObjectTable = NULL,
                      Unit = "px",
                      ObjectDistance = F,
                      FacetRef = NULL) {
  ObjectLoc <- NULL
  variable <- NULL
  ObjectName <- NULL
  Time <- NULL
  value <- NULL
  if(is.null(CoordRef) & is.character(Distance) & !ObjectDistance) {
    OutputPlot <- ggplot2::ggplot(data = CoordTable, ggplot2::aes_string(x = "Time", y = Distance))+
      ggplot2::geom_line()+
      ggplot2::scale_x_continuous(expand = c(0,0))+
      ggplot2::scale_y_continuous(expand = c(0,0))+
      ggplot2::ylab(label = paste0("Distance (", Unit, ")"))+
      ggplot2::xlab(label = "Time (s)")+
      ggplot2::theme_classic()+
      ggplot2::theme(legend.title.align=0.5, plot.margin = ggplot2::margin(4, 10, 4, 4, "pt"))
    if(is.character(FacetRef)) {
      OutputPlot <- OutputPlot+ggplot2::facet_wrap(facets = FacetRef)
    }
    return(OutputPlot)
  } else if(is.character(CoordRef) & is.character(Distance) & !ObjectDistance) {
    x <- paste0(CoordRef, "_x")
    y <- paste0(CoordRef, "_y")
    if(sum(grepl(x = names(CoordTable), pattern = paste0(c(x, y), collapse = "|")))!=2) {
      stop("CoordRef missmatch")
    }
    OutputPlot <- ggplot2::ggplot(data = CoordTable, ggplot2::aes_string(x = x, y = y, colour = Distance))+
      ggplot2::geom_path(size=1, lineend = "round", linejoin = "round", linemitre = 1)+
      ggplot2::scale_color_viridis_c()+
      ggplot2::labs(colour = paste0("Distance\n(", Unit, ")"))+
      ggplot2::theme_void()+
      ggplot2::theme(legend.title.align=0.5, plot.margin = ggplot2::margin(4, 10, 4, 4, "pt"))
    if(is.data.table(ObjectTable)) {
      labels <- gsub("[^\\d]+", "", ObjectTable[,ObjectLoc], perl=TRUE)
      if(any(suppressWarnings(expr = is.na(as.integer(labels))))) {
        labels <- ObjectTable[,ObjectLoc,]
      }
      OutputPlot <- OutputPlot+
        ggplot2::geom_point(data = ObjectTable, ggplot2::aes(x = x, y = y), shape = 21, colour = "black", alpha = 0.5, size = 8, stroke = 2)+
        ggplot2::geom_text(data = ObjectTable, inherit.aes = F, ggplot2::aes(x = x, y = y, label = labels))
    }
    if(is.character(FacetRef)) {
      OutputPlot <- OutputPlot+ggplot2::facet_wrap(facets = FacetRef)
    }
    return(OutputPlot)
  } else if(is.null(CoordRef) & is.character(Distance) & ObjectDistance & is.data.table(ObjectTable)) {
    ####################### work here
    tmpFrame <- melt(data = CoordTable, id="Time", measure = paste(ObjectTable[,ObjectLoc], Distance,sep = "_"))
    for(i in ObjectTable[,ObjectLoc]) {
      tmpFrame[grepl(x = variable, pattern = i), ObjectName:=paste(unlist(strsplit(i, split = "_")), collapse = " "),]
    }
  
    OutputPlot <- ggplot2::ggplot(data = tmpFrame, ggplot2::aes(x = Time, y = value, group = ObjectName, colour = ObjectName))+
      ggplot2::geom_line()+
      ggplot2::scale_x_continuous(expand = c(0,0))+
      ggplot2::scale_y_continuous(expand = c(0,0))+
      ggplot2::labs(colour = "Objects")+
      ggplot2::scale_color_viridis_d()+
      ggplot2::ylab(label = paste0("Distance (", Unit, ")"))+
      ggplot2::xlab(label = "Time (s)")+
      ggplot2::theme_classic()+
      ggplot2::theme(legend.title.align=0.5, plot.margin = ggplot2::margin(4, 10, 4, 4, "pt"))
    if(is.character(FacetRef)) {
      OutputPlot <- OutputPlot+ggplot2::facet_wrap(facets = FacetRef)
    }
    return(OutputPlot)
  } else {
    warning("Missing arguments")
  }
}

#' Plot Angle
#'
#' This function plots the speed of the object used as speed reference. When adding the x/y 
#' references a 2D-plot is drawn using colour-coding for the speed. If the x/y information 
#' is omitted the speed will be plotted against time.
#' @param CoordTable A data.table used as input.
#' @param Angle A string referencing the angle.
#' @param CoordRef A string referencing coordinate columns.
#' @param ObjectTable Optional data.table with Objects.
#' @param colourScheme A string indicating the colour scheme (default = "dark").
#' @param ObjectHighlight A string indicating if the reference object should be highlighted (default = "alpha", other options: "colour", "stroke", "none" or colour valid string).
#' @param FacetRef A string as reference for facet wrap (for multi-view).
#'
#' @return Returns a ggplot.
#' @export
AnglePlot <- function(CoordTable,
                      Angle,
                      CoordRef = NULL,
                      ObjectTable = NULL,
                      colourScheme = "dark",
                      ObjectHighlight = "alpha",
                      FacetRef = NULL) {
  Time <- NULL
  AngleVec <- NULL
  ObjectLoc <- NULL
  AngleShift <- NULL
  if(is.null(CoordRef) & is.character(Angle)) {
    tmpFrame <- data.table::data.table(AngleVec = CoordTable[[Angle]]*180/pi,Time = CoordTable[,Time])
    if(is.character(FacetRef)) {
      if(sum(FacetRef==colnames(CoordTable))==1) {
        tmpFrame[, eval(FacetRef) := CoordTable[,get(FacetRef)],,]
        tmpFrame[,AngleShift:=data.table::shift(AngleVec, n = -1),by=eval(FacetRef)]
        tmpFrame[,ColCut:=abs(AngleVec-AngleShift)<270,][is.na(ColCut), ColCut:=TRUE,]
        tmpFrame[,AngleShift:=NULL,][,ColCut:=as.integer(ColCut),]
      } else {
        message("Incorrect FacetRef. FacetRef will be ignored.")
      }
    } else {
      tmpFrame[,ColCut:=abs(AngleVec-data.table::shift(AngleVec, n = -1))<270,]
      tmpFrame[is.na(ColCut), ColCut:=TRUE,][,ColCut:=as.integer(ColCut),]
    }
    
    OutputPlot <- ggplot2::ggplot(data = tmpFrame, ggplot2::aes(x = Time, y = AngleVec, alpha = ColCut))+
      ggplot2::geom_line()+
      ggplot2::scale_alpha(range = c(0,1))+
      ggplot2::scale_x_continuous(expand = c(0,0))+
      ggplot2::scale_y_continuous(expand = c(0,0), limits = c(-180,180), breaks = c(-180, -90, 0, 90, 180))+
      ggplot2::ylab(label = "Angle (degrees)")+
      ggplot2::xlab(label = "Time (s)")+
      ggplot2::theme_classic()+
      ggplot2::theme(legend.position = "none", plot.margin = ggplot2::margin(4, 10, 4, 4, "pt"))
    if(is.character(FacetRef)) {
      OutputPlot <- OutputPlot+ggplot2::facet_wrap(facets = FacetRef)
    }
    return(OutputPlot)
  } else if(is.character(CoordRef) & is.character(Angle)) {
    x <- paste0(CoordRef, "_x")
    y <- paste0(CoordRef, "_y")
    if(sum(grepl(x = names(CoordTable), pattern = paste0(c(x, y), collapse = "|")))!=2) {
      stop("CoordRef missmatch")
    }
    if(is.null(FacetRef)) {
      tmpFrame <- data.table::data.table(AngleVec = CoordTable[[Angle]]*180/pi, x = CoordTable[[x]], y = CoordTable[[y]])
    } else {
      tmpFrame <- data.table::data.table(AngleVec = CoordTable[[Angle]]*180/pi, x = CoordTable[[x]], y = CoordTable[[y]])
      tmpFrame[, eval(FacetRef) := CoordTable[,get(FacetRef)],,]
    }
    OutputPlot <- ggplot2::ggplot(data = tmpFrame, ggplot2::aes(x = x, y = y, colour = AngleVec))+
      ggplot2::geom_path(size=1, lineend = "round", linejoin = "round", linemitre = 1)+
      ggplot2::labs(colour = "Angle\n(degrees)")+
      ggplot2::scale_color_gradientn(colours = CustomColourPalette(Mode = colourScheme, n = 5), limits = c(-180,180), breaks = c(-180, -90, 0, 90, 180))+
      ggplot2::theme_void()+
      ggplot2::theme(legend.title.align=0.5, plot.margin = ggplot2::margin(4, 10, 4, 4, "pt"))
    if(is.data.table(ObjectTable)) {
      labels <- gsub("[^\\d]+", "", ObjectTable[,ObjectLoc], perl=TRUE)
      if(any(suppressWarnings(expr = is.na(as.integer(labels))))) {
        labels <- ObjectTable[,ObjectLoc,]
      }
      targetObject <- sapply(ObjectTable[,ObjectLoc], function(x) { grepl(pattern = x, x = Angle)})
      if(any(grepl(ObjectHighlight, grDevices::colors()))) {
        chosenColour <- ObjectHighlight
        ObjectHighlight <- "customChoice"
      }
      if(!sum(grepl(pattern = ObjectHighlight, x = c("alpha", "colour", "stroke", "customChoice", "none"), fixed = TRUE))==1) {
        ObjectHighlight <- "alpha"
        message("No valid argument for 'ObjectHighlight'. Switch to default 'alpha'.")
      }
      switch (ObjectHighlight,
        "alpha" = {OutputPlot <- OutputPlot+
          ggplot2::geom_point(data = ObjectTable, ggplot2::aes(x = x, y = y),
                     shape = 21,
                     colour = "black",
                     alpha = ifelse(targetObject, 0.9, 0.5),
                     size = 8,
                     stroke = 2)+
          ggplot2::geom_text(data = ObjectTable, inherit.aes = F, ggplot2::aes(x = x, y = y, label = labels))},
        "colour" = {OutputPlot <- OutputPlot+
          ggplot2::geom_point(data = ObjectTable, ggplot2::aes(x = x, y = y),
                     shape = 21,
                     colour = ifelse(targetObject, "red", "black"),
                     alpha = 0.7,
                     size = 8,
                     stroke = 2)+
          ggplot2::geom_text(data = ObjectTable, inherit.aes = F, ggplot2::aes(x = x, y = y, label = labels))},
        "stroke" = {OutputPlot <- OutputPlot+
          ggplot2::geom_point(data = ObjectTable, ggplot2::aes(x = x, y = y),
                     shape = 21,
                     colour = "black",
                     alpha = 0.5,
                     size = 8,
                     stroke = ifelse(targetObject, 3, 2))+
          ggplot2::geom_text(data = ObjectTable, inherit.aes = F, ggplot2::aes(x = x, y = y, label = labels))},
        "none" = {OutputPlot <- OutputPlot+
          ggplot2::geom_point(data = ObjectTable, ggplot2::aes(x = x, y = y),
                     shape = 21,
                     colour = "black",
                     alpha = 0.5,
                     size = 8,
                     stroke = 2)+
          ggplot2::geom_text(data = ObjectTable, inherit.aes = F, ggplot2::aes(x = x, y = y, label = labels))},
        "customChoice" = {OutputPlot <- OutputPlot+
          ggplot2::geom_point(data = ObjectTable, ggplot2::aes(x = x, y = y),
                     shape = 21,
                     colour = ifelse(targetObject, chosenColour, "black"),
                     alpha = 0.7,
                     size = 8,
                     stroke = 2)+
          ggplot2::geom_text(data = ObjectTable, inherit.aes = F, ggplot2::aes(x = x, y = y, label = labels))}
      )
    }
    if(is.character(FacetRef)) {
      OutputPlot <- OutputPlot+ggplot2::facet_wrap(facets = FacetRef)
    }
    return(OutputPlot)
  } else {
    warning("Missing arguments")
  }
}

#' Plot Length
#'
#' This function plots the Length of the object used as Length reference. When adding the x/y 
#' references a 2D-plot is drawn using colour-coding for the Length If the x/y information 
#' is omitted the Length will be plotted against time. The length of a vector can be used to 
#' estimate rearing or other postures. 
#' @param CoordTable A data.table used as input.
#' @param Length A string referencing the Length column.
#' @param CoordRef A string referencing coordinate columns.
#' @param ObjectTable Optional data.table with Objects.
#' @param Unit A string indicating the speed unit (default = "px").
#' @param ColourFlip A bool indicating the whether colour scheme should be inverted (default = TRUE).
#' @param FacetRef A string as reference for facet wrap (for multi-view).
#' 
#' @return Returns a ggplot.
#' @export
LengthPlot <- function(CoordTable,
                      Length,
                      CoordRef = NULL,
                      ObjectTable = NULL,
                      Unit = "px",
                      ColourFlip = T,
                      FacetRef = NULL) {
  ObjectLoc <- NULL
  if(is.null(CoordRef)&is.character(Length)) {
    OutputPlot <- ggplot2::ggplot(data = CoordTable, ggplot2::aes_string(x = "Time", y = Length))+
      ggplot2::geom_line()+
      ggplot2::scale_x_continuous(expand = c(0,0))+
      ggplot2::scale_y_continuous(expand = c(0,0))+
      ggplot2::ylab(label = paste0("Length (", Unit, ")"))+
      ggplot2::xlab(label = "Time (s)")+
      ggplot2::theme_classic()+
      ggplot2::theme(legend.title.align=0.5, plot.margin = ggplot2::margin(4, 10, 4, 4, "pt"))
    if(is.character(FacetRef)) {
      OutputPlot <- OutputPlot+ggplot2::facet_wrap(facets = FacetRef)
    }
    return(OutputPlot)
  } else if(is.character(CoordRef) & is.character(Length)) {
    x <- paste0(CoordRef, "_x")
    y <- paste0(CoordRef, "_y")
    if(sum(grepl(x = names(CoordTable), pattern = paste0(c(x, y), collapse = "|")))!=2) {
      stop("CoordRef missmatch")
    }
    OutputPlot <- ggplot2::ggplot(data = CoordTable, ggplot2::aes_string(x = x, y = y, colour = Length))+
      ggplot2::geom_path(size=1, lineend = "round", linejoin = "round", linemitre = 1)+
      ggplot2::scale_color_viridis_c(direction = ifelse(test = ColourFlip, yes = -1, no = 1))+
      ggplot2::labs(colour = paste0("Length\n(", Unit, ")"))+
      ggplot2::theme_void()+
      ggplot2::theme(legend.title.align=0.5, plot.margin = ggplot2::margin(4, 10, 4, 4, "pt"))
    if(is.data.table(ObjectTable)) {
      labels <- gsub("[^\\d]+", "", ObjectTable[,ObjectLoc], perl=TRUE)
      if(any(suppressWarnings(expr = is.na(as.integer(labels))))) {
        labels <- ObjectTable[,ObjectLoc,]
      }
      OutputPlot <- OutputPlot+
        ggplot2::geom_point(data = ObjectTable, ggplot2::aes(x = x, y = y), shape = 21, colour = "black", alpha = 0.5, size = 8, stroke = 2)+
        ggplot2::geom_text(data = ObjectTable, inherit.aes = F, ggplot2::aes(x = x, y = y, label = labels))
    }
    if(is.character(FacetRef)) {
      OutputPlot <- OutputPlot+ggplot2::facet_wrap(facets = FacetRef)
    }
    return(OutputPlot)
  } else {
    warning("Missing arguments")
  }
}

### extend function to work for all plotting
ObjectHighlighting <- function(ObjectTable, ObjectHighlight, OutputPlot){
  ObjectLoc <- NULL
  x <- NULL
  y <- NULL
  labels <- gsub("[^\\d]+", "", ObjectTable[,ObjectLoc], perl=TRUE)
  if(any(suppressWarnings(expr = is.na(as.integer(labels))))) {
    labels <- ObjectTable[,ObjectLoc,]
  }
  targetObject <- sapply(ObjectTable[,ObjectLoc], function(x) { grepl(pattern = x, x = Angle)})
  if(any(grepl(ObjectHighlight, grDevices::colors()))) {
    chosenColour <- ObjectHighlight
    ObjectHighlight <- "customChoice"
  }
  if(!sum(grepl(pattern = ObjectHighlight, x = c("alpha", "colour", "stroke", "customChoice", "none"), fixed = TRUE))==1) {
    ObjectHighlight <- "alpha"
    message("No valid argument for 'ObjectHighlight'. Switch to default 'alpha'.")
  }
  switch (ObjectHighlight,
          "alpha" = {OutputPlot <- OutputPlot+
            ggplot2::geom_point(data = ObjectTable, ggplot2::aes(x = x, y = y),
                                shape = 21,
                                colour = "black",
                                alpha = ifelse(targetObject, 0.9, 0.5),
                                size = 8,
                                stroke = 2)+
            ggplot2::geom_text(data = ObjectTable, inherit.aes = F, ggplot2::aes(x = x, y = y, label = labels))},
          "colour" = {OutputPlot <- OutputPlot+
            ggplot2::geom_point(data = ObjectTable, ggplot2::aes(x = x, y = y),
                                shape = 21,
                                colour = ifelse(targetObject, "red", "black"),
                                alpha = 0.7,
                                size = 8,
                                stroke = 2)+
            ggplot2::geom_text(data = ObjectTable, inherit.aes = F, ggplot2::aes(x = x, y = y, label = labels))},
          "stroke" = {OutputPlot <- OutputPlot+
            ggplot2::geom_point(data = ObjectTable, ggplot2::aes(x = x, y = y),
                                shape = 21,
                                colour = "black",
                                alpha = 0.5,
                                size = 8,
                                stroke = ifelse(targetObject, 3, 2))+
            ggplot2::geom_text(data = ObjectTable, inherit.aes = F, ggplot2::aes(x = x, y = y, label = labels))},
          "none" = {OutputPlot <- OutputPlot+
            ggplot2::geom_point(data = ObjectTable, ggplot2::aes(x = x, y = y),
                                shape = 21,
                                colour = "black",
                                alpha = 0.5,
                                size = 8,
                                stroke = 2)+
            ggplot2::geom_text(data = ObjectTable, inherit.aes = F, ggplot2::aes(x = x, y = y, label = labels))},
          "customChoice" = {OutputPlot <- OutputPlot+
            ggplot2::geom_point(data = ObjectTable, ggplot2::aes(x = x, y = y),
                                shape = 21,
                                colour = ifelse(targetObject, chosenColour, "black"),
                                alpha = 0.7,
                                size = 8,
                                stroke = 2)+
            ggplot2::geom_text(data = ObjectTable, inherit.aes = F, ggplot2::aes(x = x, y = y, label = labels))}
  )
}