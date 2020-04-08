#' Colour scheme
#'
#' This function generates colourblind friendly schemes and returns them as hex-string.
#' @param Mode A string for the colour scheme ("dark", "light").
#' @param n An interger indicating length of output.
#'
#' @return Returns a vector string containing colours in hex-code.
#' @import ggplot2
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
#' references a 2D-plot is drawn using colour-coding for the speed. If the x/y inforamtion 
#' is omitted the speed will be plotted against time.
#' @param DataTable A data.table used as input.
#' @param Speed A string referencing the speed column.
#' @param x A string referencing the x-cordinate column.
#' @param y A string referencing the y-cordinate column.
#' @param ObjectTable Optional data.table with Objects.
#' @param Unit A string indicating the speed unit (default = "px/s").
#'
#' @return Returns a ggplot.
#' @export
SpeedPlot <- function(DataTable,
                      Speed,
                      x = NULL,
                      y = NULL,
                      ObjectTable = NULL,
                      Unit = "px/s") {
  if(is.null(x)&is.null(y)&is.character(Speed)) {
    OutputPlot <- ggplot(data = DataTable, aes_string(x = "Time", y = Speed))+
      geom_line()+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      ylab(label = paste0("Speed (", Unit, ")"))+
      xlab(label = "Time (s)")+
      theme_classic()+
      theme(legend.title.align=0.5, plot.margin = margin(4, 10, 4, 4, "pt"))
    return(OutputPlot)
  } else if(is.character(x) & is.character(y) & is.character(Speed)) {
    OutputPlot <- ggplot(data = DataTable, aes_string(x = x, y = y, colour = Speed))+
      geom_path(size=1, lineend = "round", linejoin = "round", linemitre = 1)+
      scale_color_viridis_c()+
      labs(colour = paste0("Speed\n(", Unit, ")"))+
      theme_void()+
      theme(legend.title.align=0.5)
    if(is.data.table(ObjectTable)) {
      labels <- unlist(lapply(X = strsplit(ObjectTable[,ObjectLoc], split = "_"), FUN = function(x){x[2]}))
        OutputPlot <- OutputPlot+
          geom_point(data = ObjectTable, aes(x = x, y = y), shape = 21, colour = "black", alpha = 0.5, size = 8, stroke = 2)+
          annotate("text", x = ObjectTable[,x], y = ObjectTable[,y], label = labels)
      }
    return(OutputPlot)
  } else {
    warning("Missing arguments")
  }
}

#' Plot Location
#'
#' This function plots the speed of the object used as speed reference. When adding the x/y 
#' references a 2D-plot is drawn using colour-coding for the speed. If the x/y inforamtion 
#' is omitted the speed will be plotted against time.
#' @param DataTable A data.table used as input.
#' @param x A string referencing the x-cordinate column.
#' @param y A string referencing the y-cordinate column.
#' @param ObjectTable Optional data.table with Objects.
#' @param Density A bool indicating whether the density should be plotted.
#' @param BinNumber An integer indicating the number of bins for the density (default = 500).
#'
#' @return Returns a ggplot.
#' @export
LocationPlot <- function(DataTable,
                         x = NULL,
                         y = NULL,
                         ObjectTable = NULL,
                         Density = TRUE,
                         BinNumber = 500) {
  if(is.character(x) & is.character(y) & Density) {
    OutputPlot <- ggplot(data = DataTable, aes_string(x = x, y = y))+
      stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE, n = c(BinNumber, BinNumber))+
      scale_fill_viridis_c()+
      labs(fill = "Density")+
      theme_void()+
      theme(legend.title.align=0.5, plot.margin = margin(4, 10, 4, 4, "pt"))
    if(is.data.table(ObjectTable)) {
      labels <- unlist(lapply(X = strsplit(ObjectTable[,ObjectLoc], split = "_"), FUN = function(x){x[2]}))
        OutputPlot <- OutputPlot+
        geom_point(data = ObjectTable, aes(x = x, y = y), shape = 21, colour = "black", alpha = 0.5, size = 8, stroke = 2)+
        annotate("text", x = ObjectTable[,x], y = ObjectTable[,y], label = labels)
    }
    return(OutputPlot)
  } else if(is.character(x) & is.character(y) & !Density) {
    OutputPlot <- ggplot(data = DataTable, aes_string(x = x, y = y))+
      geom_path(size=1, lineend = "round", linejoin = "round", linemitre = 1, colour = "black")+
      theme_void()+
      theme(legend.title.align=0.5, plot.margin = margin(4, 10, 4, 4, "pt"))
    if(is.data.table(ObjectTable)) {
      labels <- unlist(lapply(X = strsplit(ObjectTable[,ObjectLoc], split = "_"), FUN = function(x){x[2]}))
        OutputPlot <- OutputPlot+
        geom_point(data = ObjectTable, aes(x = x, y = y), shape = 21, colour = "black", alpha = 0.5, size = 8, stroke = 2)+
        annotate("text", x = ObjectTable[,x], y = ObjectTable[,y], label = labels)
    }
    return(OutputPlot)
  } else {
    warning("Missing arguments")
  }
}

#' Plot Distance
#'
#' This function plots the distance of the object used as distance reference. When adding the x/y 
#' references a 2D-plot is drawn using colour-coding for the distance. If the x/y inforamtion 
#' is omitted the distance will be plotted against time.
#' @param DataTable A data.table used as input.
#' @param Distance A string referencing the distance column.
#' @param x A string referencing the x-cordinate column.
#' @param y A string referencing the y-cordinate column.
#' @param ObjectTable Optional data.table with Objects.
#' @param Unit A string indicating the speed unit (default = "px/s").
#' @param ObjectDistance A bool indicating if reference is linked to stationary object(s) (default = FALSE).
#'
#' @return Returns a ggplot.
#' @export
DistancePlot <- function(DataTable,
                      Distance,
                      x = NULL,
                      y = NULL,
                      ObjectTable = NULL,
                      Unit = "px",
                      ObjectDistance = F) {
  if(is.null(x) & is.null(y) & is.character(Distance) & !ObjectDistance) {
    OutputPlot <- ggplot(data = DataTable, aes_string(x = "Time", y = Distance))+
      geom_line()+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      ylab(label = paste0("Distance (", Unit, ")"))+
      xlab(label = "Time (s)")+
      theme_classic()+
      theme(legend.title.align=0.5, plot.margin = margin(4, 10, 4, 4, "pt"))
    return(OutputPlot)
  } else if(is.character(x) & is.character(y) & is.character(Distance) & !ObjectDistance) {
    OutputPlot <- ggplot(data = DataTable, aes_string(x = x, y = y, colour = Distance))+
      geom_path(size=1, lineend = "round", linejoin = "round", linemitre = 1)+
      scale_color_viridis_C()+
      labs(colour = paste0("Distance\n(", Unit, ")"))+
      theme_void()+
      theme(legend.title.align=0.5, plot.margin = margin(4, 10, 4, 4, "pt"))
    if(is.data.table(ObjectTable)) {
      labels <- unlist(lapply(X = strsplit(ObjectTable[,ObjectLoc], split = "_"), FUN = function(x){x[2]}))
      OutputPlot <- OutputPlot+
        geom_point(data = ObjectTable, aes(x = x, y = y), shape = 21, colour = "black", alpha = 0.5, size = 8, stroke = 2)+
        annotate("text", x = ObjectTable[,x], y = ObjectTable[,y], label = labels)
    } 
    return(OutputPlot)
  } else if(is.null(x) & is.null(y) & is.character(Distance) & ObjectDistance & is.data.table(ObjectTable)) {
    ####################### work here
    tmpFrame <- melt(data = DataTable, id="Time", measure = paste(ObjectTable[,ObjectLoc], Distance,sep = "_"))
    for(i in ObjectTable[,ObjectLoc]) {
      tmpFrame[grepl(x = variable, pattern = i), ObjectName:=paste(unlist(strsplit(i, split = "_")), collapse = " "),]
    }
  
    OutputPlot <- ggplot(data = tmpFrame, aes(x = Time, y = value, group = ObjectName, colour = ObjectName))+
      geom_line()+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      labs(colour = "Objects")+
      scale_color_viridis_d()+
      ylab(label = paste0("Distance (", Unit, ")"))+
      xlab(label = "Time (s)")+
      theme_classic()+
      theme(legend.title.align=0.5, plot.margin = margin(4, 10, 4, 4, "pt"))
    return(OutputPlot)
  } else {
    warning("Missing arguments")
  }
}

#' Plot Angle
#'
#' This function plots the speed of the object used as speed reference. When adding the x/y 
#' references a 2D-plot is drawn using colour-coding for the speed. If the x/y inforamtion 
#' is omitted the speed will be plotted against time.
#' @param DataTable A data.table used as input.
#' @param x A string referencing the x-ccordinate column.
#' @param y A string referencing the y-ccordinate column.
#' @param ObjectTable Optional data.table with Objects.
#' @param colourScheme A string indicating the colour scheme (default = "dark").
#' @param ObjectHighlight A string indicating if the reference object should be highlighted (default = "alpha", other options: "colour", "stroke", "none" or colour valid string).
#'
#' @return Returns a ggplot.
#' @export
AnglePlot <- function(DataTable,
                      Angle,
                      x = NULL,
                      y = NULL,
                      ObjectTable = NULL,
                      colourScheme = "dark",
                      ObjectHighlight = "alpha") {
  if(is.null(x) & is.null(y) & is.character(Angle)) {
    tmpFrame <- data.frame(AngleVec = DataTable[[Angle]]*180/pi, Time = DataTable[,Time])
    OutputPlot <- ggplot(data = tmpFrame, aes(x = Time, y = AngleVec))+
      geom_line()+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0), limits = c(-180,180), breaks = c(-180, -90, 0, 90, 180))+
      ylab(label = "Angle (degrees)")+
      xlab(label = "Time (s)")+
      theme_classic()+
      theme(legend.title.align=0.5, plot.margin = margin(4, 10, 4, 4, "pt"))
    return(OutputPlot)
  } else if(is.character(x) & is.character(y) & is.character(Angle)) {
    tmpFrame <- data.frame(AngleVec = DataTable[[Angle]]*180/pi, x = DataTable[[x]], y = DataTable[[y]])
    OutputPlot <- ggplot(data = tmpFrame, aes(x = x, y = y, colour = AngleVec))+
      geom_path(size=1, lineend = "round", linejoin = "round", linemitre = 1)+
      labs(colour = "Angle\n(degrees)")+
      scale_color_gradientn(colours = CustomColourPalette(Mode = colourScheme, n = 5), limits = c(-180,180), breaks = c(-180, -90, 0, 90, 180))+
      theme_void()+
      theme(legend.title.align=0.5, plot.margin = margin(4, 10, 4, 4, "pt"))
    if(is.data.table(ObjectTable)) {
      labels <- unlist(lapply(X = strsplit(ObjectTable[,ObjectLoc], split = "_"), FUN = function(x){x[2]}))
      targetObject <- sapply(ObjectTable[,ObjectLoc], function(x) { grepl(pattern = x, x = Angle)})
      if(any(grepl(ObjectHighlight, colors()))) {
        chosenColour <- ObjectHighlight
        ObjectHighlight <- "colourChoice"
      }
      if(!sum(grepl(pattern = ObjectHighlight, x = c("alpha", "colour", "stroke", "colourChoice", "none")))==1) {
        ObjectHighlight <- "alpha"
        message("No valid argument for 'ObjectHighlight'. Switch to default 'alpha'.")
      }
      switch (ObjectHighlight,
        "alpha" = {OutputPlot <- OutputPlot+
          geom_point(data = ObjectTable, aes(x = x, y = y),
                     shape = 21,
                     colour = "black",
                     alpha = ifelse(targetObject, 0.9, 0.5),
                     size = 8,
                     stroke = 2)+
          annotate("text", x = ObjectTable[,x], y = ObjectTable[,y], label = labels)},
        "colour" = {OutputPlot <- OutputPlot+
          geom_point(data = ObjectTable, aes(x = x, y = y),
                     shape = 21,
                     colour = ifelse(targetObject, "red", "black"),
                     alpha = 0.7,
                     size = 8,
                     stroke = 2)+
          annotate("text", x = ObjectTable[,x], y = ObjectTable[,y], label = labels)},
        "stroke" = {OutputPlot <- OutputPlot+
          geom_point(data = ObjectTable, aes(x = x, y = y),
                     shape = 21,
                     colour = "black",
                     alpha = 0.5,
                     size = 8,
                     stroke = ifelse(targetObject, 3, 2))+
          annotate("text", x = ObjectTable[,x], y = ObjectTable[,y], label = labels)},
        "none" = {OutputPlot <- OutputPlot+
          geom_point(data = ObjectTable, aes(x = x, y = y),
                     shape = 21,
                     colour = "black",
                     alpha = 0.5,
                     size = 8,
                     stroke = 2)+
          annotate("text", x = ObjectTable[,x], y = ObjectTable[,y], label = labels)},
        "colourChoice" = {OutputPlot <- OutputPlot+
          geom_point(data = ObjectTable, aes(x = x, y = y),
                     shape = 21,
                     colour = ifelse(targetObject, chosenColour, "black"),
                     alpha = 0.7,
                     size = 8,
                     stroke = 2)+
          annotate("text", x = ObjectTable[,x], y = ObjectTable[,y], label = labels)}
      )
      }
    return(OutputPlot)
  } else {
    warning("Missing arguments")
  }
}

#' Plot Length
#'
#' This function plots the Length of the object used as Length reference. When adding the x/y 
#' references a 2D-plot is drawn using colour-coding for the Length If the x/y inforamtion 
#' is omitted the Length will be plotted against time. The length of a vector can be used to 
#' estimate rearing or other postures. 
#' @param DataTable A data.table used as input.
#' @param Length A string referencing the Length column.
#' @param x A string referencing the x-cordinate column.
#' @param y A string referencing the y-cordinate column.
#' @param ObjectTable Optional data.table with Objects.
#' @param Unit A string indicating the speed unit (default = "px").
#' @param ColourFlip A bool indicating the whether colour scheme should be inverted (default = TRUE).
#' @return Returns a ggplot.
#' @export
LengthPlot <- function(DataTable,
                      Length,
                      x = NULL,
                      y = NULL,
                      ObjectTable = NULL,
                      Unit = "px",
                      ColourFlip = T) {
  if(is.null(x)&is.null(y)&is.character(Length)) {
    OutputPlot <- ggplot(data = DataTable, aes_string(x = "Time", y = Length))+
      geom_line()+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      ylab(label = paste0("Length (", Unit, ")"))+
      xlab(label = "Time (s)")+
      theme_classic()+
      theme(legend.title.align=0.5, plot.margin = margin(4, 10, 4, 4, "pt"))
    return(OutputPlot)
  } else if(is.character(x) & is.character(y) & is.character(Length)) {
    OutputPlot <- ggplot(data = DataTable, aes_string(x = x, y = y, colour = Length))+
      geom_path(size=1, lineend = "round", linejoin = "round", linemitre = 1)+
      scale_color_viridis_c(direction = ifelse(test = ColourFlip, yes = -1, no = 1))+
      labs(colour = paste0("Length\n(", Unit, ")"))+
      theme_void()+
      theme(legend.title.align=0.5, plot.margin = margin(4, 10, 4, 4, "pt"))
    if(is.data.table(ObjectTable)) {
      labels <- unlist(lapply(X = strsplit(ObjectTable[,ObjectLoc], split = "_"), FUN = function(x){x[2]}))
      OutputPlot <- OutputPlot+
        geom_point(data = ObjectTable, aes(x = x, y = y), shape = 21, colour = "black", alpha = 0.5, size = 8, stroke = 2)+
        annotate("text", x = ObjectTable[,x], y = ObjectTable[,y], label = labels)
    }
    return(OutputPlot)
  } else {
    warning("Missing arguments")
  }
}
