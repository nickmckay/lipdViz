# Timeseries plots: lines, ensemble lines, ensemble ribbons, and stacks.


#' @family plot
#' @export
#' @author Nick McKay
#' @title Plot or add a line to plot
#' @description Plots or adds a line to aplot
#' @import ggplot2
#' @param X A LiPD variable list to plot, including values, units, names, and more
#' @param Y A LiPD variable list to plot, including values, units, names, and more
#' @param color Line color (following ggplot rules)
#' @param alp Line transparency
#' @param add.to.plot A ggplot object to add these lines to. Default is ggplot() . 
#' @return A ggplot object
plotLine = function(add.to.plot=ggplot(),X,Y,color="black",alp = 1){
  
  #X and Y and are LiPD variable list, including values, units, names, etc...
  df = data.frame(x = X$values, y = Y$values)
  plot = add.to.plot+ geom_line(data=df,aes(x=x,y=y),color =color, alpha = alp)+
    ylab(axisLabel(Y))+
    geoChronRPlotTheme()
  
  if(grepl("AD",X$units) | grepl("CE",X$units)){
    plot = plot+scale_x_continuous(name = axisLabel(X))
  }else{
    plot = plot+scale_x_reverse(name = axisLabel(X))
  }
  return(plot)
}


#' @export
#' @family plot
#' @author Nick McKay
#' @title Plot an ensemble timeseries as a set of lines
#' @description Plot an ensemble timeseries as a set of lines. Useful for displaying a handful of ensemble members to characterize individual paths. 
#' @import ggplot2 dplyr RColorBrewer
#' @importFrom tidyr pivot_longer
#'
#' @param X A LiPD variable list to plot, including values, units, names, and more
#' @param Y A LiPD variable list to plot, including values, units, names, and more
#' @param color Either 1) A line color (following ggplot rules) to use for all lines (e.g., "blue"), 2) An RColorBrewer pallette to repeat over the lines (e.g. "Blues") or 3) a vector specifying the color for all lines (e.g., c("red","white","blue"))
#' @param n.ens.plot Whats the maximum number of lines to plot?
#' @param alp Line transparency
#' @param na.rm Remove NAs from X and Y? Set to FALSE to preserve line breaks where data are missing. (default = TRUE)
#' @param add.to.plot A ggplot object to add these lines to. Default is ggplot() . 
#'
#' @return A ggplot object
plotTimeseriesEnsLines = function(add.to.plot=ggplot(),
                                  X,
                                  Y,
                                  alp=.2,
                                  color = "blue",
                                  n.ens.plot=100,
                                  na.rm = TRUE){
  #check to see if time and values are "column lists"
  
  oX = X
  oY = Y
  if(is.list(X)){X=as.data.frame(X$values)}
  if(is.list(Y)){Y=as.data.frame(Y$values)}
  
  
  
  X=as.data.frame(X)
  Y=as.data.frame(Y)
  
  if(nrow(X)!=nrow(Y)){
    stop("X and Y must have the same number of observations")
  }
  
  np = min(n.ens.plot,ncol(X)*ncol(Y))
  #sample randomly what to plot
  pX = sample.int(ncol(X),size = np,replace = TRUE)
  pXn <- paste0("x",pX)
  pY = sample.int(ncol(Y),size = np,replace = TRUE)
  pYn <- paste0("Y",pY)
  
  Xs <- X[,pX]
  colnames(Xs) <- pXn
  Ys <- Y[,pY]
  colnames(Ys) <- pYn
  
  Xplot <- tidyr::pivot_longer(Xs,cols = everything(),names_to = "xEns",values_to = "x")
  Yplot <- tidyr::pivot_longer(Ys,cols = everything(),names_to = "yEns",values_to = "y")
  
  dfXY <- dplyr::bind_cols(Xplot,Yplot)
  dfXY <- dfXY[order(dfXY$xEns), ]
  
  if(na.rm){
    dfXY <- dfXY[!is.na(dfXY$x) & !is.na(dfXY$y), ]
  }
  
  #deal with colors
  if(color %in% rownames(RColorBrewer::brewer.pal.info)){#
    #then it's an RColorBrewer pallette
    colorScale <- rep_len(suppressWarnings(RColorBrewer::brewer.pal(n = np,name = color)),length.out = np)
  }else{#it's not
    if(length(color) == 1){#apply one color to all
      colorScale <- rep(color,times = np)
    }else{
      if(length(color) == np){
        colorScale <- color
      }else{
        stop("color must be either 1) a single color to repeated, 2) an RColorBrewer palette or 3) a string the same length of the number of lines to be plotted")
      }
    }
  }
  
  linePlot = add.to.plot+
    geom_path(data=dfXY,
              stat = "identity",
              aes(x=x,y=y,color = xEns),
              alpha=alp)+
    scale_color_manual(values = colorScale)+
    geoChronRPlotTheme()+
    theme(legend.position = "none")
  
  #add labels
  linePlot = linePlot+xlab(axisLabel(oX))+ylab(axisLabel(oY))
  
  #reverse the xaxis if the units are BP
  if(any(grepl(pattern = "BP",x = axisLabel(oX))) | (grepl(pattern = "ka",x = axisLabel(oX))) | (grepl(pattern = "B2k",x = axisLabel(oX))) | (grepl(pattern = "kyr",x = axisLabel(oX)))){
    linePlot = linePlot + scale_x_reverse(name = axisLabel(oX))
  }
  
  
  
  return(linePlot)
  
}
#' @export
#' @family plot
#' @author Nick McKay
#' @title Plot an ensemble timeseries as ribbons of probabilities
#' @description Plot an ensemble timeseries as a set of bands of probability. Useful for displaying the full range of probability across ensemble members.
#' @import ggplot2 tibble
#' @param X A LiPD variable list to plot, including values, units, names, and more
#' @param Y A LiPD variable list to plot, including values, units, names, and more
#' @param probs a vector of probabilities to plot as ribbons. It will create bands as ribbons of quantiles moving inward. If there's an odd number, it plots the middle quantile as a line. 
#' @param color.low Color of the outermost band; the extreme quantiles of the distribution
#' @param color.high Color of the innermost band; the central quantiles of the distribution
#' @param color.line Line color (following ggplot rules)
#' @param color.vector A vector (of length equal to the number of bands) that specifies the colors for the ribbons from the outermost band in (default = NA). Colors specified as string according to ggplot2 conventions. If present, this overrules color.high and color.low
#' @param line.width Width of the line
#' @param n.bins number bins over which to calculate intervals. Used to calculate x.bin if not provided.
#' @param x.bin vector of bin edges over which to bin.
#' @param y.bin vector of bin edges over which to bin.
#' @param alp alpha (transparency) parameter for the ribbons
#' @param add.to.plot A ggplot object to add this plot to. Default is ggplot() . 
#' @param export.quantiles If TRUE, return the plotted quantiles rather than the plot
#' @inheritDotParams quantile2d
#' @return A ggplot object OR list of plotted quantiles, depending on export.quantiles
plotTimeseriesEnsRibbons = function(add.to.plot=ggplot(),
                                    X,
                                    Y,
                                    alp=1,
                                    probs=c(0.025,.25,.5,.75,.975),
                                    x.bin=NA,
                                    y.bin=NA,
                                    n.bins=200,
                                    color.low="white",
                                    color.high="grey70",
                                    color.line="Black",
                                    color.vector = NA,
                                    line.width=1,
                                    export.quantiles = FALSE,
                                    ...){
  
  
  #check to see if time and values are "column lists"
  oX = X
  oY = Y
  if(is.list(X)){X=as.data.frame(X$values)}
  if(is.list(Y)){Y=as.data.frame(Y$values)}
  
  X=as.matrix(X)
  Y=as.matrix(Y)
  
  #check to make sure that at least one is a matrix
  if(ncol(X)==1 & ncol(Y)==1){
    #then just plot a line
    df = data.frame(x=X,y=Y)
    bandPlot=add.to.plot+geom_line(data=df,aes(x=X,y=Y),color=color.line)+geoChronRPlotTheme()
    
  }else{
    
    if(nrow(X)!=nrow(Y)){
      stop("X and Y must have the same number of observations")
    }
    
    ###DEPRECATED - old method.
    # binned = bin2d(X,Y,x.bin=x.bin,y.bin = y.bin,n.bins=n.bins)
    # binned = kde2d(X,Y,x.bin=x.bin,y.bin = y.bin,n.bins=n.bins)
    # find cum sum probabilities  
    
    
    # #nbox = prod(dim(binned$density))
    # colSums = apply(binned$density,2,sum)
    # colCount = colSums*nbox
    # good.cols = which(colCount>nrow(binned$density))
    # sumMat= t(matrix(colSums, nrow=length(colSums),ncol=nrow(binned$density)))
    # bmcs = apply(binned$density/sumMat,2,cumsum)
    # good.cols = which(!apply(is.na(bmcs),2,all) & scale(colSums)>-3)
    
    # probMat = matrix(data = NA,nrow=length(good.cols),ncol=length(probs))
    
    # for(p in 1:length(probs)){
    #   probMat[,p]=apply(bmcs[,good.cols],MARGIN=2,function(x) approx(x,binned$y.bin,probs[p],method = "constant")$y)
    # }
    ###END DEPRECATED - old method.
    
    probMatList <- quantile2d(X,
                              Y,
                              n.bins = n.bins,
                              x.bin = x.bin,
                              probs = probs,
                              ...)
    
    if(export.quantiles){
      probMat <- cbind(probMatList$x.bin,probMatList$quants)
      colnames(probMat) <- c("ages",as.character(probs))
      probMat <- tibble::as_tibble(probMat)
      return(probMat)
    }
    
    
    
    probMat  <-  probMatList$quants
    
    probMat=as.data.frame(probMat)
    
    #Line labels are deprecated
    
    # line.labels=as.character(probs)
    
    
    # #make labels better
    # goodName= c("-2 sigma","-1 sigma","Median","1  sigma","2  sigma")
    # realProb= c(pnorm(-2:2))
    # for(i in 1:length(line.labels)){
    #   p=which(abs(as.numeric(line.labels[i])-realProb)<.001)
    #   if(length(p)==1){
    #     line.labels[i]=goodName[p]
    #   }
    # }
    # names(probMat) = line.labels
    
    #plot it!
    #make pairs of bands moving in 
    
    #if probs is odd, the center one is just a line
    if(ncol(probMat)%%2==1){
      center = data.frame(x=probMatList$x.bin,y=probMat[,median(1:length(probs))])
      
      bandMat =  probMat[,-median(1:length(probs))]
    }else{
      center =NA
      bandMat =  probMat
    }
    
    #if the colors are specified, use the that
    if(!all(is.na(color.vector))){
      if(length(color.vector) != ncol(bandMat)/2){
        stop("The number of colors provided in color.vector does not match the number of bands")
      }
      fillCol <- color.vector
    }else{ #use a ramp
      fillCol=colorRampPalette(c(color.low,color.high))( ncol(bandMat)/2+1 )[-1]
    }
    
    for(b in 1:(ncol(bandMat)/2)){
      if(b==1){
        bandPlot = add.to.plot+geoChronRPlotTheme()
      }
      bands=data.frame(x=probMatList$x.bin,
                       ymin = bandMat[,b],
                       ymax = bandMat[,ncol(bandMat)-b+1])
      
      bandPlot = bandPlot+
        geom_ribbon(data=bands,aes(x=x,ymin=ymin,ymax=ymax),fill=fillCol[b],alpha=alp)
    }
    
    if(!all(is.na(center))){
      bandPlot <- bandPlot +
        geom_line(data=center,aes(x=x,y=y),color=color.line,linewidth=line.width)
    }
    
  }
  
  #add labels
  bandPlot = bandPlot+xlab(axisLabel(oX))+ylab(axisLabel(oY))
  
  #reverse the xaxis if the units are BP
  if(any(grepl(pattern = "BP",x = axisLabel(oX))) | (grepl(pattern = "ka",x = axisLabel(oX))) | (grepl(pattern = "B2k",x = axisLabel(oX))) | (grepl(pattern = "kyr",x = axisLabel(oX)))){
    bandPlot = bandPlot + scale_x_reverse(axisLabel(oX))
  }
  
  
  return(bandPlot)
  
}

#' @export
#' @title Plot a bunch of timeseries in a vertical stack
#' @description Creates a stack of timeseries plots
#' @import ggplot2
#' @importFrom ggridges geom_ridgeline theme_ridges
#' @import dplyr
#' @import RColorBrewer
#' @import grDevices
#' @import scales
#' @param plot.df A tidy data.frame, typically the output of tidyTs()
#' @param time.var Which variable to put on the x-axis. Must be in plot.df. Typically "year", "age", or "depth"
#' @param color.var Which variable to color the timeseries by. The default ("paleoData_TSid") will give each timeseries it's own color. Common other options include "paleoData_variable", "archiveType", or "paleoData_units", but any variable in plot.df should work.
#' @param invert.var Which variable to use to invert the timeseries. This should point to a variable of "positive" and "negative" (searches on "neg"), or a vector of 1s and -1s. (default = NA, which flips nothing)
#' @param fill.alpha Transparency of the shading
#' @param scale.factor Controls how much the timeseries should overlap, with larger numbers overlapping more. (default = 1/3)
#' @param scale.height Controls how large the y-axes will be. 1 is equivalent to end-to-end coverage with no space. (default = 0.75)
#' @param lab.buff Fraction of the x axis to space the tick marks away from the axes bars (default = 0.02)
#' @param lab.size Font size for the ylabels
#' @param line.size thickness of the line (default = 0.5)
#' @param color.ramp Specify the colors to use in the plot arranged along color.var. You can do this as single color as a character that will be repeated, as a vector of characters, or a function that creates colors given nColors input  (default = NA, which becomes RColorBrewer::brewer.pal(nColors,"Dark2"))
#' @param lab.space Multiplier on lab.buff for the axis label separation from the y-scale
#' @return A ggplot object of the plot
#' @family plot
#' @author Nick McKay
#' @section Long-form example:
#' \href{http://nickmckay.github.io/GeoChronR/articles/PlotTimeseriesStack.html}{View a full-fledged example of how to use this function.} 
plotTimeseriesStack <- function(plot.df,
                                time.var = "year", 
                                color.var = "paleoData_TSid", 
                                invert.var = NA,
                                fill.alpha = 0.2, 
                                line.size = 0.5,
                                scale.factor = 1/3,
                                scale.height = .75, 
                                lab.buff = 0.02, 
                                lab.size = 3,  
                                lab.space= 2,
                                color.ramp = NA){

  #convert a TS list or wide TS tibble (one row per timeseries, list columns) into a long, tidy data.frame
  if(!is.data.frame(plot.df)){
    plot.df <- lipdR::tidyTs(plot.df,age.var = time.var)
  }else if(is.list(plot.df$paleoData_values)){
    plot.df <- lipdR::tidyTs(purrr::transpose(dplyr::ungroup(plot.df)),age.var = time.var)
  }

  #flatten single column matrices into vectors, ggplot2 >= 4 will not accept them
  for(v in intersect(c(time.var,"paleoData_values"),names(plot.df))){
    if(NCOL(plot.df[[v]]) == 1){
      plot.df[[v]] <- as.vector(plot.df[[v]])
    }
  }

  #force grouping by TSid
  plot.df <- dplyr::group_by(plot.df,paleoData_TSid)
  
  #create the color function
  #start with some error checking...
  if(all(is.na(color.ramp))){
    color.ramp <-  function(nColors){RColorBrewer::brewer.pal(nColors,"Dark2")}
  }
  
  if(is.character(color.ramp)){#then use that for the ramp
    #color.fun <- function(nColors,color.ramp){rep(grDevices::rgb(maxColorValue = 255,t(grDevices::col2rgb(color.ramp))),nColors)}
    color.fun <- function(nColors,color.ramp){rep(color.ramp,nColors)}
  }else{
    color.fun <- function(nColors,color.ramp){grDevices::colorRampPalette(color.ramp(nColors))(nColors)}
  }
  
  
  #check the plot.df for required variables
  reqVar <- c("paleoData_values","paleoData_TSid","paleoData_units","paleoData_variableName","dataSetName", "archiveType",time.var)
  
  for(r in 1:length(reqVar)){
    if(!any(reqVar[r] == names(plot.df))){
      stop(paste(reqVar[r],"must be in plot.df"))
    }
  }
  
  #check to see if time.var is an age ensemble
  
  if(NCOL(plot.df[[time.var]]) > 1){
    stop(glue::glue("It looks like your time.var ({time.var}) has more than 1 column ({NCOL(plot.df[[time.var]])}). plotTimeseriesStack() cannot yet handle age ensembles."))
  }
  
  
  plot.df <- plot.df %>%
    dplyr::mutate(scaled = as.numeric(scale(paleoData_values)*scale.factor)) %>%
    dplyr::filter(is.finite(scaled), is.finite(.data[[time.var]]))
  
  if(!is.na(invert.var)){# then make some negative
    iv <- plot.df[[invert.var]]
    if(is.character(iv)){
      ivn <- rep(1,nrow(plot.df))
      ivn[grepl("neg",iv,ignore.case = TRUE)] <- -1
      ivn[grepl("-1",iv,ignore.case = TRUE)] <- -1
      iv <- ivn
    }else if(is.numeric(iv)){
      ivn <- rep(1,nrow(plot.df))
      ivn[iv<0] <- -1
      iv <- ivn
    }
    if(all(iv == 1 | iv == -1)){
      plot.df$scaled <- plot.df$scaled*iv# flip as needed
      plot.df$iv <- iv
    }else{
      stop("inverting the variables based on invert.var failed. Check help for details.")
    }
  }else{#then all positive
    plot.df$iv <- 1
  }
  
  #arrange the data.frame by TSid factors
  plot.df$paleoData_TSid <- factor(plot.df$paleoData_TSid,levels = unique(plot.df$paleoData_TSid))
  
  
  #copy the color variable into plot.df
  plot.df$cv = plot.df[[color.var]]
  
  plot.df$cv <- factor(plot.df$cv,levels = unique(plot.df$cv))
  
  axisStats <- plot.df %>%
    dplyr::summarize(variableName = unique(paleoData_variableName),
                     units = unique(paleoData_units),
                     dataSetName = unique(dataSetName),
                     archiveType = unique(archiveType), 
                     invert = mean(iv),
                     mean = mean(paleoData_values,na.rm = T),
                     sdhigh = sd(paleoData_values,na.rm = T)/scale.factor*scale.height+mean(paleoData_values,na.rm = T),
                     sdlow = -sd(paleoData_values,na.rm = T)/scale.factor*scale.height+mean(paleoData_values,na.rm = T),
                     color.var = unique(cv)) %>%
    dplyr::mutate(axisLabel = paste0(variableName," (",units,")")) %>%
    dplyr::mutate(axisMin = ifelse(invert == 1,as.character(signif(sdlow,3)),as.character(signif(sdhigh,3))))  %>%
    dplyr::mutate(axisMax = ifelse(invert == 1,as.character(signif(sdhigh,3)),as.character(signif(sdlow,3))))
  
  
  colOrder <- match(unique(plot.df$paleoData_TSid),axisStats$paleoData_TSid)
  
  axisStats <- axisStats[colOrder,]
  
  nlines <- length(unique(plot.df$paleoData_TSid))
  
  if(getRversion() >= "4"){
    nColors <- min(length(axisStats$color.var),nlines)
  }else{
    nColors <- min(length(levels(axisStats$color.var)),nlines)
  }
  colVec <- color.fun(nColors,color.ramp)
  axisStats$colors <- colVec[match(axisStats$color.var,levels(axisStats$color.var))]
  
  spag <- ggplot(plot.df) +
    geom_ridgeline(aes(x = .data[[time.var]],height = scaled, y = paleoData_TSid,color = cv, fill = cv),
                   min_height = -Inf,alpha = fill.alpha,linewidth = line.size)+
    scale_color_manual(name = color.var,values = colVec)+
    scale_fill_manual(name = color.var,values = colVec)+
    theme_ridges(grid = TRUE)+
    theme_bw()
  
  ylow <- seq_len(nlines)-scale.height
  yhigh <-  seq_len(nlines)+scale.height
  
  
  my.x.range <- getPlotRanges(spag)$x.lims
  
  xpos <- rep(my.x.range,times = ceiling(nlines/2))[seq_len(nlines)]
  
  #guess position for label
  xrtick <- my.x.range+c(-1 ,1)*abs(diff(my.x.range))*lab.buff*.25
  xposTick <- rep(xrtick,times = ceiling(nlines/2))[seq_len(nlines)]
  
  xrtickLabel <- my.x.range+c(-1 ,1)*abs(diff(my.x.range))*lab.buff
  xposTickLabel <- rep(xrtickLabel,times = ceiling(nlines/2))[seq_len(nlines)]
  
  xrlab <- my.x.range+c(-1 ,1)*abs(diff(my.x.range))*lab.buff*lab.space
  xposLab <- rep(xrlab,times = ceiling(nlines/2))[seq_len(nlines)]
  if(time.var == "year"){
    xlabName <- paste0("Year (",plot.df$yearUnits[1],")")
  }else if(time.var == "age"){
    xlabName <- paste0("Age (",plot.df$ageUnits[1],")")
  }else if(time.var == "depth"){
    xlabName <- paste0("Depth (",plot.df$depth.units[1],")")
  }else{
    xlabName <- "Unknown"
  }
  
  spag <- spag+annotate(geom = "segment", color = axisStats$colors , x = xpos, xend = xpos, y = ylow, yend  = yhigh)+
    annotate(geom = "segment", color = axisStats$colors , x = xpos, xend = xposTick, y = ylow, yend  = ylow)+
    annotate(geom = "segment", color = axisStats$colors , x = xpos, xend = xposTick, y = yhigh, yend  = yhigh)+
    annotate(geom = "text", color = axisStats$colors , x = xposTickLabel, y = ylow, label = axisStats$axisMin,size = lab.size)+
    annotate(geom = "text", color = axisStats$colors , x = xposTickLabel, y = yhigh, label = axisStats$axisMax,size = lab.size)+
    annotate(geom = "text", color = axisStats$colors , x = xposLab, y = seq_len(nlines), label = axisStats$axisLabel,size = lab.size,angle = 90)+
    scale_y_discrete(name = NULL,labels = axisStats$dataSetName, expand = c(0.02,-0.75))+
    scale_x_continuous(name = xlabName, expand = c(0.02,0))
  
  if(length(unique(axisStats$colors))==1){
    spag = spag+theme(legend.position = "none")
  }
  
  return(spag)
}

#' @export
#' @family plot
#' @title Plot a single timeseries
#' @description Plot one LiPD timeseries against its time axis, with axis
#'   labels and a title taken from its metadata. Takes one element of a TS
#'   list (from `lipdR::extractTs()`) or one row of a timeseries tibble (from
#'   `lipdR::ts2tibble()`).
#'
#'   By default the time axis is the timeseries' `time`, with the units,
#'   datum and direction that `lipdR::extractTs()` records, so a record whose
#'   primary time column is year CE is plotted in year CE. Ages (yr BP, ka)
#'   are plotted on a reversed axis, so time runs left to right. With
#'   `secondary.axis = TRUE`, the other time scale is added on top: yr BP
#'   above a year CE axis, or year CE above an age axis.
#' @import ggplot2
#'
#' @param ts a single timeseries: one element of a TS list, or a one-row timeseries tibble / data frame
#' @param time.var name of the time variable. NA (default) uses "time" if the timeseries has time values (from `lipdR::extractTs()`), and otherwise "year" if it has year values, and "age" if not.
#' @param time.units time scale to plot on, e.g. "yr BP", "ka" or "CE". NA (default) keeps the timeseries' own units. Applies when time.var is "time", "age" or "year".
#' @param secondary.axis add the other time scale (yr BP or year CE) as a second x axis? (default = FALSE)
#' @param color line color (default = "black")
#' @param points also draw a point at each observation? (default = FALSE)
#' @param title plot title. NA (default) uses the dataset name. Use NULL for no title.
#'
#' @return a ggplot object
#' @examples
#' \dontrun{
#' TS <- lipdR::extractTs(D)
#' plotTs(TS[[1]])
#'
#' tib <- lipdR::ts2tibble(TS)
#' plotTs(tib[1, ], secondary.axis = TRUE)
#' }
plotTs <- function(ts,
                   time.var = NA,
                   time.units = NA,
                   secondary.axis = FALSE,
                   color = "black",
                   points = FALSE,
                   title = NA){

  # one row of a data frame -> a list
  if(is.data.frame(ts)){
    if(nrow(ts) != 1){
      stop("plotTs() plots one timeseries, but got ", nrow(ts), " rows. Use plotTimeseriesStack() for several.")
    }
    ts <- purrr::transpose(as.list(ts))[[1]]
  }
  if(!is.list(ts) || is.null(ts[["paleoData_values"]])){
    if(is.list(ts) && length(ts) > 0 && is.list(ts[[1]]) && !is.null(ts[[1]][["paleoData_values"]])){
      stop("plotTs() plots one timeseries, but got a TS list with ", length(ts), " entries. Use TS[[i]] for one, or plotTimeseriesStack() for several.")
    }
    stop("ts must be a single LiPD timeseries (one element of a TS list or one row of a timeseries tibble) with paleoData_values")
  }

  hasVals <- function(x){!is.null(x) && length(unlist(x)) > 0 && !all(is.na(unlist(x)))}

  # choose the time variable
  if(is.na(time.var)){
    if(hasVals(ts[["time"]])){
      time.var <- "time"
    }else if(hasVals(ts[["year"]])){
      time.var <- "year"
    }else if(hasVals(ts[["age"]])){
      time.var <- "age"
    }else{
      stop("This timeseries has no time, year or age values. Set time.var to the name of its time variable.")
    }
  }
  if(!hasVals(ts[[time.var]])){
    stop("This timeseries has no values for time.var = '", time.var, "'")
  }

  vals <- ts[["paleoData_values"]]
  first <- function(x){
    x <- unlist(x)
    if(length(x) == 0 || is.na(x[1]) || !nzchar(as.character(x[1]))) NA_character_ else as.character(x[1])
  }
  isTimeVar <- time.var %in% c("time", "age", "year")
  if(NCOL(ts[[time.var]]) > 1 || NCOL(vals) > 1){
    stop("plotTs() plots a single time axis and a single series. For ensembles, use plotTimeseriesEnsRibbons() or plotTimeseriesEnsLines().")
  }
  if(isTimeVar){
    #time, age or year: read (and optionally convert) with its metadata
    entry <- if(time.var == "time") ts[c("time", "timeUnits", "timeDatum", "timeDirection", "timeExponent")] else ts[c(time.var, paste0(time.var, "Units"))]
    names(entry) <- if(time.var == "time") c("time", "timeUnits", "timeDatum", "timeDirection", "timeExponent") else c(time.var, paste0(time.var, "Units"))
    entry <- entry[!vapply(entry, is.null, logical(1))]
    time <- lipdR::getTsTime(entry, units = time.units, ensemble = FALSE)
    timeUnits <- attr(time, "timeUnits")
    if(is.na(time.units) && time.var != "time" && !is.na(first(entry[[paste0(time.var, "Units")]]))){
      timeUnits <- first(entry[[paste0(time.var, "Units")]]) #keep the record's own label
    }
  }else{
    time <- ts[[time.var]]
    timeUnits <- first(ts[[paste0(time.var, "Units")]])
  }
  time <- suppressWarnings(as.numeric(unlist(time)))
  vals <- suppressWarnings(as.numeric(unlist(vals)))
  if(length(time) != length(vals)){
    stop("The time variable (", length(time), " values) and paleoData_values (", length(vals), " values) have different lengths.")
  }
  df <- data.frame(time = time, value = vals)
  df <- df[is.finite(df$time) & is.finite(df$value), ]
  df <- df[order(df$time), ]

  # labels from metadata
  isCE <- if(isTimeVar) isProgradeUnits(timeUnits) else (time.var == "year" || grepl("AD|CE", timeUnits))
  timeName <- if(time.var == "time") (if(isCE) "year" else "age") else time.var
  timeLab <- if(is.na(timeUnits)) timeName else paste0(timeName, " (", timeUnits, ")")
  vn <- first(ts[["paleoData_variableName"]])
  if(is.na(vn)) vn <- "value"
  vu <- first(ts[["paleoData_units"]])
  valLab <- if(is.na(vu)) vn else paste0(vn, " (", vu, ")")
  if(length(title) == 1 && !is.null(title) && is.na(title)){
    title <- first(ts[["dataSetName"]])
    if(is.na(title)) title <- NULL
  }

  # secondary axis: the other time scale, if this one can be converted
  other <- if(isCE) "yr BP" else "CE"
  canConvert <- isTimeVar && !inherits(try(lipdR::convertTime(0, from = timeUnits, to = other), silent = TRUE), "try-error")
  if(secondary.axis && !canConvert){
    warning("Can't add a secondary time axis for ", timeLab, ", so it is left off.")
    secondary.axis <- FALSE
  }

  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line(color = color) +
    labs(y = valLab, title = title) +
    geoChronRPlotTheme()
  if(points){
    p <- p + geom_point(color = color, size = 0.8)
  }

  if(isCE){
    sec <- if(secondary.axis) sec_axis(~ lipdR::convertTime(., from = timeUnits, to = "yr BP"), name = "age (yr BP)") else waiver()
    p <- p + scale_x_continuous(name = timeLab, sec.axis = sec)
  }else{
    sec <- if(secondary.axis) sec_axis(~ lipdR::convertTime(., from = timeUnits, to = "CE"), name = "year (CE)") else waiver()
    p <- p + scale_x_reverse(name = timeLab, sec.axis = sec)
  }
  return(p)
}
