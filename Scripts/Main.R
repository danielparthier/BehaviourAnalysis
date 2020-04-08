# Main script to process data
library(BehaviouR)
library(ggplot2)
library(patchwork)
library(data.table)

FileList <- list.files(path = "~/PhD/Data/Behaviour/", ".csv")
for(FileCount in seq_along(FileList)) {
  FileName <- paste0("~/PhD/Data/Behaviour/", FileList[FileCount])
  SplitName <- unlist(strsplit(x = FileList[FileCount], split = "_"))
  AnimalID <- SplitName[2]
  ExpDate <- SplitName[1]
  NovelObjectLocation <- unlist(strsplit(x = SplitName[6], "Mar"))[1]
# FileName <- paste0("RawData/20180903_Schmitz_PaS_B1_C1_NL_",
#                   "Day1_NOVEL_DSC001663DLC_resnet50_NOVEL_",
#                   "VIDEONov7shuffle1_100000.csv")

FrameRate <- 30
ObjectNumber <- 2

# generate lists
MouseBodyList <- list()
MouseBodyList$head <- c("ear", "nose")
MouseBodyList$body <- c("ear", "tail")

ObjectList <- list("object")

# load data
MouseDataTable <- DeepLabCutLoad(FileName = FileName,
                                 FrameRate = FrameRate,
                                 MouseLabels = MouseBodyList,
                                 ObjectLabels = ObjectList,
                                 ObjectNumber = ObjectNumber,
                                 xScale = 1,
                                 yScale = 1,
                                 JumpCorrections = F,
                                 includeAll = F)

# compute body motion properties
DistSpeedCalc(CoordTable = MouseDataTable$DataTable,
              SpeedRef = "bodyCentroid",
              Interval = 1/FrameRate)
AddCentroid(CoordTable = MouseDataTable$DataTable,
            CornerNames = list("ear"),
            ReferenceColumn = "frame",
            OutputName = "BetweenEars")
VectorLength(CoordTable = MouseDataTable$DataTable,
             VectorStart = "tailbase",
             VectorEnd = "BetweenEars",
             OutputName = "BodyLength")
AngleCalc(CoordTable = MouseDataTable$DataTable,
          VectorStart = "BetweenEars",
          VectorEnd = "nose",
          OutputName = "HeadAngle")
AngleCalc(CoordTable = MouseDataTable$DataTable,
          VectorStart = "tailbase",
          VectorEnd = "BetweenEars",
          OutputName = "BodyAngle")
AngleDiff(CoordTable = MouseDataTable$DataTable,
          Angle1 = "BodyAngle",
          Angle2 = "HeadAngle",
          OutputName = "ViewAngle")

# compute object related properties
ObjectDistance(CoordTable = MouseDataTable$DataTable,
               ObjectTable = MouseDataTable$ObjectTable,
               ObjectLabels = ObjectList,
               Ref = "headCentroid")
ObjectAngle(CoordTable = MouseDataTable$DataTable,
            ObjectTable = MouseDataTable$ObjectTable,
            ObjectLabels = ObjectList,
            Ref = "headCentroid")
VectorLength(CoordTable = MouseDataTable$DataTable,
             VectorStart = "nose",
             VectorEnd = "BetweenEars",
             OutputName = "headLength")
AddCentroid(CoordTable = MouseDataTable$DataTable,
            CornerNames = list("ear"),
            ReferenceColumn = "frame",
            OutputName = "BetweenEars")
for(i in MouseDataTable$ObjectTable$ObjectLoc) {
  ObjectString <- grep(pattern = paste0("^", i, "[_][alphanum]",".*","[_]Angle", "$"),
       x = colnames(MouseDataTable$DataTable),
       value = T)
  AngleDiff(CoordTable = MouseDataTable$DataTable,
            Angle2 = "HeadAngle",
            Angle1 = ObjectString,
            OutputName = paste0(i,"_HeadAngle_Angle"))
  ZoneEntry(CoordTable = MouseDataTable$DataTable,
            DistanceRef = paste0(i,"_headCentroid_Distance"),
            Length = quantile(MouseDataTable$DataTable$BodyLength, 0.95),
            AngleInclusion = T,
            AngleRef = paste0(i,"_HeadAngle_Angle"),
            AngleRange = pi/10, 
            Overwrite = T)
}


# Plot functions
SpeedPlot <- SpeedPlot(DataTable = MouseDataTable$DataTable,
                       Speed = "SpeedbodyCentroid",
                       x = "headCentroid_x",
                       y = "headCentroid_y",
                       ObjectTable = MouseDataTable$ObjectTable)

DensityPlot <- LocationPlot(DataTable = MouseDataTable$DataTable,
                            x = "headCentroid_x",
                            y = "headCentroid_y",
                            ObjectTable = MouseDataTable$ObjectTable,
                            Density = T)


if(ObjectNumber>0) {
  ObjectAnglePlots <- lapply(X = 1:ObjectNumber, FUN = function(objID) {
    AnglePlot(DataTable = MouseDataTable$DataTable,
              Angle = paste0("object_",objID,"_HeadAngle_Angle"),
              x = "headCentroid_x",
              y = "headCentroid_y",
              ObjectTable = MouseDataTable$ObjectTable,
              colourScheme = "dark",
              ObjectHighlight = "alpha")
  })   
}


SpeedPlotLine <- SpeedPlot(DataTable = MouseDataTable$DataTable,
                           Speed = "SpeedbodyCentroid")

DistancePlotLine <- DistancePlot(DataTable = MouseDataTable$DataTable,
                                 Distance = "CumDistbodyCentroid")

ObjectDistancePlotLine <- DistancePlot(DataTable = MouseDataTable$DataTable,
                                       Distance = "headCentroid_Distance",
                                       ObjectTable = MouseDataTable$ObjectTable,
                                       ObjectDistance = T)

RearingPlotLine <- LengthPlot(DataTable = MouseDataTable$DataTable,
                              Length = "BodyLength")

RearingPlot <- LengthPlot(DataTable = MouseDataTable$DataTable,
                          Length = "BodyLength",
                          x = "headCentroid_x",
                          y = "headCentroid_y",
                          ObjectTable = MouseDataTable$ObjectTable) +
  scale_color_viridis_c(guide = guide_colourbar(title = "Rearing", label = FALSE, reverse = T), direction = -1)

# Arrange Plots
if(ObjectNumber>0) {
  MovementPlot <- (SpeedPlot | ObjectAnglePlots[[1]] | ObjectAnglePlots[[2]]) / ObjectDistancePlotLine + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24))
#  ggsave(plot = MovementPlot, filename = paste0("Plots/MovementPlot","_",AnimalID,"_", ExpDate,".pdf"), device = "pdf", width = 10.4, height = 6)
}

OutPutPlotRearing <- RearingPlotLine + RearingPlot
OutPutPlotMap <- SpeedPlot + DensityPlot + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24)) 
OutPutPlotMovement <- SpeedPlotLine + DistancePlotLine + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24)) 
OutPutPlotRearingDensity <- DensityPlot + RearingPlot + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24)) 

#ggsave(plot = OutPutPlotMap, filename = paste0("Plots/OutPutPlotMap","_",AnimalID,"_", ExpDate,".pdf"), device = "pdf", width = 10.4, height = 4)
#ggsave(plot = OutPutPlotMovement, filename = paste0("Plots/OutPutPlotMovement","_",AnimalID,"_", ExpDate,".pdf"), device = "pdf", width = 10.4, height = 4)
#ggsave(plot = ObjectDistancePlotLine, filename = paste0("Plots/ObjectDistancePlotLine","_",AnimalID,"_", ExpDate,".pdf"), device = "pdf", width = 5, height = 3)
#ggsave(plot = OutPutPlotRearing, filename = paste0("Plots/RearingPlot","_",AnimalID,"_", ExpDate,".pdf"), device = "pdf", width = 5, height = 3)
#ggsave(plot = OutPutPlotRearingDensity, filename = paste0("Plots/RearingDensityPlot","_",AnimalID,"_", ExpDate,".pdf"), device = "pdf", width = 10.4, height = 4)


##### change names according to old and new location ####
switch (NovelObjectLocation,
        "left" = {newColNames <- gsub(x = colnames(MouseDataTable$DataTable), pattern = MouseDataTable$ObjectTable[x==min(x),ObjectLoc,], replacement = "novel")
        newColNames <- gsub(x = newColNames, pattern = MouseDataTable$ObjectTable[x!=min(x),ObjectLoc,], replacement = "old")
        },
        "right" = {newColNames <- gsub(x = colnames(MouseDataTable$DataTable), pattern = MouseDataTable$ObjectTable[x==max(x),ObjectLoc,], replacement = "novel")
        newColNames <- gsub(x = newColNames, pattern = MouseDataTable$ObjectTable[x!=max(x),ObjectLoc,], replacement = "old")
        }
)

data.table::setnames(x = MouseDataTable$DataTable, old = colnames(MouseDataTable$DataTable),
                     new = newColNames)

##### indeces
# Calculate cumulative entries
CumulativeEntries <- data.table::copy(MouseDataTable$DataTable[,.(CumNovel = cumsum(novel_headCentroid_Distance_inArea_entry),
                                                                  CumOld = cumsum(old_headCentroid_Distance_inArea_entry),
                                                                  InNovel = novel_headCentroid_Distance_inArea,
                                                                  InOld = old_headCentroid_Distance_inArea,
                                                                  Time = Time,
                                                                  TimeDiff = 1/FrameRate)])
# PreferenceIndex over time
quantRibbonMin <- CumulativeEntries[,.(Time= min(Time)),by=.(CumNovel,CumOld)]
quantRibbonMax <- CumulativeEntries[,.(Time= max(Time)),by=.(CumNovel,CumOld)]
quantRibbon <- rbindlist(l = list(quantRibbonMin, quantRibbonMax))
quantRibbon <- quantRibbon[,lapply(c(0.025,0.25,0.5, 0.75,0.975), function(x) {qbeta(p = x,shape1 = CumNovel+1, shape2 = CumOld+1)}),
                           by=Time]

data.table::setnames(x = quantRibbon, old = colnames(quantRibbon),
                     new = c(colnames(quantRibbon)[1], paste0("q", c(0.025,0.25,0.5, 0.75,0.975)*1000)))
setorder(x = quantRibbon, Time)



ObjectPrefTime <- ggplot(data = quantRibbon, aes(x = Time, y = q500))+
  geom_line()+
  geom_ribbon(aes(ymin=q25, ymax=q975), alpha = 0.2, fill = "cornflowerblue")+
  geom_ribbon(aes(ymin=q250, ymax=q750), alpha = 0.1, fill = "cornflowerblue")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  scale_x_continuous(expand = c(0,0))+
  geom_hline(yintercept = 0.5, linetype = "dashed")+
  ylab(expression(paste("Preference Index:", ~~ frac("object"["novel"], "object"["novel"]+"object"["old"]))))+
  theme_classic()+theme(plot.margin = margin(4, 10, 4, 4, "pt"))

shapePar <- c(max(CumulativeEntries$CumNovel)+1, max(CumulativeEntries$CumOld)+1)
ObjectPrefDensTable <- data.table(x = seq(0,1,0.001))
ObjectPrefDensTable[,`:=`(y=dbeta(x = x, shape1 = shapePar[1], shape2 = shapePar[2]),
                          quantile=pbeta(q = x, shape1 = shapePar[1], shape2 = shapePar[2])),]

ObjectPref <- ggplot(data = ObjectPrefDensTable, aes(y = y, x = x, fill = quantile))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0), limits = c(0,1))+
  geom_area(mapping = aes(x = x), fill = "cornflowerblue", alpha = 0.3) +
  geom_area(mapping = aes(x = ifelse(quantile<=0.025 , x, NaN)), fill = "cornflowerblue", alpha = 0.5) +
  geom_area(mapping = aes(x = ifelse(quantile>=0.975 , x, NaN)), fill = "cornflowerblue", alpha = 0.5) +
  geom_line()+
  labs(x = expression(paste("Preference Index:", ~~ frac("object"["novel"], "object"["novel"]+"object"["old"]))), y = "Density")+
  geom_vline(xintercept = 0.5, linetype = "dashed")+
  theme_classic()+theme(plot.margin = margin(4, 10, 4, 4, "pt"))

ObjectPrefEntryPlots <- ObjectPref + ObjectPrefTime + plot_layout(ncol=2,widths=c(1,2)) + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24))
#ggsave(plot = ObjectPrefEntryPlots, filename = paste0("Plots/ObjectPrefEntryPlots","_",AnimalID,"_", ExpDate,".pdf"), device = "pdf", width = 10.4, height = 4)

OverAllPlot <- (SpeedPlot + RearingPlot + DensityPlot) / (ObjectAnglePlots[[1]] + ObjectAnglePlots[[2]] + ObjectDistancePlotLine) / (ObjectPref + ObjectPrefTime) + plot_annotation(title = paste("Overview for Animal:",AnimalID,"on", ExpDate), tag_levels = "A") & theme(plot.title = element_text(size=20),plot.tag = element_text(size=24))

ggsave(plot = OverAllPlot, filename = paste0("Plots/OverAllPlot","_",AnimalID,"_", ExpDate,".pdf"), device = "pdf", width = 12, height = 10)

# time spent at object in total
TimeAtObject <- sapply(c("Novel", "Old"), FUN = function(x){
  CumulativeEntries[get(paste0("In",x))==1,.(TimeSpent=sum(TimeDiff)), .(get(paste0("Cum",x)))][,sum(TimeSpent),]
})

TimeAtObjectOverTime <- rbindlist(lapply(c("Novel", "Old"), FUN = function(x){
  if(x == "Novel") {
    LocationSite <- NovelObjectLocation
  } else {
    LocationSite <- c("right", "left")[!grepl(x = c("right", "left"), pattern = NovelObjectLocation)]
  }
  CumulativeEntries[get(paste0("In",x))==1,.(TimeSpent=sum(TimeDiff), EntryTime=min(Time)), .(get(paste0("Cum",x)))][,.(TimeSpent, EntryTime, Site=LocationSite ,Location=x, Date=ExpDate, Animal = AnimalID),]
}))


# calculate index from total time spent at object
(TimeAtObject[1]-TimeAtObject[2])/(TimeAtObject[1]+TimeAtObject[2])


if(FileCount == 1) {
  TotalTable <- copy(x = MouseDataTable$DataTable)
  TimeAtObjectTotal <- copy(TimeAtObjectOverTime)
} else {
  TotalTable <- rbindlist(l = list(TotalTable, MouseDataTable$DataTable))
  TimeAtObjectTotal <- rbindlist(l = list(TimeAtObjectTotal, TimeAtObjectOverTime))
  }

}


#### estimation for gamma posterior (k and theta) ####
posteriorGamma <- function(X,
                           P_prior = 1,
                           Q_prior = 1,
                           R_prior = 1,
                           S_prior = 1,
                           stepSizeK = 1e-2,
                           maxK = 30,
                           stepSizeTheta = 1e-2) {
  k <- seq(1e-12,maxK, stepSizeK)
  PpriorLog <- log(P_prior)
  PprodLog <- sum(log(X))
  qsum <- sum(X)
  n_samples <- length(X)
  newLogP <- PpriorLog+PprodLog
  newQ <- Q_prior+qsum
  newR <- R_prior+n_samples
  newS <- S_prior+n_samples
  densK <- exp((newLogP*(k-1)+lgamma(newS*k+1))-(log(newQ)*(k*newS+1)+lgamma(k)*newR))/sum(exp((newLogP*(k-1)+lgamma(newS*k+1))-(log(newQ)*(k*newS+1)+lgamma(k)*newR)))
  densK <- densK/sum(densK)
  
  ksample <- sample(size = 1e4, x = k, prob = densK, replace = T)
  maxTheta <- quantile(max(X)/ksample, probs = 0.9)*10
  message(paste("max Theta:",maxTheta))
  theta <- seq(1e-4,maxTheta, stepSizeTheta)
  thetaTable <- data.table(theta)
  thetaTable[,Dist:=sum(exp((newLogP*(ksample-1)-1/theta*newQ)-(lgamma(ksample)*newR+ksample*newS*log(theta)))),by=theta]
  thetaTable[,Dist:=Dist/sum(Dist),]
  return(list("k"=k[densK>.Machine$double.eps] ,"k_Dens"=densK[densK>.Machine$double.eps], "theta"=thetaTable$theta[thetaTable$Dist>.Machine$double.eps], "theta_Dens"=thetaTable$Dist[thetaTable$Dist>.Machine$double.eps]))
}

TimeObject1Avg <- posteriorGamma(X = TimeAtObjectInterval[[1]], maxK = 20)
ParameterPlot(TimeObject1Avg)
TimeObject2Avg <- posteriorGamma(X = TimeAtObjectInterval[[2]], maxK = 20)
ParameterPlot(TimeObject2Avg)

posteriorTheta1 <- sample(x = TimeObject1Avg$theta, size = 1e5, replace = T, prob = TimeObject1Avg$theta_Dens)
posteriorK1 <- sample(x = TimeObject1Avg$k, size = 1e5, replace = T, prob = TimeObject1Avg$k_Dens)
distVec1 <- vector(mode = "numeric", length = length(posteriorK1))
for(i in seq_along(posteriorK1)) {
  distVec1[i] <- rgamma(n = 1, shape = posteriorK1[i], scale = posteriorTheta1[i])
}
plot(density(distVec1))

posteriorTheta2 <- sample(x = TimeObject2Avg$theta, size = 1e5, replace = T, prob = TimeObject2Avg$theta_Dens)
posteriorK2 <- sample(x = TimeObject2Avg$k, size = 1e5, replace = T, prob = TimeObject2Avg$k_Dens)
distVec1 <- vector(mode = "numeric", length = length(posteriorK2))
for(i in seq_along(posteriorK2)) {
  distVec2[i] <- rgamma(n = 1, shape = posteriorK2[i], scale = posteriorTheta2[i])
}
lines(density(distVec2), col="red", add = T)

##### bayes factor ####
(BF10 <- (length(distVec1)/sum(1/distVec1))/(length(distVec2)/sum(1/distVec2)))
1/BF10
PosteriorSamplePlot(Data = TimeAtObjectInterval[[1]],posteriorTheta = posteriorTheta1, posteriorK = posteriorK1, n_samples = 200, to = 20)
