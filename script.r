# Copyright (c) Microsoft Corporation.  All rights reserved.

# Third Party Programs. This software enables you to obtain software applications from other sources. 
# Those applications are offered and distributed by third parties under their own license terms.
# Microsoft is not developing, distributing or licensing those applications to you, but instead, 
# as a convenience, enables you to use this software to obtain those applications directly from 
# the application providers.
# By using the software, you acknowledge and agree that you are obtaining the applications directly
# from the third party providers and under separate license terms, and that it is your responsibility to locate, 
# understand and comply with those license terms.
# Microsoft grants you no license rights for third-party software or applications that is obtained using this software.


#TEMP: Debug in RStudio
fileRda = "C:/Users/boefraty/projects/PBI/R/tempData1.Rda"
if(file.exists(dirname(fileRda)))
{
  if(Sys.getenv("RSTUDIO")!="")
    load(file= fileRda)
  else
    save(list = ls(all.names = TRUE), file=fileRda)
}


############ User Parameters #########
# Set of parameters from GUI
# comments on params and functions


##PBI_PARAM The type of plot for visualization
#Type:string, Default:"scatter", Range:NA, PossibleValues:"scatter","boxplot","density" 
plotType = 'scatter' # 'boxplot', 'scatter', 'density'
if(exists("mySettingsViz_plotType")){
  plotType = mySettingsViz_plotType
}


##PBI_PARAM Color of scatterplot inlier points
#Type:string, Default:"orange", Range:NA, PossibleValues:"orange","blue","green","black" etc
inlierColor = "blue"
if(exists("mySettingsMark_inlierColor")){
  inlierColor = mySettingsMark_inlierColor
}

##PBI_PARAM Color of scatterplot outlier points
#Type:string, Default:"orange", Range:NA, PossibleValues:"orange","blue","green","black" etc
outlierColor = "red";
if(exists("mySettingsMark_outlierColor")){
  outlierColor = mySettingsMark_outlierColor
}

#PBI_PARAM Size of points on the plot
#Type:numeric, Default: 1 , Range:[0.1,5], PossibleValues:NA, Remarks: NA
pntSize = 1
if(exists("mySettingsMark_weight")){
  pntSize = mySettingsMark_weight / 10
}

#PBI_PARAM Sparsification of scatterplot points
#Type:bool, Default:TRUE, Range:NA, PossibleValues:NA, Remarks: NA
sparsify = TRUE
if(exists("mySettingsMark_sparsify")){
  sparsify = mySettingsMark_sparsify
}

#PBI_PARAM Transparency of scatterplot points
#Type:numeric, Default:0.4, Range:[0,1], PossibleValues:NA, Remarks: NA
transparency = 0.4
if(exists("mySettingsMark_percentile")){
  transparency = mySettingsMark_percentile/100
}

#PBI_PARAM Size of labels on axes
#Type:numeric, Default:12, Range:[0,20], PossibleValues:NA, Remarks: NA
sizeLabel = 12
if(exists("mySettingsAxes_textSize")){
  sizeLabel = mySettingsAxes_textSize
}

#PBI_PARAM Size of ticks on axes 
#Type:numeric, Default:6, Range:[1,20], PossibleValues:NA, Remarks: NA
sizeTicks = 6
if(exists("mySettingsAxes_sizeTicks")){
  sizeTicks = as.numeric(mySettingsAxes_sizeTicks)
}

#PBI_PARAM Color of labels on axes
#Type:string, Default:"gray", Range:NA, PossibleValues:"orange","blue","green","black" etc
colLabel = "gray"
if(exists("mySettingsAxes_colLabel")){
  colLabel = mySettingsAxes_colLabel
}

#PBI_PARAM Format of X-axis
#Type:string, Default:"none", Range:NA, PossibleValues:"none","scientific","dollar","comma" 
scaleXformat = "none"
if(exists("mySettingsAxes_scaleXformat")){
  scaleXformat = mySettingsAxes_scaleXformat
}

#PBI_PARAM Format of X-axis
#Type:string, Default:"none", Range:NA, PossibleValues:"none","scientific","dollar","comma" 
scaleYformat = "none"
if(exists("mySettingsAxes_scaleYformat")){
  scaleYformat = mySettingsAxes_scaleYformat
}

#PBI_PARAM Vizualize outlier scores, rather than original values (not applicable for 'manual' method)
#Type:bool, Default:TRUE, Range:NA, PossibleValues:NA, Remarks: NA
visualizeOutlierScore = FALSE
if(exists("mySettingsViz_visualizeOutlierScore")){
  visualizeOutlierScore = mySettingsViz_visualizeOutlierScore
}

#PBI_PARAM Outlier's rejection method. PeaksAndSubpeaks =  all. Peaks =  outliers above average. Subpeaks = outliers below average. 
#Type:string, Default:'PeaksAndSubpeaks', Range:NA, PossibleValues:'Peaks', 'Subpeaks', 'PeaksAndSubpeaks'
thresholdType = 'PeaksAndSubpeaks' # 'Peaks', 'Subpeaks', 'PeaksAndSubpeaks'
if(exists("mySettingsDet_thresholdType")){
  thresholdType = mySettingsDet_thresholdType
}

#PBI_PARAM Outliers  method. 
#Type:string, Default:'zscore', Range:NA, PossibleValues: 'zscore' , 'Tukey' , 'LOF', 'manual', 'coots'
# 'zscore' = threshold applied on standardized value
# 'Tukey' = threshold applied on IQR-scaled value
# 'LOF' = result is based on local density 
# 'manual' = lower and upper thresholds defined by user
# 'coots' =  outlier score is a measure computed with respect to a regression model 
algName = 'zscore' # 'zscore' , 'Tukey' , LOF, manual, etc  
if(exists("mySettingsDet_algName")){
  algName = mySettingsDet_algName
}

#PBI_PARAM Number of standard deviations (threshold)
#Type:numeric, Default:4, Range:[2,10], PossibleValues:NA
numSig = 4
if(exists("mySettingsDet_numSig")){
  numSig = as.numeric(mySettingsDet_numSig)
}

#PBI_PARAM IQRs (threshold)
#Type:numeric, Default:1.5, Range:[1,10], PossibleValues:NA
IQR = 1.5
if(exists("mySettingsDet_IQR")){
  IQR = as.numeric(mySettingsDet_IQR)
}

#PBI_PARAM LOF threshold in terms of mean measure in population
#Type:numeric, Default:3, Range:[2,10], PossibleValues:NA
LOFThresh = 3
if(exists("mySettingsDet_LOFThresh")){
  LOFThresh = as.numeric(mySettingsDet_LOFThresh)
}

#PBI_PARAM LOF neighbour cardinality
#Type:numeric, Default:3, Range:[2,10], PossibleValues:NA
LOFK = 3
if(exists("mySettingsDet_LOFK")){
  LOFK = as.numeric(mySettingsDet_LOFK)
}

#PBI_PARAM manual threshold lower
#Type:numeric, Default:-100, Range:[-Inf,Inf], PossibleValues:NA
lThresh = -100
if(exists("mySettingsDet_lThresh")){
  lThresh = mySettingsDet_lThresh
}

#PBI_PARAM manual threshold upper
#Type:numeric, Default:-100, Range:[-Inf,Inf], PossibleValues:NA
hThresh = 100
if(exists("mySettingsDet_hThresh")){
  hThresh = mySettingsDet_hThresh
}

#PBI_PARAM Cook's threshold upper
#Type:numeric, Default:3, Range:[1,Inf], PossibleValues:NA
cooksThresh = 3
if(exists("mySettingsDet_cooksThresh")){
  cooksThresh = as.numeric(mySettingsDet_cooksThresh)
}

#PBI_PARAM To scale numerical dimensions?
#Type:bool, Default:TRUE, Range:NA, PossibleValues:NA
toScale = TRUE
if(exists("mySettingsDet_toScale")){
  toScale = mySettingsDet_toScale
}

##PBI_PARAM: export out data to HTML?
#Type:logical, Default:FALSE, Range:NA, PossibleValues:NA, Remarks: NA
keepOutData = FALSE
if(exists("settings_export_params_show"))
  keepOutData = settings_export_params_show 

##PBI_PARAM: method of export interface
#Type: string , Default:"copy",  Range:NA, PossibleValues:"copy", "download",  Remarks: NA
exportMethod = "copy"
if(exists("settings_export_params_method"))
  exportMethod = settings_export_params_method 

##PBI_PARAM: limit the out table exported
#Type: string , Default:1000,  Range:NA, PossibleValues:"1000", "10000", Inf,  Remarks: NA
limitExportSize = 1000
if(exists("settings_export_params_limitExportSize"))
  limitExportSize = as.numeric(settings_export_params_limitExportSize)

###############Internal parameters definitions#################
# Set of parameters, which are not exported to GUI (yet)

minRows = 10 # for Z/manual/Tukey/cooks
minRowsLOF = 20 # for LOF need more rows
USEPLOTLY = TRUE #for debug purposes
sizeWarn = 10 # size of warning fonts
SMALL_EPSILON = 0.000001

maxRows = 95000 # randomly remove extra rows
maxRowsLOF = 6500 # randomly remove extra rows
yRangeExtend = 1.055

options(warn=-1)
###############Library Declarations###############

source('./r_files/flatten_HTML.r')
source('./r_files/utils.r')


libraryRequireInstall("ggplot2");
libraryRequireInstall("plotly")
libraryRequireInstall("DMwR")
libraryRequireInstall("scales")
libraryRequireInstall("caTools")

###############Internal functions definitions#################

#Error checking + messages
CheckInputForErrors = function(XX,YY,algName,params)
{
  
  E = NULL
  if(is.null(XX))
  {  
    E[length(E) + 1] = "'Variables' field should include at least one numeric column"
    return(E)
  }
  if(nrow(XX) < params$minRows)
  {
    E[length(E) + 1] = paste("'Variables' table has fewer rows than what are required for analysis", as.character(params$minRows))
    return(E)
  }
  
  if(nrow(XX) < params$minRowsLOF && algName == "LOF")
  {
    E[length(E) + 1] = paste("'Variables' table has less rows than required for LOF analysis", as.character(params$minRowsLOF))
    return(E)
  }
  
  if(is.null(YY)  && algName == "cooks")
  {  
    E[length(E) + 1] = "'Independent Variables' field should include at least one numeric column"
    return(E)
  }
  
}

#small hack because lofactor with small K sometimes return NaN
my_lofactor = function(DF, k)
{
  scores = lofactor(DF, k = k)
  if(any(is.nan(scores)))
    scores = lofactor(DF, k = k + 1)
  
  return (scores)
}


RemoveExtraRows = function(Values,ID, IndepVar , Tooltips, algName, params)
{
  NR = 0
  if(!is.null(Values))
    NR = nrow(Values)
  
  N = NR
  NR = ifelse(NR > params$maxRows, params$maxRows, NR)
  if(algName == 'LOF')
    NR = ifelse(NR > params$maxRowsLOF, params$maxRowsLOF, NR)
  
  if(N > NR)
  {
    ii = sort(sample(N,NR, replace = FALSE))
    
    Values = Values[ii, , drop=FALSE]
    if(!is.null(ID))
      ID = ID[ii, , drop=FALSE]
    if(!is.null(IndepVar))
      IndepVar = IndepVar[ii, , drop=FALSE]
    if(!is.null(Tooltips))
      Tooltips = Tooltips[ii, , drop=FALSE]
  }  
  
  return(list(Values = Values,ID = ID, IndepVar = IndepVar , Tooltips = Tooltips))
  
  
}


# clean, standardize and check input data
TransformInputData = function(Values,ID, IndepVar, Tooltips, algName, toScale, params)
{
  XX = Values
  YY = IndepVar
  TT = Tooltips
  Errors = NULL
  
  #remove bad columns 
  XX = RemoveNonNumeric(XX)
  YY = RemoveNonNumeric(YY)
  
  #remove bad rows
  if(!is.null(XX))
  {
    goodRows = complete.cases(XX)
    if(!is.null(YY))
      goodRows = as.logical(complete.cases(YY) * goodRows)
    
    countBad = sum(!goodRows)
    if(countBad)
    {
      if(!is.null(XX))
        XX = subset(XX,goodRows)
      
      if(!is.null(YY))
        YY = subset(YY,goodRows)
      
      if(!is.null(TT))
        TT = subset(TT,goodRows)
      
      if(!is.null(ID))
        ID = subset(ID,goodRows)
      
      if(!is.null(Values))
        Values = subset(Values,goodRows)
      
      if(!is.null(IndepVar))
        IndepVar = subset(IndepVar,goodRows)
      
    }
  }
  
  
  #check for errors
  Errors = CheckInputForErrors(XX, YY, algName, params)
  if(is.null(Errors))
  {
    if(!is.null(ID))
    {
      if(is.factor(ID[,1]))
        ID[,1] = as.character(ID[,1])
      
      # sort all by ID 
      ord = sort(ID[,1], index.return = TRUE)$ix
      if(!is.null(XX))
        XX[,] = XX[ord,]
      if(!is.null(YY))
        YY[,] = YY[ord,]
      if(!is.null(TT))
        TT[,] = TT[ord,]
      if(!is.null(Values))
        Values[,] = Values[ord,]
      if(!is.null(IndepVar))
        IndepVar[,] = IndepVar[ord,]
      
      ID[,1] = ID[ord,1]
    }
    
    NXX = NYY = NTT = 0
    
    if(!is.null(XX))
      NXX = ncol(XX)
    if(!is.null(YY))
      NYY = ncol(YY)
    if(!is.null(TT))
      NTT = ncol(TT)
    
    if(length(intersect(algName,c('zscore', 'manual', 'Tukey', 'LOF'))) && is.null(Errors))
    {
      if(NXX > 1)
      {
        if(length(intersect(algName,'LOF')))
        {#only scale
          if(toScale)
            XX = standardizeColumns(XX)
        }
        else{# scale + PCA + keep 1 
          XX1 = pcaColumns(XX, toscale = toScale)
          XX1[,seq(from = 2, to = NXX)] = NULL
          colnames(XX1)[1] = paste("PC1(" ,paste(colnames(XX), sep= "", collapse = ",")
                                   ,")" ,sep = "")
          XX = XX1
        }
        
      }
    }
    if(length(intersect(algName,c('cooks')))  && is.null(Errors))
    {
      if(NXX > 1)
      {
        XX1 = pcaColumns(XX, toscale = toScale)
        XX1[,seq(from = 2, to = NXX)] = NULL
        colnames(XX1)[1] = paste("PC1(",paste(colnames(XX),sep="", collapse = ",")
                                 ,")",sep ="")
        XX = XX1
      }
      
    }
  }
  
  #result in list
  res = list(XX= XX, YY = YY, TT = TT, Errors = Errors, Values = Values,ID = ID, IndepVar = IndepVar, Tooltips = TT)
  return(res) 
  
}

# define X/Y data and labels wrt input data and parameters
TransformVisualizationData = function (DFX,DFY, plotType,algName,params,outlier_score, toScale = TRUE)
{
  
  res = NULL
  NRX = nrow(DFX)
  NCX = ncol(DFX)
  
  VXX = NULL
  VYY = NULL
  VXX_name = "VXX_name"
  VYY_name = "VYY_name"
  
  
  if(length(intersect(algName,c('zscore', 'manual', 'tukey', 'Tukey'))))
  {
    VXX = DFX[,1]
    VYY = seq(1,NRX)
    VXX_name = colnames(DFX)[1]
    VYY_name = "order"
    if(params$visualizeOutlierScore && length(intersect(algName,c('zscore', 'tukey', 'Tukey'))))
    {
      VXX = outlier_score
      VXX_name = paste('score(',colnames(DFX)[1],')',sep="")
    }
  }
  
  if(length(intersect(algName,c('LOF'))))
  {
    #density and boxplot --> VXX is outlier_score, VYY is seq(1,NRX)
    if(length(intersect(plotType,c('density','boxplot'))))
    {
      VXX = outlier_score
      VYY = seq(1,NRX)
      
      VXX_name = paste("score(",paste(colnames(DFX), sep = "", collapse = ","), ")", sep = "") 
      VYY_name = "order"
    }
    
    #scatter + NCX == 1 --> VXX is DFX, VYY is seq(1,NRX)
    if(length(intersect(plotType,c('scatter'))) && NCX == 1)
    {
      VXX = DFX[,1]
      VYY = seq(1,NRX)
      
      VXX_name = colnames(DFX)[1]
      VYY_name = "order"
      if(params$visualizeOutlierScore)
      {
        VXX = outlier_score
        VXX_name = paste("score(", VXX_name, ")", sep = "") 
      }
    }
    
    #scatter + NCX == 2 --> VXX is DFX[,1], VYY is DFX[,2]
    if(length(intersect(plotType,c('scatter'))) && NCX == 2)
    {
      VXX = DFX[,1]
      VYY = DFX[,2]
      
      VXX_name = colnames(DFX)[1]
      VYY_name = colnames(DFX)[2]
      if(params$visualizeOutlierScore)
      {
        VXX = outlier_score
        VXX_name = paste("score(", VXX_name, ")", sep = "") 
        
        VYY = seq(1,NRX)
        VYY_name = "order"
      }
    }
    #scatter + NCX > 2 --> VXX is PC1(DFX), VYY is PC2(DFX)
    if(length(intersect(plotType,c('scatter'))) && NCX > 2)
    {
      temp = pcaColumns(DFX, toscale = TRUE)
      # colnames(temp)[c(1,2)] = c("PC1","PC2")
      VXX = temp[, 1]
      VYY = temp[, 2]
      
      VXX_name = paste("PC1(", paste(colnames(DFX), sep = "", collapse = ",") ,")", sep = "")
      VYY_name = paste("PC2(",paste(colnames(DFX), sep = "", collapse = ","),")", sep = "")
      
      if(params$visualizeOutlierScore)
      {
        VXX = outlier_score
        VXX_name = paste("score(",paste(colnames(DFX), sep = "", collapse = ","), ")", sep = "") 
        
        VYY = seq(1,NRX)
        VYY_name = "order"
      }
    }
  }
  
  if(length(intersect(algName,c('cooks'))))
  {
    NCY = ncol(DFY)
    #density and boxplot --> VXX is cooks_dist, VYY is seq(1,NRX)
    if(length(intersect(plotType, c('density','boxplot'))) || params$visualizeOutlierScore )
    {
      VXX = outlier_score
      VYY = seq(1,NRX)
      
      VYY_name = "order"
      VXX_name = paste("score(",paste(colnames(DFX), sep = "", collapse = ","),
                       "~",
                       paste(colnames(DFY), sep = "", collapse = ","),
                       ")", sep = "")
    }
    else
    {
      #scatter + NCX == 1 --> VXX is DFX
      if(length(intersect(plotType, c('scatter'))) && NCX == 1)
      {
        VXX = DFX[, 1]
        VXX_name = colnames(DFX)[1] 
      }
      
      #scatter + NCY == 1 --> VYY is DFY
      if(length(intersect(plotType, c('scatter'))) && NCY == 1)
      {
        VYY = DFY[,1]
        VYY_name = colnames(DFY)[1] 
      }
      
      #scatter + NCX > 1 --> VXX is PC1(DFX)
      if(length(intersect(plotType, c('scatter'))) && NCX > 1)
      {
        temp = pcaColumns(DFX, toscale = toScale)#TODO toScale
        colnames(temp)[c(1, 2)] = c("PC1_X", "PC2_X")
        VXX = temp[, 1]
        VYY_name = paste("PC1(",paste(colnames(DFX), sep = "", collapse = ","),")",sep ="")
      }
      #scatter + NCY > 1 --> VYY is PC1(DFY)
      if(length(intersect(plotType,c('scatter'))) && NCY > 1)
      {
        temp = pcaColumns(DFY, toscale = TRUE)
        VYY = temp[,1]
        VYY_name = paste("PC1(", paste(colnames(DFY), sep = "", collapse = ","),")", sep ="")
      }
    }
  }
  
  #result in list
  res = list(VXX= VXX, VYY = VYY, VXX_name = VXX_name, VYY_name = VYY_name)
  return(res) 
}

# the function for outlier detection with several algorithms
getOutliers= function(DFX, DFY, algType, peaksOrSubpeaks,params)
{
  myOutliers = NULL
  outlier_scores = NULL
  
  
  if(algType == 'Tukey')
  {
    tuk = boxplot.stats(DFX[,1], do.conf = FALSE, coef = params$IQR)
    myOutliers = DFX[,1] %in% tuk$out 
    outlier_scores = abs((DFX[, 1] - median(DFX[, 1]) + SMALL_EPSILON)) / (IQR(DFX[,1]) + SMALL_EPSILON)
  }
  else
  {
    if(algType == 'zscore')
    {
      outlier_scores = (abs(scale(DFX[, 1])))
      myOutliers =  (outlier_scores > params$numSig)
    }
    else 
    {
      if(algType == 'manual')
      {
        outlier_scores = DFX[,1]
        myOutliers =  (DFX[,1] > params$hThresh) | (DFX[,1] < params$lThresh) 
      }
      else 
      {
        if(algType == 'LOF')
        {
          outlier_scores = my_lofactor(DFX, k = params$LOFK)
          # pick top as outliers
          myOutliers <- (outlier_scores > mean(outlier_scores)*params$LOFThresh)
          outlier_scores = (outlier_scores + SMALL_EPSILON) / (mean(outlier_scores) + SMALL_EPSILON) #normalize scale 
        }
        else # cooks
        {
          mod <- lm(DFX[,1] ~ ., data=DFY)
          outlier_scores <- cooks.distance(mod)
          myOutliers <- (outlier_scores > mean(outlier_scores) * params$cooksThresh)
          outlier_scores = (outlier_scores + SMALL_EPSILON) / (mean(outlier_scores) + SMALL_EPSILON) #normalize scale
        }
        
      }
      
    }
  }
  
  if(length(intersect(algType, c('Tukey','zscore','manual'))))
  {
    M = mean(DFX[,1]);
    
    if(peaksOrSubpeaks == 'Peaks')
      myOutliers = myOutliers & (DFX[,1] > M)
    if(peaksOrSubpeaks =='Subpeaks')
      myOutliers = myOutliers & (DFX[,1] < M)
  }
  res = list(myOutliers = myOutliers, outlier_scores = outlier_scores)
  return(res)
  
}

#utility function for axes scale format
scaleXYformat = function (scaleType, XorY, fp)
{
  if(XorY %in% "X")
  {
    if(scaleType %in% c("comma"))
      fp <- fp + scale_x_continuous( labels = comma)
    
    if(scaleType %in% c("scientific"))
      fp <- fp + scale_x_continuous(labels = scientific)
    
    if(scaleType %in% c("dollar"))
      fp <- fp + scale_x_continuous(labels = dollar)
    
  }
  if(XorY %in% "Y")
  {
    if(scaleType %in% c("comma"))
      fp <- fp + scale_y_continuous( labels = comma)
    
    if(scaleType %in% c("scientific"))
      fp <- fp + scale_y_continuous(labels = scientific)
    
    if(scaleType %in% c("dollar"))
      fp <- fp + scale_y_continuous(labels = dollar)
    
  }
  return (fp)
  
}

#label format: hAxis.format scientific, dollar , comma
GetGooFormat = function(scaleFormat)
{
  mapFormat = c('','scientific','currency', 'decimal')
  names(mapFormat) = c('none','scientific','dollar', 'comma')
  return(mapFormat[scaleFormat])
}

ConvertDF64encoding = function (df, withoutEncoding = FALSE)
{
  header_row <- paste(names(df), collapse=", ")
  tab <- apply(df, 1, function(x)paste(x, collapse=", "))
  
  if(withoutEncoding){
    text <- paste(c(header_row, tab), collapse="\n")
    x <- text
  }
  else
  {
    text <- paste(c(header_row, tab), collapse="\n")
    x <- caTools::base64encode(text)
  }
  return(x)
}


KeepOutDataInHTML = function(df, htmlFile = 'out.html', exportMethod = "copy", limitExportSize = 1000)
{
  if(!is.null(df) && nrow(df)>limitExportSize)
    df = df[1:limitExportSize,]
  
  outDataString64 = ConvertDF64encoding(df)
  
  linkElem = '\n<a href=""  download="data.csv"  style="position: absolute; top:0px; left: 0px; z-index: 20000;" id = "mydataURL">export</a>\n'
  updateLinkElem = paste('<script>\n link_element = document.getElementById("mydataURL");link_element.href = outDataString64href;', '\n</script> ', sep =' ')
  var64 = paste('<script> outDataString64 ="', outDataString64, '"; </script>', sep ="")
  var64href = paste('<script> outDataString64href ="data:;base64,', outDataString64, '"; </script>', sep ="")
  
  buttonElem = '<button style="position: absolute; top:0px; left: 0px; z-index: 20000;"  onclick="myFunctionCopy(1)">copy to clipboard</button>'
  funcScript = '<script> 
  function myFunctionCopy(is64) 
  {
  const el = document.createElement("textarea");
  if(is64)
  {
  el.value = atob(outDataString64);
  }
  else
  {
  el.value = outDataStringPlane;
  }
  document.body.appendChild(el);
  el.select();
  document.execCommand("copy");
  document.body.removeChild(el);};	
  </script>'
  
  if(exportMethod == "copy")
    endOfBody = paste(var64,funcScript, buttonElem,'\n</body>',sep ="")
  else#"download"
    endOfBody = paste(linkElem,var64, var64href,updateLinkElem,'\n</body>',sep ="")
  
  ReadFullFileReplaceString('out.html', 'out.html', '</body>', endOfBody)
  
}

############# Input validation & initializations ############# 

if(!exists('Values'))
  Values = NULL

if(!exists('ID'))
  ID = NULL

if(!exists('IndepVar'))
  IndepVar = NULL

if(!exists('Tooltips'))
  Tooltips = NULL

################### Actual code ####################
params = NULL
params$numSig = numSig
params$IQR = IQR
params$hThresh = hThresh
params$lThresh = lThresh
params$LOFThresh = LOFThresh
params$LOFK = LOFK
params$cooksThresh = cooksThresh
params$minRows = minRows # for Z/manual/Tukey/cooks
params$minRowsLOF = minRowsLOF
params$visualizeOutlierScore = visualizeOutlierScore
params$maxRows = maxRows
params$maxRowsLOF = maxRowsLOF

rrr0 = RemoveExtraRows(Values,ID, IndepVar , Tooltips, algName, params)
Values = rrr0$Values
ID = rrr0$ID
IndepVar = rrr0$IndepVar
Tooltips = rrr0$Tooltips

rrr = TransformInputData(Values,ID, IndepVar , Tooltips, algName, toScale,params)
XX = rrr$XX
YY = rrr$YY
TT = rrr$TT

Values = rrr$Values
ID = rrr$ID
IndepVar = rrr$IndepVar
Tooltips = rrr$Tooltips


#if errors skip all and show errors
if(!is.null(rrr$Errors))
{
  plotType = 'errors'
  params.errors = rrr$Errors
}else{
  
  rrr_outliers = getOutliers(DFX = XX, DFY = YY, algType = algName, 
                             peaksOrSubpeaks = thresholdType,params = params)
  
  myOutliers = rrr_outliers$myOutliers
  outliers_scores = rrr_outliers$outlier_scores
  
  rrr2 = TransformVisualizationData(XX,YY, plotType, algName, params, outliers_scores, toScale = toScale)
  data4viz = data.frame(X = rrr2$VYY, Y = rrr2$VXX)
  
  NP = nrow(XX)   
  pointsCol = rep(c(inlierColor), NP) 
  pointsCol[myOutliers] = outlierColor
  pointsSize = rep(pntSize * 1.5, NP)
  pointsSize[myOutliers] = pntSize * 2.5
  
  # sparsify 
  if(sparsify)
  {
    drawPoints = SparsifyScatter(data4viz)# remove points from dense regions
    data4viz=data4viz[drawPoints,]
    myOutliers = myOutliers[drawPoints]
    pointsCol = pointsCol[drawPoints]
    pointsSize = pointsSize[drawPoints]
    outliers_scores = outliers_scores[drawPoints]
    
  }else
    drawPoints = SparsifyScatter(data4viz,minMaxPoints = c(Inf, Inf))
  
  rrr2$VYY_name = cutStr2Show( rrr2$VYY_name, strCex = sizeLabel / 6, isH = FALSE, partAvailable = 0.85)
  rrr2$VXX_name = cutStr2Show( rrr2$VXX_name, strCex = sizeLabel / 6, isH = (plotType == 'density'), partAvailable = 0.85)
}


if(plotType == 'scatter')
{
  # plot as scatter 
  g <- ggplot(data=data4viz, 
              aes( x = X, y = Y,  color=factor(myOutliers))) +  
    geom_point(shape = 19, colour = alpha(pointsCol,transparency), size = pntSize * 2 ) + 
    xlab(rrr2$VYY_name) + ylab(rrr2$VXX_name) +
    theme(legend.position = "none")
  
  g = scaleXYformat(scaleXformat,"X",g)
  g = scaleXYformat(scaleYformat,"Y",g)
  
  g = g + labs (title = NULL, caption = NULL) + theme_bw() + 
    theme(plot.title  = element_text(hjust = 0.5, size = sizeWarn), 
          axis.title  =element_text(size =  sizeLabel, colour = colLabel),
          axis.text = element_text(size =  sizeTicks),
          panel.border = element_blank(), axis.line = element_line())
  
}

if(plotType == 'boxplot')
{ # plot as boxplot
  data4viz$X = 'box plot'
  g <- ggplot(data=data4viz, 
              aes( x = X, y = Y)) + 
    geom_boxplot(data=data4viz, aes(x = X, y = Y),outlier.shape = NA,  
                 outlier.size = NA, notch = FALSE,show.legend=F) + 
    geom_point(aes(x= 0.7, y = Y), shape = 19, colour = alpha(pointsCol,transparency), size = pointsSize, show.legend = F) + 
    xlab("") + ylab(rrr2$VXX_name) +
    theme(legend.position = "none") 
  
  g = scaleXYformat(scaleYformat,"Y", g)
}

# plot as distribution  
if(plotType == 'density')
{
  colnames(data4viz) = c('Y','X')
  g <-  ggplot(data4viz, aes(X)) +
    geom_density(show.legend = F, alpha = 0.2, fill = "#FF6666") +
    geom_point(aes(x= X, y = 0), colour = alpha(pointsCol,transparency), size = pointsSize) + 
    xlab(rrr2$VXX_name) + ylab("density") +
    theme(legend.position = "none") 
  
  g = scaleXYformat(scaleXformat,"X", g)
  
}

if(plotType == 'errors')
  g <-  ggplot()

g = g + labs (title = rrr$Errors[1], caption = NULL) + theme_bw() +
  theme(plot.title  = element_text(hjust = 0.5, size = sizeWarn),
        axis.title = element_text(size =  sizeLabel, colour = colLabel),
        axis.text = element_text(size =  sizeTicks),
        panel.border = element_blank(), axis.line = element_line())


if(plotType == 'errors') # remove box from empty plot 
{ 
  g = g + theme(axis.line = element_blank())
  ddd = data.frame()
}


if(USEPLOTLY)
{
  gply <-  ggplotly(g) 
  
  disabledButtonsList <- list('toImage', 'sendDataToCloud', 'zoom2d', 'pan', 'pan2d', 'select2d', 'lasso2d', 'hoverClosestCartesian', 'hoverCompareCartesian')
  gply$x$config$modeBarButtonsToRemove = disabledButtonsList
  
  gply <- config(gply, staticPlot = FALSE, editable = FALSE, sendData = FALSE, showLink = FALSE,
                 displaylogo = FALSE,  collaborate = FALSE, cloud=FALSE)
  
  if(plotType != 'errors')# take care of tooltips
  {
    ddd = Values
    if(!is.null(IndepVar)) ddd = cbind(ddd,IndepVar)
    if(!is.null(Tooltips)) ddd = cbind(ddd,Tooltips)
    ddd = cbind(ddd[drawPoints,, drop=FALSE],score = outliers_scores)
    ntt = generateNiceTooltips(as.data.frame(ddd), digits = 2,  maxRows = 3)
    
    layerScatterInScatter = 1 # first layer is scatter in scatter 
    layerScatterInBoxplot = 2 # sec layer is scatter in boxplot/density
    
    # tooltips on scatter
    if(plotType == 'scatter')
      gply$x$data[[layerScatterInScatter]]$text = ntt
    if(plotType %in% c('boxplot','density'))
      gply$x$data[[layerScatterInBoxplot]]$text = ntt
    
    #extend Y-axis range slightly 
    yrange = gply$x$layout$yaxis$range
    if(!is.null(yrange))
      gply$x$layout$yaxis$range = (yrange - mean(yrange))*yRangeExtend + mean(yrange)
  
  }
  if(Sys.getenv("RSTUDIO") != "")#DEBUG
    print(gply)
}else{
  print(g)
}


############# Create and save widget ###############
internalSaveWidget(gply, 'out.html');
# resolve bug in plotly (margin of 40 px)
ReadFullFileReplaceString('out.html', 'out.html', ',"padding":40,', ',"padding":0,')

if(keepOutData && plotType != 'errors') 
{
  exportDF = as.data.frame(ddd)
  exportDF$outliers = rrr_outliers$myOutliers
  
  KeepOutDataInHTML(df = exportDF, htmlFile = 'out.html', exportMethod = exportMethod, limitExportSize = limitExportSize)
}