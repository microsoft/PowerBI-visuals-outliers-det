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

#utils.r 
#file with small util methods no external dependencies

libraryRequireInstall = function(packageName, ...)
{
  if(!require(packageName, character.only = TRUE)) 
    warning(paste("*** The package: '", packageName, "' was not installed ***", sep=""))
}

# Postprocess text string (usually to show as labels)
# if very very long abbreviate
# if looooooong convert to lo...
# if shorter than maxChar remove 
cutStr2Show = function(strText, strCex = 0.8, abbrTo = 100, isH = TRUE, maxChar = 3, partAvailable = 1)
{
  # partAvailable, wich portion of window is available, in [0,1]
  if(is.null(strText))
    return (NULL)
  
  SCL = 0.075 * strCex / 0.8
  pardin = par()$din
  gStand = partAvailable*(isH*pardin[1]+(1-isH)*pardin[2]) /SCL
  
  # if very very long abbreviate
  if(nchar(strText)>abbrTo && nchar(strText)> 1)
    strText = abbreviate(strText, abbrTo)
  
  # if looooooong convert to lo...
  if(nchar(strText)>round(gStand) && nchar(strText)> 1)
    strText = paste(substring(strText,1,floor(gStand)),"...",sep = "")
  
  # if shorter than maxChar remove 
  if(gStand <= maxChar)
    strText = NULL
  
  return(strText) 
}

#if it attributeColumn is legal colors() use them 
#if all the entries in attributeColumn are the same number - use defaultColor
#if it has many numeric variables color from green to red range 
#if it has few unique strings - use rainbow to color them 
ColorPerPoint = function (attributeColumn, defaultColor = pointsCol, sizeColRange = 30)
{
  N = length(attributeColumn)
  if(sum(attributeColumn %in% colors()) == N) # all legal colors
    return(attributeColumn)
  
  UN = length(unique(attributeColumn))
  if(UN == 1) # single number 
    return(defaultColor)
  
  sortedUniqueValues = sort(unique(attributeColumn))
  
  if((UN > sizeColRange*3) || (UN >= N - 2 && is.numeric(attributeColumn))) # many numbers --> color range 
  {
    rangeColors = terrain.colors(sizeColRange)# 30 colors
    if(is.numeric(attributeColumn))
    {
      breaks = seq(min(sortedUniqueValues), max(sortedUniqueValues), length.out = sizeColRange + 1)
      pointsCol = as.character(cut(attributeColumn, breaks, labels = rangeColors))
      return(pointsCol)
    }
    else
    {# spread colors
      outCol = rep(rangeColors, each = ceiling(N / sizeColRange), length.out = N)
      return(outCol)
    }
  } else {
    rangeColors = rainbow(UN)
    names(rangeColors) = sortedUniqueValues
    return(rangeColors[as.character(attributeColumn)])
  }
}


#randomly remove points from scatter if too many 
SparsifyScatter = function (xyDataFrame, numXstrips = 9, numYstrips = 7, minMaxPoints = c(3000,9000), minmaxInStrip =  c(900,9000), maxInCell = 300, remDuplicated = TRUE)
{
  
  N_big = N = nrow(xyDataFrame)
  usePoints = rep(TRUE, N)
  
  if(N <= minMaxPoints[1]) # do nothing
    return (usePoints)
  
  if(remDuplicated) # remove duplicated
  {
    usePoints = usePoints & (!duplicated(xyDataFrame))
    N = sum(usePoints)
  }
  
  if(N <= minMaxPoints[1]) # do nothing
    return (usePoints)
  
  rangeX = range(xyDataFrame[,1])
  rangeY = range(xyDataFrame[,2])
  
  gridX = seq(rangeX[1],rangeX[2], length.out = numXstrips + 1)
  gridY = seq(rangeY[1],rangeY[2], length.out = numYstrips + 1)
  
  #go cell by cell and sparsify 
  for (iX in seq(1,numXstrips))
  {
    smallRangeX = c(gridX[iX],gridX[iX+1])
    inStrip = xyDataFrame[,1]>= smallRangeX[1] & xyDataFrame[,1]<= smallRangeX[2] &  usePoints
    if(sum(inStrip) > minmaxInStrip[1])
      for (iY in seq(1,numYstrips))
      {
        smallRangeY = c(gridY[iY],gridY[iY+1])
        inCell = xyDataFrame[,2]>= smallRangeY[1] & xyDataFrame[,2]<= smallRangeY[2] &  inStrip
        if(sum(inCell) > maxInCell)
        {
          inCellIndexes = seq(1,N_big)[inCell]
          #randomly select maxInCell out of inCellIndexes
          iii = sample(inCellIndexes,size = sum(inCell) - maxInCell, replace = FALSE)
          usePoints[iii] = FALSE
        }
      }
    
  }
  N = sum(usePoints)
  
  #if by the end still too many points --> go on whole set  
  if(N > minMaxPoints[2])
  {
    inIndexes = seq(1,N_big)[usePoints]
    #randomly select minMaxPoints[2] out of inIndexes
    iii = sample(inIndexes,size = minMaxPoints[2], replace = FALSE)
    usePoints[-iii] = FALSE
    
  }
  return (usePoints)
}

# return FALSE if canvas is too small
goodPlotDimension = function(minWidthInch = 3,minHeightInch = 2.2)
{
  re = (par()$din[1] > minWidthInch) & (par()$din[2] > minHeightInch)
  return(re)
}

#scale all numeric columns
standardizeColumns = function (mydataFrame)
{
  Q = mydataFrame
  Q[,sapply(Q,is.numeric)]= sapply(Q[,sapply(Q,is.numeric)],scale)
  names(Q)[sapply(Q,is.numeric)]= paste("scaled_",names(Q)[sapply(Q,is.numeric)],sep = "")
  return(Q) 
}

#PCA applied to all numeric columns
pcaColumns = function (myDF,center = TRUE, toscale = TRUE)
{
  Q = myDF
  x =  prcomp(Q[,sapply(Q,is.numeric)],center = center, scale. = toscale)$x
  Q[,sapply(Q,is.numeric)]= x
  names(Q)[sapply(Q,is.numeric)] = colnames(x)
  return(Q) 
}

#remove non-numeric columns
RemoveNonNumeric = function(df)
{
  newDF = as.data.frame(df[,sapply(df,is.numeric)])
  colnames(newDF) = colnames(df)[sapply(df,is.numeric)]
  if(length(newDF)==0)
    newDF = NULL
  return(newDF)
}

#paste tooltips together separated by <br>
generateNiceTooltips = function(dataset, br = "<br>", digits = 2, maxRows = 3)
{
 
  myNames = colnames(dataset)
  
  nuc = sapply(dataset,is.numeric)
  if(sum(nuc))
    dataset[,nuc] = round(dataset[,nuc], digits)
  
  LMN = length(myNames)
  s = 1
  
  nms = myNames[s:LMN]
  dta = dataset[,s:LMN]
  niceTooltips = NULL
  
  chunksInOneLine =  ceiling(LMN/maxRows)
  countInLine = 0;
  
  for (n in c(1:length(nms)))
  {
 #   browser()
    countInLine = countInLine + 1
    br1 = ifelse(countInLine %% chunksInOneLine == 0, br, ';')
      
    
    if(length(nms) == 1)
      niceTooltips = paste(nms," = ", dta, sep = "") 
    else
    {
      niceTooltips = paste(niceTooltips,nms[n]," = ", dta[,n], sep = "")  
      if(n < length(nms))
        niceTooltips = paste(niceTooltips, br1, sep = "")
    }
  }
  return(niceTooltips)
}
