# constCol1, constCol2, and plotCol are vectors of the columns of interest.
# constVar1, constVar2, and varName are (optional) strings for the names of these variables, to be used in the plot labels.
# displayHistogram plots the frequencies of entries in plotCol, 
# for records where the value of constCol1 is col1Val and constCol2 is col2Val.
# Because this is a histogram, it is best when plotCol is a category.
displayHistogram = function(constCol1, col1Val, constCol2, col2Val, plotCol, constVar1="", constVar2="", varName="") {
  data = data.frame(constCol1, constCol2, as.factor(plotCol))
  colnames(data) = c("col1", "col2", "col3")
  data = subset(data, (data$col1 == col1Val & data$col2 == col2Val))
  plotTitle = paste("Histogram for ", constVar1, " = ", col1Val, " and ", constVar2, " = ", col2Val)
  plot(data$col3, main = plotTitle, xlab = varName, ylab = "Count")
  return(data)
}

# constCol1, constCol2, xCol, yCol are vectors of the columns of interest.
# constVar1, constVar2, xName, and yName are (optional) strings for the names of these variables, to be used in the plot labels.
# displayScatterplot plots xCol vs yCol, for records where the value of constCol1 = col1Val and constCol2 = col2Val.
# Because this is a scatterplot, xCol and yCol should be interpretable as numbers.
displayScatterplot = function(constCol1, col1Val, constCol2, col2Val, xCol, yCol, constVar1="", constVar2="", xName="", yName="") {
  data = data.frame(constCol1, constCol2, as.numeric(xCol), as.numeric(yCol))
  colnames(data) = c("col1", "col2", "colX", "colY")
  data = subset(data, (data$col1 == col1Val & data$col2 == col2Val))
  plotTitle = paste("Scatterplot for ", constVar1, " = ", col1Val, " and ", constVar2, " = ", col2Val)
  plot(data$colX, data$colY, main = plotTitle, xlab = xName, ylab = yName)
  return(data)
}

# constCol1, constCol2, catCol, yCol are vectors of the columns of interest.
# constVar1, constVar2, catName, and yName are (optional) strings for the names of these variables, to be used in the plot labels.
# displayBoxplot plots a boxplot of yCol values for each value "level" of catCol, 
# for records where the value of constCol1 = col1Val and constCol2 = col2Val.
# Because this is a boxplot, yCol should be interpretable as a number and catCol should be a category.
displayBoxplot = function(constCol1, col1Val, constCol2, col2Val, catCol, yCol, constVar1="", constVar2="", catName="", yName="") {
  data = data.frame(constCol1, constCol2, as.factor(catCol), as.numeric(yCol))
  colnames(data) = c("col1", "col2", "colCat", "colY")
  data = subset(data, (data$col1 == col1Val & data$col2 == col2Val))
  plotTitle = paste("Boxplot for ", constVar1, " = ", col1Val, " and ", constVar2, " = ", col2Val)
  boxplot(colY~colCat, data, main = plotTitle, xlab = catName, ylab = yName)
  return(data)
}

# Sample use: (navigate to the proper directory of course, and change the file name to yours)
# hf = read.csv("HF/2009_LA_allColsPresent.csv", colClasses = "character")
# 
# testHist = displayHistogram(hf$sex, 1, hf$race, 1, hf$agecat20, "Gender", "Race", "Age Category")
# testScatter = displayScatterplot(hf$sex, 1, hf$race, 1, hf$los, hf$charge, "Gender", "Race", "Length of Stay", "Total Charges")
# testBox = displayBoxplot(hf$sex, 1, hf$race, 1, hf$pay_cat, hf$los, "Gender", "Race", "Payment Category", "Length of Stay")
