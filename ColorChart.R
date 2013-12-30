# efg, Stowers Institute for Medical Research
# efg's Research Notes:
#   http://research.stowers-institute.org/efg/R/Color/Chart
#
# 6 July 2004.  Modified 23 May 2005.

pdf("ColorChart.pdf", width=6, height=10)

# save to reset at end
oldparameters <- par(mar=c(1,1,2,1), mfrow=c(2,1))

# Be cautious in case definition of "colors" changes.
# Use some hard-coded constants since this is not expected
# to change.
stopifnot(length(colors()) == 657)

# 0. Setup

# For a given color, define a text color that will have good contrast.
#   Examples:
#     > SetTextContrastColor("white")
#     [1] "black"
#     > SetTextContrastColor("black")
#     [1] "white"
#     > SetTextContrastColor("red")
#     [1] "white"
#     > SetTextContrastColor("yellow")
#     [1] "black"
SetTextContrastColor <- function(color)
{
  ifelse( mean(col2rgb(color)) > 127, "black", "white")
}

# Define this array of text contrast colors that correponds to each
# member of the colors() array.
TextContrastColor <- unlist( lapply(colors(), SetTextContrastColor) )


# 1a. Plot matrix of R colors, in index order, 25 per row.
# This example plots each row of rectangles one at a time.
colCount <- 25  # number per row
rowCount <- 27

plot( c(1,colCount), c(0,rowCount), type="n", ylab="", xlab="",
     axes=FALSE, ylim=c(rowCount,0))
title("R colors")
mtext("http://research.stowers-institute.org/efg/R/Color/Chart",
      cex=0.6)

for (j in 0:(rowCount-1))
{
  base <- j*colCount
  remaining <- length(colors()) - base
  RowSize <- ifelse(remaining < colCount, remaining, colCount)
  rect((1:RowSize)-0.5,j-0.5, (1:RowSize)+0.5,j+0.5,
    border="black",
    col=colors()[base + (1:RowSize)])
  text((1:RowSize), j, paste(base + (1:RowSize)), cex=0.7,
       col=TextContrastColor[base + (1:RowSize)])
}

# 1b.  Plot matrix of R colors, in "hue" order, 25 per row.
# This example plots each rectangle one at a time.
RGBColors <- col2rgb(colors()[1:length(colors())])
HSVColors <- rgb2hsv( RGBColors[1,], RGBColors[2,], RGBColors[3,], maxColorValue=255)
HueOrder  <- order( HSVColors[1,], HSVColors[2,], HSVColors[3,] )

plot(0, type="n", ylab="", xlab="",
     axes=FALSE, ylim=c(rowCount,0), xlim=c(1,colCount))

title("R colors -- Sorted by Hue, Saturation, Value")

for (j in 0:(rowCount-1))
{
  for (i in 1:colCount)
  {
    k <- j*colCount + i
    if (k <= length(colors()))
    {
      rect(i-0.5,j-0.5, i+0.5,j+0.5, border="black", col=colors()[ HueOrder[k] ])
      text(i,j, paste(HueOrder[k]), cex=0.7, col=TextContrastColor[ HueOrder[k] ])
    }
  }
}


# 2. Create 7-page color chart showing rectangle block of color, along with
# index, color name, and RGB constants in hex and decimal.

# Define string vector of RGB hex and decimal constants for given color
# as a string.
#   Example:
#     > GetColorHexAndDecimal("yellow")
#     [1] "#FFFF00   255 255   0"
GetColorHexAndDecimal <- function(color)
{
  c <- col2rgb(color)
  sprintf("#%02X%02X%02X   %3d %3d %3d", c[1],c[2],c[3], c[1], c[2], c[3])
}

# Restore, change and save graphics parameters
par(oldparameters)
oldparameters <- par(mar=c(1,1,1,1))

# Prepare text vectors to be displayed, in addition to color names.
index <- paste(1:length(colors()))
HexAndDec <- unlist( lapply(colors(), GetColorHexAndDecimal) )

PerColumn <- 50
PerPage   <- 2*PerColumn

# Plot a column of color rectangles at a time for each page.
for (page in 1: (trunc( (length(colors()) + (PerPage-1)) / PerPage) )  )
{

  plot(0, type="n", ylab="", xlab="",
       axes=FALSE, ylim=c(PerColumn,0), xlim=c(0,1))
  title("R colors")
  mtext(paste("page ", page), SOUTH<-1, adj=1, line=-1)

  base <- PerPage*(page-1)

  # Column 1
  remaining <- length(colors()) - base
  ColumnSize <- ifelse(remaining < PerColumn, remaining, PerColumn)

  rect(0.00, 0:(ColumnSize-1),
       0.49, 1:ColumnSize,
       border="black",
       col=colors()[(base+1):(base+ColumnSize)])
  text(0.045, 0.45+(0:(ColumnSize-1)), adj=1,
       index[(base+1):(base+ColumnSize)], cex=0.6,
       col=TextContrastColor[(base+1):(base+ColumnSize)])
  text(0.06, 0.45+(0:(ColumnSize-1)), adj=0,
       colors()[(base+1):(base+ColumnSize)], cex=0.6,
       col=TextContrastColor[(base+1):(base+ColumnSize)])
  save <- par(family="mono")  # use mono-spaced font with number columns
  text(0.25, 0.45+(0:(ColumnSize-1)), adj=0,
       HexAndDec[(base+1):(base+ColumnSize)], cex=0.6,
       col=TextContrastColor[(base+1):(base+ColumnSize)])
  par(save)

  # Column 2
  if (remaining > PerColumn)
  {
    remaining <- remaining - PerColumn
    ColumnSize <- ifelse(remaining < PerColumn, remaining, PerColumn)
    rect(0.51, 0:(ColumnSize-1),
         1.00, 1:ColumnSize,
         border="black",
         col=colors()[(base+PerColumn+1):(base+PerColumn+ColumnSize)])
    text(0.545, 0.45+(0:(ColumnSize-1)), adj=1,
         index[(base+PerColumn+1):(base+PerColumn+ColumnSize)], cex=0.6,
         col=TextContrastColor[(base+PerColumn+1):(base+PerColumn+ColumnSize)])
    text(0.56, 0.45+(0:(ColumnSize-1)), adj=0,
         colors()[(base+PerColumn+1):(base+PerColumn+ColumnSize)], cex=0.6,
         col=TextContrastColor[(base+PerColumn+1):(base+PerColumn+ColumnSize)])
    save <- par(family="mono")
    text(0.75, 0.45+(0:(ColumnSize-1)), adj=0,
         HexAndDec[(base+PerColumn+1):(base+PerColumn+ColumnSize)], cex=0.6,
         col=TextContrastColor[(base+PerColumn+1):(base+PerColumn+ColumnSize)])
    par(save)
  }

}

par(oldparameters)
dev.off()
