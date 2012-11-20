
#system("python leaderGrabber.py")

FILENAME <- "columbia-university-introduction-to-data-science-fall-2012_public_leaderboard.csv"
TITLE <- "Columbia Intro Data Science 2012, Kaggle Competition"
YRANGE <- c(.4, .8)

#Load the main data
scores <- read.csv(FILENAME)
scores$SubmissionDate <- strptime(scores$SubmissionDate, "%m/%d/%Y %r")
scores <- scores[order(scores$SubmissionDate), ]

#mindate <- min(scores$SubmissionDate)
#mindate <- strptime(format(mindate, "%Y-%m-01"), "%Y-%m-%d")
#hard coding mindate as 10-10-2012 because that's when it was handed out
mindate <- strptime("2012-10-10", "%Y-%m-%d")
maxdate <- max(scores$SubmissionDate)
maxdate <- strptime(format(maxdate, "%Y-%m-01"), "%Y-%m-%d")
maxdate <- seq(maxdate, by="month", length=2)[2]
maxdate <- min(c(maxdate, Sys.time()))
xrange <- c(mindate, maxdate)

#Ensure teh text size and trim length are set properly to show as much name as possible
TEXTSIZE <- .75
MAXLEN <- 16/TEXTSIZE
adjustmentfactor=(YRANGE[2]-YRANGE[1])*.015
adjustmentpadding=adjustmentfactor*.005
palette(c("#E41A1C", "purple", "#A6D854", "#A6761D", "orange", "#377EB8", 
"#FF00AA", "#1B9E77", "turquoise", "#66A61E", "blue", 
"red", "forest green", "#FC8D62", "orange",
"#7570B3", "#E78AC3", "#CF0234", "#1B9E77", "#66A61E", 
"#D95F02", "#E6AB02", "blue"))
colors <- palette()


#Make sure the final labels will be sufficiently spread out
#This finds any points that are close together than adjustmentfactor times the
#text size And moves the top one up, and the bottom one down by about half the
#space Required to make them adjustmentfactor*TEXTSIZE apart. It movies the top
#one up slightly more because that was more aesthetically pleasing
bests <- aggregate(scores$Score, list(TeamName = scores$TeamName), max)
bests <- bests[order(bests$x), ]
badPoints <- which(diff(bests$x) < adjustmentfactor*TEXTSIZE)
i <- 0
while (length(badPoints) > 0)
  {
  bests$x[badPoints] <- bests$x[badPoints] - (((adjustmentfactor * TEXTSIZE + adjustmentpadding) - diff(bests$x)[badPoints])*0.5)
  bests$x[badPoints+1] <- bests$x[badPoints+1] + (((adjustmentfactor * TEXTSIZE + adjustmentpadding) - diff(bests$x)[badPoints])*0.5)
  badPoints <- which(diff(bests$x) < adjustmentfactor*TEXTSIZE)
  i <- i + 1
  }
nTeams <- nrow(bests)
print(paste("Spreading required", i, "iterations"))

png(filename="datascience_leaderboard.png", width=1024, height=1024)

#Setup the plot, title, axis labels, etc
par(mar=par()$mar+c(0,0,0,6),bty="l",yaxs="i", xaxs="i")
plot(xrange, YRANGE, type="n", xaxt='n', xlab="Submission Time", ylab="Score", main=TITLE)
atx <- seq(mindate, maxdate, by=(maxdate-mindate)/6)
axis(1, at=atx, labels=format(atx, "%b\n%d"), padj=0.5)
mtext(side=3, text=paste(nrow(scores), "submissions by", nTeams, "teams as of ", format(maxdate, format="%B %d %Y %l:%M %p")))
mtext(side=4, text="Team & Current Score", at=YRANGE[2], las=2, line=-0.5)

colori = 1

#For each TeamName plot their Scores and label in the margin
for (TeamName in unique(scores$TeamName)) {
    i = which(unique(scores$TeamName)==TeamName)
    currScore <- max(scores$Score[scores$TeamName==TeamName])
    xvals <- scores$SubmissionDate[scores$TeamName==TeamName]
    yvals <- scores$Score[scores$TeamName==TeamName]
	#These next two lines add a datapoint for their current Score right now
    xvals <- append(xvals, maxdate)
    yvals <- append(yvals, currScore)
    
    #if they stayed still they'll be black, otherwise we get another color
    if (grepl("Benchmark", TeamName, ignore.case=TRUE)) { 
      color <- "black"
    } else if (min(yvals) == max(yvals)) {
        color <- "gray30"
    } else {
        color <- colors[colori]
        colori <- colori + 1
    }
    
    lines(xvals, yvals, col=color, lwd=2, type="s")
    displayName <- TeamName
    #Trim the TeamName name if it's too long to be shown
    if (nchar(as.character(displayName)) > MAXLEN) {
        displayName <- paste(substring(displayName, 0, MAXLEN-3), "...", sep="")
    }
    mtext(side=4, at=bests$x[bests$TeamName==TeamName], text=paste(displayName, round(currScore, 2)), col=color, line=0.5, las=2, cex=TEXTSIZE)
    print(paste(TeamName, color))
}
dev.off()


library(rjson)
sink('scores.json')
cat(toJSON(scores))
sink()
