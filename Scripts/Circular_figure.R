library(circlize)
#par(mfrow = c(1,2))
julday<-1:365
monstart<-c(1,31,59,90,120,151,181,212,243,273,304,334,367)
mon <- c("January","February","March","April","May","June","July","August","September","October","November","December")
days <- rep(31,12)
days[c(4,6,9,11)] <- 30
days[2] <- 28
mon.xlim <- cbind(rep(1,12),days)

glMon <- as.numeric(format(as.Date(glDates[,2],origin = as.Date("2011-01-01")),"%m"))
glDay <- as.numeric(format(as.Date(glDates[,2],origin = as.Date("2011-01-01")),"%d"))

NBd <- hist(glDates[,2],plot = FALSE, breaks = 10)
Ba <- hist(glDates[,3],plot = FALSE, breaks = 10)
Bd <- hist(glDates[,5],plot = FALSE, breaks =15)
NBa <- hist(glDates[,6],plot = FALSE, breaks = 10)

################## FIGURE arrival departure ##############################################################33
#tiff("CircularPlot.tiff",res = 600, width = 3600, height = 3600, units = "px",compression = "lzw")
#circos.clear()
#circos.par(start.degree = 270,
#           gap.degree = 0,
#           cell.padding = c(0,0,0,0))
#circos.initialize(factors = 1:12, xlim =mon.xlim)
#circos.trackPlotRegion(factors = 1:12, ylim = c(0,15),track.height = 0.6,bg.border = NA)

#circos.trackLines(factors = as.numeric(format(as.Date(round(NBd$mids),origin = as.Date("2011-01-01")),"%m")),
#                  x = as.numeric(format(as.Date(round(NBd$mids),origin = as.Date("2011-01-01")),"%d")),
#                  y = NBd$counts, col = "black", lwd = 2, type = "h")

#circos.trackLines(factors = as.numeric(format(as.Date(round(Ba$mids),origin = as.Date("2011-01-01")),"%m")),
#                  x = as.numeric(format(as.Date(round(Ba$mids),origin = as.Date("2011-01-01")),"%d")),
#                  y = Ba$counts, col = "gray", lwd = 2, type = "h")
#

#circos.trackLines(factors = as.numeric(format(as.Date(round(Bd$mids),origin = as.Date("2011-01-01")),"%m")),
#                  x = as.numeric(format(as.Date(round(Bd$mids),origin = as.Date("2011-01-01")),"%d")),
#                  y = Bd$counts, col = "black", lwd = 2, type = "h")

#circos.trackLines(factors = as.numeric(format(as.Date(round(NBa$mids),origin = as.Date("2011-01-01")),"%m")),
#                  x = as.numeric(format(as.Date(round(NBa$mids),origin = as.Date("2011-01-01")),"%d")),
#                  y = NBa$counts, col = "gray", lwd = 2, type = "h")

#circos.trackPlotRegion(factors = 1:12, ylim = c(0,1),track.height = 0.01,bg.border = NA)
# Depart points #
#glMon1 <- as.numeric(format(as.Date(c(glDates[,2],glDates[,5]),origin = as.Date("2011-01-01")),"%m"))
#glDay1 <- as.numeric(format(as.Date(c(glDates[,2],glDates[,5]),origin = as.Date("2011-01-01")),"%d"))

#circos.trackPoints(factor = c(glMon1), x = glDay1, y = rep(0,42),pch =20,cex = 0.4, col = "black")

# Arrive points #
#glMon2 <- as.numeric(format(as.Date(c(glDates[,3],glDates[,6]),origin = as.Date("2011-01-01")),"%m"))
#glDay2 <- as.numeric(format(as.Date(c(glDates[,3],glDates[,6]),origin = as.Date("2011-01-01")),"%d"))
#circos.trackPoints(factor = c(glMon2), x = glDay2, y = rep(1,42),pch = 20,cex = 0.4, col = "gray")
#circos.track(ylim = c(0, 1), factors = 1:12, bg.col = "gray88", track.height = 0.10)

#for(i in 1:39){
#circos.link(sector.index1 = glMon1[!is.na(glMon1)][i], glDay1[!is.na(glMon1)][i], 
#            sector.index2 = glMon2[!is.na(glMon1)][i],glDay2[!is.na(glMon1)][i], col = "gray60", h = 0.1, lwd = 0.25)
#}
#for(i in 1:12){
#circos.axis(sector.index = i,h = "top", direction = "inside",
#            major.at = c(1,5,15,25,30), 
#            labels =c("","","","","",""),
#            major.tick.length = 0.2, labels.cex = 1,
#            labels.away.percentage = 4)
#}
#circos.trackText(x = rep(15, 12), y = rep(0.35, 12),
#    labels = substr(mon,1,3), facing = "bending",niceFacing = TRUE,
#    cex = 0.9, factors = 1:12, col = "black", font = 2)


#cols <- colorRampPalette(c("blue","yellow","red"))(26)
#r <- raster::raster(nrow = 1, ncol = 26)
#r[]<-0:25
#par(new = TRUE, fig = c(0.3,0.75,0.05,0.26))
#plot(r, legend.only = TRUE, breaks = seq(0,25,1),col = cols,horiz = TRUE,
#legend.width=1, legend.shrink=0.75,
#     axis.args=list(at=c(0,5,9,13,17,21,25),
#                    labels=c("0","100","200","300","400","500","600+"), 
#                    cex.axis=0.75,
#                    mgp = c(0,0,0),
#                    tck = -0.5),
#     legend.args=list(text=expression("Migration rate km day"^-1), side=1, font=2, line=1.1, tck = -1,cex=0.9,mgp = c(0,0,0)))

#par(new = TRUE, fig = c(0,0.4,0.005,0.395))
#par(bty = "l",mar = c(4,4,0,0))
#plot(glDates[,3]~glDates[,2],
#     xlim = c(95,130), ylim = c(120,140),pch = 19,
#     yaxt = "n",
#     ylab = "",
#     xlab = "",
#     xaxt = "n",
#     cex = 1,
#     cex.axis = 1,
#     col = cols[cut(glDates[,4],c(seq(0,600,25),5000))])
#axis(2, las = 2, cex.axis = 0.8,tck=-0.02,mgp=c(0,0.5,0))
#axis(1, las = 1, at = seq(95,130,5), labels = seq(95,130,5),cex.axis = 0.8,tck=-0.02,mgp=c(0.5,0,0))
#polygon(x = c(nb.depart,rev(nb.depart)),
#        y = c(D$q2.5$b.arrive,rev(D$q97.5$b.arrive)),
#        border = rgb(150,150,150,150,maxColorValue = 255),
#        col = rgb(150,150,150,150, maxColorValue = 255))
#points(D$mean$b.arrive~nb.depart,type = "l")
#points(glDates[,3]~glDates[,2], pch = 19, cex = 0.5,col = cols[cut(glDates[,4],seq(0,600,25))])
#mtext(side = 1, text = "Non-breeding\nDeparture",line = 1.8, cex = 0.8)
#mtext(side = 2, text = "Breeding\nArrival",line = 2, cex = 0.8)

#par(new = TRUE, fig = c(0.55,0.95,0.005,0.395))
#par(bty = "l",mar = c(4,4,0,0))
#plot(glDates[,6]~glDates[,5],
#     xlim = c(210,300), ylim = c(230,310),pch = 19,
#     yaxt = "n",
#     xaxt = "n",
#     ylab = "",
#     xlab = "",
#     cex = 1,
#     cex.axis = 1,
#     col = cols[cut(glDates[,7],c(seq(0,600,25),5000))])
#axis(2, las = 2, at = seq(230,310,20), labels = seq(230,310,20),cex.axis = 0.8,tck=-0.02,mgp=c(0,0.5,0))
#axis(1, at = seq(210,300,20), labels = seq(210,300,20), cex.axis = 0.8,tck=-0.02,mgp=c(0.5,0,0))
#polygon(x = c(b.depart,rev(b.depart)),
#        y = c(D$q2.5$nb.arrive,rev(D$q97.5$nb.arrive)),
#        border = rgb(150,150,150,150,maxColorValue = 255),
#        col = rgb(150,150,150,150, maxColorValue = 255))
#points(D$mean$nb.arrive~b.depart,type = "l")
#points(glDates[,6]~glDates[,5], pch = 19, cex = 0.5,col = cols[cut(glDates[,7],c(seq(0,600,25),5000))])
#mtext(side = 1, text = "Breeding\nDeparture",line = 1.8, cex = 0.8)
#mtext(side = 2, text = "Non-breeding\nArrival",line = 2, cex = 0.8)
#dev.off()
system("open CircularPlot.tiff")
