# http://stackoverflow.com/questions/15014595/how-to-use-black-and-white-fill-patterns-instead-of-color-coding-on-calendar-hea?lq=1
##########################################
extra.calendarHeat <- function(dates, 
                               values, 
                               ncolors=99, 
                               color="b2w", 
                               pch.symbol = 15:20,
                               cex.symbol =2,
                               col.symbol ="#00000044",
                               pvalues=values,
                               varname="Values",
                               date.form = "%Y-%m-%d", ...) {
  
  require(latticeExtra) 
  require(grid)
  require(chron)
  
# Reshape data  ------------------------------------------------------------------ 
  transformdata <- function(dates,  values,pvalues, date.form = "%Y-%m-%d", ...) {
    
    if (class(dates) == "character" | class(dates) == "factor" ) {
      dates <- strptime(dates, date.form)
    }
    caldat <- data.frame( dates = dates,value = values,pvalue=pvalues)
    min.date <- as.Date(paste(format(min(dates), "%Y"),
                              "-1-1",sep = ""))
    max.date <- as.Date(paste(format(max(dates), "%Y"),
                              "-12-31", sep = ""))
    dates.f <- seq(min.date, max.date, by="days")
    
    # Merge moves data by one day, avoid
    caldat <- data.frame(value = values, dates = dates,pvalue=pvalues)
    caldat <- data.frame(date.seq =  seq(min.date, max.date, by="days") , 
                         value = NA,
                         pvalue=NA)
    dates <- as.Date(dates) 
    caldat$value[match(dates, caldat$date.seq)] <- values
    caldat$pvalue[match(dates, caldat$date.seq)] <- pvalues
    caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
    caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1
    caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
    caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
    yrs <- as.character(unique(caldat$yr))
    d.loc <- as.numeric()                        
    for (m in min(yrs):max(yrs)) {
      d.subset <- which(caldat$yr == m)  
      sub.seq <- seq(1,length(d.subset))
      d.loc <- c(d.loc, sub.seq)
    }  
    
    caldat <- cbind(caldat, seq=d.loc)
    caldat$pvalue.n <- round(caldat$pvalue/max(caldat$pvalue,na.rm=T),2)
    caldat

    
  }
  
  dat <- transformdata(dates, values,pvalues,date.form)
  

  ## color style -----------------------------------------
#color styles------------------------------------------------

  col.sty <- switch(color,
  "r2b"=c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020"), #red to blue                                                                               
  'r2g'=c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")  , #red to green
  'w2b'=c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6"),   #white to blue
  'b2w'=grey.colors(5) )## black to white
 # assign("col.sty", get(color))
  calendar.pal <- colorRampPalette((col.sty), space = "Lab")
  
  
## scales ------------------------------------------------------------------  
  
  yrs <- (unique(dat$yr))
  nyr <- length(yrs)
  
  scales = list(
    x = list( at= c(seq(2.9, 52, by=4.42)),
              labels = month.abb,
              alternating = c(1, rep(0, (nyr-1))),
              tck=0,
              cex =1),
    y=list(
      at = c(0, 1, 2, 3, 4, 5, 6),
      labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                 "Friday", "Saturday"),
      alternating = 1,
      cex =1,
      tck=0))

# theme ------------------------------------------------------------------  
  calendar.theme <- list(
    xlab=NULL,ylab=NULL,
    strip.background = list(col = "transparent"),
    strip.border = list(col = "transparent"),
    axis.line = list(col="transparent"),
    par.strip.text=list(cex=2))
  
  
  #calendar divsion-----------------------------------------------------------------------  
  calendar.division <- function(...)
  {
    xyetc <- list(...)
    subs <- dat[xyetc$subscripts,]
    dates.fsubs <- dat[dat$yr == unique(subs$yr),]
    y.start <- dates.fsubs$dotw[1]
    y.end   <- dates.fsubs$dotw[nrow(dates.fsubs)]
    dates.len <- nrow(dates.fsubs)
    adj.start <- dates.fsubs$woty[1]
    
    
    for (k in 0:6) {
      if (k < y.start) {
        x.start <- adj.start + 0.5
      } else {
        x.start <- adj.start - 0.5
      }
      if (k > y.end) {
        x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5
      } else {
        x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5
      }
      grid.lines(x = c(x.start, x.finis), y = c(k -0.5, k - 0.5), 
                 default.units = "native", gp=gpar(col = "grey", lwd = 1))
    }
    if (adj.start <  2) {
      grid.lines(x = c( 0.5,  0.5), y = c(6.5, y.start-0.5), 
                 default.units = "native", gp=gpar(col = "grey", lwd = 1))
      grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5), default.units = "native",
                 gp=gpar(col = "grey", lwd = 1))
      grid.lines(x = c(x.finis, x.finis), 
                 y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                 gp=gpar(col = "grey", lwd = 1))
      if (dates.fsubs$dotw[dates.len] != 6) {
        grid.lines(x = c(x.finis + 1, x.finis + 1), 
                   y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                   gp=gpar(col = "grey", lwd = 1))
      }
      grid.lines(x = c(x.finis, x.finis), 
                 y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                 gp=gpar(col = "grey", lwd = 1))
    }
    for (n in 1:51) {
      grid.lines(x = c(n + 1.5, n + 1.5), 
                 y = c(-0.5, 6.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
    }
    x.start <- adj.start - 0.5
    
    if (y.start > 0) {
      grid.lines(x = c(x.start, x.start + 1),
                 y = c(y.start - 0.5, y.start -  0.5), default.units = "native",
                 gp=gpar(col = "black", lwd = 1.75))
      grid.lines(x = c(x.start + 1, x.start + 1),
                 y = c(y.start - 0.5 , -0.5), default.units = "native",
                 gp=gpar(col = "black", lwd = 1.75))
      grid.lines(x = c(x.start, x.start),
                 y = c(y.start - 0.5, 6.5), default.units = "native",
                 gp=gpar(col = "black", lwd = 1.75))
      if (y.end < 6  ) {
        grid.lines(x = c(x.start + 1, x.finis + 1),
                   y = c(-0.5, -0.5), default.units = "native",
                   gp=gpar(col = "black", lwd = 1.75))
        grid.lines(x = c(x.start, x.finis),
                   y = c(6.5, 6.5), default.units = "native",
                   gp=gpar(col = "black", lwd = 1.75))
      } else {
        grid.lines(x = c(x.start + 1, x.finis),
                   y = c(-0.5, -0.5), default.units = "native",
                   gp=gpar(col = "black", lwd = 1.75))
        grid.lines(x = c(x.start, x.finis),
                   y = c(6.5, 6.5), default.units = "native",
                   gp=gpar(col = "black", lwd = 1.75))
      }
    } else {
      grid.lines(x = c(x.start, x.start),
                 y = c( - 0.5, 6.5), default.units = "native",
                 gp=gpar(col = "black", lwd = 1.75))
    }
    
    if (y.start == 0 ) {
      if (y.end < 6  ) {
        grid.lines(x = c(x.start, x.finis + 1),
                   y = c(-0.5, -0.5), default.units = "native",
                   gp=gpar(col = "black", lwd = 1.75))
        grid.lines(x = c(x.start, x.finis),
                   y = c(6.5, 6.5), default.units = "native",
                   gp=gpar(col = "black", lwd = 1.75))
      } else {
        grid.lines(x = c(x.start + 1, x.finis),
                   y = c(-0.5, -0.5), default.units = "native",
                   gp=gpar(col = "black", lwd = 1.75))
        grid.lines(x = c(x.start, x.finis),
                   y = c(6.5, 6.5), default.units = "native",
                   gp=gpar(col = "black", lwd = 1.75))
      }
    }
    for (j in 1:12)  {
      last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
      x.last.m <- dates.fsubs$woty[last.month] + 0.5
      y.last.m <- dates.fsubs$dotw[last.month] + 0.5
      grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, y.last.m),
                 default.units = "native", gp=gpar(col = "black", lwd = 1.75))
      if ((y.last.m) < 6) {
        grid.lines(x = c(x.last.m, x.last.m - 1), y = c(y.last.m, y.last.m),
                   default.units = "native", gp=gpar(col = "black", lwd = 1.75))
        grid.lines(x = c(x.last.m - 1, x.last.m - 1), y = c(y.last.m, 6.5),
                   default.units = "native", gp=gpar(col = "black", lwd = 1.75))
      } else {
        grid.lines(x = c(x.last.m, x.last.m), y = c(- 0.5, 6.5),
                   default.units = "native", gp=gpar(col = "black", lwd = 1.75))
      }
    }
  }
  
# major plot ------------------------------------------------------------------
  
 p <- levelplot(value~woty*dotw | yr, data=dat, border = "black",
            layout = c(1, nyr%%7),
            col.regions = calendar.pal(ncolors),
            aspect='iso',
            between = list(x=0, y=c(1,1)),
            strip=TRUE,
            main =varname, 
            panel = function(x,y,z,...) {
              panel.levelplot(x,y,z,...)
              ll <- list(...)
              at = pretty(dat$pvalue)
              ind.pch <- cut(dat$pvalue, at, include.lowest = TRUE, labels = FALSE)
              pch.symbols <- pch.symbol[ind.pch]
              subscripts <- ll$subscripts
              x <- x[subscripts]
              y <- y[subscripts]
              pch.symbols <-  pch.symbols[subscripts]
              panel.xyplot(x, y, fill =col.symbol ,col.symbol =col.symbol,
                           pch = pch.symbols, cex=cex.symbol,
                           ,  ...)
              calendar.division(...)  
            },
            scales= scales,
            xlim =extendrange(dat$woty,f=0.01),
            xlab="",ylab="",
            ylim=extendrange(dat$dotw,f=0.1),
            cuts= ncolors - 1,
            colorkey= list(col = calendar.pal(ncolors), width = 0.6, height = 0.5),
            subscripts=TRUE,
            par.settings = calendar.theme)
  p
}

##----------- example---------------------------------------------------
if (FALSE) {
  stock <- "MSFT"
  start.date <- "2012-01-12"
  end.date <- Sys.Date()
  quote <- paste("http://ichart.finance.yahoo.com/table.csv?s=",
                 stock,
                 "&a=", substr(start.date,6,7),
                 "&b=", substr(start.date, 9, 10),
                 "&c=", substr(start.date, 1,4), 
                 "&d=", substr(end.date,6,7),
                 "&e=", substr(end.date, 9, 10),
                 "&f=", substr(end.date, 1,4),
                 "&g=d&ignore=.csv", sep="")             
  stock.data <- read.csv(quote, as.is=TRUE)

  ## default symbols 
  p1 <- extra.calendarHeat(dates= stock.data$Date, values = stock.data$Adj.Close,
                           pvalues = stock.data$Volume,
                           varname="W&B MSFT Adjusted Close 
                                    \n Volume as no border symbol ")
                           

  ## multiply symbols
  p2 <- extra.calendarHeat(dates= stock.data$Date, values = stock.data$Adj.Close,
                           pvalues = stock.data$Volume,
                           varname="W&B MSFT Adjusted Close \n black Volume as multiply symbol ",
                           pch.symbol = c(3,4,8,9),
                           col.symbol='black')

  ## circles symbols
  p3 <- extra.calendarHeat(dates= stock.data$Date, values = stock.data$Adj.Close,
                           pvalues = stock.data$Volume,
                           varname="W&B  MSFT Adjusted Close \n blue Volume as circles",
                           pch.symbol = c(1,10,13,16,18),
                           col.symbol='blue')

  ## triangles  symbols
  p4 <- extra.calendarHeat(dates= stock.data$Date, values = stock.data$Adj.Close,
                           pvalues = stock.data$Volume,
                           varname="W&B MSFT Adjusted Close \n red Volume as triangles",
                           pch.symbol = c(2,6,17,24,25),
                           col.symbol='red')

  ## symbols are letters 
  p5 <- extra.calendarHeat(dates= stock.data$Date, values = stock.data$Adj.Close,
                           varname="MSFT Adjusted Close",
                           pch.symbol = LETTERS,
                           col.symbol='black')

  # symbols are LETTERS
  p6 <- extra.calendarHeat(dates= stock.data$Date, values = stock.data$Adj.Close,
                           pvalues = stock.data$Volume,
                           varname="MSFT Adjusted Close  \n Volume as LETTERS symbols",
                           pch.symbol = letters,
                           color='r2b')

  print(p1)
  print(p2)
  print(p3)
  print(p4)
  print(p5)
  print(p6)
}