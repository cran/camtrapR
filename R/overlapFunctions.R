# functions taken and adapted from Mike Meredith's package "overlap"


# does a nice plot of two density curves with overlap shaded

overlapPlot <-
  function(A, B, xscale=24, xcenter=c("noon", "midnight"),
           linetype=c(1, 2), linecol=c('black', 'blue'),
           linewidth=c(1,1), olapcol='lightgrey', rug=FALSE, extend=NULL,
           n.grid=128, kmax = 3, adjust = 1, ...)  {
    # xlab="Time", ylab="Density", ylim, now passed via "..."

    isMidnt <- match.arg(xcenter) == "midnight"

    bwA <- getBandWidth(A, kmax=kmax) / adjust
    bwB <- getBandWidth(B, kmax=kmax) / adjust
    if(is.na(bwA) || is.na(bwB))
      stop("Bandwidth estimation failed.")
    xsc <- if(is.na(xscale)) 1 else xscale / (2*pi)
    # xxRad <- seq(0, 2*pi, length=n.grid)
    if (is.null(extend)) {
      xxRad <- seq(0, 2*pi, length=n.grid)
    } else {
      xxRad <- seq(-pi/4, 9*pi/4, length=n.grid)
    }
    if(isMidnt)
      xxRad <- xxRad - pi
    xx <- xxRad * xsc
    densA <- densityFit(A, xxRad, bwA) / xsc
    densB <- densityFit(B, xxRad, bwB) / xsc
    densOL <- pmin(densA, densB)

    # Deal with ... argument:
    dots <- list(...)
    if(length(dots) == 1 && class(dots[[1]]) == "list")
      dots <- dots[[1]]
    defaultArgs <- list(
      main=paste(deparse(substitute(A)), "and", deparse(substitute(B))),
      xlab="Time", ylab="Density",
      bty="o", type="l", xlim=range(xx), ylim = c(0, max(densA, densB)))
    useArgs <- modifyList(defaultArgs, dots)

    selPlot <- names(useArgs) %in%
      c(names(as.list(args(plot.default))), names(par(no.readonly=TRUE)))
    plotArgs <- useArgs[selPlot]
    plotArgs$x <- 0
    plotArgs$y <- 0
    plotArgs$type <- "n"
    plotArgs$xaxt <- "n"
    do.call(plot, plotArgs, quote=TRUE)

    plotTimeAxis2(xscale)
    polygon(c(max(xx), min(xx), xx), c(0, 0, densOL), border=NA, col=olapcol)
    if(!is.null(extend)) {
      if(isMidnt) {
        wrap <- c(-pi, pi) * xsc
      } else {
        wrap <- c(0, 2*pi) * xsc
      }
      edge <- par('usr')
      rect(c(edge[1], wrap[2]), rep(edge[3], 2), c(wrap[1], edge[2]), rep(edge[4],2),
           border=NA, col=extend)
      box(bty=useArgs$bty)
    }

    # if(rug)
    segments(xx[1], 0, xx[n.grid], 0, lwd=0.5)
    lines(xx, densA, lty=linetype[1], col=linecol[1], lwd=linewidth[1])
    lines(xx, densB, lty=linetype[2], col=linecol[2], lwd=linewidth[2])
    if(rug) {
      if(isMidnt) {
        A <- ifelse(A < pi, A, A - 2*pi)
        B <- ifelse(B < pi, B, B - 2*pi)
      }
      axis(1, at=A*xsc, labels=FALSE, tcl= 0.35, lwd=0, lwd.ticks=0.5, col=linecol[1])
      axis(1, at=B*xsc, labels=FALSE, tcl=-0.35, lwd=0, lwd.ticks=0.5, pos=0, col=linecol[2])
    }
    return(invisible(data.frame(x = xx, densityA = densA, densityB = densB)))
  }




# Plots a kernel density for circular data

# A: a sample of times of observations in radians
# adjust: smoothing parameter (adjust = 1/c in old code)

densityPlot <-
  function(A, xscale=24, xcenter=c("noon", "midnight"),
           add=FALSE, rug=FALSE, extend="lightgrey",
           n.grid=128, kmax = 3, adjust = 1, ...)  {
    # ylim, xlab="Time", ylab="Density" now included in defaultArgs

    isMidnt <- match.arg(xcenter) == "midnight"
    bw <- getBandWidth(A, kmax=kmax) / adjust
    if(is.na(bw))
      stop("Bandwidth estimation failed.")
    # xx <- seq(0, 2*pi, length=n.grid)
    if (is.null(extend)) {
      xx <- seq(0, 2*pi, length=n.grid)
    } else {
      xx <- seq(-pi/4, 9*pi/4, length=n.grid)
    }
    if(isMidnt)
      xx <- xx - pi
    densA <- densityFit(A, xx, bw)
    xsc <- if(is.na(xscale)) 1 else xscale / (2*pi)
    toPlot <- cbind(x = xx * xsc, y = densA / xsc)

    # Deal with ... argument:
    dots <- list(...)
    if(length(dots) == 1 && class(dots[[1]]) == "list")
      dots <- dots[[1]]
    defaultArgs <- list(main=deparse(substitute(A)), bty="o", type="l",
                        xlab="Time", ylab="Density", ylim = c(0, max(toPlot[,'y'])))
    useArgs <- modifyList(defaultArgs, dots)

    if(!add)  {
      selPlot <- names(useArgs) %in%
        c(names(as.list(args(plot.default))), names(par(no.readonly=TRUE)))
      plotArgs <- useArgs[selPlot]
      plotArgs$x <- toPlot
      plotArgs$y <- NULL
      plotArgs$type <- "n"
      plotArgs$xaxt <- "n"
      do.call(plot, plotArgs, quote=TRUE)

      plotTimeAxis2(xscale)
      abline(h=0, col='grey')
      if(!is.null(extend)) {
        if(isMidnt) {
          wrap <- c(-pi, pi) * xsc
        } else {
          wrap <- c(0, 2*pi) * xsc
        }
        edge <- par('usr')
        rect(c(edge[1], wrap[2]), rep(edge[3], 2), c(wrap[1], edge[2]), rep(edge[4],2),
             border=NA, col=extend)
        box(bty=useArgs$bty)
      }
    }
    selPlot <- names(useArgs) %in% names(par(no.readonly=TRUE))
    plotArgs <- useArgs[selPlot]
    plotArgs$x <- toPlot
    plotArgs$y <- NULL
    do.call(lines, plotArgs, quote=TRUE)

    if(rug)  {
      if(isMidnt)
        A <- ifelse(A < pi, A, A - 2*pi)
      rug(A * xsc, ...)  # do.call(rug, doesn't work !!
    }
    return(invisible(as.data.frame(toPlot)))
  }




plotTimeAxis <- function(xscale) {
  if(is.na(xscale)) {
    axis(1, at=c(-pi, -pi/2, 0, pi/2, pi, 3*pi/2, 2*pi),
         labels=c(expression(-pi), expression(-pi/2), "0",
                  expression(pi/2), expression(pi),
                  expression(3*pi/2), expression(2*pi)))
  } else if(xscale == 24) {
    axis(1, at=c(-12, -6, 0,6,12,18,24),
         labels=c("12:00", "18:00", "0:00", "6:00", "12:00", "18:00", "24:00"))
  } else if(xscale == 1) {
    axis(1, at=c(-0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),
         labels=TRUE)
  } else {
    axis(1)
  }
}

plotTimeAxis2 <- function(xscale) {
  if(is.na(xscale)) {
    axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
         labels=c("0",
                  expression(pi/2), expression(pi),
                  expression(3*pi/2), expression(2*pi)))
  } else if(xscale == 24) {
    axis(1, at=c(0,6,12,18,24),
         labels=c("0:00", "6:00", "12:00", "18:00", "24:00"))
  } else if(xscale == 1) {
    axis(1, at=c(0, 0.25, 0.5, 0.75, 1),
         labels=TRUE)
  } else {
    axis(1)
  }
}