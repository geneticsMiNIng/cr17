#
boundsCuminc <- function(whichRisk, whichGroup, target, toPlot){
    risk <- NULL
    group <- NULL
    whichRisk <- as.character(whichRisk)
    whichGroup <- as.character(whichGroup)
    tmp <- as.data.frame(filter(toPlot, risk == whichRisk & group == whichGroup))
    whichTime <- which(tmp$time <= target)
    nr <- length(whichTime)
    lower <- tmp$lowerBound[nr]
    upper  <- tmp$upperBound[nr]
    est <- tmp$est[nr]
    c(lower, est, upper)

}

barsDataCuminc <- function(risks, groups, target, toPlot){
    barsData <- expand.grid(risks, groups)
    colnames(barsData) <- c("risk", "group")
    barsData <- as.data.frame(barsData)


    low <- numeric(nrow(barsData))
    up <- numeric(nrow(barsData))
    est <- numeric(nrow(barsData))
    for(i in 1:nrow(barsData)){
        tmpBounds <- as.numeric(boundsCuminc(barsData[i,1],barsData[i,2], target, toPlot))
        low[i] <- tmpBounds[1]
        est[i] <- tmpBounds[2]
        up[i] <- tmpBounds[3]
    }

    barsData <- cbind(barsData, low, est, up)
    barsData

}



#' @title Cumulative incidences curves
#' @name plotCuminc
#' @description Plots cumulative incidences curves for each risk and group.
#' @param ci a result of function fitCuminc.
#' @param cens value of 'risk' indicating censored observation (default 0).
#' @param target point in time, in which the confidence bounds should be plotted (default NULL, no confidence bounds plotted).
#' @param ggtheme ggtheme to be used (default: theme_minimal()).
#' @param titleCuminc a title of a plot (default: "Cumulative incidence functions").
#' @param xtitle a title of x axis (default: "Time").
#' @param ytitleCuminc a title of y axis (default: "Cumulative incidences")
#' @param legendtitle a title of a legend (default: "Group").
#' @return a ggplot containing n graphs, where n is number of risks. Each graph represents cumulative incidence curves for given risk.  One curve corresponds to one group.
#' @seealso \code{\link[ggplot2]{ggplot}} \code{\link[ggplot2]{ggtheme}}
#' @examples fitC <- fitCuminc(time = LUAD$time, risk = LUAD$event, group = LUAD$gender, cens = "alive")
#' plotCuminc(ci = fitC, cens = "alive", target = 1200)
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot position_dodge geom_step geom_errorbar facet_grid ggtitle theme scale_y_continuous scale_x_continuous scale_color_discrete theme_minimal
#' @importFrom stats model.matrix na.omit pchisq

plotCuminc <-function(ci,
                      cens = NULL,
                      target = NULL,
                      ggtheme = theme_minimal(),
                      titleCuminc = "Cumulative incidence functions",
                      xtitle = "Time",
                      ytitleCuminc = "Cumulative incidences",
                      legendtitle = "Group"){

    low <- NULL
    up <- NULL
    est <- NULL
    time <-NULL
    group <- NULL
    risk <- NULL

    if(is.null(cens)) cens <- as.character(risk[1])

    #make long format
    nrTests <- which(names(ci) == "Tests")
    ci <- ci[-nrTests]

    timePoints <- ci[[1]]$timePoints

    aggNames <- names(ci)

    toPlot <- data.frame()

    for(i in 1:length(aggNames)){
        tmp <- as.data.frame(cbind(ci[[i]]$time, ci[[i]]$est, ci[[i]]$var, rep(aggNames[i], times = length(ci[[i]]$time))))
        toPlot <- as.data.frame(rbind(toPlot, tmp))
    }

    colnames(toPlot) <- c("time", "est", "var", "aggname")

    toPlot[,1:3] <- sapply(1:3, function(x) as.numeric(as.character(toPlot[,x])))

    risks <- levels(ci[[1]]$risk)
    groups <- levels(ci[[1]]$group)



    riskGroup <- expand.grid(risks, groups)
    riskGroup$aggname <- sapply(1:nrow(riskGroup), function(x){
        paste(riskGroup[x,1], riskGroup[x,2])
    })

    toPlot <- merge(toPlot, riskGroup, by = "aggname")
    toPlot <- toPlot[, !names(toPlot) %in% "aggname"]
    colnames(toPlot)[4:5] <- c("risk", "group")


    #adding conf intervals
    toPlot$lowerBound <- sapply(1:nrow(toPlot), function(x){
        est <- toPlot[x, "est"]
        var <- toPlot[x, "var"]
        exp(log(est) - 1.96*sqrt(var)/est)
    })

    toPlot$upperBound <- sapply(1:nrow(toPlot), function(x){
        est <- toPlot[x, "est"]
        var <- toPlot[x, "var"]
        exp(log(est) + 1.96*sqrt(var)/est)
    })


    if(!is.null(target) & is.numeric(target)){
    barsData <- barsDataCuminc(risks, groups, target, toPlot)}

    pd <- position_dodge(0.9)



    #making a plot
    plot1 <- ggplot(data = toPlot, aes(time, est, color = group)) +
        geom_step(size=1) +
        facet_grid(~risk)

    #adding errorbars
    if( !is.null(target) & is.numeric(target)){
    plot1 <- plot1 +
        geom_errorbar(data = barsData, aes(x = target, ymin = low, ymax = up),
                      size = 1,
                      alpha = 0.7,
                      width = 0.7,
                      position = pd)}

    plot1 <- plot1 + ggtheme

    #making it beauty
    plot1 <- plot1 +
        ggtitle(titleCuminc) +
        theme(plot.title = element_text(size=13, face="bold", hjust = 0.5), legend.position = "top") +
        scale_y_continuous(ytitleCuminc, limits = c(0,1)) +
        scale_x_continuous(xtitle, breaks = timePoints, limits = range(timePoints))+
        theme(legend.title = element_text(size=10, face="bold"))+
        scale_color_discrete(name=legendtitle, labels = groups)

    plot1
}

