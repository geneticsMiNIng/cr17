
#data frame for plotting
toPlotDf <- function(fit){

    risks <- names(fit)
    risks <- levels(factor(risks))


    #dealing with factor names of strata
    badGroupNames <- levels(fit[[1]]$strata)
    strataMapping <- 1:length(badGroupNames)

    #ISSUE nazwy grup nie moga mieć w środku '='
    groups <- sapply(as.character(badGroupNames), function(x) gsub("groups=", replacement = "", x))
    strataMapping <- cbind(strataMapping, groups)

    colnames(strataMapping) <- c("strata", "group")

    toPlot <- data.frame()

    for(i in risks){
        tmp <- cbind(fit[[i]]$time,
                     fit[[i]]$surv,
                     fit[[i]]$strata,
                     fit[[i]]$lower,
                     fit[[i]]$upper,
                     rep(i, times = length(fit[[i]]$time)))
        tmp <- as.data.frame(tmp)
        toPlot <- as.data.frame(rbind(toPlot, tmp))
    }


    colnames(toPlot) <- c("time", "prob", "strata", "lowerBound", "upperBound", "risk")
    toPlot <- merge(toPlot, strataMapping, by = "strata")

    toPlot$time <- as.numeric(as.character(toPlot$time))
    toPlot$prob <- as.numeric(as.character(toPlot$prob))
    toPlot$lowerBound <- as.numeric(as.character(toPlot$lowerBound))
    toPlot$upperBound <- as.numeric(as.character(toPlot$upperBound))
    toPlot <- toPlot[, !names(toPlot) %in% "strata"]


    #adding starting points
    zeros <- expand.grid(risks, groups)
    colnames(zeros) <- c("risk", "group")
    zeros$time <- 0
    zeros$prob <- 1
    zeros$lowerBound <- 1
    zeros$upperBound <- 1

    zeros <- zeros[, colnames(toPlot)]


    toPlot <- rbind(toPlot, zeros)
    toPlot

}



#########################

#confidence intervals for simple analysis

boundsSimpleSurv <- function(whichRisk, whichGroup, target, toPlot){
    whichRisk <- as.character(whichRisk)
    whichGroup <- as.character(whichGroup)
    tmp <- as.data.frame(filter(toPlot, toPlot$risk == whichRisk & toPlot$group == whichGroup))
    tmp <- tmp[order(tmp$time),]
    whichTime <- which(tmp$time < target)
    nr <- length(whichTime)
    lower <- tmp$lowerBound[nr]
    upper  <- tmp$upperBound[nr]
    prob <- tmp$prob[nr]
    c(lower, prob, upper)
}



#######################



#barsData for survival curves plotting

barsDataSimpleSurv <- function(toPlot, target, risks, groups){
    barsData <- expand.grid(risks, groups)
    low <- numeric(nrow(barsData))
    up <-  numeric(nrow(barsData))
    prob <-  numeric(nrow(barsData))
    for(i in 1:nrow(barsData)){
        tmpBounds <- as.numeric(boundsSimpleSurv(barsData[i,1],barsData[i,2],target, toPlot))
        low[i] <- tmpBounds[1]
        prob[i] <- tmpBounds[2]
        up[i] <- tmpBounds[3]
    }

    barsData <- cbind(barsData, low, prob, up)
    colnames(barsData)[1:2] <- c("risk", "group")
    barsData
}



#######################

#' @title Survival curves
#' @name plotSurvival
#' @description The function plots survival curves for each risk and group.
#' @param fit a result of fitSurvival function.
#' @param target point in time, in which the confidence bounds should be plotted.
#' @param ggtheme ggtheme to be used (default: theme_minimal()).
#' @param titleSurv a title of a plot (default: "Survival curves").
#' @param xtitle a title of x axis (default: "Time").
#' @param ytitleSurv a title of y axis (default: "Probability of survivng up to time t")
#' @param legendtitle a title of a legend (default: "Group").
#' @return a ggplot containing n graphs, where n is number of risks. Each graph represents survival curves for given risk. One curve corresponds to one group.
#' @export
#' @examples fitS <- fitSurvival(time = "time", risk = "event", group = "gender", data = LUAD, cens = "alive", type = "kaplan-meier", conf.int = 0.95, conf.type = "log")
#' plotSurvival(fit = fitS, target = 1200)
#' @importFrom ggplot2 ggplot
#' @importFrom dplyr filter
#' @importFrom scales extended_breaks

plotSurvival <- function(fit,
                         target = NULL,
                         ggtheme = theme_minimal(),
                         titleSurv = "Survival curves",
                         xtitle = "Time",
                         ytitleSurv = "Probability of survivng up to time t",
                         legendtitle = "Group"

                         ){

    toPlot <- toPlotDf(fit)

    timePoints <- extended_breaks()(toPlot$time)

    #defining risks

    risks <- unique(toPlot$risk)
    risks <- levels(factor(risks))


    #defining groups
    groups <- unique(toPlot$group)
    groups <- factor(groups)


    if(!is.null(target) & is.numeric(target)){
    barsData <- barsDataSimpleSurv(toPlot, target, risks, groups)}
    pd <- position_dodge(0.9)

    #making a plot

    plot1 <- ggplot(data = toPlot, aes(time, prob, color = group)) +
        geom_step(size=1) +
        facet_grid(~risk)

    if(!is.null(target) & is.numeric(target)){
    plot1 <- plot1 +
        geom_errorbar(data = barsData,
                      mapping = aes(x = target, ymin = low, ymax = up),
                      size = 1,
                      alpha = 0.7,
                      width = 0.7,
                      position = pd)}

    #theme
    plot1 <- plot1 + ggtheme

    #making it beauty
    plot1 <- plot1 +
        ggtitle(titleSurv) +
        theme(plot.title = element_text(size=13, face="bold", hjust = 0.5), legend.position = "top") +
        scale_y_continuous(ytitleSurv, limits = c(0,1)) +
        scale_x_continuous(xtitle, breaks = timePoints)+
        theme(legend.title = element_text(size=10, face="bold"))+
        scale_color_discrete(name=legendtitle, labels = groups)

    plot1

}
