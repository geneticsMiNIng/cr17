
boundsCuminc <- function(ri, gr, target, toPlot){
    ri <- as.character(ri)
    gr <- as.character(gr)
    tmp <- as.data.frame(filter(toPlot, fac == ri & col == gr))
    whichTime <- which(tmp$time <= target)
    nr <- length(whichTime)
    lower <- tmp$lowerBound[nr]
    upper  <- tmp$upperBound[nr]
    est <- tmp$est[nr]
    c(lower, est, upper)

}

barsDataCuminc <- function(risks, groups, target, toPlot){
    barsData <- expand.grid(risks, groups)
    colnames(barsData) <- c("fac", "col")
    barsData <- as.data.frame(barsData)


    low <- c()
    up <- c()
    est <- c()
    for(i in 1:nrow(barsData)){
        tmpBounds <- as.numeric(boundsCuminc(barsData[i,1],barsData[i,2], target, toPlot))
        low <- c(low, tmpBounds[1])
        est <- c(est, tmpBounds[2])
        up <- c(up, tmpBounds[3])
    }

    barsData <- cbind(barsData, low, est, up)
    barsData

}



#' @title Cumulative incidences curves
#' @name plotCuminc
#' @description The function plots cumulative incidences curves for each risk and group.
#' @param ci a result of function fitCuminc.
#' @param risk name of a column indicating type of event, can be numeric or factor/character.
#' @param group name of a column indicating group variable, can be numeric or factor/character.
#' @param target point in time, in which the confidence bounds should be plotted (default NULL, no confidence bounds plotted).
#' @return a ggplot containing n graphs, where n is number of risks. Each graph represents cumulative incidence curves for given risk in each group.
#' @export
#' @examples fitC <- fitCuminc(time = "time", risk = "event", group = "gender", data = LUAD, cens = "alive")
#' plotCuminc(ci = fitC, risk = "event", group = "gender", target = 1200)
#' @importFrom dplyr filter
#' @importFrom cmprsk cuminc


plotCuminc <-function(ci, risk, group, target = NULL){

    #make long format
    ci <- ci[-length(ci)]
    aggNames <- names(ci)

    toPlot <- c()

    for(i in aggNames){
        tmp <- as.data.frame(ci[[i]])
        tmp$name <- i
        toPlot <- as.data.frame(rbind(toPlot, tmp))
    }

    riskGroup <- sapply(toPlot$name, function(x){
        unlist(strsplit(x, split = " "))
    })



    riskGroup <- as.data.frame(riskGroup)
    riskGroup <- t(riskGroup)
    colnames(riskGroup) <- c(risk, group)
    rownames(riskGroup) <- NULL

    risks <- unique(riskGroup[,risk])
    risks <- levels(factor(risks))

    groups <- unique(riskGroup[,group])
    groups <- levels(factor(groups))

    toPlot <- cbind(toPlot, riskGroup)
    toPlot <- toPlot[, !names(toPlot) %in% "name"]


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

    colnames(toPlot)[which(colnames(toPlot) == risk)] <- "fac"
    colnames(toPlot)[which(colnames(toPlot) == group)] <- "col"



    if(!is.null(target) & is.numeric(target)){
    barsData <- barsDataCuminc(risks, groups, target, toPlot)}

    pd <- position_dodge(0.9)

    #making a plot
    plot1 <- ggplot(data = toPlot, aes(time, est, color = col)) +
        geom_step(size=1) +
        facet_grid(~fac, scales = "free")

    #adding errorbars
    if( !is.null(target) & is.numeric(target)){
    plot1 <- plot1 +
        geom_errorbar(data = barsData, aes(x = target, ymin = low, ymax = up),
                      size = 1,
                      alpha = 0.7,
                      width = 0.7,
                      position = pd)}

    #making it beauty
    plot1 <- plot1 +
        theme_minimal() +
        ggtitle("Cumulative incidence function") +
        theme(plot.title = element_text(size=13, face="bold", hjust = 0.5), legend.position = "top") +
        scale_y_continuous("Cumulative incidences", limits = c(0,1)) +
        scale_x_continuous("Time")+
        theme(legend.title = element_text(size=10, face="bold"))+
        scale_color_discrete(name="Group", labels = groups)

    plot1
}

