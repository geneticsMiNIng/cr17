#' @title Number at risk table.
#' @name riskTab
#' @description The function creates tables for each risk with number of observation at risk in given time in groups.
#' @param time name of a column indicating time of an event or follow-up, must be numeric.
#' @param risk name of a column indicating type of event, can be numeric or factor/character.
#' @param group name of a column indicating group variable, can be numeric or factor/character.
#' @param data data.frame, data.table or matrix containing time, risk and group columns.
#' @param cens value of 'risk' indicating censored observation (default 0).
#' @return A grob with n tables, where n is number of risks. Each table contains number of observations at risk in each group in given time points (the time points correspond to breaks at x-axis of plots with survival curves).
#' @export
#' @examples riskTab(time = "time", risk = "event", group = "gender", data = LUAD, cens = "alive")
#' @importFrom dplyr filter
#' @importFrom scales extended_breaks
#' @importFrom grid textGrob


riskTab <- function(time, risk, group, data, cens = 0){

    data <- as.data.frame(data)
    timeCol <- data[, time]
    groups <- factor(data[, group])
    risks <- riskVec(data, risk, cens)
    nrOfRisks <- as.numeric(nrow(risks))

    #extended_breaks
    fit <- lapply(risks, function(x) {
        localStatus <- {data[,risk] == x}
        summary(survfit(Surv(timeCol, localStatus)~groups
        ))
    })
    names(fit) <- risks
    tmp <- toPlotDf(fit)
    timePoints <- extended_breaks()(tmp$time)

    uniGroups <- unique(groups)
    uniGroups <- levels(factor(uniGroups))

    initialValues <- as.data.frame(sapply(uniGroups, function(x) length(groups[groups == x])))
    colnames(initialValues) <- timePoints[1]

    #countEvents counts how many events is now less in risk set for given risk
    makeRow <- function(whichRisk, whichGroup){
        nrOfEvents <- filter(data, data[, group] == whichGroup & data[, risk]%in% c(whichRisk, cens))
        nrOfEvents <- as.data.frame(nrOfEvents)
        countEvents <- sapply(timePoints[-1], function(x){
            tmp <- which(nrOfEvents[,time] < x)
            length(tmp)
        })
        newRow <- c(initialValues[whichGroup,], initialValues[whichGroup,] - countEvents)

        newRow
    }


    makeTab <- function(whichRisk){
        tab <- sapply(uniGroups, function(x) makeRow(whichRisk, x))
        tab <- as.data.frame(tab)
        tab <- t(tab)
        colnames(tab) <- timePoints
        tab
    }


    riskTable <- lapply(risks, makeTab)
    names(riskTable) <- risks

    args <- lapply(riskTable, function(x) arrangeGrob(tableGrob(x, theme = ttheme_minimal())))
    args$top <- textGrob("Number at Risk", gp=gpar(fontface="bold"), vjust = 1)
    args$ncol <- length(risks)

    do.call(grid.arrange, args)
}
