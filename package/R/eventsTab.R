
#' @title Number of events tables.
#' @name eventsTab
#' @description The function creates tables for each risk with number of events up to given time in groups.
#' @param time name of a column indicating time of an event or follow-up, must be numeric.
#' @param risk name of a column indicating type of event, can be numeric or factor/character.
#' @param group name of a column indicating group variable, can be numeric or factor/character.
#' @param data data.frame, data.table or matrix containing time, risk and group columns.
#' @param cens value of 'risk' indicating censored observation (default 0).
#' @return A grob with n tables, where n is number of risks. Each table contains number of events that have happened in each group up to given time point (the time points correspond to breaks at x-axis of plots with cumulative incidence curves).
#' @export
#' @examples eventTab(time = "time", risk = "event", group = "gender", data = LUAD, cens = "alive")
#' @importFrom dplyr filter
#' @importFrom scales extended_breaks
#' @importFrom grid textGrob


eventTab <- function(time, risk, group, data, cens = 0){

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

    makeRow <- function(whichRisk, whichGroup){
        tmp <- filter(data, data[, risk] == whichRisk & data[, group] == whichGroup)
        newRow <- sapply(1:length(timePoints), function(x){
            length(which(tmp$time <= timePoints[x]))
        })
        newRow
    }

    makeTable <- function(whichRisk){
        tab <- sapply(uniGroups, function(x) makeRow(whichRisk, x))
        tab <- as.data.frame(tab)
        tab <- t(tab)
        colnames(tab) <- timePoints
        tab
    }



    eventTable <- lapply(risks, function(x) makeTable(x))

    names(eventTable) <- risks

    args <- lapply(eventTable, function(x) arrangeGrob(tableGrob(x, theme = ttheme_minimal())))
    args$top <- textGrob("Number of events", gp=gpar(fontface="bold"), vjust = 1)
    args$ncol <- length(risks)

    do.call(grid.arrange, args)
}



