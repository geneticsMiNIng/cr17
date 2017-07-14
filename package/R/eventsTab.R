
#' @title Number of events tables.
#' @name eventsTab
#' @description The function creates tables for each risk with number of events up to given time in groups.
#' @param time vector with times of the first event or follow-up, must be numeric.
#' @param risk vector with type of event, can be numeric or factor/character.
#' @param group vector with group variable, can be numeric or factor/character.
#' @param cens value of 'risk' indicating censored observation (if NULL, the first value of 'risk' vector will be taken).
#' @param title title of a table.
#' @return A grob with n tables, where n is number of risks. Each table contains number of events that have happened in each group up to given time point (the time points correspond to breaks at x-axis of plots with cumulative incidence curves).
#' @export
#' @examples eventTab(time = LUAD$time, risk = LUAD$event, group = LUAD$gender, cens = "alive",
#' title = "Number of events")
#' @importFrom dplyr filter
#' @importFrom scales extended_breaks
#' @importFrom grid textGrob gpar
#' @importFrom gridExtra tableGrob grid.arrange arrangeGrob ttheme_minimal
#' @importFrom survival Surv survfit

eventTab <- function(time, risk, group, cens = NULL, title = "Number of Events"){

    if(is.null(cens)) cens <- risk[1]

    risks <- riskVec(risk, cens)
    nrOfRisks <- as.numeric(nrow(risks))

    #extended_breaks
    fit <- lapply(risks, function(x) {
        localStatus <- {risk == x}
        summary(survfit(Surv(time, localStatus)~group
        ))
    })
    names(fit) <- risks
    tmp <- toPlotDf(fit)
    timePoints <- extended_breaks()(tmp$time)


    uniGroups <- unique(group)
    uniGroups <- levels(factor(uniGroups))

    dt <- cbind(time, as.character(risk), group)
    dt <- as.data.frame(dt)
    colnames(dt) <- c("time", "risk", "group")
    dt$time <- as.numeric(as.character(dt$time))

    makeRow <- function(whichRisk, whichGroup){
        tmp <- filter(dt, dt$risk == whichRisk & dt$group == whichGroup)
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

    args <- lapply(1:length(eventTable), function(x) arrangeGrob(tableGrob(eventTable[[x]], theme = ttheme_minimal()), top = names(eventTable)[x]))
    args$top <- textGrob(title, gp=gpar(fontface="bold"), vjust = 0.5)
    args$ncol <- length(risks)

    do.call(grid.arrange, args)
}



