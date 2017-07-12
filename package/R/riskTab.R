#' @title Number at risk table.
#' @name riskTab
#' @description The function creates tables for each risk with number of observation at risk in given time in groups.
#' @param time vector with times of the first event or follow-up, must be numeric.
#' @param risk vector with type of event, can be numeric or factor/character.
#' @param group vector with group variable, can be numeric or factor/character.
#' @param cens value of 'risk' indicating censored observation (default 0).
#' @param title title of a table
#' @return A grob with n tables, where n is number of risks. Each table contains number of observations at risk in each group in given time points (the time points correspond to breaks at x-axis of plots with survival curves).
#' @export
#' @examples riskTab(time = LUAD$time, risk = LUAD$event, group = LUAD$gender, cens = "alive", title = "Number at risk")
#' @importFrom dplyr filter
#' @importFrom scales extended_breaks
#' @importFrom grid textGrob


riskTab <- function(time, risk, group, cens = 0, title = "Number at risk"){

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

    initialValues <- as.data.frame(sapply(uniGroups, function(x) length(group[group == x])))
    colnames(initialValues) <- timePoints[1]

    dt <- cbind(time, as.character(risk), group)
    dt <- as.data.frame(dt)
    colnames(dt) <- c("time", "risk", "group")
    dt$time <- as.numeric(as.character(dt$time))

    dt <- na.omit(dt)

    #countEvents counts how many events is now less in risk set for given risk
    makeRow <- function(whichRisk, whichGroup){
        nrOfEvents <- filter(dt, dt$group == whichGroup & dt$risk %in% c(whichRisk, cens))
        nrOfEvents <- as.data.frame(nrOfEvents)
        countEvents <- sapply(timePoints[-1], function(x){
            tmp <- which(nrOfEvents$time < x)
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


    args <- lapply(1:length(riskTable), function(x) {arrangeGrob(tableGrob(riskTable[[x]], theme = ttheme_minimal()), top = names(riskTable)[x])})
    args$top <- textGrob(title, gp=gpar(fontface="bold"), vjust = 0.5)
    args$ncol <- length(risks)

    do.call(grid.arrange, args)
}
