#' @title Number at risk tablew
#' @name riskTab
#' @description The function creates tables for each risk with number of observation at risk in given time in groups.
#' @param fit a result of fitSurvival function.
#' @return A grob with n tables, where n is number of risks. Each table contains number of observations ar risk in each group in given time points (the time points correspond to breaks at x-axis of plots with survival curves).
#' @export
#' @examples fitS <- fitSurvival(time = "time", risk = "event", group = "gender", data = LUAD, cens = "alive", type = "kaplan-meier", conf.int = 0.95, conf.type = "log")
#' riskTab(fit = fitS)
#' @importFrom dplyr filter
#' @importFrom scales extended_breaks
#' @importFrom grid textGrob


riskTab <- function(fit){

    toPlot <- toPlotDf(fit)

    #table - number at risk
    risks <- names(fit)
    risks <- levels(factor(risks))

    #dealing with factor names of strata
    badGroupNames <- levels(fit[[1]]$strata)
    strataMapping <- 1:length(badGroupNames)
    #ISSUE nazwy grup nie moga mieć w środku '='
    groups <- sapply(as.character(badGroupNames), function(x) strsplit(x, split = "=")[[1]][2])
    strataMapping <- cbind(strataMapping, groups)
    colnames(strataMapping) <- c("strata", "group")


    timePoints <- sapply(risks, function(x){
        tmp <- filter(toPlot, risk == x)
        extended_breaks()(tmp$time)
    })
    names(timePoints) <- risks

    forTables <- c()
    for(i in risks){
        tmp <- cbind(fit[[i]]$time,
                     fit[[i]]$n.risk,
                     fit[[i]]$strata,
                     rep(i, times = length(fit[[i]]$time)))

        tmp <- as.data.frame(tmp)
        forTables <- as.data.frame(rbind(forTables, tmp))

    }
    colnames(forTables) <- c("time", "n.risk", "strata", "risk")
    forTables <- merge(forTables, strataMapping, by = "strata")
    forTables[,1:3] <- sapply(forTables[,1:3], function(x) as.numeric(as.character(x)))

    makeRow <- function(ri, gr){
        tmp <- filter(forTables, risk == ri, group == gr)
        newRow <- c()
        tp <- timePoints[[ri]]
        for(i in tp){
            tmp2 <- filter(tmp, tmp$time >= i)
            nr <- which.min(tmp2$time)
            newValue <- tmp2$n.risk[nr]
            newRow <- c(newRow, newValue)
        }

        if(length(newRow) != length(tp)){
            lack <- length(tp) - length(newRow)
            newRow <- c(newRow, rep(0, times = lack))
        }

        newRow
    }

    makeTable <- function(risk){
        tab <- sapply(groups, function(x) makeRow(risk, x))
        tab <- t(tab)
        tab <- as.data.frame(tab)
        colnames(tab) <- timePoints[[risk]]
        rownames(tab) <- groups
        tab
    }

    riskTable <- sapply(risks, makeTable)
    names(riskTable) <- risks


    lay <- rbind(c(1,1),
                 c(2,3))
    grid.arrange(arrangeGrob(tableGrob(riskTable[[1]], theme = ttheme_minimal())),
                 arrangeGrob(tableGrob(riskTable[[2]], theme = ttheme_minimal())),
                 top = textGrob("Number at Risk", gp=gpar(fontface="bold"), vjust = 1),ncol= 2)

}
