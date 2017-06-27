
#' @title number of events up to time t
#' @name eventsTab
#' @description tables with number of events up to time t, for given risk and group
#' @param fit fitSurvival object
#' @export
#' @importFrom dplyr filter
#' @importFrom scales extended_breaks
#' @importFrom grid textGrob


eventTab <- function(fit,
                     ci,
                     risk,
                     group){


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

    toPlot <- cbind(toPlot, riskGroup)
    toPlot <- toPlot[, !names(toPlot) %in% "name"]

    risks <- as.data.frame(unique(toPlot[,risk]))
    risks <- levels(factor(risks[,1]))

    groups <- as.data.frame(unique(toPlot[,group]))
    groups <- levels(factor(groups[,1]))



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

    #dealing with factor names of strata
    badGroupNames <- levels(fit[[1]]$strata)
    strataMapping <- 1:length(badGroupNames)
    #ISSUE nazwy grup nie moga mieć w środku '='
    groups <- sapply(as.character(badGroupNames), function(x) strsplit(x, split = "=")[[1]][2])
    strataMapping <- cbind(strataMapping, groups)
    colnames(strataMapping) <- c("strata", "group")


    timePoints <- lapply(risks, function(x){
        tmp <- filter(toPlot, fac == x)
        extended_breaks()(tmp$time)
    })

    names(timePoints) <- risks



    forTables <- c()
    for(i in as.character(risks)){
        tmp <- cbind(fit[[i]]$time,
                     fit[[i]]$n.event,
                     fit[[i]]$strata,
                     rep(i, times = length(fit[[i]]$time)))

        tmp <- as.data.frame(tmp)
        forTables <- as.data.frame(rbind(forTables, tmp))

    }
    colnames(forTables) <- c("time", "n.event", "strata", "risk")
    forTables <- merge(forTables, strataMapping, by = "strata")
    forTables[,1:3] <- sapply(forTables[,1:3], function(x) as.numeric(as.character(x)))

    makeRow <- function(ri, gr){
        tmp <- filter(forTables, risk == ri, group == gr)
        newRow <- c()
        tp <- timePoints[[ri]]
        for(i in tp){
            tmp2 <- filter(tmp, tmp$time <= i)
            newValue <- sum(tmp2$n.event)
            newRow <- c(newRow, newValue)
        }

        newRow
    }

    makeTable <- function(ri){
        tab <- sapply(groups, function(x) makeRow(ri, x))
        tab <- t(tab)
        tab <- as.data.frame(tab)
        colnames(tab) <- timePoints[[ri]]
        rownames(tab) <- groups
        tab
    }


    eventTable <- lapply(risks, function(x) makeTable(x))


    names(eventTable) <- risks

    grid.arrange(arrangeGrob(tableGrob(eventTable[[1]], theme = ttheme_minimal())),
                 arrangeGrob(tableGrob(eventTable[[2]], theme = ttheme_minimal())),
                 top = textGrob("Number of events", gp=gpar(fontface="bold"), vjust = 1),ncol= 2)

}

