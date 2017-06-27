
#ci <- fitCuminc("time", "event", "gender", LUAD, "alive")
#ci <- fitCuminc("time", "risk", "group", data)


#' @title fitting cuminc object
#' @name fitCumul
#' @description list with cumulative incidences and a test of differences between them
#' @param time time must be numeric
#' @param risk can be numeric or factor/character
#' @param group can be numeric or factor/character
#' @param data can be data frame or matrix
#' @param conf.int level of two sided conf int
#' @param conf.type "none", "plain", "log" (default), "log-log
#' @export
#' @importFrom dplyr filter
#' @importFrom cmprsk cuminc

fitCuminc <- function(time,
                     risk,
                     group,
                     data,
                     cens = 0,
                     rho = 0){

    data <- as.data.frame(data)

    risks <- riskVec(data, risk, cens)
    uniRisks <- 1:length(risks)
    mapRisks <- cbind(risks, uniRisks)
    colnames(mapRisks)[1] <- risk

    groups <- as.data.frame(unique(data[, group]))
    groups <- filter(groups, !is.na(groups))
    uniGroups <- letters[1:nrow(groups)]
    mapGroups <- cbind(groups, uniGroups)
    colnames(mapGroups)[1] <- group

    data <- merge(data, mapRisks, by = risk)
    data <- merge(data, mapGroups, by = group)

    ci <- cuminc(ftime = data[, time],
                 fstatus = data[, "uniRisks"],
                 group = data[, "uniGroups"],
                 rho = rho,
                 cencode = cens)

    aggnames <- names(ci)
    aggnames <- aggnames[-length(aggnames)]

    tab <- c()
    for(i in aggnames){
        tmp <- unlist(strsplit(i, " "))
        tab <- rbind(tab, c(i, tmp))
    }
    tab <- as.data.frame(tab)

    colnames(tab) <- c("aggnames", "uniGroups", "uniRisks")

    tab <- merge(tab, mapRisks, by = "uniRisks")
    tab <- merge(tab, mapGroups, by = "uniGroups")

    tab$newname <- paste(tab[, risk], tab[,group])

    names(ci)[1:length(ci)-1] <- tab$newname

    ci
}


#' @title Log rank test for competing risks
#' @name testCuminc
#' @description Log rank test for competing risks
#' @param time time must be numeric
#' @param risk can be numeric or factor/character
#' @param group can be numeric or factor/character
#' @param data can be data frame or matrix
#' @param conf.int level of two sided conf int
#' @param conf.type "none", "plain", "log" (default), "log-log
#' @export
#' @importFrom dplyr filter
#' @importFrom cmprsk cuminc
#' @importFrom gridExtra tableGrob

testCuminc <- function(ci){

    aggNames <- names(ci)
    aggNames <- aggNames[-length(ci)]

    riskGroup <- sapply(aggNames, function(x){
        unlist(strsplit(x, split = " "))
    })

    riskGroup <- as.data.frame(riskGroup)
    riskGroup <- t(riskGroup)
    colnames(riskGroup) <- c("risk", "group")

    risks <- unique(riskGroup[,"risk"])
    risks <- levels(factor(risks))

    groups <- unique(riskGroup[, "group"])
    groups <- levels(factor(groups))

    tab <- as.data.frame(ci[[length(ci)]])
    p <- as.data.frame(t(tab$pv))
    colnames(p) <- risks


    tab1 <- c()
    for(i in 1:length(risks)){
        tmp <- p[,which(colnames(p) == risks[i])]
        tmp <- round(tmp, digits = 4)
        tab1 <- as.data.frame(cbind(tab1,tmp))
    }


    colnames(tab1) <- risks
    rownames(tab1) <- "CompRisks LRT"

    as.data.frame(tab1)
}

