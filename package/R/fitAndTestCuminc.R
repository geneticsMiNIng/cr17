
#' @title Cumulative Incidences Curves.
#' @name fitCumul
#' @description Fitting cumulative incidence function across different groups and risks.
#' @param time name of a column indicating time of an event or follow-up, must be numeric.
#' @param risk name of a column indicating type of event, can be numeric or factor/character.
#' @param group name of a column indicating group variable, can be numeric or factor/character.
#' @param data data.frame, data.table or matrix containing time, risk and group columns.
#' @param cens value of 'risk' indicating censored observation (default 0).
#' @return list of length [(number of risks)*(number of groups) + 1], containing estimation of cumulative incidences curves for each risk and group. The last element of a group is a data.frame with results of a K-sample test.
#' @export
#' @examples fitCuminc(time = "time", risk = "event", group = "gender", data = LUAD, cens = "alive")
#' @importFrom dplyr filter
#' @importFrom cmprsk cuminc

fitCuminc <- function(time,
                     risk,
                     group,
                     data,
                     cens = 0){

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

    #adding info about group and risk to each element of group
    for(i in 1:nrow(tab)){
        ci[[i]]$group <- tab$gender[i]
        ci[[i]]$risk <- tab$event[i]
    }



    ci$Tests <- as.data.frame(ci$Tests)

    ci
}


#' @title K-sample Test for Competing Risks.
#' @name testCuminc
#' @description Testing differences in cumulative incidences function between groups using K-sample test.
#' @param ci a result of fitCumin function.
#' @return data.frame containing p-values of K-sample test for each risk.
#' @examples fitC <- fitCuminc(time = "time", risk = "event", group = "gender", data = LUAD, cens = "alive")
#' testCuminc(fitC)
#' @export
#' @importFrom dplyr filter
#' @importFrom cmprsk cuminc
#' @importFrom gridExtra tableGrob

testCuminc <- function(ci){

    risks <- levels(ci[[1]]$risk)

    tab <- as.data.frame(ci[[length(ci)]])
    p <- as.data.frame(t(tab$pv))
    colnames(p) <- risks


    tab1 <- vector()
    for(i in 1:length(risks)){
        tmp <- p[,which(colnames(p) == risks[i])]
        tmp <- round(tmp, digits = 4)
        tab1 <- as.data.frame(cbind(tab1,tmp))
    }


    colnames(tab1) <- risks
    rownames(tab1) <- "K-Sample Test"

    as.data.frame(tab1)
}

