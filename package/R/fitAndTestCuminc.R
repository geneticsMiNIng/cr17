
#' @title Cumulative Incidences Curves
#' @name fitCuminc
#' @description Fits cumulative incidence function across different groups and risks.
#' @param time vector with times of the first event or follow-up, must be numeric.
#' @param risk vector with type of event, can be numeric or factor/character.
#' @param group vector with group variable, can be numeric or factor/character.
#' @param cens value of 'risk' indicating censored observation (if NULL, the first value of 'risk' vector will be taken).
#' @return list of length [(number of risks)*(number of groups) + 1], containing estimation of cumulative incidences curves for each risk and group. The last element of a list is a data.frame with results of a K-sample test, containing test statistic, p-value and degrees od freedom for each risk.
#' @export
#' @seealso \code{\link[cmprsk]{cuminc}}
#' @examples fitCuminc(time = LUAD$time, risk = LUAD$event, group = LUAD$gender, cens = "alive")
#' @importFrom dplyr filter
#' @importFrom cmprsk cuminc
#' @importFrom survival Surv survfit

fitCuminc <- function(time,
                     risk,
                     group,
                     cens = NULL){

    if(is.null(cens)) cens <- as.character(risk[1])

    risks <- riskVec(risk, cens)
    uniRisks <- 1:length(risks)
    mapRisks <- cbind(risks, uniRisks)
    mapRisks <- rbind(mapRisks, c(cens, 0))
    mapRisks <- as.data.frame(mapRisks)
    colnames(mapRisks)[1] <- "risk"

    groups <- as.data.frame(unique(group))
    groups <- filter(groups, !is.na(groups))
    uniGroups <- letters[1:nrow(groups)]
    mapGroups <- cbind(groups, uniGroups)
    colnames(mapGroups)[1] <- "group"

    dt <- cbind(time, as.character(risk), group)
    dt <- as.data.frame(dt)
    colnames(dt) <- c("time", "risk", "group")
    dt$time <- as.numeric(as.character(dt$time))


    dt <- merge(dt, mapRisks, by = "risk")
    dt <- merge(dt, mapGroups, by = "group")

    ci <- cuminc(ftime = dt[, "time"],
                 fstatus = dt[, "uniRisks"],
                 group = dt[, "uniGroups"],
                 cencode = 0)

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

    tab$newname <- paste( tab[, "risk"], tab[,"group"])

    names(ci)[1:length(ci)-1] <- tab$newname

    tab$risk <- factor(as.character(tab$risk))
    tab$group <- factor(as.character(tab$group))

    #extended_breaks
    fit <- lapply(risks, function(x) {
        localStatus <- {risk == x}
        summary(survfit(Surv(time, localStatus)~group
        ))
    })
    names(fit) <- risks
    tmp <- toPlotDf(fit)
    timePoints <- extended_breaks()(tmp$time)


    #adding info about group and risk to each element of group
    for(i in 1:nrow(tab)){
        ci[[i]]$group <- tab[i, "group"]
        ci[[i]]$risk <- tab[i, "risk"]
        ci[[i]]$timePoints <- timePoints
    }

    ci$Tests <- as.data.frame(ci$Tests)

    ci
}


#' @title K-sample Test for Competing Risks
#' @name testCuminc
#' @description tests differences in cumulative incidences function between groups using K-sample test.
#' @param ci a result of fitCumin function.
#' @return data.frame containing p-values of K-sample test for each risk.
#' @examples fitC <- fitCuminc(time = LUAD$time, risk = LUAD$event, group = LUAD$gender, cens = "alive")
#' testCuminc(fitC)
#' @export
#' @seealso \code{\link[cr17]{fitCuminc}}

testCuminc <- function(ci){

    risks <- levels(ci[[1]]$risk)

    tab <- as.data.frame(ci[["Tests"]])
    p <- as.data.frame(t(tab$pv))
    colnames(p) <- risks


    tab1 <- vector()
    for(i in 1:length(risks)){
        tmp <- p[,which(colnames(p) == risks[i])]
        tmp <- signif(tmp, digits = 2)
        tab1 <- as.data.frame(cbind(tab1,tmp))
    }


    colnames(tab1) <- risks
    rownames(tab1) <- "K-Sample test"

    as.data.frame(tab1)
}

