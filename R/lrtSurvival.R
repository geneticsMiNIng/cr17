#' @title Log rank test for differences between groups
#' @name lrtSurvival
#' @description The function tests, if there are differences between groups for two risks independently
#' @param time time must be numeric
#' @param risk can be numeric or factor/character
#' @param group can be numeric or factor/character
#' @param data can be data frame or matrix
#' @param type "kaplan-meier", "fleming-harrington" or "fh2"
#' @param conf.int level of two sided conf int
#' @param conf.type "none", "plain", "log" (default), "log-log
#' @param error "greenwood (default)', "tsiatis", "aalen
#' @export
#' @importFrom dplyr filter
#' @importFrom survival survdiff Surv
#' @importFrom gridExtra tableGrob

lrtSurvival <- function(time,
                        risk,
                        group,
                        data,
                        cens = 0,
                        rho = 0)
{

    #data preparation
    data <- as.data.frame(data)
    timeVec <- data[, time]

    #risks - a vector indicating possible risk values
    risks <- as.data.frame(unique(data[, risk]))
    risks <- filter(risks, risks != cens)
    risks <- risks[,1]
    nrOfRisks <- as.numeric(length(risks))

    diff <- lapply(risks, function(x){
        localStatus <- {data[,risk] == x}
        localGroup <- factor(data[, group])
        survdiff(Surv(timeVec, localStatus) ~ localGroup, rho = rho)
    })

    p <- sapply(1:nrOfRisks, function(x) {
         pchisq(diff[[x]]$chisq, length(diff[[x]]$n)-1, lower.tail = FALSE)})

    p <- t(as.data.frame(p))
    colnames(p) <- risks
    rownames(p) <- "Fleming-Harrington test"

    tableGrob(p)

}


# lrtSurvival("time", "risk", "group", data) -> x
