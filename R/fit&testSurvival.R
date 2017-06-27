#fit <- fitSurvival("time", "risk", "group", data)
#fit <- fitSurvival("CI_time", "CI_status", "DM", dt)
#fit <- fitSurvival("time", "event", "gender", LUAD, "alive")

riskVec <- function(data, risk, cens){
    risks <- as.data.frame(unique(data[, risk]))
    risks <- filter(risks, risks != cens)
    risks <- risks[,1]
    risks <- levels(factor(risks))
}

#' @title simple survfit for each risk separetly
#' @name fitSurvival
#' @description The function fits Kaplan-Mayer models for each risk
#' treating other events as censoring
#' @param time time must be numeric
#' @param risk can be numeric or factor/character
#' @param group can be numeric or factor/character
#' @param data can
#' be data frame or matrix
#' @param type "kaplan-meier", "fleming-harrington" or "fh2"
#' @param conf.int level of two sided conf int
#' @param conf.type "none", "plain", "log" (default), "log-log
#' @param error "greenwood (default)', "tsiatis", "aalen
#' @export
#' @importFrom dplyr filter group_by
#' @importFrom survival Surv survfit

fitSurvival <- function(time,
                        risk,
                        group,
                        data,
                        cens = 0,
                        type = "kaplan-meier",
                        conf.int = 0.95,
                        conf.type = "log"
)

{

    #errors
    stopifnot(type %in% c("kaplan-meier", "fleming-harrington", "fh2"))
    stopifnot(conf.type %in% c("none", "plain", "log" , "log-log"))

    #data preparation
    data <- as.data.frame(data)
    timeCol <- data[, time]
    groups <- factor(data[, group])


    risks <- riskVec(data, risk, cens)

    nrOfRisks <- as.numeric(nrow(risks))

    #for each risk we fit a survfit object, trating other events as censoring
    #fit is a list of survfit objects assuming other events as censoring
    fit <- lapply(risks, function(x) {
        localStatus <- {data[,risk] == x}
        summary(survfit(Surv(timeCol, localStatus)~groups,
                        type = type,
                        conf.int = conf.int,
                        conf.type = conf.type
        ))
    })

    names(fit) <- risks
    fit
}

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

    options(scipen=999)

    #data preparation
    data <- as.data.frame(data)
    timeCol <- data[, time]

    #risks - a vector indicating possible risk values
    risks <- riskVec(data, risk, cens)


    nrOfRisks <- as.numeric(length(risks))

    diff <- lapply(risks, function(x){
        localStatus <- {data[,risk] == x}
        localGroup <- factor(data[, group])
        survdiff(Surv(timeCol, localStatus) ~ localGroup, rho = rho)
    })

    p <- sapply(1:nrOfRisks, function(x) {
        pchisq(diff[[x]]$chisq, length(diff[[x]]$n)-1, lower.tail = FALSE)})

    p <- t(as.data.frame(p))
    p <- round(p, digits = 4)

    colnames(p) <- risks
    rownames(p) <- "F-H Test"

    as.data.frame(p)

}


# lrtSurvival("time", "risk", "group", data) -> x

