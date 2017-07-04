
riskVec <- function(data, risk, cens){
    risks <- as.data.frame(unique(data[, risk]))
    risks <- filter(risks, risks != cens)
    risks <- risks[,1]
    risks <- levels(factor(risks))
}

#' @title Estimation of survival curves for each risk separetly.
#' @name fitSurvival
#' @description The function fits survival curves for each risk treating other events as censoring.
#' @param time name of a column indicating time of an event or follow-up, must be numeric.
#' @param risk name of a column indicating type of event, can be numeric or factor/character.
#' @param group nam of a column indicating group variable, can be numeric or factor/character.
#' @param data data.frame, data.table or matrix containg time, risk and group columns.
#' @param cens value of 'risk' indicating censored observation (default 0).
#' @param type type of survival curve to be fitted. Possible values are "kaplan-meier" (default), "fleming-harrington" or "fh2".
#' @param conf.int level of two sided confidence interval.
#' @param conf.type type of confidence interval. Possilble values: "none", "plain", "log" (default), "log-log".
#' @return List, which elements are survfit.summary object for each risk separetly.
#' @export
#' @examples fitSurvival(time = "time", risk = "event", group = "gender", data = LUAD, cens = "alive", type = "kaplan-meier", conf.int = 0.95, conf.type = "log")
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

#' @title Fleming-Harrington test for differences between groups.
#' @name lrtSurvival
#' @description The function tests, if there are differences between groups for survival curves estimating for all risks separately (treating other events as censoring).
#' @param time name of a column indicating time of an event or follow-up, must be numeric.
#' @param risk name of a column indicating type of event, can be numeric or factor/character.
#' @param group nam of a column indicating group variable, can be numeric or factor/character.
#' @param data data.frame, data.table or matrix containg time, risk and group columns.
#' @param cens value of 'risk' indicating censored observation.
#' @param rho rho parameter from Fleming-Harrington Test.
#' @return a data.frame containing p-values of Fleming-Harrington Test for each risk.
#' @export
#' @examples lrtSurvival(time = "time", risk = "event", group = "gender", data = LUAD, cens = "alive", rho = 0)
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
