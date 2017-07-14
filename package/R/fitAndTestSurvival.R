
riskVec <- function(risk, cens){
    risks <- as.data.frame(unique(risk))
    risks <- filter(risks, risks != cens)
    risks <- risks[,1]
    risks <- levels(factor(risks))
}

#' @title Estimation of survival curves for each risk separately.
#' @name fitSurvival
#' @description The function fits survival curves for each risk treating other events as censoring.
#' @param time vector with times of the first event or follow-up, must be numeric.
#' @param risk vector with type of event, can be numeric or factor/character.
#' @param group vector with group variable, can be numeric or factor/character.
#' @param cens value of 'risk' indicating censored observation (if NULL, the first value of 'risk' vector will be taken).
#' @param type type of survival curve to be fitted. Possible values are "kaplan-meier" (default), "fleming-harrington" or "fh2".
#' @param conf.int level of two-sided confidence interval (default = 0.95).
#' @param conf.type type of confidence interval. Possible  values: "none", "plain", "log" (default), "log-log".
#' @return List, which elements are survfit.summary object for each risk separately.
#' @export
#' @examples fitSurvival(time = LUAD$time, risk = LUAD$event, group = LUAD$gender, cens = "alive",
#' type = "kaplan-meier", conf.int = 0.95, conf.type = "log")
#' @importFrom dplyr filter group_by
#' @importFrom survival Surv survfit

fitSurvival <- function(time,
                        risk,
                        group,
                        cens = NULL,
                        type = "kaplan-meier",
                        conf.int = 0.95,
                        conf.type = "log"
)

{
    if(is.null(cens)) cens <- risk[1]

    #errors
    stopifnot(type %in% c("kaplan-meier", "fleming-harrington", "fh2"))
    stopifnot(conf.type %in% c("none", "plain", "log" , "log-log"))

    #data preparation
    group <- factor(group)

    risks <- riskVec(risk, cens)

    nrOfRisks <- as.numeric(nrow(risks))

    #for each risk we fit a survfit object, trating other events as censoring
    #fit is a list of survfit objects assuming other events as censoring
    fit <- lapply(risks, function(x) {
        localStatus <- {risk == x}
        summary(survfit(Surv(time, localStatus)~group,
                        type = type,
                        conf.int = conf.int,
                        conf.type = conf.type
        ))
    })


    names(fit) <- risks
    fit
}

#' @title Fleming-Harrington test for differences between groups.
#' @name testSurvival
#' @description The function tests, if there are differences between groups for survival curves estimating for all risks separately (treating other events as censoring).
#' @param time vector with times of the first event or follow-up, must be numeric.
#' @param risk vector with type of event, can be numeric or factor/character.
#' @param group vector with group variable, can be numeric or factor/character.
#' @param cens value of 'risk' indicating censored observation (default 0).
#' @param rho rho parameter from Fleming-Harrington Test.
#' @return a data.frame containing p-values of Fleming-Harrington Test for each risk.
#' @export
#' @examples testSurvival(time = LUAD$time, risk = LUAD$event, group = LUAD$gender, cens = "alive", rho = 0)
#' @importFrom dplyr filter
#' @importFrom survival survdiff Surv

testSurvival <- function(time,
                        risk,
                        group,
                        cens = 0,
                        rho = 0)
{

    options(scipen=999)


    #risks - a vector indicating possible risk values
    risks <- riskVec(risk, cens)


    nrOfRisks <- as.numeric(length(risks))

    diff <- lapply(risks, function(x){
        localStatus <- {risk == x}
        localGroup <- factor(group)
        survdiff(Surv(time, localStatus) ~ localGroup, rho = rho)
    })

    p <- sapply(1:nrOfRisks, function(x) {
        pchisq(diff[[x]]$chisq, length(diff[[x]]$n)-1, lower.tail = FALSE)})

    p <- t(as.data.frame(p))
    p <- signif(p, digits = 2)
    colnames(p) <- risks
    rownames(p) <- "Fleming-Harrington test"

    as.data.frame(p)

}
