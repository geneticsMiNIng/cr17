#' @title simple cox model
#' @name simpleCox
#' @description The function fits cos models for each risk
#' treating other events as censoring
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
#' @importFrom gridExtra tableGrob
#' @importFrom survival Surv coxph
fitCox <- function(time,
                      risk,
                      group,
                      data,
                      cens = 0)
    {

    options(scipen=999)

    #data preparation
    data <- as.data.frame(data)
    timeVec <- data[,time]

    #risks - a vector indicating possible risk values
    risks <- riskVec(data, risk, cens)
    nr_of_risks <- as.numeric(length(risks))

    fit <- lapply(risks, function(x){
        localrisk <- as.numeric(data[,risk] == x)
        localGroup <- factor(data[,group])
        summary(coxph(Surv(timeVec, localrisk)~localGroup))
        }
    )
    names(fit) <- risks
    fit

}
#' @title test cox model
#' @name testCox
#' @description The function fits cos models for each risk
#' treating other events as censoring
#' @param fitCox time must be numeric
#' @export
#' @importFrom dplyr filter
#' @importFrom gridExtra tableGrob
#' @importFrom survival Surv coxph

testCox <- function(fitCox){

    nr_of_risks <- length(fitCox)

    tab <- sapply(1:nr_of_risks, function(x){
       c(fitCox[[x]]$logtest[3], fitCox[[x]]$waldtest[3], fitCox[[x]]$sctest[3])
    })

    tab <- round(tab, digits = 4)


    colnames(tab) <- names(fitCox)
    rownames(tab) <- c("LRT", "Wald Test", "Logrank Test")

    as.data.frame(tab)


}
