#' @title Cox model for each type of event separately.
#' @name simpleCox
#' @description The function fits Cox models for each type of event
#' treating other events as censoring.
#' @param time name of a column indicating time of an event or follow-up, must be numeric.
#' @param risk name of a column indicating type of event, can be numeric or factor/character.
#' @param group name of a column indicating grouping variable, can be numeric or factor/character.
#' @param data data.frame, data.table or matrix containing time, risk and group columns.
#' @param cens value of 'risk' indicating censored observation (default 0).
#' @return a list of length n, where n is number of risks. Each element of a list is a result of summary.coxph function from package survival, where there is only one type of event possible (other are treating as censored).
#' @export
#' @examples fitCox(time = "time", risk = "event", group = "gender", data = LUAD, cens = "alive")
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
#' @title Testing differences between groups in Cox models.
#' @name testCox
#' @description The function provides 3 tests for comparing models estimated from fitCox function. The tests are: Likelihood Ratio Test, Wald Test and Logrank Test.
#' @param fitCox a result of function fitCox.
#' @return a data.frame with p-values of 3 tests for each risk.
#' @export
#' @examples fitC <- fitCox(time = "time", risk = "event", group = "gender", data = LUAD, cens = "alive")
#' testCox(fitC)
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
