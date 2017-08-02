#' @title Cox model for each type of event separately
#' @name simpleCox
#' @description fits Cox models for each risk and group, treating other type of events as censoring.
#' @param time vector with times of the first event or follow-up, must be numeric.
#' @param risk vector with type of event, can be numeric or factor/character.
#' @param group vector with group variable, can be numeric or factor/character.
#' @param cens value of 'risk' indicating censored observation (if NULL, the first value of 'risk' vector will be taken).
#' @param conf.int conf.int level of two-sided confidence interval (default = 0.95).
#' @return a list of length n, where n is number of risks. Each element of a list is a result of summary.coxph function from package survival, where there is only one type of event possible (other are treating as censored).
#' @export
#' @seealso \code{\link[survival]{coxph}}  \code{\link[survival]{summary.coxph}}
#' @examples fitCox(time = LUAD$time, risk = LUAD$event, group = LUAD$gender, cens = "alive", conf.int = 0.95)
#' @importFrom dplyr filter
#' @importFrom survival Surv coxph


fitCox <- function(time,
                   risk,
                   group,
                   cens = NULL,
                   conf.int = 0.95)
    {

    options(scipen=999)

    if(is.null(cens)) cens <- as.character(risk[1])

    #risks - a vector indicating possible risk values
    risks <- riskVec(risk, cens)
    nr_of_risks <- as.numeric(length(risks))

    fit <- lapply(risks, function(x){
        localrisk <- as.numeric(risk == x)
        localGroup <- factor(group)
        summary(coxph(Surv(time, localrisk)~localGroup), conf.int= conf.int)
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
#' @seealso \code{\link[cr17]{fitCox}}
#' @examples fitC <- fitCox(time = LUAD$time, risk = LUAD$event, group = LUAD$gender, cens = "alive")
#' testCox(fitC)

testCox <- function(fitCox){

    nr_of_risks <- length(fitCox)

    tab <- sapply(1:nr_of_risks, function(x){
       c(fitCox[[x]]$logtest[3], fitCox[[x]]$waldtest[3], fitCox[[x]]$sctest[3])
    })

    tab <- signif(tab, digits = 2)


    colnames(tab) <- names(fitCox)
    rownames(tab) <- c("Likelihood ratio test", "Wald test", "Logrank test")

    as.data.frame(tab)


}
