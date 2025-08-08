#' Risk assessment based on backward transition probability 
#'
#' @param D max deviation parameter
#' @param V variance of the deviation
#' @param js threshold of forbidden values
#' @param s confidentiality threshold
#' @param prior_pi type of prior on P(X=i)
#' @param Ncell Number of cells computed (required for prior_pi="uniform")
#' @param lambda expectation and variance of poisson distribution (required for prior_pi="poisson")
#' @param freq frequencies of counts (required for prior_pi="custom")
#' @param I original counts to test 
#' @param J perturbed counts to test
#'
#' @returns data.frame with the following columns:
#'  - i : original value(s) taken by X
#'  - j : perturbed value(s) taken by X'
#'  - pi_hat : prior on counts distribution P(X=i)
#'  - pij : transition probabilty: P(X'=j|X=i)
#'  - qij : inverted transition probability: P(X=i|X'=j)
#'  
#' @details
#' For i in N and j in N,
#' qij = P(X=i|X'=j) = P(X=i) * pij / P(X'= j)
#' 
#' The function here computes qIJ, where I and J can be sets of values:
#' qIJ = P(X in I | X' in J)
#'   
#' @export
#'
#' @examples
#' risk_assessment(D=15,V=10,js=0,s=10,prior_pi="poisson",lambda=1,I=1:9,J=1:9)
risk_assessment <- function(
    D,
    V,
    js=0,
    s,
    prior_pi = c("custom","uniform","poisson"),
    Ncell = NULL,
    lambda = NULL,
    freq = NULL,
    freq_name = "tab",
    I,
    J
){
  require(ckm)
  require(dplyr)
  
  if(any(I > s+2*D)){
    message("The function is limited to i <= s+2*D")
    return(NULL)
  }
  if(any(J > s+3*D)){
    message("The function is limited to j <= s+3*D")
    return(NULL)
  }
  if(s < js){
    message("The function is based on js < s")
    return(NULL)
  }
  if(prior_pi == "poisson" & (lambda <= 0 || is.null(lambda))){
    message("lambda has to be > 0")
    return(NULL)
  }
  
  trans <- ckm::create_transition_matrix(D, V, js)
  
  if(is.null(trans)){
    return(NULL)
  }
  
  if(prior_pi == "poisson"){
    frequencies <- data.frame(
      i = 0:(s+2*D)
    ) |>
      mutate(p_hat = dpois(i, lambda))
  }else if(prior_pi == "uniform"){
    frequencies <- data.frame(
      i = 0:(s+2*D),
      p_hat = 1/Ncell
    )
  }else{#custom
    if("i" %in% names(freq) & "p_hat" %in% names(freq)){
      frequencies <- freq |> filter(i %in% 0:(s+2*D)) |> select(i, p_hat)
    }else{
      message("The columns i and p_hat are required in freq")
    }
  }
  
  if(max(J) <= js){
    vi <- paste(I, collapse = ", ")
    vj <- paste(J, collapse = ", ")
    risk_df <- expand.grid(i=vi,j=vj,KEEP.OUT.ATTRS = FALSE) |>
      as.data.frame() |>
      mutate(pi_hat = NA, pij = NA, qij = 0)
  }else{
    risk_df <- ckm::assess_risk(trans, frequencies, I = I, J)
  }
  
  return(
     risk_df |>
      mutate(
        D = D, V = V, js = js,
        tab = freq_name,
        s = s
      ) |>
      relocate(D:s, .before = 1)
  )
}

# I = 1:(params$s-1), 
# J=(params$js+1):(params$s-1)
