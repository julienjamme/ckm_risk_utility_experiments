#' Utility assessment
#'
#' @param D max deviation parameter
#' @param V variance of the deviation
#' @param js threshold of forbidden values
#' @param freq frequencies of counts
#' @param freq_name name to be given at freq in the df to return
#' @param precision 
#'
#' @returns data.frame
#' 
#' @details
#' U1 : utility measured as the probability for a big count to be deviated 
#' by less than absolute given precision. A big count is a count on which 
#' a perfectly symmetrical distributed noise is put. 
#' 
#' U2 : Expectation of the absolute deviation applied to a big count.
#' 
#' U3 : utility measured as the probability for any count,except the zeroes, 
#' to be deviated by less than absolute given precision, weighted by the frequencies of its 
#' appearance in the studied frequency table.
#' 
#' U4 : Expectation of the absolute deviation of any non-zero count. Frequencies are 
#' used as weights: E(|Z|) = E(|Z| | X) et P(|Z|=z | X=i) = P(|Z|=z, X=i)/P(X=i).
#' 
#' So, U1 and U2 are theoretical values of the utility for any big count in any table,
#' while U3 and U4 are empirical values of the utility of a whole
#' (all non-zero counts even the small ones) real table.
#' 
#' 
#' Note that if U1 and U3 are direct utility measures (the bigger the value,
#' the more useful the parameter set), U2 and U3 are actually direct information
#' loss measures (the bigger the value, the less useful the parameter set). 
#' U1 and U3 can be turned as information loss measures by replacing them by 
#' 1-U1 and 1-U3, respectively.
#' 
#' @export
#'
#' @examples
#' utility_assessment(
#'   D=10, V=20, js=5, 
#'   freq = read.csv("data/freq_census_tab2.csv")
#' )
utility_assessment <- function(
    D,
    V,
    js,
    freq,
    freq_name = "tab",
    precision = 5
){
  require(ckm)
  require(dplyr)
  
  trans <- ckm::create_transition_matrix(D=D, V=V, js=js)
  
  if(is.null(trans)){
    U1 <- U2 <- U3 <- U4 <- NA
  }else{
    
    pert_table <- trans@pTable
    
    max_i <- max(pert_table$i)
    
    U1 <- pert_table |> 
      filter( i == max_i) |>
      summarise(u = sum(p[abs(v) < precision])) |>
      pull(u)
    
    U2 <- pert_table |> 
      filter( i == max_i) |>
      summarise(u = sum(abs(v) * p)) |>
      pull(u)
    
    U3 <- pert_table |> 
      select(i,v,p) |>
      full_join(
        freq |> mutate(i = ifelse(i>max_i,max_i,i)) |>
          group_by(i) |>
          summarise(p_hat = sum(p_hat), .groups="drop"),
        by = "i"
      ) |>
      filter(abs(v) < precision) |>
      group_by(i, p_hat) |>
      summarise(proba = sum(p), .groups="drop") |>
      summarise(u = sum(p_hat*proba)) |>
      pull(u)
    
    U4 <- pert_table |> 
      select(i,v,p) |>
      full_join(
        freq |> mutate(i = ifelse(i>max_i,max_i,i)) |>
          group_by(i) |>
          summarise(p_hat = sum(p_hat), .groups="drop"),
        by = "i"
      ) |> 
      filter(i > 0) |>
      group_by(i, p_hat) |>
      summarise(expect = sum(abs(v) * p), .groups="drop") |>
      summarise(u = sum(p_hat*expect)) |>
      pull(u)
    
    U4 <- U4/(sum(freq$p_hat[freq$i > 0]))
  }
  
  return(
    data.frame(
      D = D, V = V, js = js, 
      tab = freq_name,
      precision = precision,
      U1 = U1,
      U2 = U2,
      U3 = U3,
      U4 = U4
    )
  )
}

