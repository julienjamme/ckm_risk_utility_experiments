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
#' U2 : utility measured as the probability for any count to be deviated 
#' by less than absolute given precision, weighted by the frequencies of its 
#' appearance in the studied frequency table.
#' 
#' So, U1 is a theoretical value of the utility for any big count in any table,
#' while U2 is an empirical value of the utility of a whole (all counts even the 
#' small ones) real table.
#' 
#' @export
#'
#' @examples
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
    U1 <- U2 <- NA
  }else{
    
    pert_table <- trans@pTable
    
    max_i <- max(pert_table$i)
    
    U1 <- pert_table |> 
      filter( i == max_i) |>
      summarise(u = sum(p[abs(v) < precision])) |>
      pull(u)
    
    U2 <- pert_table |> 
      select(i,v,p) |>
      full_join(
        freq_cens_tab |> mutate(i = ifelse(i>max_i,max_i,i)) |>
          group_by(i) |>
          summarise(N = sum(N), .groups="drop") |>
          mutate(p_hat = N/sum(N)),
        by = "i"
      ) |>
      filter(abs(v) < precision) |>
      group_by(i, p_hat) |>
      summarise(proba = sum(p), .groups="drop") |>
      summarise(u = sum(p_hat*proba)) |>
      pull(u)
  }
  
  return(
    data.frame(
      D = D, V = V, js = js, 
      tab = freq_name,
      precision = precision,
      U1 = U1,
      U2 = U2
    )
  )
}

