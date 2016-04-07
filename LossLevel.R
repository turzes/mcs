#   > LossLevel
function (realized, evaluated, which = "SE") 
{
  if (all(which != c("SE", "AE"))) 
    stop("Not valid choice of \"which\". Valid choices are \"SE\",\"AE\" ")
  if (is.matrix(evaluated)) {
    m = ncol(evaluated)
  }
  else {
    m = 1
  }
  if (!is.matrix(evaluated)) {
    evaluated = as.matrix(evaluated)
  }
  if (!is.matrix(realized)) {
    realized = as.matrix(realized)
  }
  if (ncol(realized) > 1) 
    stop("realized must of one column")
  if (nrow(realized) != nrow(evaluated)) 
    stop("the number of realized and evaluated observation must be the same")
  if (which == "SE") 
    loss = (evaluated - realized)^2
  if (which == "AE") 
    loss = abs(evaluated - realized)
  return(loss)
}
<environment: namespace:MCS>