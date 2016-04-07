#  > LossVol
function (realized, evaluated, which = "SE1") 
{
  if (all(which != c("SE1", "SE2", "QLIKE", "R2LOG", "AE1", 
                     "AE2"))) 
    stop("Not valid choice of \"which\". Valid choices are \"SE1\",\"SE2\",\"QLIKE\",\"R2LOG\",\"AE1\",\"AE2\" ")
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
  if (which == "SE1") 
    loss = (evaluated - realized)^2
  if (which == "SE2") 
    loss = (evaluated^2 - realized^2)^2
  if (which == "QLIKE") 
    loss = log(evaluated^2) + realized^2 * evaluated^-2
  if (which == "R2LOG") 
    loss = (log(realized^2 * evaluated^-2))^2
  if (which == "AE1") 
    loss = abs(evaluated - realized)
  if (which == "AE2") 
    loss = abs(evaluated^2 - realized^2)
  return(loss)
}
<environment: namespace:MCS>