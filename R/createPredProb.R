createPredProb = function(variable, data){
  return(pnorm(q = as.numeric(Matrix::sparse.model.matrix(variable$formulaRHS, data = data) %*% variable$beta)))
}
