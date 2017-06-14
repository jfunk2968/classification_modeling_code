


piece.formula <- function(var.name, knots) {
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  paste(var.name, "+",
        paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
}

K <- 3

knots <- seq(min(check$age), max(check$age), len = K + 2)[-c(1, K + 2)]

paste("y ~", piece.formula("AGE_mean", knots))
      
I(pmax(df$AGE_mean - 50, 0))