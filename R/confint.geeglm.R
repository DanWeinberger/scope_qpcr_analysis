confint.geeglm <- function(object, parm, level = 0.95, ...) {
  cc <- coef(summary(object))
  mult <- qnorm((1+level)/2)
  citab <- with(as.data.frame(cc),
                cbind(RR=exp(Estimate),
                      lwr=exp(Estimate-mult*Std.err),
                      upr=exp(Estimate+mult*Std.err)))
  rownames(citab) <- rownames(cc)
  citab[parm,]
}
