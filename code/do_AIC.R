
#### name:AIC ####
# now one model without a buffer effect
fit_no_buff <- glm(cases ~ urban + offset(log(1+pops)), family = 'poisson', data = dat2)
summary(fit_no_buff)
estat <- data.frame(
  model = c("no buffer", "buffer x urban interaction"),
  aic = c(AIC(fit_no_buff),AIC(fit3))
  )
estat$delta_aic <- estat$aic - min(estat$aic)
estat
# as we thought, it is a much better model
