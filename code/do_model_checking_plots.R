#### name:do_model_checking_plots ####
# with multiplicative model we can use the predict function and 
# plotting the slopes and se are easy
summary(fit2)
dat2$pred <-  predict(fit2, type='response')
dat2
plot(
  dat2$buffer,
  (dat2$cases/dat2$pops)*1000,
  type='p',ylim=ylims, xlim = c(0,7.5), ylab='Incidence Rate per 1000',xlab='Buffer (Kilometres)',
  col = dat2$urban + 1, pch = 16
  )
legend("topright", legend = c("Eastern", "Urban"), pch = 16, col = c("black", "red"))
lines(dat2$buffer,(dat2$pred/dat2$pops)*1000,lwd=2)
se <- predict(fit2, type = "response", se = T)
dat2$se <- se$se.fit
dat2
par(mfrow=c(2,1))
for(class_i in c(0,1)){
#class_i <- 1
with(dat2[dat2$urban == class_i,],
     plot(buffer, (cases/pops) * 1000, ylim = ylims)
     )
with(dat2[dat2$urban == class_i,],
     lines(buffer, (pred/pops) * 1000)
     )
with(dat2[dat2$urban == class_i,],
     lines(buffer, ((pred + 1.96 * se)/pops) * 1000, lty = 2)
     )
with(dat2[dat2$urban == class_i,],
     lines(buffer, ((pred - 1.96 * se)/pops) * 1000, lty = 2)
     )
}
