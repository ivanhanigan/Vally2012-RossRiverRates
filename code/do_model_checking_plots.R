

# plotting the slopes are easy
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
