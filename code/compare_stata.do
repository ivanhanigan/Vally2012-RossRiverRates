// do check R against stata

insheet using "~/projects/Vally2012-RossRiverRates/data/rrv_bunbury_urban_rural_combined.csv"   
gen log1pop = log(1+pops)

poisson cases i.urban##c.buffer, offset(log1pop) 

meglm cases buffer , offset(log1pop) || urban:, family(poisson) link(log)

/*
Mixed-effects GLM                               Number of obs      =        30
Family:                 Poisson
Link:                       log
Group variable:           urban                 Number of groups   =         2

                                                Obs per group: min =        15
                                                               avg =      15.0
                                                               max =        15

Integration method: mvaghermite                 Integration points =         7

                                                Wald chi2(1)       =      4.52
Log likelihood = -59.630055                     Prob > chi2        =    0.0335
------------------------------------------------------------------------------
       cases |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
      buffer |  -.0662473   .0311622    -2.13   0.034    -.1273242   -.0051705
       _cons |  -5.047999   .1239276   -40.73   0.000    -5.290893   -4.805105
     log1pop |          1  (offset)
-------------+----------------------------------------------------------------
urban        |
   var(_cons)|   1.35e-32   2.47e-17                             .           .
------------------------------------------------------------------------------
LR test vs. Poisson regression:      chi2(0) =     0.00   Prob > chi2 =      .

Note: LR test is conservative and provided only for reference.




same as in R
# Adding fixed-effects predictors
# Predict cases from distance buffer
Norm2 <-glmer(cases ~ buffer + (1|urban),
             data=dat2,
             family = 'poisson',
             offset = log(1+pops)
             )

summary(Norm2)

Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
 Family: poisson  ( log )
Formula: cases ~ buffer + (1 | urban)
   Data: dat2
 Offset: log(1 + pops)

     AIC      BIC   logLik deviance df.resid 
   125.3    129.5    -59.6    119.3       27 

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-1.5901 -0.8978 -0.4232  0.4132  2.2455 

Random effects:
 Groups Name        Variance Std.Dev.
 urban  (Intercept) 0        0       
Number of obs: 30, groups:  urban, 2

Fixed effects:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -5.04800    0.12393  -40.73   <2e-16 ***
buffer      -0.06625    0.03116   -2.13   0.0335 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation of Fixed Effects:
       (Intr)
buffer -0.827
*/
