#### EXAMPLE ####
# Code to show the model proposed in the article: ####
# Pennoni, F. Genge E. (2019). 
# Analysing the course of public trust via hidden Markov models:
# a focus on the Polish society, 
# Statistical Methods & Applications, 28,  
# Doi https://doi.org/10.1007/s10260-019-00483-9 

require(LMest)

load("dtwc.Rdata")

### Selection of the best number of latent states of the of the basic latent Markov model 
# using sampling weights ww 
out <- lmestSearch(responsesFormula = Y1+Y2+Y3+Y4+Y5+Y6+Y7+Y8+Y9+Y10+Y11 ~ NULL,
                   index = c("id","col"),
                   version = "categorical",
                   k = 1:4,
                   weights = ww,
                   modBasic = 1,
                   data = dtwc,
                   seed = 123)

summary(out)

#### Fix the matrix of the estimated conditional response probabilities ####

Psi4 <- out$out.single[[4]]$Psi


#### Latent Markov model with covariates, fixed Psi and account for survey weights ####
# There is one continous time-varying covariate and two 
# binary time-varying covariates 
# that affect the initial and the transition probabilities by a  multinomial model
# where the intercept is present

out4 <- lmest(responsesFormula = Y1+Y2+Y3+Y4+Y5+Y6+Y7+Y8+Y9+Y10+Y11~NULL,
              latentFormula = ~  X6 + X7 + X1,
              index = c("id","col"),
              data = dtwc,
              parInit = list(Psi = Psi4, fixPsi =TRUE),
              k = 4,
              weights = ww,
              tol=1e-8)

summary(out4)

