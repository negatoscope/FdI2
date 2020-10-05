
## WANT: FIND mu and sigma such that when
## X is defined by P(X<=x) = .5 Phi ((x+mu)/sigma)) + .5 Phi ((x-mu)/sigma))
## we have Var[X] = 1
## NOTE THAT WE will satisfy E[X] 0 since the means of the 2 normals are -mu and mu.
## We just need to find sigma so that Var[X]=1. 

## Find the PDF:
## f(x) = d/dx F(x) =  .5 psi ((x+mu)/sigma)) (1/sigma) + 
##                    .5 psi ((x-mu)/sigma))(1/sigma),
## where psi is the PDF of the standard normal.

pdf = function(x, mu, sigma) {
  .5* dnorm ((x+mu)/sigma) *(1/sigma) + 
    .5 *dnorm ((x-mu)/sigma)*(1/sigma)
}


E[X] = \int_-infty ^ infty xf(x) dx
E[X^2] = \int_-infty ^ infty x^2f(x) dx = 
  \int   x^2 .5 dnorm((x+mu)/sigma)(1/sigma) + 
  \int   x^2 .5 dnorm((x-mu)/sigma)(1/sigma)  = 
  
  .5*E[Y^2] +  .5*E[Z^2] , where Y~normal ( -mu,sigma) and Z~normal(mu, sigma)
= .5(2)(sigma^2 - mu^2) = sigma^2 - mu^2

Var[X] = E[X^2]-(E[X])^2
Var[Y] = sigma^2 = E[Y^2] - mu^2 

E[Y]^2 = sigma^2 - mu^2

numsim = isolate(input$n)*isolate(input$nsim)
numsim = 100000
mu = .92
sigma = sqrt(1-mu^2)
"bimodal" = rnorm(numsim, mu*2*(rbinom(n=numsim, 
                                       size=1, prob=.5)-.5), sd=sigma) ##, ncol=isolate(input$n)))
hist(bimodal)
sd(bimodal)
mean(bimodal)


## Compute Q1, Q3:  YES THEY ARE -mu and mu!!!
x = -mu
.5*pnorm(x, -mu, sigma) + .5*pnorm(x, mu, sigma)
