# ------------------
# App Title: Sampling distribution demonstration
#    Author: Gail Potter
# ------------------

Q1=function(x) quantile(x, .25)
Q3=function(x) quantile(x, .75)
CV=function(x) sd(x)/mean(x)

## Compute population parameters.  Populations are standardized so that they all have mean =0, 
##  standard deviation = 1

parameters = data.frame(
  row.names= c("media", "standard deviation", "Q1", "median", "Q3",  "minimum", "maximum"),
  bimodal=rep(NA,7), normal=rep(NA,7), left.skewed=rep(NA,7), right.skewed=rep(NA,7),uniform=rep(NA,7))

parameters[1,]=0
parameters[2,]=1

## normal quantiles
parameters$normal[3:5] = qnorm(c(.25, .5, .75))

## left-skewed quantiles
parameters$left.skewed[3:5] = c(
  (10-qgamma(1-.25, shape=2, scale=5)) / (5*sqrt(2)), ## Q1
  (10-qgamma(1-.5, shape=2, scale=5)) / (5*sqrt(2)) , ## Q2
  (10-qgamma(1-.75, shape=2, scale=5)) / (5*sqrt(2))) ## Q3


## right-skewed quantiles:
parameters$right.skewed[3:5] = c(
  (qgamma(.25, shape=2, scale=5)-10 ) / (5*sqrt(2)),
  (qgamma(.5, shape=2, scale=5)-10 )  / (5*sqrt(2)),
  (qgamma(.75, shape=2, scale=5)-10 ) / (5*sqrt(2)))

## uniform quantiles
parameters$uniform[3:5]=c(qunif(.25, -sqrt(3), sqrt(3)), 0, qunif(.75, -sqrt(3), sqrt(3)))

parameters$bimodal[3:5]= c(-.92, 0, .92)

parameters[6,] = -Inf 
parameters[7,] =  Inf
parameters[7, "left.skewed"] = 10/(5*sqrt(2))
parameters[6, "right.skewed"] = -10/(5*sqrt(2))
parameters[6:7, "uniform"] = c(-sqrt(3), sqrt(3))


shinyServer(function(input, output, session) {
  
  draw.sample <- reactiveValues()
  
  observe({
    if (input$n > 0  & input$n <= 1000 & is.numeric(input$n) & 
        (input$n %% 1==0) & !is.na(input$n)) 
      return()
    showshinyalert(session, "shinyalert1", 
                   paste("Introduce un número entero entre 1 y 1000:"))
  })
  
  observe({
    if (input$nsim > 0  & input$nsim <= 100000 & is.numeric(input$nsim) & 
        (input$nsim %% 1==0) & !is.na(input$nsim)) 
      return()
    showshinyalert(session, "shinyalert2", 
                   paste("Introduce un número entero entre 1 y 100,000:"))
  })
  
  observe({
    if (is.numeric(input$popmean) & !is.na(input$popmean)) 
      return()
    showshinyalert(session, "shinyalert3", 
                   paste("Introduce el valor de la media de la población (o muestral):"))
  })
  
  
  observe({
    if (is.numeric(input$popsd) & !is.na(input$popsd)) 
      return()
    showshinyalert(session, "shinyalert4", 
                   paste("Introduce el valor de la desviación estándar de la población (o muestral):"))
  })
  
  observe({
    input$go      
    
    x = switch(isolate(input$popdist), 
               
               "normal"= matrix(rnorm(isolate(input$n)*isolate(input$nsim), 0,1), ncol=isolate(input$n)),
               
               "right.skewed" = matrix(rgamma(isolate(input$n)*isolate(input$nsim), 
                                              shape=2, scale=5)/(5*sqrt(2))-10/(5*sqrt(2)), 
                                       ncol=isolate(input$n)),        
               
               "left.skewed" = matrix(10/(5*sqrt(2))-rgamma(isolate(input$n)*isolate(input$nsim), 
                                                            shape=2, scale=5)/(5*sqrt(2)), 
                                      ncol=isolate(input$n)),
               
               "uniform" = matrix(runif(isolate(input$n)*isolate(input$nsim), 
                                        -sqrt(3),sqrt(3)), ncol=isolate(input$n)),
               
               "bimodal" = matrix(rnorm(isolate(input$n)*isolate(input$nsim), 
                                        2*.92*(rbinom(n=isolate(input$n)*isolate(input$nsim), 
                                                      size=1, prob=.5)-.5), sd=sqrt(1-.92^2)),
                                  ncol=isolate(input$n)))
    
    x = isolate(input$popsd)*x + isolate(input$popmean)
    
    f=switch(isolate(input$statistic),
             media=mean,
             median=median,
             Q1=Q1,
             Q3=Q3,
             "standard deviation"=sd,
             maximum=max,
             minimum=min,
             CV=CV)
    
    withProgress(session, {
      if(isolate(input$nsim)>1000) setProgress(message = "Calculando...",
                                               detail = " ", value=.5)
      sample.statistics = isolate(apply(x, 1, f))
      draw.sample$sample.statistics <- 
        c(sample.statistics, isolate(draw.sample$sample.statistics))
      draw.sample$x = x[1,]
    })  
    
  })
  
  
  observe({
    input$n
    input$clear
    input$popdist
    input$statistic
    input$popmean
    input$popsd
    draw.sample$x<-NULL
    draw.sample$sample.statistics=NULL
  })
  
  output$popdistn <- renderPlot({
    
    popname = switch(input$popdist,
                     "normal" = "Normal" ,
                     "left.skewed"= "Left-skewed", 
                     "uniform" = "Uniform",
                     "right.skewed" =  "Right-skewed" ,
                     "bimodal" = "Bimodal")
    
    pdf = switch(input$popdist,
                 "normal"= dnorm,
                 "right.skewed" = function(x) 5*sqrt(2)*dgamma(5*sqrt(2)*x+10, shape=2, scale=5),
                 "left.skewed" = function(x) 5*sqrt(2)*dgamma(10-5*sqrt(2)*x, shape=2, scale=5),
                 "uniform" = function(x) dunif(x, -sqrt(3), sqrt(3)),
                 "bimodal" = function(x) (dnorm(x, mean=-.92, sd=sqrt(1-.92^2))+
                                            dnorm(x, mean=.92, sd=sqrt(1-.92^2)))/2
    )
    
    xlim = switch(input$popdist,
                  "normal"=c(-3,3),
                  "right.skewed" = c(-3,3),
                  "left.skewed" = c(-3,3),
                  "uniform" = c(-2,2),
                  "bimodal" = c(-2,2))                        
    par(mfrow=c(1,2), mar=rep(2,4))
    
    xlim = input$popsd*xlim + input$popmean
    
    parameters = input$popsd*parameters + input$popmean
    parameters[2,]=input$popsd
    
    title = paste("Distribución", popname,  
                  input$statistic, "=", round(parameters[input$statistic, input$popdist], 2))
    if (input$statistic=="standard deviation") title = 
      paste(popname,", ", input$statistic, " = ", 
            round(parameters[input$statistic, input$popdist], 2), sep="")
    curve(pdf((x-input$popmean)/input$popsd), xlim=xlim, xlab="", ylab="", main=title, cex=.75)
    
    pop.parameter = parameters[input$statistic, input$popdist]
    if (input$statistic=="standard deviation"){
      height=.2
      if (input$popdist=="uniform") height=.1
      abline(v=input$popmean, lty=2, col="red")
      segments(input$popmean, height, (input$popmean+input$popsd), height,  col="red") 
      s=input$popsd
      text(input$popmean + .5*input$popsd, height+.05, expression(sigma==s), cex=1.25)
    } else abline(v=pop.parameter, col="red")
    
  })
  
  output$dotplot <- renderPlot({
    input$n
    x = draw.sample$x
    stats=draw.sample$sample.statistics
    this.statistic = stats[1]
    
    par(mfrow=c(1,2))
    
    if (!is.null(x)){
      ## Compute lower and upper limits for the histogram
      default.lower  = -4*(input$popdist=="normal")+
        (-1.5)*(input$popdist=="right.skewed")+
        (-2)*(input$popdist=="uniform") + 
        (-2.5)*(input$popdist=="bimodal")+
        (-1.5)*(input$popdist=="left.skewed")
      
      default.lower = input$popsd*default.lower + input$popmean
      
      default.upper = 4*(input$popdist!="uniform" & input$popdist!= "bimodal" ) + 
        2*(input$popdist == "bimodal" | input$popdist=="uniform")  
      
      default.upper = input$popsd*default.upper + input$popmean
      
      xmin = min(default.lower, floor(min(x)-.5))
      xmax = max(default.upper, ceiling(max(x)+.5))
      
      hist1.details = hist(x, col="slategray1", border="darkgray",
                           main=paste("Histograma de la muestra",input$statistic,"=",
                                      round(this.statistic,2)), 
                           xlab="Datos de la última muestra",breaks=seq(xmin,xmax,length.out=20))
      abline(h=0)
      height = max(hist1.details$counts)/2
      
      if (input$statistic=="standard deviation") {
        abline(v=mean(x), lty=2, col="red")
        segments(mean(x), height, mean(x)+sd(x), height, lwd=2, col="red") 
        text(mean(x)+.5*sd(x), height+.2, paste("s=", round(sd(x),2)), cex=1.25)
      } else if (input$statistic!="CV") abline(v=this.statistic, col="red", lwd=2)
      
      parameters = input$popsd*parameters + input$popmean
      parameters[2,]=input$popsd
      pop.parameter = parameters[input$statistic, input$popdist]
      
      
      sample.size = input$n
      xmin=min(pop.parameter - input$popsd, floor(min(stats)-.5))
      xmax=max(pop.parameter + input$popsd, ceiling(max(stats)+.5))
      
      hist.details = hist(draw.sample$sample.statistics,  
                          breaks=seq(xmin, xmax, length.out = 20), plot=FALSE)
      ylim = c(0, max(6, max(hist.details$counts)+2))
      
      title.end = switch(isolate(input$statistic),
                         media="de la media",
                         median = "de la mediana muestral",
                         minimum = "del mínimo muestral", 
                         maximum = "del máximo muestral",
                         Q1 = "del primer cuartil (Q1)",
                         Q3 = "del tercer cuartil (Q3)",
                         "standard deviation"= "de la desviación estándar",
                         CV = "del coeficiente de variabilidad (CV)")
      
      hist2.details = hist(draw.sample$sample.statistics,  col="tomato",#572, 
                           xlab=paste("Muestra ", input$statistic, "s", sep=""), ylim=ylim,
                           main=paste("Distribución muestral \n",title.end),
                           breaks=seq(xmin,xmax,length.out=20) , border="darkgray")
      abline(h=0)
      if(input$display ){
        n.stats = length(draw.sample$sample.statistics)
        height2 = (max(hist2.details$counts)/2)
        textheight = (max(hist2.details$counts)/2)*(n.stats>10)*1.1 + 
          ((max(hist2.details$counts)/2)+1)*(n.stats<=10)
        
        abline(v=mean(stats), lty=2, lwd=1.25)
        segments(mean(stats),  lwd=1.25,
                 height2, mean(stats)+
                   sd(stats), height2) 
        text(mean(stats)+.5*sd(stats),textheight, 
             round(sd(stats),2), cex=1.2)
        text(mean(stats)+.5*sd(stats), 
             max(max(hist2.details$counts)*.9, ylim[2]*.9),
             round(mean(stats),2), cex=1.25)
      }
      
    } 
    
  })
  
  
  output$numsims  = renderText({  
    paste("Total de muestras extraídas =",
          as.character(length(draw.sample$sample.statistics)),
          "                          ")
  })
  
  output$display = renderText({
    f=switch(isolate(input$statistic),
             media="mean",
             median="median",
             Q1="Q1",
             Q3="Q3",
             "standard deviation"="sd",
             maximum="max",
             minimum="min",
             CV="CV")
    
    if (input$display) {
      str1 = paste("Media de ", input$statistic, "s muestrales= ", round(mean(draw.sample$sample.statistics),2), sep="")
      str2 = paste("Desviación estándar de ",input$statistic, "s muestrales= ", round(sd(draw.sample$sample.statistics),2), sep="")
      
      HTML(paste(str1, str2, sep = '<br/>'))
    }
  })
  
})
