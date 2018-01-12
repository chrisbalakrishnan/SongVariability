---
  title: "R Notebook"
output: html_notebook
---
  
  Summary:
  
  response measure is an estimate of song dissimilarity between two birds. 

within each of three Subpops, we have these estimates of all pairwise combinations.

We are testing for an effect of Subpop (fixed effect) on a set of these song parameter distances (e.g. frequency modulation, FM). Because these distances are from pairs of individuals, I’m treating both individuals in the comparison as random factors (template and target). Within one of the three Subpops, there are actually 2 subSubpops, so I am including subSubpop also as a random factor (subpop).

I have no idea what I am doing, but this is what I have done so far.

model <- lmer(FM~ Subpop + (1 | Template) + (1| Target) + (1|Subpop), data = PCAandMMA, REML = FALSE) #should I REML or not REML?
model2<- lmer(FM~ Subpop + (1 | Template) + (1| Target), data = PCAandMMA, REML = FALSE)
anova(model, model2) # in most cases including the Subpop does not improve the fit very much, so generally I have excluded and then tested the simpler model
anova(model2)
difflsmeans(model, test.effs = "Subpop")
plot(Effect(c("Subpop"),model))

##MWM
Okay so first lets load and take a look at the data so that I can better understand the design.

```{r}
path<-"~/Documents/Projects/Domestication"
dat=read.table(paste(path,"KL_forPCAandMMAv2.txt",sep="/"),sep = "\t",header=TRUE)
str(dat)

require(ggplot2)
print(ggplot(dat,aes(x=Subpop,y=FM))+geom_boxplot())
#giving error
print(ggplot(dat,aes(x=Subpop,y=FM))+geom_boxplot()+facet_wrap(~Subpop))
```

So I wonder based on your description of the experiemnt if you need to treat both template and target as random effects?  If you think of the response as distance from the template then the identity of the target becomes irrelevant?  It just becomes part of the residual variation implicit in your random variable??? I think?

That would simplify the analysis a lot. Also, for the subSubpop question...the two subpopulatons do not look distinct from the box plot...so I would either ignore it...or treat the two as two wholey seperate Subpops instead of as subSubpops.  THen you would have 5 Subpops for the random effect...I will proceed with ignoring the subSubpop strucutre for now.

```{r}
require(lme4)
m1 <- lmer(FM ~ Population + (1 | Subpop) + (1 | Template) , data = dat, REML = FALSE) 
VarCorr(m1) # Check for singularity
plot(m1) # residuals plot
qqnorm(resid(m1)); qqline(resid(m1)) #qqplot

```
Can see the classic cone pattern in the residuals and also the long upper tail in the qqplot suggesting either a lognormal or poisson distribution.  However, since these are not counts...(I dont think?)...we should probably stick with the lognormal.  But first we need to examine the data range...since a log transformation is asymetrical.

```{r}
range(dat$FM)
```

Shit! The data are > and < 1 so we cant do log transformation. 

So we are kinda screwed with regards to goo distribution models.  WE could fit Gamma errors but that gets complicated. Lets just do the log transform and see if we get any problems.

```{r}
print(ggplot(dat,aes(x=Subpop,y=log(FM)))+geom_boxplot())

```

Doesnt look as though we get any estreme outliers as is caused when values are very close to zero...so lets try the analysis again.

```{r}
m2 <- lmer(log(FM) ~ Population + (1 | Subpop) + (1 | Template) , data = dat, REML = FALSE) 
VarCorr(m2) # Check for singularity
plot(m2) # residuals plot
qqnorm(resid(m2)); qqline(resid(m2)) #qqplot

```

Absolutely lovely!
  
  now we can test whether there is a Subpop effect.

```{r}
m3 <- lmer(log(FM) ~ 1 + (1 | Subpop) + (1 | Template) , data = dat, REML = FALSE) 
anova(m2,m3)

```

So there is a significant Subpop effect!!!
  
  Lets load a cheesy little function for extracting the predictions and CIs for plotting.

```{r}
easyPredCInorm <- function(model,newdata,alpha=0.05) {
  ## baseline prediction, on the linear predictor (logit) scale:
  pred0 <- predict(model,re.form=NA,newdata=newdata)
  ## fixed-effects model matrix for new data
  X <- model.matrix(formula(model,fixed.only=TRUE)[-2],
                    newdata)
  beta <- fixef(model) ## fixed-effects coefficients
  V <- vcov(model)     ## variance-covariance matrix of beta
  pred.se <- sqrt(diag(X %*% V %*% t(X))) ## std errors of predictions
  ## inverse-link (logistic) function: could also use plogis()
  linkinv <- I
  ## construct 95% Normal CIs on the link scale and
  ##  transform back to the response (probability) scale:
  crit <- -qnorm(alpha/2)
  linkinv(cbind(lwr=pred0-crit*pred.se,
                upr=pred0+crit*pred.se))
}

```


Now we can extract model predicted means and confidence intervals and draw a pretty picture.

```{r}
summary(m2)

dat$predicted=exp(predict(m2,re.form=NA))
pframe <- data.frame(Subpop=dat$Subpop)
dat$LCI=as.vector(exp(easyPredCInorm(m2,pframe))[,1])
dat$UCI=as.vector(exp(easyPredCInorm(m2,pframe))[,2])
#str(dat)
ggplot(dat,aes(x=Subpop,y=predicted))+geom_point(size=3)+geom_errorbar(aes(ymin=LCI,ymax=UCI))+
  ylab("FM (Distance from Template)")+xlab("Subpop")+theme_bw()
```

```{r}
library(effects)
library (lmerTest)
difflsmeans(m2, test.effs = "Subpop")
plot(Effect(c("Population"),m2))
```