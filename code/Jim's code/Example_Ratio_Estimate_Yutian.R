# Function to generate intra-annual ratio estimate where bycatch per unit effort (BPUE) is calculated separately for each year.
# You could calculate a single BPUE for all years combined, which would provide more stable estimates, due to rare-event biases in estimation.

# load example data (which represents real drift gillnet fishery bycatch data, but is anonymous with regard to fishing trip details). The annual
# details of these data are published in tabular form in Carretta 2023 https://repository.library.noaa.gov/view/noaa/52092

   load("Ratio.Estimate.RData")
   
# `df1` represents observer data for drift gillnet fishery. one row = one fishing set
# `df2` represents observer coverage for the fishery
 
 
####

   RatioEstimate <- function(df1, df2) {
  
    BootStrpAnn <- function(x) { iter=999
    mean.boot <- matrix(NA, iter)
  
      for (i in 1:iter) { b <- sample(x, length(x), replace=T)
        mean.boot[i]=mean(b)  }
  
        boot.CV <- sd(mean.boot)/mean(x)
        boot.CV  }
  
       Estimated.Effort.Ann <- df2$EffortSets
       Observed.Effort.Ann <- df2$ObservedSets
       Observer.Coverage.Ann <- Observed.Effort.Ann / Estimated.Effort.Ann
  
       Observed.Ann.Bycatch <- tapply(df1$bycatch, df1$year, sum)
  # point estimate is inverse of observer coverage in this example
       Ratio.Est.Ann <- Observed.Ann.Bycatch * 1/ Observer.Coverage.Ann
  
       Year <- df2$year
  
       Observed.Ann.Trip <- matrix(NA, length(Year))
       CV.Ratio.Ann <- matrix(NA, length(Year))
  
  for (y in 1:length(Year)) {
    
    SpAnnTemp <- df1[df1$year==Year[y],]
    Observed.Ann.Trip.Temp <- tapply(SpAnnTemp$bycatch, SpAnnTemp$trip, sum)
    CV.Ratio.Ann[y] <- BootStrpAnn(Observed.Ann.Trip.Temp)
    
  }
  
  Observer.Coverage.Ann <- round(Observer.Coverage.Ann, 3)
  Ratio.Est.Ann <- round(Ratio.Est.Ann, 1)
  CV.Ratio.Ann <- round(CV.Ratio.Ann, 2)
  
  CV.Ratio.Ann[which(Ratio.Est.Ann==0)] <- NA
  
  Ratio.Est.df <- cbind.data.frame(Year, Observed.Ann.Bycatch, Observed.Effort.Ann, Estimated.Effort.Ann, Observer.Coverage.Ann, Ratio.Est.Ann, CV.Ratio.Ann)
  Ratio.Est.df  }
   
###### end function
   
   # generate a data frame of ratio estimates with estimated uncertainty expressed as coefficient of variation

  Ratio.Estimates.df <- RatioEstimate(df1, df2)