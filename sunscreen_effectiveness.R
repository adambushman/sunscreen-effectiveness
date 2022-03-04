# Install packages
install.packages('Sleuth3')
install.packages('car')

# Load libraries
library('Sleuth3')

data = ex0430
head(data)

# Goal:
  # Analyze the data to estimate and provide a confidence interval for the
  # sunlight projection factor

# Hypotheses
  # H0: (u.pre - u.sunscreen) = 0 ... no difference in sunlight protection 
  # H1: (u.pre - u.sunscreen) != 0 ... there is a difference in protection

par(bg="#FFF7F8")

boxplot(data
        , main = "Distribution of Sunlight Tolerance"
        , xlab = "Minutes of Sunlight Tolerance"
        , col = c("#BB342F", "#DDA448")
        , horizontal = TRUE)

# Assumptions:
  # Data is not independent; we have a paired dataset so we'll take the
    # difference in values and work with those

    data$Difference = data$Sunscreen - data$PreTreatment
    
    boxplot(data$Difference
            , main = "Distribution of Sunlight Tolerance Difference"
            , xlab = "Minutes Difference of Sunlight Tolerance"
            , col = "#33CA7F"
            , horizontal = TRUE)
    
  # The data appears normally distributed here, but a quick shapiro test
    # will confirm
    
    shapiro.test(data$Difference)
  
  # Since we took the difference of our paired dataset, there's no need
    # to consider equal variance since we're now dealing with just one
    # population


# Test
  # We'll perform a two-sided, single population t-test
  # We do a t-test because we're working with numerical data and we've met
    # the requisite assumptions
    
results = t.test(data$Difference
                , alternative = "two.sided"
                , conf.level = 0.95)

results

# Summary
  # Our p-value of the difference in sunlight tolerance is sufficiently small
  # to instill strong confidence that the true population difference is not
  # zero. We therefore reject the null hypothesis.

u.est = results$estimate
conf = results$conf.int[1:2]

# Plot Confidence Interval

plot(c(floor(conf[1]-15),ceiling(conf[2]+15)),c(-1,1)
     , type = "n"
     , main = "95% Confidence Interval"
     , ylab = ""
     , yaxt = "n"
     , xlab = "Difference in Minute Sunlight Tolerance")
polygon(c(conf[1],conf[1],conf[2],conf[2]),c(0.5,-0.5,-0.5,0.5)
        , col = "#33CA7F"
        , border = "#4C5B5C")
abline(h=0
       , lwd = 1.5
       , lty = 2
       , col = "#4C5B5C")
abline(v=u.est, lwd=2)
text(u.est, 0.85
     , labels = round(u.est,1)
     , adj = 1
     , pos = 4
     , cex = 0.9)
text(conf[1], 0.6
     , labels = round(conf[1],1)
     , adj = 0.5
     , pos = 3
     , cex = 0.7
     , col ="#4C5B5C")
text(conf[2], 0.6
     , labels = round(conf[2],1)
     , adj = 0.5
     , pos = 3
     , cex = 0.7
     , col ="#4C5B5C")

