#https://bconnelly.net/posts/analyzing_microbial_growth_with_r/


library(reshape2)
library(dplyr)
library(ggplot2)


rawdata <- read.csv("growth-raw.csv")

reshaped <- melt(rawdata, id=c("Time", "Temperature"), variable.name="Well",
                 value.name="OD600")

summary(reshaped)


head(reshaped)


platemap <- read.csv("platemap.csv")
head(platemap, n=10)


# Combine the reshaped data with the plate map, pairing them by Well value
annotated <- inner_join(reshaped, platemap, by="Well")

# Take a peek at the first few records in annotated
head(annotated)

# Write the annotated data set to a CSV file
write.csv(annotated, "data-annotated.csv")

grouped <- group_by(annotated, Strain, Environment, Time)

stats <- summarise(grouped, N=length(OD600), Average=mean(OD600), StDev=sd(OD600))

# Create a function that calculates 95% confidence intervals for the given
# data vector using a t-distribution
conf_int95 <- function(data) {
    n <- length(data)
    error <- qt(0.975, df=n-1) * sd(data)/sqrt(n)
    return(error)
}

# Create summary for each group containing sample size, average OD600, and
# 95% confidence limits
stats <- summarise(grouped, N=length(OD600), Average=mean(OD600),
                   CI95=conf_int95(OD600))


stats <- annotated %>%
          group_by(Environment, Strain, Time) %>%
          summarise(N=length(OD600), 
                    Average=mean(OD600),
                    CI95=conf_int95(OD600)) %>%
          filter(!is.na(Strain))


ggplot(data=stats, aes(x=Time/3600, y=Average, color=Strain)) + 
       geom_line() + 
       labs(x="Time (Hours)", y="Absorbance at 600 nm")


ggplot(data=stats, aes(x=Time/3600, y=Average, color=Strain)) + 
       geom_line() + 
       facet_grid(Environment ~ .) +
       labs(x="Time (Hours)", y="Absorbance at 600 nm")


ggplot(data=stats, aes(x=Time/3600, y=Average, color=Strain)) +
       geom_ribbon(aes(ymin=Average-CI95, ymax=Average+CI95, fill=Strain),
                   color=NA, alpha=0.3) + 
       geom_line() +
       scale_y_log10() +
       facet_grid(Environment ~ .) +
       labs(x="Time (Hours)", y="Absorbance at 600 nm")


