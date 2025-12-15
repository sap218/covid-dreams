# @date: 08-12-2025
# @author: sap218

# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gridExtra)

# Data --------------------------------------------------------------------

primarydir <- "~/Python Scripts/covid-dreams/"
of_interest = c("Positive","Negative","Oddity","Intensity")

# Monthly -----------------------------------------------------------------

{
a <- read.csv(paste0(primarydir,"output/03_after_monthly.csv"))
b <- read.csv(paste0(primarydir,"output/03_before_monthly.csv"))

d <- read.csv(paste0(primarydir,"output/03_during_monthly.csv"))
d <- d %>% 
  mutate(
    Month = as.factor(Month),
    Date = as.Date(Date),
    Cases = as.numeric(Cases)
  )
}

nudge = -12.5

plot_var <- function(var_name) {
  ggplot(d, aes(x=Date, y=Cases)) +
    geom_bar(stat="identity", fill="lightgray", position=position_nudge(x=nudge) ) +
    
    #geom_line(aes(y=b[[var_name]], color="Before"), linewidth=1) +
    #geom_line(aes(y=d[[var_name]], color="During"), linewidth=1) +
    
    geom_smooth(aes(y=b[[var_name]], color="Before"), method="loess", se=FALSE, linewidth=1) +
    geom_smooth(aes(y=d[[var_name]], color="During"), method="loess", se=FALSE, linewidth=1) +
    geom_smooth(aes(y=a[[var_name]], color="After"), method="loess", se=FALSE, linewidth=1) +    
        
    scale_color_manual(values=c("Before"="#FF68A1",
                                "During"="#00C19A",
                                "After"="orange"
                                ),
                       breaks=c("Before",
                                "During",
                                "After"
                                )) +
    
    scale_x_date(breaks=as.Date(d$Date), date_labels="%b") +
    geom_vline(data=d, aes(xintercept=Date), color="darkgrey", linetype="solid") +
    
    labs(title=var_name, x="", y="") +
    theme_bw() +
    theme(axis.text.y=element_blank(), plot.title=element_text(hjust=0.5), 
          legend.position="bottom", legend.title=element_blank())
}

p1 <- plot_var("Positive")
p2 <- plot_var("Negative")
p3 <- plot_var("Oddity")
p4 <- plot_var("Intensity")

p <- arrangeGrob(p1, p2, p3, p4, ncol=2, nrow=2,
                 top="Types of dream searches over the years")

ggsave(filename=file.path(paste0(primarydir, "plots/baseline_bar.png")), plot=p,
       width=12, height=9, dpi=300)

rm(p, p1, p2, p3, p4, a)

# loop

for (i in of_interest) {
  print(" ")
  print(i)
  
  # if during is greater than b, ignoring magnitude of difference
  print( paste0("binom p-value = ",
                binom.test(sum(d[[i]] > b[[i]]), n=length(b[[i]]))$p.value
  ))
  
  # normality
  normTest <- shapiro.test(b[[i]] - d[[i]])$p.value
  #print(paste0("shapiro (normality) p-value = ", normTest))
  
  if (normTest > 0.05) {
    # tests whether the mean of the paired differences is zero
    print( paste0 ("t-test p-value = ",
                   t.test(b[[i]], d[[i]], paired=TRUE)$p.value
    ))
  } else {
    
    # tests whether the distribution of differences is centered around zero
    print( paste0 ("wilcox p-value = ",
                   wilcox.test(d[[i]], b[[i]], paired=TRUE, 
                               alternative = "two.sided"
                               #alternative="greater" # if d > b
                   )$p.value
    ))
  }
  
  # variance difference
  print( paste0 ("f-test p-value = ",
                 var.test(d[[i]], b[[i]])$p.value
  ))
  
  print(paste0("correlation = ", cor(d[[i]], b[[i]], method="spearman")))
  print(paste0("p-value = ", cor.test(d[[i]], b[[i]], method="spearman", exact=FALSE)$p.value))
}

# scatter

p1 <- ggplot(d, aes(x=d$Positive, y=b$Positive)) + geom_point() +
  geom_smooth(method="lm", se=TRUE, color="#7CAE00") +
  labs(x=" ",y=" ",title="Positive") +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank() )

p2 <- ggplot(d, aes(x=d$Negative, y=b$Negative)) + geom_point() +
  geom_smooth(method="lm", se=TRUE, color="#F8766D") +
  labs(x=" ",y=" ",title="Negative") +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank() )

p3 <- ggplot(d, aes(x=d$Oddity, y=b$Oddity)) + geom_point() +
  geom_smooth(method="lm", se=TRUE, color="#00BFC4") +
  labs(x=" ",y=" ",title="Oddity") +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank() )

p4 <- ggplot(d, aes(x=d$Intensity, y=b$Intensity)) + geom_point() +
  geom_smooth(method="lm", se=TRUE, color="#C77CFF") +
  labs(x=" ",y=" ",title="Intensity") +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank() )

p <- arrangeGrob(p1, p2, p3, p4, ncol=2, nrow=2,
                 top="Monthly relationship of dream searches before and during COVID",
                 bottom="During COVID", left="Before COVID")

ggsave(filename=file.path(paste0(primarydir, "plots/baseline_monthly_scatter.png")), plot=p,
       width=12, height=9, dpi=300)

rm(p1, p2, p3, p4, p)

# Weekly ------------------------------------------------------------------

{
  b <- read.csv(paste0(primarydir,"output/03_before_merged.csv"))
  
  d <- read.csv(paste0(primarydir,"output/03_during_merged.csv"))
  d <- d %>% 
    mutate(
      Month = as.factor(Month),
      Date = as.Date(Date),
      Cases = as.numeric(Cases)
    )
}

# loop

for (i in of_interest) {
  print(" ")
  print(i)
  
  # if during is greater than b, ignoring magnitude of difference
  print( paste0("binom p-value = ",
  binom.test(sum(d[[i]] > b[[i]]), n=length(b[[i]]))$p.value
  ))

  # normality
  normTest <- shapiro.test(b[[i]] - d[[i]])$p.value
  #print(paste0("shapiro (normality) p-value = ", normTest))
  
  if (normTest > 0.05) {
    # tests whether the mean of the paired differences is zero
    print( paste0 ("t-test p-value = ",
                   t.test(b[[i]], d[[i]], paired=TRUE)$p.value
                   ))
  } else {
  
    # tests whether the distribution of differences is centered around zero
    print( paste0 ("wilcox p-value = ",
    wilcox.test(d[[i]], b[[i]], paired=TRUE, 
                alternative = "two.sided"
                #alternative="greater" # if d > b
    )$p.value
    ))
  }
  
  # variance difference
  print( paste0 ("f-test p-value = ",
    var.test(d[[i]], b[[i]])$p.value
  ))
  
  print(paste0("correlation = ", cor(d[[i]], b[[i]], method="spearman")))
  print(paste0("p-value = ", cor.test(d[[i]], b[[i]], method="spearman", exact=FALSE)$p.value))
}

# scatter

p1 <- ggplot(d, aes(x=d$Positive, y=b$Positive)) + geom_point() +
  geom_smooth(method="lm", se=TRUE, color="#7CAE00") +
  labs(x=" ",y=" ",title="Positive") +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank() )

p2 <- ggplot(d, aes(x=d$Negative, y=b$Negative)) + geom_point() +
  geom_smooth(method="lm", se=TRUE, color="#F8766D") +
  labs(x=" ",y=" ",title="Negative") +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank() )

p3 <- ggplot(d, aes(x=d$Oddity, y=b$Oddity)) + geom_point() +
  geom_smooth(method="lm", se=TRUE, color="#00BFC4") +
  labs(x=" ",y=" ",title="Oddity") +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank() )

p4 <- ggplot(d, aes(x=d$Intensity, y=b$Intensity)) + geom_point() +
  geom_smooth(method="lm", se=TRUE, color="#C77CFF") +
  labs(x=" ",y=" ",title="Intensity") +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank() )

p <- arrangeGrob(p1, p2, p3, p4, ncol=2, nrow=2,
              top="Weekly relationship of dream searches before and during COVID",
              bottom="During COVID", left="Before COVID")

ggsave(filename=file.path(paste0(primarydir, "plots/baseline_weekly_scatter.png")), plot=p,
       width=12, height=9, dpi=300)

rm(p1, p2, p3, p4, p)

# End ---------------------------------------------------------------------
