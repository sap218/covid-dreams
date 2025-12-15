# @date: 05-12-2025
# @author: sap218

# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)

# Data --------------------------------------------------------------------

primarydir <- "~/Python Scripts/covid-dreams/"

{
df <- read.csv(paste0(primarydir,"output/03_during_merged.csv"))
df <- df %>% 
  mutate(
    Month = as.factor(Month),
    Date = as.Date(Date),
    Cases = as.numeric(Cases)
  )

monthly <- read.csv(paste0(primarydir,"output/03_during_monthly.csv"))
monthly <- monthly %>% 
  mutate(
    Month = as.factor(Month),
    Date = as.Date(Date),
    Cases = as.numeric(Cases)
  )

ev <- read.delim(paste0(primarydir,"input/events.tsv"))
ev$date <- as.Date(ev$date, format="%d-%m-%Y")
}

of_interest = c("Positive","Negative","Oddity","Intensity","COVID")

# normality checking
# shapiro.test(df$Cases)$p.value
# p<0.05 = not normal

# (A) Bar plot w/ lines ---------------------------------------------------

y_top <- max(monthly$Cases) - 250000
nudge = -12.5

a <- 
  ggplot(monthly, aes(x=Date, y=Cases)) +
  geom_bar(stat="identity", fill="lightgray", position=position_nudge(x=nudge) ) +
  
  #geom_line(aes(y=Positive, color="Positive"), linewidth=1) +
  geom_smooth(aes(y=Positive, color="Positive"), method="loess", se=FALSE, linewidth=1) +
  #geom_line(aes(y=Negative, color="Negative"), linewidth=1) +
  geom_smooth(aes(y=Negative, color="Negative"), method="loess", se=FALSE, linewidth=1) +
  #geom_line(aes(y=Oddity, color="Oddity"), linewidth=1) +
  geom_smooth(aes(y=Oddity, color="Oddity"), method="loess", se=FALSE, linewidth=1) +
  #geom_line(aes(y=Intensity, color="Intensity"), linewidth=1) +
  geom_smooth(aes(y=Intensity, color="Intensity"), method="loess", se=FALSE, linewidth=1) +
  #geom_line(aes(y=COVID, color="COVID"), linewidth=1) +
  geom_smooth(aes(y=COVID, color="COVID"), method="loess", se=FALSE, linewidth=1) +
  
  scale_color_manual(values=c("Positive"="#7CAE00",
                              "Negative"="#F8766D",
                              "Oddity"="#00BFC4",
                              "Intensity"="#C77CFF",
                              "COVID"="#E68613"
                              ),
                     breaks=c("Positive","Negative","Oddity","Intensity","COVID")) +
  
  geom_vline(data=ev, aes(xintercept=as.numeric(date)), color="#FF61CC",linetype="dashed") + # dotted
  geom_text(data=ev, aes(x=date, y=y_top, label=event), angle=90, vjust=-0.5, color="black", size=4) +
  
  geom_text(aes(label=Cases), vjust=-0.3, size=3.5, position=position_nudge(x=nudge) ) +
  
  scale_x_date(breaks=as.Date(monthly$Date), labels=monthly$Month) +
  geom_vline(data=monthly, aes(xintercept=Date), color="darkgrey", linetype="solid") +

  labs(title="UK COVID case counts and Dream searches", x="", y="") +
  #theme_minimal() +
  theme_bw() +
  theme(axis.text.y=element_blank(), plot.title=element_text(hjust=0.5), 
        legend.position="top", legend.title=element_blank())

ggsave(filename=file.path(paste0(primarydir, "plots/monthly_bar.png")), plot=a, 
       width=20, height=9, dpi=300)

rm(a, nudge, y_top, ev)

# (B) Correlation ---------------------------------------------------------

# trend-driven

# monthly
# against cases
for (i in of_interest) {
  print(" ")
  print(i)
  
  #print(paste0("correlation = ", cor(monthly$Cases, monthly[[i]], method="spearman")))
  print(paste0("p-value = ", cor.test(monthly$Cases, monthly[[i]], method="spearman", exact=FALSE)$p.value))
}
# against Google
for (i in of_interest[-length(of_interest)]) {
  print(" ")
  print(i)
  
  #print(paste0("correlation = ", cor(monthly$COVID, monthly[[i]], method="spearman")))
  print(paste0("p-value = ", cor.test(monthly$COVID, monthly[[i]], method="spearman", exact=FALSE)$p.value))
}

{
  cor_mat <- cor(monthly[ , c("Cases", of_interest)], use="complete.obs")
  png(file.path(primarydir, "plots/monthly_corr.png"), width=6, height=6, units="in", res=300)
  c <- corrplot(cor_mat, method="color", addCoef.col="black", tl.col="black", tl.srt=0, 
                type="upper", main="Monthly correlation matrix", mar=c(.1,.1,2,.1))
  dev.off()
}

# weekly
# against cases
for (i in of_interest) {
  print(" ")
  print(i)
  
  #print(paste0("correlation = ", cor(df$Cases, df[[i]], method="spearman")))
  print(paste0("p-value = ", cor.test(df$Cases, df[[i]], method="spearman", exact=FALSE)$p.value))
}
# against Google
for (i in of_interest[-length(of_interest)]) {
  print(" ")
  print(i)
  
  #print(paste0("correlation = ", cor(df$COVID, df[[i]], method="spearman")))
  print(paste0("p-value = ", cor.test(df$COVID, df[[i]], method="spearman", exact=FALSE)$p.value))
}

{
cor_mat <- cor(df[ , c("Cases", of_interest)], use="complete.obs")
png(file.path(primarydir, "plots/weekly_corr.png"), width=6, height=6, units="in", res=300)
c <- corrplot(cor_mat, method="color", addCoef.col="black", tl.col="black", tl.srt=0, 
              type="upper", main="Weekly correlation matrix", mar=c(.1,.1,2,.1))
dev.off()
}

# due to more data, we can:
# remove trend and look at row-to-row changes whether weekly fluctuations are correlated
# computes differences from one row to next, reduces correlation (double upward trend) & fixes independence
df_diff <- df %>% 
  mutate(
    Cases = c(NA, diff(Cases)),
    Positive = c(NA, diff(Positive)),
    Negative = c(NA, diff(Negative)),
    Oddity = c(NA, diff(Oddity)),
    Intensity = c(NA, diff(Intensity)),
    COVID = c(NA, diff(COVID))
  ) %>% select(-Date, -Month)
df_diff <- df_diff[-1, ]

for (i in of_interest) {
  print(" ")
  print(i)
  
  print(paste0("correlation =", cor(df_diff$Cases, df_diff[[i]], method="spearman")))
  print(paste0("p-value =", cor.test(df_diff$Cases, df_diff[[i]], method="spearman", exact=FALSE)$p.value))
}

rm(df_diff, c, cor_mat)

# Scatter w/ Regression
# shows relationship direction between COVID counts and dream searches:

# monthly against cases
{
  p1 <- ggplot(monthly, aes(x=Cases, y=Positive)) + geom_point() +
    geom_smooth(method="lm", se=TRUE, color="#7CAE00") +
    labs(x="Positive",y=" ") +
    theme(axis.text.y=element_blank(), axis.text.x=element_blank() )
  
  p2 <- ggplot(monthly, aes(x=Cases, y=Negative)) + geom_point() +
    geom_smooth(method="lm", se=TRUE, color="#F8766D") +
    labs(x="Negative",y=" ") +
    theme(axis.text.y=element_blank(), axis.text.x=element_blank() )
  
  p3 <- ggplot(monthly, aes(x=Cases, y=Oddity)) + geom_point() +
    geom_smooth(method="lm", se=TRUE, color="#00BFC4") +
    labs(x="Oddity",y=" ") +
    theme(axis.text.y=element_blank(), axis.text.x=element_blank() )
  
  p4 <- ggplot(monthly, aes(x=Cases, y=Intensity)) + geom_point() +
    geom_smooth(method="lm", se=TRUE, color="#C77CFF") +
    labs(x="Intensity",y=" ") +
    theme(axis.text.y=element_blank(), axis.text.x=element_blank() )
  
  p5 <- ggplot(monthly, aes(x=Cases, y=COVID)) + geom_point() +
    geom_smooth(method="lm", se=TRUE, color="#E68613") +
    labs(x="COVID",y=" ") +
    theme(axis.text.y=element_blank(), axis.text.x=element_blank() )
}

b <- arrangeGrob(p1, p2, p3, p4, p5, ncol=3, nrow=2,
                 top="Monthly relationship of UK COVID case counts and Dream searches")

ggsave(filename=file.path(paste0(primarydir, "plots/monthly_scatter.png")), plot=b,
       width=12, height=9, dpi=300)

# monthly against trends
{
  p1 <- ggplot(monthly, aes(x=COVID, y=Positive)) + geom_point() +
    geom_smooth(method="lm", se=TRUE, color="#7CAE00") +
    labs(x="Positive",y=" ") +
    theme(axis.text.y=element_blank(), axis.text.x=element_blank() )
  
  p2 <- ggplot(monthly, aes(x=COVID, y=Negative)) + geom_point() +
    geom_smooth(method="lm", se=TRUE, color="#F8766D") +
    labs(x="Negative",y=" ") +
    theme(axis.text.y=element_blank(), axis.text.x=element_blank() )
  
  p3 <- ggplot(monthly, aes(x=COVID, y=Oddity)) + geom_point() +
    geom_smooth(method="lm", se=TRUE, color="#00BFC4") +
    labs(x="Oddity",y=" ") +
    theme(axis.text.y=element_blank(), axis.text.x=element_blank() )
  
  p4 <- ggplot(monthly, aes(x=COVID, y=Intensity)) + geom_point() +
    geom_smooth(method="lm", se=TRUE, color="#C77CFF") +
    labs(x="Intensity",y=" ") +
    theme(axis.text.y=element_blank(), axis.text.x=element_blank() )
}

b <- arrangeGrob(p1, p2, p3, p4, ncol=2, nrow=2,
                 top="Monthly relationship of COVID searches against Dreams")

ggsave(filename=file.path(paste0(primarydir, "plots/monthly_trends_scatter.png")), plot=b,
       width=12, height=9, dpi=300)

#
# weekly against cases
{
p1 <- ggplot(df, aes(x=Cases, y=Positive)) + geom_point() +
  geom_smooth(method="lm", se=TRUE, color="#7CAE00") +
  labs(x="Positive",y=" ") +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank() )

p2 <- ggplot(df, aes(x=Cases, y=Negative)) + geom_point() +
  geom_smooth(method="lm", se=TRUE, color="#F8766D") +
  labs(x="Negative",y=" ") +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank() )

p3 <- ggplot(df, aes(x=Cases, y=Oddity)) + geom_point() +
  geom_smooth(method="lm", se=TRUE, color="#00BFC4") +
  labs(x="Oddity",y=" ") +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank() )

p4 <- ggplot(df, aes(x=Cases, y=Intensity)) + geom_point() +
  geom_smooth(method="lm", se=TRUE, color="#C77CFF") +
  labs(x="Intensity",y=" ") +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank() )

p5 <- ggplot(df, aes(x=Cases, y=COVID)) + geom_point() +
  geom_smooth(method="lm", se=TRUE, color="#E68613") +
  labs(x="COVID",y=" ") +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank() )
}

b <- arrangeGrob(p1, p2, p3, p4, p5, ncol=3, nrow=2,
             top="Weekly relationship of UK COVID case counts and Dream searches")

ggsave(filename=file.path(paste0(primarydir, "plots/weekly_scatter.png")), plot=b,
       width=12, height=9, dpi=300)

# weekly against trends
{
  p1 <- ggplot(df, aes(x=COVID, y=Positive)) + geom_point() +
    geom_smooth(method="lm", se=TRUE, color="#7CAE00") +
    labs(x="Positive",y=" ") +
    theme(axis.text.y=element_blank(), axis.text.x=element_blank() )
  
  p2 <- ggplot(df, aes(x=COVID, y=Negative)) + geom_point() +
    geom_smooth(method="lm", se=TRUE, color="#F8766D") +
    labs(x="Negative",y=" ") +
    theme(axis.text.y=element_blank(), axis.text.x=element_blank() )
  
  p3 <- ggplot(df, aes(x=COVID, y=Oddity)) + geom_point() +
    geom_smooth(method="lm", se=TRUE, color="#00BFC4") +
    labs(x="Oddity",y=" ") +
    theme(axis.text.y=element_blank(), axis.text.x=element_blank() )
  
  p4 <- ggplot(df, aes(x=COVID, y=Intensity)) + geom_point() +
    geom_smooth(method="lm", se=TRUE, color="#C77CFF") +
    labs(x="Intensity",y=" ") +
    theme(axis.text.y=element_blank(), axis.text.x=element_blank() )
}

b <- arrangeGrob(p1, p2, p3, p4, ncol=2, nrow=2,
                 top="Weekly relationship of COVID searches against Dreamss")

ggsave(filename=file.path(paste0(primarydir, "plots/weekly_trends_scatter.png")), plot=b,
       width=12, height=9, dpi=300)

##

rm(p1, p2, p3, p4, p5, b)

# (C) Linear Regression ---------------------------------------------------
# quantify how much dream searches increase with COVID counts

# monthly against cases
for (i in of_interest) {
  print(" ")
  print(paste0(i, " - linear regression p-value"))
  
  lm_model <- lm(monthly[[i]] ~ Cases, data=monthly)
  print(summary(lm_model)$coefficients["Cases", "Pr(>|t|)"])
}
# monthly against searches
for (i in of_interest[-length(of_interest)]) {
  print(" ")
  print(paste0(i, " - linear regression p-value"))
  
  lm_model <- lm(monthly[[i]] ~ COVID, data=monthly)
  print(summary(lm_model)$coefficients["COVID", "Pr(>|t|)"])
}

# weekly against cases
for (i in of_interest) {
  print(" ")
  print(paste0(i, " - linear regression p-value"))
  
  lm_model <- lm(df[[i]] ~ Cases, data=df)
  print(summary(lm_model)$coefficients["Cases", "Pr(>|t|)"])
}
# weekly against searches
for (i in of_interest[-length(of_interest)]) {
  print(" ")
  print(paste0(i, " - linear regression p-value"))
  
  lm_model <- lm(df[[i]] ~ COVID, data=df)
  print(summary(lm_model)$coefficients["COVID", "Pr(>|t|)"])
}

##

lm_model <- lm(Cases ~ Positive + Negative + Oddity + Intensity, data = df)
print(summary(lm_model) )

rm(lm_model)

# End ---------------------------------------------------------------------
