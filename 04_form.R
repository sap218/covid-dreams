# @date: 08-12-2025
# @author: sap218

# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(scales)

# Data --------------------------------------------------------------------

primarydir <- "~/Python Scripts/covid-dreams/"

df <- read.delim(paste0(primarydir,"input/COVID Dreams (Responses) - Form Responses 1.tsv"),
                 skip=1,header=FALSE)

df <- df %>%
  rename(Responses=V2) %>%
  select(-V1) %>%
  mutate(Responses = as.factor(Responses) )

# Plot --------------------------------------------------------------------

p <- ggplot(df, aes(x=Responses, fill=Responses)) +
  #geom_bar(stat="count", width=0.5) +
  geom_bar(aes(y=after_stat(count) / sum(after_stat(count))), width=0.5) +
  scale_y_continuous(labels=percent) +
  
  labs(title="Poll responses if dreams were out of the ordinary with COVID", x="", y="") +
  scale_x_discrete(limits=c("Yes", "No")) +
  scale_fill_manual(values=c("Yes"="#00C19F",
                             "No"="#619CFF") ) + ##D39200
  theme_classic()

ggsave(filename=file.path(paste0(primarydir, "plots/poll.png")), plot=p, 
       width=7, height=5, dpi=300)

# Statistics --------------------------------------------------------------

yes_count <- sum(df$Responses=="Yes")
n <- nrow(df)

# p<0.05 = Yes occurs more often than No

binom.test(yes_count, n, p=0.5)

binom.test(yes_count, n, p=0.5, alternative="greater") # compares against "No" directly

# End ---------------------------------------------------------------------
