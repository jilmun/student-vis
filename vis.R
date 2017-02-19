library(tidyverse)


# flatten data ------------------------------------------------------------

dat_orig <- read_csv("Master-Profiles.csv")
dat_long <- dat_orig %>% 
  gather(Question, Value, `Favorite Subject - Math`:`Favorite Project - Solving a problem`)
write_csv(dat_long, "Master-Long.csv")

# dat_long$Question <- gsub("P\\.E\\...Gym", "Gym", dat_long$Question)
# dat_long$Question <- gsub("Through.music...if", "Through music if", dat_long$Question)
# dat_long$Question <- gsub("\\.\\.\\.", "_", dat_long$Question)

dat_long <- dat_long %>%
  separate(Question, c("Question", "Response"), sep=" - ")
# dat_long$Question <- gsub("\\.", " ", dat_long$Question)
# dat_long$Response <- gsub("\\.", " ", dat_long$Response)


# summary stats -----------------------------------------------------------

dat_summ <- dat_long %>%
  group_by(Period, Grade, Question, Response) %>%
  summarise(Total = sum(Value))
dat_summ
dat_summ$Period <- factor(dat_summ$Period, levels=c("First", "Second", "Third", "Fourth"))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

dat_summ %>% 
  filter(Question == "Favorite Subject") %>%
  ggplot(aes(x=Response, y=Total, fill=as.factor(Grade))) + 
    geom_bar(position="dodge", stat="identity") +
    facet_wrap(~Grade) +
  ggtitle("Favorite Subject by Grade") + 
  ylab("Total Responses") + 
  xlab("") + 
  guides(fill=FALSE) + 
  scale_fill_manual(values=cbPalette[c(2,4)])


# print charts ------------------------------------------------------------

to_print <- c("Favorite Subject",
              "Favorite Specials",
              "Favorite Project",
              "Information Delivery Method",
              "Expression Format",
              "Presentation Format",
              "Learning Method",
              "Learning Space",
              "Motivation",
              "NWEA Reading Strength",
              "NWEA Reading Weakness")

for (i in to_print) {
  # export png as 800 width x 520 height  
  png(file=paste0(i, ".png"), width=800, height=520)
  
  g <- dat_summ %>%  
    filter(Question == i) %>%
    ggplot(aes(x=Response, y=Total, fill=as.factor(Grade))) + 
    geom_bar(position="dodge", stat="identity") +
    facet_wrap(~Period, ncol=2) +
    ggtitle(paste0(i, " by Period")) + 
    ylab("Total Responses") + 
    xlab("") + 
    guides(fill=FALSE) + 
    scale_fill_manual(values=cbPalette[c(2,4,2,4)]) +
    theme(axis.text.x=element_text(angle = -90, hjust = 0)) 
  
  print(g)
  dev.off()
  graphics.off()
}
