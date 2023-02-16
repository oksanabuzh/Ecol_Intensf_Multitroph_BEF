# Code for the quantitative analysis: 
# summarizing and visualization of results

# Required packages

library(tidyverse)
library(ggplot2)
library(patchwork)

# Data----

data <- read.csv("Data/Data_Ecol_Intensf_Multitroph_BEF.csv", header = TRUE)
str(data)

# Filter papers that are relevant for our search criteria
dat <- data%>% 
  filter(Relevant=="Yes")

str(dat)
names(dat)

                                      
#Fig3----

##Fig3b----
# Summarize number and % of the studies for each continent

dat_Fig3b <- dat %>% 
  distinct(Paper.number, .keep_all = TRUE) %>% 
  count(Continent) %>% # 
  drop_na() %>% # one study is simulation modelling
  add_count(wt = sum(n), name = "sum") %>% 
  add_count(Continent, wt = round(n*100/sum, digits = 2), name = "perc") 

dat_Fig3b

# Figure 3b (barplot)
ggplot(dat_Fig3b, aes(y=n, x=Continent)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="grey")+
  coord_flip() +
  labs(y = "Number of studies", x =element_blank() ) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())

# Number of studies for each country per continent

dat %>% 
  distinct(Paper.number, .keep_all = TRUE) %>% 
  count(Continent, Country_broad) %>%
  drop_na # simulation model study (n=1)

# Number and % of the studies with the different biodiversity focus

dat %>% 
  distinct(Paper.number, .keep_all = TRUE) %>%  
  count(BEF_focus)%>%
  drop_na() %>% 
  add_count(wt=sum(n), name = "sum")%>%
  add_count( BEF_focus , wt = round(n*100/sum, digits=2), name = "perc") 

# Treatments

dat %>% 
  distinct(Paper.number, .keep_all = TRUE) %>%  
  count(Treatment_broad)%>%
  add_count(wt=sum(n), name = "sum")%>%
  add_count( Treatment_broad, wt = round(n*100/sum, digits=2), name = "perc") 

##Fig3c----
# Number and % of publications per ecosystem type

dat_Fig3c <- dat %>% 
  distinct(Paper.number, .keep_all = TRUE) %>% 
  mutate(Ecosystem_type_broad =fct_relevel(Ecosystem_type_broad ,
                                     c("forests, tree plantations",
                                       "tree plantations",
                                       "forests",
                                       "croplands, grasslands",
                                       "grasslands",
                                       "croplands"))) %>%
  count(Ecosystem_type_broad) %>%
  add_count( wt = sum(n), name = "sum") %>% 
  add_count( Ecosystem_type_broad, wt = round(n*100/sum, digits = 2), name = "perc")

dat_Fig3c

# Figure 3c
ggplot(dat_Fig3c, aes(y=n, x=Ecosystem_type_broad)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="grey")+
  coord_flip() +
  labs(y = "Number of studies", x =element_blank() ) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())




#Fig4----
## Biodiversity & Ecosystem Functions investigated

# Count number of investigated functions and biodiversity measures simultaneously

dat %>% 
  distinct(EF_specific, Taxon_spec, Div_level, Div_index_specific, 
           Paper.number, .keep_all = TRUE) %>%  
  count(EF_specific, Taxon_spec, Div_level, Div_index_specific)%>%
  summarise(sum(n))


# Count number of BEF tested, % of positive, negative, neutral

dat %>% 
  distinct(EF_specific, Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%
  count(Relationship_diversity) %>%
  drop_na()  %>%
  add_count( wt = sum(n), name = "sum_BEF") %>% 
  add_count( Relationship_diversity , wt = n*100/sum_BEF, name = "perc")


# Number of Ecosystem Functions (EFs) investigated (disregard of biodiversity)

dat %>% 
  distinct(EF_specific, Paper.number, .keep_all = TRUE) %>%  
  count(EF_specific) %>% 
  summarise(sum(n))

# Number of cases Multifunctionality was studied
dat %>% 
  distinct(EF_specific, Paper.number, .keep_all = TRUE) %>%
  filter(EF_specific=="Multifunctionality") %>% 
  count(EF_specific) %>% 
  summarise(sum(n))


# Number of biodiversity metrics investigated (disregard of EFs)

dat %>% 
  distinct(Taxon_spec, Div_level, Div_index_specific, Paper.number, 
           .keep_all = TRUE) %>%  
  count(Div_index_specific) %>% 
  summarise(sum(n))

##FigS2a----
# Investigated biodiversity across trophic levels

FigS2a <- dat %>% 
  distinct(Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%
  mutate(Trophic.group_broad =fct_relevel(Trophic.group_broad ,c("plants", 
                                                                 "1st_cons", 
                                                                 "2nd_cons", 
                                                                 "consumers", 
                                                                 "whole-food-web")))%>%
  count(Trophic.group_broad) %>% 
  add_count(wt = sum(n), name = "sum")%>% 
  mutate(perc=round(n*100/sum, digits=1))

FigS2a

FigS2a%>% 
  summarise(sum(n)) # n=152 biodiversity investigated 

col <- c("seagreen3","magenta", "mediumvioletred", "orange",  "skyblue" )

ggplot(FigS2a, aes(y=n, x=Trophic.group_broad, fill=Trophic.group_broad)) + 
  geom_bar(position="stack", stat="identity", colour = col, fill=col)+
  coord_flip() +
  labs(y = "Investigated biodiversity", x =element_blank() ) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())



##FigS2b----
# Number of biodiversity measures investigated per taxa groups studied
# Broad categories of taxa groups studied

dat_FigS2b <- dat %>% 
  distinct(Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>% 
  mutate(Taxon_broad=fct_recode(Taxon_broad, "incl. vertebrates" = "vertebrates, invertebrates")) %>%
  mutate(Taxon_broad=fct_recode(Taxon_broad, "incl. vertebrates" = "vertebrates, invertebrates, microorganisms")) %>%
  mutate(Taxon_broad=fct_recode(Taxon_broad,"across groups"  = "invertebrates, microorganisms")) %>%
  mutate(Taxon_broad=fct_recode(Taxon_broad,"across groups" =  "plants, invertebrates")) %>%
  mutate(Taxon_broad=fct_recode(Taxon_broad, "across groups" = "plants, invertebrates, microorganisms")) %>%
  mutate(Taxon_broad =fct_relevel(Taxon_broad ,c("incl. vertebrates",
                                   "across groups",
                                   "vertebrates",
                                   "invertebrates",
                                   "microorganisms",
                                   "plants"))) %>%
  count(Taxon_broad)%>%
  mutate(perc=n*100/152) # 152 is the number of biodiversity metrics tested (disregard of EFs), see above

dat_FigS2b

dat_FigS2b%>% 
  summarise(sum(n)) # n=152 biodiversity investigated 


ggplot(dat_FigS2b, aes(y=n, x=Taxon_broad)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="grey")+
  coord_flip() +
  labs(y = "Investigated biodiversity", x =element_blank() ) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())


##Fig3d----

# Biodiversity indices used across the study papers
dat_Fig3d <- dat %>% 
  distinct(Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>% 
  mutate(Div_level =fct_relevel(Div_level ,c("food web",
                                   "multidiversity",
                                   "phylogenetic",
                                   "functional",
                                   "taxa",
                                   "species"))) %>%
  count(Div_level)%>%
  add_count(wt=sum(n), name = "sum")%>%
  mutate(perc= round(n*100/sum, digits = 2))

dat_Fig3d

dat_Fig3d%>% 
  summarise(sum(n)) # n=152 biodiversity investigated 

ggplot(dat_Fig3d, aes(y=n, x=Div_level)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="grey")+
  coord_flip() +
  labs(y = "Investigated biodiversity", x =element_blank() ) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())


# Diversity index per system level measured

dat %>% 
  distinct(Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%  
  count(Div_level, Div_index)%>%
  add_count(Div_level, wt=sum(n), name = "sum")%>%
  mutate(perc= round(n*100/sum, digits = 2))


# Diversity index per taxa category

 dat %>% 
  distinct(Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%  
   mutate(Taxon_broad=fct_recode(Taxon_broad, "incl. vertebrates" = "vertebrates, invertebrates")) %>%
   mutate(Taxon_broad=fct_recode(Taxon_broad, "incl. vertebrates" = "vertebrates, invertebrates, microorganisms")) %>%
   mutate(Taxon_broad=fct_recode(Taxon_broad,"across groups"  = "invertebrates, microorganisms")) %>%
   mutate(Taxon_broad=fct_recode(Taxon_broad,"across groups" =  "plants, invertebrates")) %>%
   mutate(Taxon_broad=fct_recode(Taxon_broad, "across groups" = "plants, invertebrates, microorganisms")) %>%
   mutate(Taxon_broad =fct_relevel(Taxon_broad ,c("incl. vertebrates",
                                    "across groups",
                                    "vertebrates",
                                    "invertebrates",
                                    "microorganisms",
                                    "plants"))) %>%
  count(Div_level, Taxon_broad)%>%
   add_count(Div_level, wt=sum(n), name = "sum")%>%
   mutate(perc= round(n*100/sum, digits = 2))

 
# Investigated EFs across aboveground vs belowground compartments and strata

dat %>% 
  distinct(EF_specific, Paper.number, .keep_all = TRUE) %>%  
  count(AG_BG, Strata_EF)%>%
  add_count( AG_BG, wt = sum(n), name = "sum") %>% 
  add_count( Strata_EF , wt = round(n*100/sum, digits=2), name = "perc") %>% 
  mutate(AG_BG =fct_relevel(AG_BG ,c("AG, BG", "BG","AG"))) 


##Fig3e_1----

# services/disservices

dat_Fig3e_1 <- dat %>% 
  distinct(EF_specific, Paper.number, .keep_all = TRUE) %>% 
  mutate(Service.disservice =fct_relevel(Service.disservice, c("service",
                                                               "disservice"))) %>%
  count(Service.disservice) %>% 
  add_count(wt = sum(n), name = "sum")%>% 
  mutate(perc=round(n*100/sum, digits=1))

dat_Fig3e_1

col2 <- c("darkolivegreen3", "yellow")

ggplot(dat_Fig3e_1, aes(y=n, x=Service.disservice, fill=Service.disservice)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill=col2)+
  labs(y = "Investigated EFs", x =element_blank() ) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=17),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())


##Fig3e_2----

# Services/disservices for each ecosystem function

dat_Fig3e_2 <- dat %>% 
  distinct(EF_specific, Paper.number, .keep_all = TRUE) %>%
  count(EF_group_broad,  Service.disservice) %>% # count entries
  mutate(EF_group_broad =fct_relevel(EF_group_broad, c("Multifunctionality",
                                                       "Multitrophic energy dinamics",
                                                       "stability", "nutrient cycle",
                                                       "hyperparasitism",  "predation/parasitism",
                                                       "decomposition", "herbivory", "pollination", 
                                                       "yield")))%>%
  mutate(Service.disservice =fct_relevel(Service.disservice, c("service",
                                                               "disservice"))) %>%
  add_count(EF_group_broad, wt=sum(n), name="sum") %>%
  mutate(perc=round(n*100/sum, digits=1))

dat_Fig3e_2


col2 <- c("darkolivegreen3", "yellow")

ggplot(dat_Fig3e_2, aes(y=n, x=EF_group_broad, fill=Service.disservice)) + 
  geom_bar(position="stack", stat="identity", colour = "black")+
  scale_fill_manual(values=col2)+ 
  coord_flip() +
  labs(y = "Investigated EFs", x =element_blank() ) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank()) 

# Count number and % of cases when each EF was investigated

dat %>% 
  distinct(EF_specific, Paper.number, .keep_all = TRUE) %>%
  count(EF_group_broad, name="EF_name") %>% # count entries
  mutate(EF_group_broad =fct_relevel(EF_group_broad, c("Multifunctionality",
                                                       "Multitrophic energy dinamics",
                                                       "stability", "nutrient cycle",
                                                       "hyperparasitism",  "predation/parasitism",
                                                       "decomposition", "herbivory", "pollination", 
                                                       "yield")))%>%
  add_count( wt=sum(EF_name), name="sum") %>%
  mutate(perc_EF=round(EF_name*100/sum, digits=1))


##FigS2c----

# EF dimensions: rates vs stocks vs fluxes

dat_FigS2c <- dat %>% 
  distinct(EF_specific, Paper.number, .keep_all = TRUE) %>%  
  mutate(EF_dimens =fct_relevel(EF_dimens ,c("combined",
                                             "soil properties",
                                             "ratio/load",
                                             "rate",
                                             "flux",
                                             "stock"))) %>%
  count(EF_dimens)%>%
  add_count( wt = sum(n), name = "sum")%>%
  add_count( EF_dimens , wt = round(n*100/sum, digits=2), name = "perc") 

dat_FigS2c

dat_FigS2c %>% 
  summarise(sum(n))# n=117 EFs 

ggplot(dat_FigS2c, aes(y=n, x=EF_dimens)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="grey")+
  coord_flip() +
  labs(y = "Investigated EFs", x =element_blank() ) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())


##FigS2d----

# Investigated EFs across aboveground vs belowground compartments

dat_FigS2d <- dat %>% 
  distinct(EF_specific, Paper.number, .keep_all = TRUE) %>% 
  mutate(AG_BG =fct_relevel(AG_BG ,c("AG, BG", "BG","AG"))) %>% 
  count(AG_BG)
   
dat_FigS2d

dat_FigS2d %>% 
  summarise(sum(n)) # n=117 EFs 


ggplot(dat_FigS2d, aes(y=n, x=AG_BG)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="grey")+
  coord_flip() +
  labs(y = "Investigated EFs", x =element_blank() ) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())


#Fig4----

dat_Fig4 <- dat %>% 
  distinct(EF_specific, Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%
  mutate(Relationship_diversity=recode_factor(Relationship_diversity, .missing="No_BEF")) %>%
  mutate(Trophic.group_broad =fct_relevel(Trophic.group_broad ,c("plants", 
                                                                 "1st_cons", 
                                                                 "2nd_cons", 
                                                                 "consumers", 
                                                                 "whole-food-web")))%>%
  mutate(EF_group_broad =fct_relevel(EF_group_broad, c("Multifunctionality",
                                                       "Multitrophic energy dinamics",
                                                       "stability", "nutrient cycle",
                                                       "hyperparasitism",  "predation/parasitism",
                                                       "decomposition", "herbivory", "pollination", 
                                                       "yield"))) %>%
  count(EF_group_broad, Trophic.group_broad, Relationship_diversity) %>% # count entries
  drop_na() %>%
  pivot_wider(names_from = Relationship_diversity, values_from = n) %>% 
  mutate(negative = ifelse(is.na(negative), 0, negative),     
      neutral = ifelse(is.na(neutral), 0, neutral),
      positive = ifelse(is.na(positive ), 0, positive )) %>% 
  mutate(BEF = (negative + neutral + positive))      %>%                
  add_count(EF_group_broad, wt=sum(BEF), name="BEF_sum")%>%
  add_count(EF_group_broad, wt=sum(No_BEF, na.rm=TRUE), name="No_BEF_sum")%>%
  mutate(BEF_pr = BEF_sum*100/140) %>% # % of EF out of total BEF
  mutate(No.BEF_pr = No_BEF_sum*100/47)%>%
  mutate(perc_BEF_TL = BEF*100/BEF_sum) %>%  
  mutate(perc_No.BEF_TL = No_BEF*100/No_BEF_sum)
 
dat_Fig4



##Fig4_1----
# Bubble chart 
# Number of cases when BEF relationships were not tested: 
ggplot(dat_Fig4, aes(x = Trophic.group_broad, 
                     y = EF_group_broad,
                     colour = Trophic.group_broad,
                     size = No_BEF)) +
  geom_point() +
  #geom_text(aes(label = No_BEF),  colour = "black",  size = 4) +
  scale_x_discrete(position = "top") +
  scale_size_continuous(range = c(10, 22)) + 
  scale_color_manual(values = c("plants" = "seagreen3",
                                "1st_cons" ="magenta",
                                "2nd_cons" ="mediumvioletred",
                                "consumers" ="orange",
                                "whole-food-web"="skyblue"))+ 
 # scale_color_brewer(palette =  "Paired") +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey80"),
        axis.ticks = element_blank(),
        axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=15))


##Fig4_2----
# Number of cases when BEF relationships were tested
# for donut chart:
ggplot(dat_Fig4%>%
         mutate(BEF = (ifelse( BEF>0, BEF, NA))),
       aes(x = Trophic.group_broad, 
                     y = EF_group_broad,
                     size = BEF)) +
  geom_point(colour = "gray") +
  geom_text(aes(label = BEF), colour = "black", size = 4) +
  scale_x_discrete(position = "top") +
  scale_size_continuous(range = c(10, 30)) + 
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey80"),
        axis.ticks = element_blank(),
        axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=15))

# Donut charts for each individual function per each thophic level:
## Ecosystem function is "EF_group_broad"
## Trophic level is "Trophic.group_broad"

dat_Fig4_2 <- dat_Fig4%>%
  select(EF_group_broad, Trophic.group_broad, neutral, positive, negative) %>% 
  pivot_longer(!Trophic.group_broad & !EF_group_broad, names_to = "Relationship_diversity", values_to = "n")  

dat_Fig4_2

dat_Fig4_2 <- dat_Fig4_2 %>% 
  mutate(hsize = 1) # hole size for donut chart

colr <- c("firebrick1", "darkgray", "dodgerblue2") # colours for negative, neutral, and positive BEF relationships

# Donut chart
### example for BEF among "plants" diversity and "Multifunctionality"
ggplot(dat_Fig4_2 %>% 
         filter(EF_group_broad=="Multifunctionality" & Trophic.group_broad=="plants"), # filter each EF per TL
       aes(x = hsize, y = n, fill = Relationship_diversity)) +
  geom_col() +
  scale_fill_manual(values = colr) +
  coord_polar(theta = "y") +
  xlim(c(0.2, 1 + 0.5))+
  theme(axis.text=element_blank(), axis.title=element_blank(),axis.ticks =  element_blank(),
        legend.position = "none",
    panel.background = element_blank())

##Fig4_3----
# Count total BEF for each EF

dat_Fig4_3 <- dat %>% 
  mutate(Relationship_diversity=recode(Relationship_diversity, .missing = "No_BEF")) %>%
  distinct(EF_specific, Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%
  filter(!(Relationship_diversity == "No_BEF")) %>%
  mutate(EF_group_broad =fct_relevel(EF_group_broad, c("Multifunctionality",
                                                       "Multitrophic energy dinamics",
                                                       "stability", "nutrient cycle",
                                                       "hyperparasitism",  "predation/parasitism",
                                                       "decomposition", "herbivory", "pollination", 
                                                      "yield")))%>%
  count(EF_group_broad, Relationship_diversity) %>% 
  add_count(EF_group_broad, wt = n, name = "n_EF")%>% 
  mutate(perc= round(n*100/n_EF, digits=1)) %>%
  add_count(wt = sum(n), name = "Total")%>% 
  mutate(EF_perc= round(n_EF*100/Total, digits=1))
  

dat_Fig4_3

dat_Fig4_3 %>% 
  summarise(sum(n))  # number of BEF tested

# Stacked barplots for BEF relationships for each function

colr <- c("firebrick1", "darkgray", "dodgerblue2")

ggplot(dat_Fig4_3, aes(fill =Relationship_diversity, y=n, x=EF_group_broad)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=colr)+ 
  coord_flip() +
  labs(y = " ", x = element_blank()) +
  theme(
    # plot.margin = unit(c(3,3,3,3), "lines"),
    axis.text.y=element_text(colour = "black", size=15),
    axis.text.x=element_text(colour = "black", size=12),
    axis.title=element_text(size=15),
    legend.text=element_text(size=10),
    legend.title=element_text(size=12) ,
    legend.position = "none",
    panel.background = element_blank(),
    axis.ticks =  element_line(colour = "grey85"),
    axis.ticks.y = element_blank())+
  geom_text(aes(label=n_EF,y=n_EF),y=1.05, size=5)+ 
  scale_y_continuous(expand = c(0, 0.1))


##Fig4_4----
# Count total BEF for each trophic level

dat_Fig4_4 <- dat %>% 
  mutate(Relationship_diversity=recode_factor(Relationship_diversity, .missing = "No_BEF")) %>%
  distinct(EF_specific, Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%
  filter(!(Relationship_diversity == "No_BEF")) %>%
  mutate(Trophic.group_broad =fct_relevel(Trophic.group_broad ,c("plants", 
                                                                 "1st_cons", 
                                                                 "2nd_cons", 
                                                                 "consumers", 
                                                               "whole-food-web")))%>%
  count(Trophic.group_broad, Relationship_diversity) %>% 
  add_count(Trophic.group_broad, wt = n, name = "n_BEF")%>% 
  mutate(perc= round(n*100/n_BEF, digits=1)) %>%
  add_count(wt = n, name = "n_BEF_Total")%>% 
  mutate(perc.TL= round(n_BEF*100/n_BEF_Total, digits=1)) 
  

dat_Fig4_4

dat_Fig4_4 %>% 
  summarise(sum(n))

# Stacked barplots for BEF relationships for each trophic level

colr <- c("firebrick1", "darkgray", "dodgerblue2")

ggplot(dat_Fig4_4, aes(fill =Relationship_diversity, y=n, x=Trophic.group_broad)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=colr)+ 
  coord_flip() +
  labs(y = " ", x = element_blank()) +
  theme(axis.text.y=element_text(colour = "black", size=15),
    axis.text.x=element_text(colour = "black", size=12),
    axis.title=element_text(size=15),
    legend.text=element_text(size=10),
    legend.title=element_text(size=12) ,
    legend.position = "none",
    panel.background = element_blank(),
    axis.ticks =  element_line(colour = "grey85"),
    axis.ticks.y = element_blank())+
  geom_text(aes(label=n_BEF,y=n_BEF),y=1.05, size=5)+ 
  scale_y_continuous(expand = c(0, 0.1))



#Fig5----

##Fig5a----
# How often was biodiversity manipulation direct?

dat$Dir.indir_biodiversity
dat$Indirect_specific


dat_Fig5a <- dat %>% 
  distinct(EF_specific, Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%
  mutate(Indirect_specific =fct_relevel(Indirect_specific, c("via land use",
                                                         "via habitat",
                                                         "trophic cascades", 
                                                         "direct")))%>%
  count(Indirect_specific,Relationship_diversity) %>%
  drop_na()   %>%
  add_count (Indirect_specific, wt=sum(n), name="sum") %>%
  mutate (perc=round(n*100/sum, digits=1))



dat_Fig5a
   
dat_Fig5a %>% 
  summarise(sum(n))

ggplot(dat_Fig5a, aes(fill=Relationship_diversity, y=n, x=Indirect_specific)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=colr)+ 
  coord_flip() +
  labs(y = " ", x = element_blank()) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=13),
        # axis.text.x=element_text(size=9,colour = "black"),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        #panel.border = element_rect(fill = NA, colour = "grey30", size=0.5),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())+
  guides(fill=guide_legend(title="Effects on diversity", size=5)) +
  geom_text(aes(label=sum,y=n),y=1.05, size=5)+ 
  scale_y_continuous(expand = c(0, 0.1))

## Count overall direct vs indirect biodiversity manipulations and the respective BEF relationships
dat %>% 
  distinct(EF_specific, Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%
  mutate(Indirect_specific =fct_relevel(Indirect_specific, c("via land use",
                                                         "via habitat",
                                                         "trophic cascades", 
                                                         "direct")))%>%
  mutate(Indirect_specific=recode(Indirect_specific, "via land use" = "indirect")) %>%
  mutate(Indirect_specific=recode(Indirect_specific, "via habitat" = "indirect")) %>%
  mutate(Indirect_specific=recode(Indirect_specific, "trophic cascades" = "indirect")) %>%
  count(Indirect_specific,Relationship_diversity) %>% # count entries
  drop_na()   %>%
  add_count ( Indirect_specific, wt=sum(n), name="sum") %>%
  mutate (perc=round(n*100/sum, digits=1))

##Fig5b----
# BEF for each experiment type


dat_Fig5b <- dat %>% 
  distinct(EF_specific, Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%
  mutate(Experiment_type =fct_relevel(Experiment_type, c("simulation model",
                                                         "microcosm/mesocosm experiment",
                                                         "manipulative field experiment", 
                                                         "field observation")))%>%
  # filter(!(Experiment_type== "simulation model"))%>%
  count(Experiment_type,Relationship_diversity) %>% # 
  drop_na()   %>%
  add_count (Experiment_type, wt=sum(n), name="sum") %>%
  mutate (perc=round(n*100/sum, digits=1))

dat_Fig5b

dat_Fig6_b %>% 
  summarise(sum(n))

ggplot(dat_Fig5b, aes(fill=Relationship_diversity, y=n, x=Experiment_type)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=colr)+ 
  coord_flip() +
  labs(y = " ", x = element_blank()) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())+
  guides(fill=guide_legend(title="Effects on diversity", size=5))+
  geom_text(aes(label=sum,y=n),y=1.05, size=5)+ 
  scale_y_continuous(expand = c(0, 0.1))


## Count overall BEF relationships for experiments vs observations

dat %>% 
  distinct(EF_specific, Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%
  filter(!(Experiment_type== "simulation model"))%>%
  mutate(Experiment_type=recode(Experiment_type, "microcosm/mesocosm experiment" = "Experiments")) %>%
  mutate(Experiment_type=recode(Experiment_type, "manipulative field experiment" = "Experiments")) %>%
  mutate(Experiment_type=recode(Experiment_type, "field observation" = "Observetional")) %>%
  count(Experiment_type,Relationship_diversity) %>% # count entries
  drop_na()   %>%
  add_count(Experiment_type, wt=sum(n), name="sum") %>%
  mutate(perc=round(n*100/sum, digits=1))



##Fig5c----
# BEF for each dimension of ecosystem functioning (i.e., fluxes vs rates vs stocks)

dat_Fig5c <- dat %>% 
  distinct(EF_specific, Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%
  mutate(EF_dimens =fct_relevel(EF_dimens ,c("combined",
                                             "soil properties",
                                             "ratio/load",
                                             "rate",
                                             "flux",
                                             "stock"))) %>%
  count(EF_dimens, Relationship_diversity) %>% # count entries
  drop_na()   %>%
  add_count(EF_dimens, wt=sum(n), name="sum") %>%
  mutate(perc=round(n*100/sum, digits=1))



dat_Fig5c

dat_Fig5c %>% 
  summarise(sum(n))

colr <- c("firebrick1", "darkgray", "dodgerblue2")

ggplot(dat_Fig5c, aes(fill=Relationship_diversity, y=n, x=EF_dimens)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=colr)+ 
  coord_flip() +
  labs(y = " ", x = element_blank()) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())+
  guides(fill=guide_legend(title="Effects on diversity", size=5))+
  geom_text(aes(label=sum,y=n),y=1.05, size=5)+ 
  scale_y_continuous(expand = c(0, 0.1))



##Fig5d----
# BEF for services vs disservices

dat_Fig5d <- dat %>% 
  distinct(EF_specific, Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%
  mutate(Service.disservice =fct_relevel(Service.disservice, c("disservice","service"))) %>%
  count(Service.disservice,Relationship_diversity) %>% # count entries
  drop_na() %>%
  add_count(Service.disservice, wt=sum(n), name="sum") %>%
  mutate(perc=round(n*100/sum, digits=1))

dat_Fig5d

dat_Fig5d %>% 
  summarise(sum(n))

colr <- c("firebrick1", "darkgray", "dodgerblue2")

ggplot(dat_Fig5d, aes(fill=Relationship_diversity, y=n, x=Service.disservice)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=colr)+ 
  coord_flip() +
  labs(y = " ", x = element_blank()) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())+
  guides(fill=guide_legend(title="Effects on diversity", size=5))+
  geom_text(aes(label=sum,y=n),y=1.05, size=5)+ 
  scale_y_continuous(expand = c(0, 0.1))


# Fig.6----

# Effects of treatment (broad types) on biodiversity and ecosystem functions


##Fig6.1---- 
# Effects of treatment (broad types) on biodiversity
# The proportion of positive, neutral, and negative effects of land use intensity, disturbance, 
# habitat complexity and restoration as drivers of biodiversity (Fig.7 left panel). 

dat_Fig6.1 <- dat %>% 
  distinct(Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%
  mutate(Trtmnt=fct_recode(Treatment_broad, "Habitat complexity"="Habitat complexity (as covariate)" )) %>%
  count(Trtmnt, Relationship_management_diversity) %>% 
  drop_na()  %>%
  filter(!(Trtmnt == "Biodiversity effects (observational)"))%>%
  filter(!(Trtmnt == "Biodiversity effects"))%>%
  add_count(Trtmnt, wt = n, name = "n_Trtmnt")%>% 
  mutate(Trtmnt = fct_relevel(Trtmnt ,c("Disturbance",
                                        "Restoration",
                                        "Habitat complexity",
                                        "Land use intensity")))


dat_Fig6.1

dat_Fig6.1 %>% 
  summarise(sum(n))

# Stacked barplots, percent cases

colr <- c("firebrick1", "darkgray", "dodgerblue2")

ggplot(dat_Fig6.1, aes(fill =Relationship_management_diversity, y=n, x=Trtmnt)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=colr)+ 
  coord_flip() +
  labs(y = " ", x = element_blank()) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=12),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.ticks =  element_line(colour = "grey85"),
        axis.ticks.y = element_blank())+
  geom_text(aes(label=n_Trtmnt,y=n_Trtmnt),y=1.05, size=5)+ 
  scale_y_continuous(expand = c(0, 0.1))

##Fig6.2----

# barplots for each trophic level and management type

dat %>% 
  distinct(Trophic.group_broad)

dat_Fig6.2 <- dat %>% 
  mutate(Trtmnt=fct_recode(Treatment_broad, "Habitat complexity" = "Habitat complexity (as covariate)" )) %>%
  distinct(Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%
  count(Trtmnt, Trophic.group_broad) %>% # count entries
  add_count(Trtmnt, wt = n, name = "n_Trtmnt")%>% # add n counts for each treatment group
  filter(!(Trtmnt %in% c("Biodiversity effects (observational)" , "Biodiversity effects") )) %>%
  mutate(Trtmnt =fct_relevel(Trtmnt ,c("Disturbance",
                                       "Restoration",
                                       "Habitat complexity",
                                       "Land use intensity")))%>%
  mutate(Trophic.group_broad =fct_relevel(Trophic.group_broad ,c("plants", 
                                                                 "1st_cons", 
                                                                 "2nd_cons", 
                                                                 "consumers", 
                                                                 "whole-food-web")))

dat_Fig6.2

col <- c("seagreen3","magenta", "mediumvioletred", "orange",  "skyblue" )

# Land use intensity
p1 <- ggplot(dat_Fig6.2%>%
               filter(Trtmnt == "Land use intensity"),
             aes(x="", y=n, fill=Trophic.group_broad))  +
  scale_fill_manual(values=col)+
  # geom_bar(position="fill", stat="identity")+
  geom_bar(stat="identity", linewidth=1, color="black") +
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text.x=element_blank() ,
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        legend.position="none") 

# Disturbance
p2 <- ggplot(dat_Fig6.2%>%
               filter(Trtmnt == "Disturbance"),
             aes(x="", y=n, fill=Trophic.group_broad))  +
  scale_fill_manual(values=c("mediumvioletred", "orange",  "skyblue"))+
  # geom_bar(position="fill", stat="identity")+
  geom_bar(stat="identity", linewidth=1, color="black") +
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text.x=element_blank() ,
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        legend.position="none") 


# Habitat complexity
p3 <- ggplot(dat_Fig6.2%>%
               filter(Trtmnt == "Habitat complexity"),
             aes(x="", y=n, fill=Trophic.group_broad))  +
  scale_fill_manual(values=c("seagreen3","magenta", "mediumvioletred", "orange",  "skyblue"))+
  # geom_bar(position="fill", stat="identity")+
  geom_bar(stat="identity", linewidth=1, color="black") +
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text.x=element_blank() ,
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        legend.position="none") 

# Restoration
p4 <- ggplot(dat_Fig6.2%>%
               filter(Trtmnt == "Restoration"),
             aes(x="", y=n, fill=Trophic.group_broad))  +
  scale_fill_manual(values=c("seagreen3", "orange",  "skyblue"))+
  # geom_bar(position="fill", stat="identity")+
  geom_bar(stat="identity", linewidth=1, color="black") +
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text.x=element_blank() ,
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        legend.position="none") 

p1+p2+p3+p4+plot_layout(ncol=1)



##Fig6.3----
# Effects of treatment (broad types) on EFs
# The proportion of positive, neutral, and negative effects of land use intensity, disturbance, 
# habitat complexity and restoration as drivers of ecosystem functions (Fig.7 right panel). 

dat_Fig6.3 <- dat %>% 
  distinct(Focus_broad, EF_specific, Paper.number, .keep_all = TRUE) %>% # create a row for each unique paper and variable (e.g. treatment)
  mutate(Trtmnt=fct_recode(Treatment_broad, "Habitat complexity"="Habitat complexity (as covariate)")) %>%
  count(Trtmnt, Relationship_management_function) %>% 
  drop_na()  %>%
  add_count(Trtmnt, wt = n, name = "n_Trtmnt")%>% 
  filter(!(Trtmnt == "Biodiversity effects (observational)")) %>%
  mutate(Trtmnt =fct_relevel(Trtmnt ,c("Disturbance",
                                       "Restoration",
                                       "Habitat complexity",
                                       "Land use intensity")))
dat_Fig6.3

dat_Fig6.3 %>% 
  summarise(sum(n))


colr <- c("firebrick1", "darkgray", "dodgerblue2")

ggplot(dat_Fig6.3, aes(fill=Relationship_management_function, y=n, x=Trtmnt)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=colr)+ 
  coord_flip() +
  labs(y = " ", x = element_blank()) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=12),
        axis.title=element_text(size=12),
        legend.position = "none",
        panel.background = element_blank(),
        axis.ticks =  element_line(colour = "grey85"),
        axis.ticks.y = element_blank())  +
  geom_text(aes(label=n_Trtmnt,y=n_Trtmnt),y=1.05, size=5)+ 
  scale_y_continuous(expand = c(0, 0.1))


##Fig6.4---- 
# pie charts: services/disservices

dat_Fig6.4 <- dat %>% 
  distinct(Focus_broad, EF_specific, Paper.number, .keep_all = TRUE) %>% 
  mutate(Trtmnt=fct_recode(Treatment_broad,  "Habitat complexity"="Habitat complexity (as covariate)" )) %>%
  count(Trtmnt, Service.disservice) %>% 
  drop_na()  %>%
  add_count(Trtmnt, wt = n, name = "n_Trtmnt")%>% 
  filter(!(Trtmnt %in% c("Biodiversity effects (observational)" , "Biodiversity effects") )) %>%
  mutate(Trtmnt =fct_relevel(Trtmnt ,c("Disturbance",
                                       "Restoration",
                                       "Habitat complexity",
                                       "Land use intensity")))
dat_Fig6.4


# Land use intensity
p5 <- ggplot(dat_Fig6.4%>%
               filter(Trtmnt == "Land use intensity"),
             aes(x="", y=n, fill=Service.disservice))  +
  scale_fill_manual(values=c("yellow","darkolivegreen3"))+
  geom_bar(stat="identity", linewidth=1, color="black") +
  coord_polar("y", start=0) +
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text.x=element_blank() ,
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        legend.position="none") 

# Disturbance
p6<-ggplot(dat_Fig6.4%>%
             filter(Trtmnt == "Disturbance"),
           aes(x="", y=n, fill=Service.disservice))  +
  scale_fill_manual(values=c("darkolivegreen3"))+
  geom_bar(stat="identity", linewidth=1, color="black") +
  coord_polar("y", start=0) +
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text.x=element_blank() ,
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        legend.position="none")

# Habitat complexity
p7<-ggplot(dat_Fig6.4%>%
             filter(Trtmnt == "Habitat complexity"),
           aes(x="", y=n, fill=Service.disservice))  +
  scale_fill_manual(values=c("yellow","darkolivegreen3"))+
  geom_bar(stat="identity", linewidth=1, color="black") +
  coord_polar("y", start=0) +
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text.x=element_blank() ,
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        legend.position="none")

# Restoration
p8<-ggplot(dat_Fig6.4%>%
             filter(Trtmnt == "Restoration"),
           aes(x="", y=n, fill=Service.disservice))  +
  scale_fill_manual(values=c("darkolivegreen3"))+
  geom_bar(stat="identity", linewidth=1, color="black") +
  coord_polar("y", start=0) +
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text.x=element_blank() ,
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        legend.position="none")


p5+p6+p7+p8+plot_layout(ncol = 1)


#Fig7----
# Specific treatment effects on biodiversity and ecosystem functions

## Fig7.1----
# Specific treatment effects on biodiversity

dat_Fig7.1 <- dat %>% 
  mutate(Trtmnt=fct_recode(Treatment_broad, "Habitat complexity"="Habitat complexity (as covariate)")) %>%
  unite("z", Taxon_spec:Div_index, Focus_broad, remove = FALSE)%>% 
  distinct(z, Paper.number, .keep_all = TRUE) %>% 
  count(Trtmnt, Focus_broad, Relationship_management_diversity) %>% 
  drop_na()  %>%
  filter(!(Trtmnt == "Biodiversity effects (observational)"))%>%
  filter(!(Trtmnt == "Biodiversity effects"))%>%
  add_count(Focus_broad, wt = n, name = "n_Trtmnt")%>% # 
  mutate(Focus_broad =fct_relevel(Focus_broad, c("Restoration",
                                                 "Disturbance",
                                                 "Crop/non-crop lanscape", "Habitat connectivity",  "Landscape heterogeneity",  
                                                 "Management intensity", "Organic/non-organic" ,
                                                 "Pesticides", "Fertilization",
                                                 "Woody encroachment ", "Grazing abandonment ", "Grazing", "Mowing",
                                                 "Pollinator hives absence", "Cover crop effects" , "Natural/croplands")))
dat_Fig7.1

# Stacked barplots, percent of cases

colr <- c("firebrick1", "darkgray", "dodgerblue2")

# Land use intensity
p7.1.1 <- ggplot(dat_Fig7.1%>%
                   filter(Trtmnt == "Land use intensity"),
                 aes(fill=Relationship_management_diversity, y=n, x=Focus_broad)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=colr)+ 
  coord_flip() +
  labs(y = " ", x = element_blank()) +
  theme(axis.text.y=element_text(colour = "black", size=17),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        legend.text=element_text(size=14),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.ticks =  element_line(colour = "grey85"),
        axis.ticks.y = element_blank())+
  geom_text(aes(label=n_Trtmnt,y=n_Trtmnt), y=1.05, size=6)+ 
  scale_y_continuous(expand = c(0, 0.1))


# "Habitat complexity"
p7.1.2 <- ggplot(dat_Fig7.1%>%
                   filter(Trtmnt ==  "Habitat complexity"),
                 aes(fill=Relationship_management_diversity, y=n, x=Focus_broad)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=colr)+ 
  coord_flip() +
  labs(y = " ", x = element_blank()) +
  theme(axis.text.y=element_text(colour = "black", size=17),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        legend.text=element_text(size=14),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.ticks =  element_line(colour = "grey85"),
        axis.ticks.y = element_blank())+
  geom_text(aes(label=n_Trtmnt,y=n_Trtmnt), y=1.05, size=6)+ 
  scale_y_continuous(expand = c(0, 0.1))

p7.1.1 + p7.1.2 + plot_layout(ncol=1)


## Fig7.2----
# Specific treatment effects on EFs

dat %>% 
  distinct(Focus_broad)

dat_Fig7.2 <- dat %>% 
  mutate(Trtmnt=fct_recode(Treatment_broad, "Habitat complexity"  = "Habitat complexity (as covariate)")) %>%
  distinct(Trtmnt, Focus_broad, EF_specific, Paper.number, .keep_all = TRUE) %>% 
  count(Trtmnt,Focus_broad, Relationship_management_function) %>% 
  drop_na()  %>%
  add_count(Focus_broad , wt = n, name = "n_Trtmnt")%>% 
  filter(!(Trtmnt == "Biodiversity effects (observational)")) %>%
  mutate(Focus_broad =fct_relevel(Focus_broad, c("Restoration",
                                                 "Disturbance",
                                                 "Crop/non-crop lanscape", "Habitat connectivity",  "Landscape heterogeneity",  
                                                 "Management intensity", "Organic/non-organic" ,
                                                 "Pesticides", "Fertilization",
                                                 "Woody encroachment ", "Grazing abandonment ", "Grazing", "Mowing",
                                                 "Pollinator hives absence", "Cover crop effects" , "Natural/croplands"
  )))
dat_Fig7.2


# Stacked barplots, percent of cases

colr <- c("firebrick1", "darkgray", "dodgerblue2")

# Land use intensity
p7.2.1 <- ggplot(dat_Fig7.2%>%
                   filter(Trtmnt == "Land use intensity"),
                 aes(fill=Relationship_management_function, y=n, x=Focus_broad)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=colr)+ 
  coord_flip() +
  labs(y = " ", x = element_blank()) +
  theme(
    # plot.margin = unit(c(3,3,3,3), "lines"),
    axis.text.y=element_text(colour = "black", size=17),
    axis.text.x=element_text(colour = "black", size=13),
    # axis.text.x=element_text(size=9,colour = "black"),
    axis.title=element_text(size=15),
    legend.text=element_text(size=14),
    legend.title=element_text(size=12) ,
    legend.position = "none",
    panel.background = element_blank(),
    axis.ticks =  element_line(colour = "grey85"),
    axis.ticks.y = element_blank())+
  # guides(fill=guide_legend(title="Effects on diversity", size=5))+
  geom_text(aes(label=n_Trtmnt,y=n_Trtmnt),y=1.05, size=6)+ 
  scale_y_continuous(expand = c(0, 0.1))


# "Habitat complexity"
p7.2.2 <-ggplot(dat_Fig7.2%>%
                  filter(Trtmnt == "Habitat complexity"),
                aes(fill=Relationship_management_function, y=n, x=Focus_broad)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=colr)+ 
  coord_flip() +
  labs(y = " ", x = element_blank()) +
  theme(
    # plot.margin = unit(c(3,3,3,3), "lines"),
    axis.text.y=element_text(colour = "black", size=17),
    axis.text.x=element_text(colour = "black", size=13),
    # axis.text.x=element_text(size=9,colour = "black"),
    axis.title=element_text(size=15),
    legend.text=element_text(size=14),
    legend.title=element_text(size=12) ,
    legend.position = "none",
    panel.background = element_blank(),
    axis.ticks =  element_line(colour = "grey85"),
    axis.ticks.y = element_blank())+
  # guides(fill=guide_legend(title="Effects on diversity", size=5))+
  geom_text(aes(label=n_Trtmnt,y=n_Trtmnt),y=1.05, size=6)+ 
  scale_y_continuous(expand = c(0, 0.1))


p7.2.1+p7.2.2+plot_layout(ncol=1)



  # Supplementary Materials -----

##Table S2 -----
# Information on the groupings of the drivers of biodiversity and ecosystem functions.
# Focus_broad	Focus_specific	Focus_details

dat_S2 <- dat %>% 
  mutate(Trtmnt=recode_factor(Treatment_broad, "Habitat complexity (as covariate)" = "Habitat complexity")) %>%
  filter(!(Trtmnt == "Biodiversity effects (observational)"))%>%
  filter(!(Trtmnt == "Biodiversity effects"))%>%
  mutate(Trtmnt =fct_relevel(Trtmnt ,c("Land use intensity",
                                       "Disturbance",
                                       "Habitat complexity",
                                       "Restoration")))%>%
  mutate(Focus_broad =fct_relevel(Focus_broad, c( 
    "Natural/croplands", "Cover crop effects",  "Pollinator hives absence",
    "Mowing","Grazing", "Grazing abandonment ","Woody encroachment ", 
    "Fertilization","Pesticides", 
    "Organic/non-organic" , "Management intensity",
    "Disturbance",
    "Landscape heterogeneity","Habitat connectivity","Crop/non-crop lanscape",
    "Restoration")))%>%
  count(Trtmnt,Focus_broad, Focus_specific, Focus_details) %>%
  drop_na()


dat_S2

write.csv(dat_S2, file = "S2.csv")

## Table S3 ----
# Information on the categories of agroecosystem types in which the studies were carried out.
# "Ecosystem_type_specific", "Ecosystem_type_broad"

dat_S3 <- data %>% 
  mutate(Ecosystem_type_broad =fct_relevel(Ecosystem_type_broad ,
                                             c("croplands",
                                               "grasslands",
                                               "croplands, grasslands",
                                               "forests",
                                               "tree plantations",
                                               "forests, tree plantations"))) %>%
  count(Ecosystem_type_broad, Ecosystem_type_specific)

dat_S3

write.csv(dat_S3, file = "S3.csv")

## Table S5----
# Information on trophic groups, trophic levels, and the categories of taxa for which 
## biodiversity was investigated across the study papers. 
# "Trophic.group_broad", "Trophic.group", "Trophic.level_biodiversity"
# "Taxon_broad",	"Taxon_spec"

dat_S5 <- data %>% 
  mutate(Trophic.group_broad =fct_relevel(Trophic.group_broad ,c("plants", 
                                                                 "1st_cons", 
                                                                 "2nd_cons", 
                                                                 "consumers", 
                                                                 "whole-food-web")))%>%
  count(Trophic.group_broad,Trophic.level_biodiversity, Trophic.group, Taxon_broad, Taxon_spec) %>% # count entries
  drop_na()


dat_S5

write.csv(dat_S5, file = "S5.csv")


##Table S6 ----
# Information on ecosystem functions that were investigated across the study papers.
# "EF_group_broad", "EF_specific", "EF_dimens", "Trophic.level_function", "Service.disservice"

dat_S6 <- dat %>% 
  mutate(EF_group_broad =fct_relevel(EF_group_broad, c("yield","pollination","herbivory","decomposition",
                                                       "predation/parasitism",
                                                       "hyperparasitism", 
                                                       "nutrient cycle", "stability",
                                                       "Multitrophic energy dinamics",
                                                       "Multifunctionality")))%>%
  count(EF_group_broad, EF_specific, Service.disservice, EF_dimens, Trophic.level_function) %>% # count entries
  drop_na()


dat_S6

write.csv(dat_S6, file = "S6.csv")



# Context dependency of BEF----
## Table S8  ----
# "BEF_context.depend_broad", "BEF_context.depend_spec"

dat_figS4 <- dat %>% 
  distinct(EF_specific, Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>%
  mutate(Relationship_diversity=recode(Relationship_diversity, .missing = "No_BEF")) %>%
  filter(!(Relationship_diversity =="No_BEF"))%>%
  filter(!(BEF_context.depend_broad =="No"))%>%
  mutate(BEF_context.depend_broad =fct_relevel(BEF_context.depend_broad,
                                               c("methodological",
                                                 "climate change factors",
                                                 "land use factors",
                                                 "abiotic properties of study system",
                                                 "biotic properties of study system")))%>%
  count(BEF_context.depend_broad, BEF_context.depend_spec) %>% # count entries
  drop_na()%>%
  add_count(wt=sum(n), name="BEF_sum")%>%
  add_count(BEF_context.depend_broad, wt=sum(n), name="sum")%>%
  mutate(perc=(round(sum*100/BEF_sum, digits=1)))

dat_figS4


# How many out of 140 BEF studies reported context dependency?
dat_figS4 %>% 
  summarise(sum(n))

write.csv(dat_figS4, "S8.csv")

## FigS4----
ggplot(dat_figS4%>%
         filter(!(BEF_context.depend_broad=="No"))%>%
         count(BEF_context.depend_broad), 
       aes(y=n, x=BEF_context.depend_broad)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="grey")+
  coord_flip() +
  labs(y = "BEF relationships", x =element_blank() ) +
  scale_x_discrete(labels= c("Methodological contexts",
                             "Climate-change factors",
                             "Land-use factors",
                             "Abiotic properties",
                             "Biotic properties"))+
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())

##FigS1----
# Number and % of publications per ecosystem type

dat_FigS1 <- dat %>% 
  distinct(Paper.number, .keep_all = TRUE) %>% 
  mutate(Experiment_type =fct_relevel(Experiment_type ,c( "simulation model",
                                                "microcosm/mesocosm experiment",
                                                "manipulative field experiment",
                                                "field observation"))) %>%
  count(Experiment_type) %>% 
  add_count( wt = sum(n), name = "sum") %>% 
  add_count( Experiment_type, wt = round(n*100/sum, digits = 2), name = "perc") 

dat_FigS1

# Figure S1
ggplot(dat_FigS1, aes(y=n, x=Experiment_type)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="grey")+
  coord_flip() +
  labs(y = "Number of studies", x =element_blank() ) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())



# Multifunctionality----

# Count the number of broad taxa groups used for the Multifunctionality studies
#
dat %>% 
  distinct(EF_specific, Taxon_spec, Div_level, Div_index_specific, Paper.number, .keep_all = TRUE) %>% 
  mutate(Taxon_broad=fct_recode(Taxon_broad, "incl. vertebrates" = "vertebrates, invertebrates")) %>%
  mutate(Taxon_broad=fct_recode(Taxon_broad, "incl. vertebrates" = "vertebrates, invertebrates, microorganisms")) %>%
  mutate(Taxon_broad=fct_recode(Taxon_broad,"across groups"  = "invertebrates, microorganisms")) %>%
  mutate(Taxon_broad=fct_recode(Taxon_broad,"across groups" =  "plants, invertebrates")) %>%
  mutate(Taxon_broad=fct_recode(Taxon_broad, "across groups" = "plants, invertebrates, microorganisms")) %>%
  mutate(Taxon_broad =fct_relevel(Taxon_broad ,c("incl. vertebrates",
                                   "across groups",
                                   "vertebrates",
                                   "invertebrates",
                                   "microorganisms",
                                   "plants"))) %>%
  filter(EF_group_broad == "Multifunctionality")%>%
  count(Taxon_broad)%>%
  add_count(wt=sum(n), name="sum")%>%
  mutate(perc=round(n*100/sum, digits=1))


##FigS3a----

dat_FigS3a <- dat %>% 
  distinct(EF_specific, EF_details, Paper.number, .keep_all = TRUE) %>%  mutate(Taxon_broad=recode(Taxon_broad, "vertibrates, invertebrates" = "incl. vertibrates")) %>%
  filter(EF_group_broad == "Multifunctionality")%>%
  mutate(EF_dimens_specific=recode_factor(EF_dimens_specific, "diversity, stocks, rates, fluxes, soil properties"="biodiversity"))%>%
  mutate(EF_dimens_specific=recode_factor(EF_dimens_specific, "diversity, stocks, rates, soil properties"="biodiversity"))%>%
  mutate(EF_dimens_specific=recode_factor(EF_dimens_specific, "diversity, stocks, soil properties"="biodiversity"))%>%
  mutate(EF_dimens_specific=recode_factor(EF_dimens_specific, "stocks, rates"="combined"))%>%
  mutate(EF_dimens_specific=recode_factor(EF_dimens_specific, "stocks, fluxes, soil properties"="combined"))%>%
  mutate(EF_dimens_specific=recode_factor(EF_dimens_specific, "stocks, rates, fluxes"="combined"))%>%
  mutate(EF_dimens_specific=recode_factor(EF_dimens_specific, "stocks, rates, soil properties"="combined"))%>%
  mutate(EF_dimens_specific=recode_factor(EF_dimens_specific, "stocks, soil properties"="combined"))%>%
  mutate(EF_dimens_specific =fct_relevel(EF_dimens_specific ,c("combined", "biodiversity", "soil properties", "flux"))) %>%
  count(EF_dimens_specific)%>%
  add_count(wt=sum(n), name="sum")%>%
  mutate(perc=round(n*100/sum, digits=2))

dat_FigS3a


ggplot(dat_FigS3a, aes(y=n, x=EF_dimens_specific)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="grey")+
  coord_flip() +
  labs(y = "Investigated Multifunctionality", x =element_blank() ) +
  scale_x_discrete(labels= c("combined
                             across dimensions",
                             "includes 
                             biodiversity",
                             "soil properties",
                             "fluxes"))+
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=13),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())


##FigS3b----
# Methods used for the multifunctionality assessments 

dat_FigS3b <- dat %>% 
  distinct(EF_specific, EF_details, Paper.number, .keep_all = TRUE) %>%  mutate(Taxon_broad=recode(Taxon_broad, "vertibrates, invertebrates" = "incl. vertibrates")) %>%
  filter(EF_group_broad == "Multifunctionality")%>%
  filter(! (EF_details == "mean log-response ratio"))%>%
  mutate(EF_details=recode_factor(EF_details, 
                                          "averaging, multiple thresholds"="combined"))%>%
  mutate(EF_details=recode_factor(EF_details, 
                                          "averaging, single threshold, multiple thresholds"="combined"))%>%
  mutate(EF_details =fct_relevel(EF_details ,c( "combined",
                                                                "multiple thresholds",
                                                                "single threshold",
                                                                "averaging"))) %>%
  count(EF_details)%>%
  add_count(wt=sum(n), name="sum")%>%
  mutate(perc=round(n*100/sum, digits=2))

dat_FigS3b

ggplot(dat_FigS3b, aes(y=n, x=EF_details)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="grey")+
  coord_flip() +
  labs(y = "Investigated Multifunctionality", x =element_blank() ) +
  scale_x_discrete(labels= c("combined 
                             across methods",
                             "multiple thresholds",
                             "single threshold",
                             "averaging"))+
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=13),
        # axis.text.x=element_text(size=9,colour = "black"),
        axis.title=element_text(size=15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12) ,
        legend.position = "none",
        panel.background = element_blank(),
        #panel.border = element_rect(fill = NA, colour = "grey30", size=0.5),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank())


# Biodiversity as part of multifunctionality measure

data %>% 
  distinct(EF_specific, EF_details, Paper.number, .keep_all = TRUE) %>%  
  filter(EF_group_broad == "Multifunctionality")%>%
  mutate(EF_dimens_specific=fct_recode(EF_dimens_specific, "biodiversity"="diversity, stocks, rates, fluxes, soil properties"))%>%
  mutate(EF_dimens_specific=fct_recode(EF_dimens_specific, "biodiversity"="diversity, stocks, rates, soil properties"))%>%
  mutate(EF_dimens_specific=fct_recode(EF_dimens_specific, "biodiversity"="diversity, stocks, soil properties"))%>%
  count(EF_dimens_specific)%>%
  add_count(wt=sum(n), name="sum")%>%
  mutate(perc=round(n*100/sum, digits=2))



##FigS3c----
# Effects of treatment (broad types) on Multifunctionality
# The proportion of positive, neutral, and negative effects of land use intensity, disturbance, 
# habitat complexity and restoration as drivers of ecosystem functions (Fig.7 right panel). 

dat_FigS3c <- dat %>% 
  distinct(Focus_broad, EF_specific, Paper.number, .keep_all = TRUE) %>% # create a row for each unique paper and variable (e.g. treatment)
  filter(EF_group_broad=="Multifunctionality") %>% 
  mutate(Trtmnt=fct_recode(Treatment_broad, "Habitat complexity"="Habitat complexity (as covariate)")) %>%
  count(Trtmnt, Relationship_management_function) %>% 
  drop_na()  %>%
  add_count(Trtmnt, wt = n, name = "n_Trtmnt")%>% 
  filter(!(Trtmnt == "Biodiversity effects (observational)")) %>%
  mutate(Trtmnt =fct_relevel(Trtmnt ,c(
                                       "Restoration",
                                       "Habitat complexity",
                                       "Land use intensity")))
dat_FigS3c

dat_FigS3c %>% 
  summarise(sum(n))


colr <- c("firebrick1", "darkgray", "dodgerblue2")

ggplot(dat_FigS3c, aes(fill=Relationship_management_function, y=n, x=Trtmnt)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=colr)+ 
  coord_flip() +
  labs(y = " ", x = element_blank()) +
  theme(axis.text.y=element_text(colour = "black", size=15),
        axis.text.x=element_text(colour = "black", size=12),
        axis.title=element_text(size=12),
        legend.position = "none",
        panel.background = element_blank(),
        axis.ticks =  element_line(colour = "grey85"),
        axis.ticks.y = element_blank())  +
  geom_text(aes(label=n_Trtmnt,y=n_Trtmnt),y=1.05, size=5)+ 
  scale_y_continuous(expand = c(0, 0.1))


#End---------------

