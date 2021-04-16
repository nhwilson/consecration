library(ggplot2)
library(gridExtra)
library(grid)
library(igraph)
library(GGally)
library(intergraph)
library(dplyr)
library(tidyr)
library(stringr)

# load data
cleaned <- read.csv("~/Dropbox/Docs/Historical_Sociology_Methods_Project_Damon/Socius_Consecration/data/tidy_awards2.csv", sep=";")

tidy_awards <- read.csv("~/Dropbox/Docs/Historical_Sociology_Methods_Project_Damon/Socius_Consecration/data/tidy_awards2.csv", sep=";", header=T)
asa_memb <- read.csv("~/Dropbox/Docs/Historical_Sociology_Methods_Project_Damon/Socius_Consecration/data/asa_memb.csv")

# create and write a csv with only the rows of multiple winners
multiples <- names(table(cleaned$name))[which(table(cleaned$name) > 2)]

cleaned_mult <- cleaned[which(cleaned$name %in% multiples),]

# make df to store three-year counts

df <- data.frame(cbind(1980:2017,1983:2020,1:38,1:38))
colnames(df) <- c("start","end","multiple","total")

# iterate over cleaned and count multiple winners for each three-year window

for(i in 1:nrow(df)){
  # subset df to year range
  temp <- cleaned[which(cleaned$year %in% df$start[i]:df$end[i]),]
  df[i,]$multiple <- length(which(table(temp$name) > 2))  
  df[i,]$total <- nrow(temp)
}

# calculate multiple awards by total awards given for each window
df$proportion <- df$multiple / df$total

# *******************************************************************************
# create main figure
# *******************************************************************************

output <- asa_memb[11:51,]

year_counts <- data.frame(1980:2020)
colnames(year_counts) <- "year"

to_merge <- data.frame(table(tidy_awards$year))
colnames(to_merge) <- c("year","person_awards")

output <- merge(output, to_merge, by="year", all=T)

output$awards <- 0
output$honorable <- 0
output$awardees <- 0

# compute both raw total awards (unique raw per year) and honorable mentions

length(unique(tidy_awards[which(tidy_awards$year==2020),]$raw))

for(i in 1:nrow(output)){
  output[i,]$awards <- length(unique(tidy_awards[which(tidy_awards$year==output[i,]$year),]$raw))
  output[i,]$awardees <- length(unique(tidy_awards[which(tidy_awards$year==output[i,]$year),]$name))
  output[i,]$honorable <- length(unique(tidy_awards[which(tidy_awards$year==output[i,]$year & !is.na(str_match(tidy_awards$raw,"Honorable Mention"))),]$raw))
}

# and merge again with df data, after renaming df$end to df$year
df$year <- df$end
output <- merge(output, df, by="year", all=T)

# tidy output in prep for graphing
out2 <- output %>%
  select(year, awards, honorable, no_sections, asa_memb, multiple) %>%
  gather(key = "variable", value = "value", -year)

# convert years to numeric from factor
out2$year <- as.numeric(as.character(out2$year))


# line graph with awards, number of sections, and asa membership all plotted
# add scaling factor for right-hand axis

coeff <- 100
out2[which(out2$variable=="asa_memb"),]$value <- out2[which(out2$variable=="asa_memb"),]$value / coeff

# create mapping for arbitrary line types in legend
# making the labels for values identical will coerce them to same legend
cols <- c("awa"="red","hon"="red","mul"="blue","nos"="black","mem"="black")
tps <- c("awa"="solid","hon"="dotdash","mul"="solid","nos"="dotted","mem"="longdash")

out2$variable <- factor(out2$variable, levels=c("awards","honorable","multiple","no_sections","asa_memb"))

ggplot(out2, aes(x=year, y=value)) +
  geom_line(data = subset(out2, variable == "awards"), aes(color="awa",linetype="awa")) +
  geom_line(data = subset(out2, variable == "honorable"), aes(color="hon",linetype="hon")) +
  geom_line(data = subset(out2, variable == "multiple"), aes(color="mul",linetype="mul")) +
  geom_line(data = subset(out2, variable == "no_sections"), aes(color="nos",linetype="nos")) +
  geom_line(data = subset(out2, variable == "asa_memb"), aes(color="mem",linetype="mem")) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position="bottom") +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Award, Section, Honorable Mentions, and Multiple Award-Winner Counts",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="ASA Membership")
  ) +
  scale_color_manual(name=" ",values=cols, labels = c("Total Awards","Honorable Mentions","ASA Membership","Multiple Winners","Number of Sections"))+ 
  scale_linetype_manual(name=" ",values=tps, labels = c("Total Awards","Honorable Mentions","ASA Membership","Multiple Winners","Number of Sections")) +
  labs(title = "Publication Awards in ASA Sections, Number of ASA Sections, Multiple Award Winners, and ASA Membership 1980-2020*", caption = "* Excludes graduate prizes and awards for which no publication was cited.")
  
  
# *******************************************************************************
# create supplementary figures **************************************************
# *******************************************************************************

# to make it pretty, stack plots
p1 <- ggplot(df[15:38,],aes(x=end,y=multiple))+
  geom_bar(stat='identity') +
  ylab("Count of Multiple Award Winners") +
  xlab("End of Four-Year Window")

p2 <- ggplot(df[15:38,],aes(x=end,y=proportion))+
  geom_bar(stat='identity') +
  ylab("Proption of Awards Given to Multiple Winners") +
  xlab("End of Four-Year Window")

grid.arrange(p1,p2,nrow=1, 
             top=textGrob('Four-Year Counts and Proportions of Awards by at Least Two ASA Sections to the Same Recipient, 1994-2020*'),
             bottom=textGrob('*Prior to 1994, there were no multiple ASA section award winners.'))
# calculate and plot individual frequencies of awards in total

indiv_counts <- data.frame(table(cleaned$name))
indiv_counts_tab <- data.frame(table(indiv_counts$Freq))

ggplot(indiv_counts_tab, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity") +
  xlab("Number of Awards") +
  ylab("Number of Recipients") +
  labs(title="ASA Section Award Recipient Counts, 1980-2020*", 
       caption="*Frequencies for 7 (N=1), 8 (N=2), and 9 (N=1) awards are hard to see at this scale.")




# create network object from adjacency matrix

el <- cbind(a=as.character(cleaned$section),b=as.character(cleaned$name))
el <- as.matrix(el)
g <- graph_from_edgelist(el)

# set network attributes for vertices for section and person
g <- set_vertex_attr(g, 'type', index = which(V(g)$name %in% unique(cleaned$section)), value = FALSE)
`%notin%` <- Negate(`%in%`)

g <- set_vertex_attr(g, 'type', index = which(V(g)$name %notin% unique(cleaned$section)), value = TRUE)

proj <- bipartite_projection(g, multiplicity=T)

g2 <- proj[[1]]
#drop isolates: tls and drugs and soc

isolated <- which(degree(g2)==0)

g3 <- delete_vertices(g2,isolated)

E(g3)$weight <- E(g3)$weight/7

ggnet2(g3, size =5,mode="kamadakawai", node.alpha= 0.2, node.color = 'black',edge.color='grey', edge.size='weight', label=V(g3)$name)


