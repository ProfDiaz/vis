###########################################
#Introduccion a la Visualización en R
#Diana Diaz and Christine Leibbrand
#Mayo 4 2021
##################################################


#Plots con la libreria graphics
demo("graphics")


###############################
###Instalar y cargar los Paquetes para ggplot
###############################

#cual es la pagina de documentacion oficial de ggplot?

# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("MASS")
# install.packages("RColorBrewer")

library("tidyverse") #ggplot2 is housed in the tidyverse, so you can just download/load the tidyverse if you prefer
library("ggplot2")
library("MASS") #this is not normally needed, we'll just use MASS to pull in a dataset
library("RColorBrewer") #this is for using color palettes for your graphs if you would like

################################
###Creating a Basic Bar Chart###
################################

###Load in our dataset from MASS
data(housing)
str(housing)

#housing has 5 variables: we want to graph the type of building (Tower, Apartment, Atrium, and Terrace) and the frequency
levels(housing$Type)

###ggplot commands ALWAYS start with ggplot(). You first fill in the "aesthetics" with your data info
ggplot(data=housing, aes(x=Type, y=Freq))+ 
  geom_bar(stat="identity") 

###Now let's customize!

#Assign a color to each type of housing
ggplot(data=housing, aes(x=Type, y=Freq, fill=Type))+
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("skyblue","dodgerblue2","blue2", "navyblue")) #you can fill in colors manually, search online for "ggplot2" colors

#Customize the axes and add in horizontal lines
ggplot(data=housing, aes(x=Type, y=Freq, fill=Type))+
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("skyblue","dodgerblue2","blue2", "navyblue")) +
  scale_y_continuous(breaks=c(seq(100,900,by=100)),limits=c(0,900))+
  geom_hline(yintercept=c(seq(100,900,by=100)),color="grey") 

#Customize the text
windowsFonts(Times=windowsFont("Times"))

ggplot(data=housing, aes(x=Type, y=Freq, fill=Type))+
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("skyblue","dodgerblue2","blue2", "navyblue")) +
  scale_y_continuous(breaks=c(seq(100,800,by=100)),limits=c(0,800),name="Frequency") +
  geom_hline(yintercept=c(seq(100,800,by=100)),color="grey") +
  labs(title="The Frequency of Different Building Types")+
  xlab("Building Type") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none") +
  theme(title = element_text(size=14), axis.text.x = element_text(color="black", size=14), axis.text.y = element_text(color="black", size=14)) +
  theme(text=element_text(family="Times"))
  
#Make the background white and re-position horizontal lines
 ggplot(data=housing, aes(x=Type, y=Freq, fill=Type))+
    geom_hline(yintercept=c(seq(100,800,by=100)),color="grey") +
    geom_bar(stat="identity") +
    scale_fill_manual(values=c("skyblue","dodgerblue2","blue2", "navyblue")) +
    scale_y_continuous(breaks=c(seq(100,800,by=100)),limits=c(0,800),name="Frequency") +
    labs(title="The Frequency of Different Building Types")+
    xlab("Building Type") +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position="none") +
    theme(title = element_text(size=14), axis.text.x = element_text(color="black", size=14), axis.text.y = element_text(color="black", size=14)) +
    theme(text=element_text(family="Times")) +
    theme(panel.background = element_rect(fill = 'white', color = 'black'))

ggsave('housingplot.pdf')

###############################################
###Making a line chart from a custom dataset###
###############################################

#Let's say you did all your analysis somewhere else (like in Stata) and want to graph the predicted results from some regression analyses
#First, create a dataframe:

HealthInsurance <- data.frame(
  FamIncome <- c(rep(c(seq(20,100,by=20)),2)),
  Gender <- c(rep("Girls",5),rep("Boys",5)),
  NoHI <- c(
    0.15,0.10,0.08,0.04,0.02,
    0.16,0.10,0.07,0.03,0.01
  ),
  NoHI_high <- c(
    0.17,0.11,0.085,0.05,0.03,
    0.18,0.12,0.075,0.035,0.015
  ),
  NoHI_low <- c(
    0.13,0.09,0.075,0.03,0.01,
    0.14,0.08,0.065,0.025,0.005
  )
)

str(HealthInsurance)

#Basic Line Plot
ggplot(data=HealthInsurance, aes(x=FamIncome, y=NoHI))+
  geom_line()

#Now we want to graph BY Gender
ggplot(data=HealthInsurance, aes(x=FamIncome, y=NoHI, group=Gender))+
  geom_line()

#Now we'd like to tell those lines apart
ggplot(data=HealthInsurance, aes(x=FamIncome, y=NoHI, group=Gender, linetype=Gender, color=Gender))+
  geom_line()

#Are those differences significant? Add in error bars
ggplot(data=HealthInsurance, aes(x=FamIncome, y=NoHI, group=Gender, linetype=Gender, color=Gender))+
  geom_line()+
  geom_errorbar(aes(ymax=NoHI_high, ymin = NoHI_low), width=1,position=position_dodge(width=2))

#Let's make this more readable and customize!
ggplot(data=HealthInsurance, aes(x=FamIncome, y=NoHI, group=Gender, linetype=Gender, color=Gender))+
  geom_line(size=1)+
  geom_errorbar(aes(ymax=NoHI_high, ymin = NoHI_low), width=0.01,position=position_dodge(width=2))+
  scale_color_brewer(palette = "Dark2")+ #or you can fill this in with a pre-created color palette
  scale_y_continuous(breaks=c(seq(0,0.19,by=0.03)),limits=c(0,0.19),name="Predicted Probability of Not Having Health Insurance\n") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  xlab("Family Income (in thousands)")+
  labs(title="The Probability of Not Having Health Insurance by Family Income")+
  theme(plot.title = element_text(hjust=0.5, size=14, face='bold'))+
  theme(axis.text.x = element_text(color="black", size=12), axis.text.y = element_text(color="black", size=12)) +
  theme(axis.title.x = element_text(size=14), axis.title.y = element_text(size=14),legend.text=element_text(size=12),legend.title=element_text(size=14))+
  theme(text=element_text(family="Times"))

ggsave('healthinsuranceplot.pdf')

#################################
###Other Graph Types Available###
#################################

#Scatterplot
ggplot(data=HealthInsurance, aes(x=FamIncome, y=NoHI))+
  geom_point(aes(color=Gender))

#Boxplot
ggplot(housing, aes(x=factor(Type),y=Freq))+
  geom_boxplot()

#Also descriptive plots like histograms and density plots
ggplot(data=data, aes(x=xvar))+
  geom_histogram()
ggplot(data=data, aes(x=xvar))+
  geom_density()

#Can also use statistics to summarize your data--dot = mean, lines = SEs
ggplot(data=housing, aes(x=Type, y=Freq))+
  stat_summary()

