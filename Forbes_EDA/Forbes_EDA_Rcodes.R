pacman::p_load(
  #data wrangling
  tidyverse, stringr, readxl,
  #data visualization
  ggplot2, RColorBrewer, ggsci,
  plotly, ggpubr, vtable
)
#####
forbes <- read_csv("2022_forbes_billionaires.csv") %>% janitor::clean_names()
forbes
#Check the content of the dataset
glimpse(forbes)
#Data Cleaning----
#Rename the columns
forbes <- tibble("Rank"=forbes$rank,
                 "Names"=forbes$name,
                 "Networth"=forbes$networth,
                 "Age"=forbes$age,
                 "Country"=forbes$country,
                 "Source"=forbes$source,
                 "Industry"=forbes$industry)
glimpse(forbes)
# check the out for duplicate values
duplicate <- unique(forbes)
duplicate
Null_values <- which(is.na(duplicate))
#convert Networth to numeric
forbes$Networth <- as.numeric(str_replace_all(forbes$Networth, "[$B]", ""))
#check the data types of the dataset
summary(forbes)
#Exploratory data analysis----
statistics <- forbes[,c("Age","Networth")] %>% st(out="csv")%>%
  as.data.frame()
statistics
#Barchart in plotly
#Top 10 billionaires and their networth----
Top <- forbes %>% head(10)%>% ggplot(aes(reorder(x=Names,-Networth),y=Networth,fill=Names)) + 
  geom_bar(stat="identity")+
  geom_text(aes(label=Networth),cex=2.5,vjust=0.05)+
  labs(title="Top 10 billionaires and their networth",
       x="Names",y="Networth")+ 
  theme_classic()+scale_fill_brewer(palette="Spectral")+
  theme(axis.text.x=element_text(angle=45,vjust=0.5))
Top
# Countries of the top 10 billionaires----
Countries<- forbes %>% head(10)%>%
  plot_ly(x=~Names, y=~Networth, type="scatter", 
  mode="markers",marker= list(size=~Networth),color=~Country)%>%
  layout(title="Countries of top 10 billionaires")

#  Top 10 industries of the billionaires-----
Industries<- forbes %>% head(10)%>%
  plot_ly(x=~Names, y=~Networth, type="scatter", 
          mode="markers",marker= list(size=~Networth),color=~Industry)%>%
  layout(title="Industry of top 10 billionaires")
  write_html(Industries, "myplot.html")
#Sources of top 10 billionaires----
Source<- forbes %>% head(10)%>%
  plot_ly(x=~Names, y=~Networth, type="scatter", 
          mode="markers",marker= list(size=~Networth),color=~Source)%>%
  layout(title="Sources of top 10 billionaires")
#Top 10 billionaires and their age----
bar <- forbes %>% head(10)%>% ggplot(aes(reorder(x=Names, -Age),y=Age,fill=Names)) + 
  geom_bar(stat="identity")+
  geom_text(aes(label=Age),cex=2.5,vjust=0.05)+
  labs(title="Top 10 billionaires and their age",
       x="Names",y="Networth")+ 
  theme_classic()+scale_fill_brewer(palette="BrBG")+
  theme(axis.text.x=element_text(angle=45,vjust=0.5))
#old billionaires----
forbes_Old <- forbes[order(forbes$Age, decreasing = TRUE), ]
Forbes_Old2 <- forbes_Old %>% head(10) %>%ggplot(aes(x=Names,y=Age,fill=Names))+ 
  geom_bar(stat="identity")+
  geom_text(aes(label=Age),cex=2.5,vjust=0.05)+
  labs(title="Top 10 oldest billionaires",
       x="Networth",y="Age")+ 
  theme_classic()+scale_fill_brewer(palette="PuOr")+
  theme(axis.text.x=element_text(angle=45,vjust=0.5))
#youngest billionaire----
forbes_Old <- forbes[order(forbes$Age, decreasing = TRUE), ]
Tail_10 <- forbes_Old %>% tail(10)%>% ggplot(aes(x=Names,y=Age,fill=Names))+
  geom_bar(stat="identity")+
  geom_text(aes(label=Age),cex=2.5,vjust=0.05)+
  labs(title="Top 10 youngest billionaires", x="Names",y="Age")+ 
  theme_classic()+scale_fill_brewer(palette="PuOr")+
  theme(axis.text.x=element_text(angle=45,vjust=0.5))
##Total worth of billionaires in top 10 countries
#Networth of top billionaires----
worth <- forbes %>% select(Country,Networth) %>% group_by(Country)%>% summarise(Top_billionaire=sum(Networth))%>%arrange(desc(Top_billionaire))
Top_countries <- worth %>% head(10)
Top_countries1 <- Top_countries %>% plot_ly(x=~Country, y=~Top_billionaire, type="scatter", 
          mode="markers",size=10,color=~Country,colors="Dark2")%>%
  layout(title="Total networth of billionaires in top 10 countries",
         yaxis=list(title="Networth"))
#Distribution of age in billionaires----
Age <- forbes%>%plot_ly(x=~Age, type="histogram", color=~Age,colors="Paired")%>%
  layout(title="Distribution of Age in billionaires",yaxis=list(title="Count"))
#Top 5 industries and their countries
Industries <- forbes %>% group_by(Country,Industry)%>% summarize(n=n())%>% arrange(desc=n)
#United state networth in all Industries----
United <- Industries %>% filter(Country=="United States")
fun_color_range <- colorRampPalette(c("Blue","Yellow","Green","Red"))
my_colors <- fun_color_range(18)
my_colors
United_States <- United %>% plot_ly(x=~Industry,y=~n, type="scatter",
                                        mode="markers",size=~n,color=~Industry,colors=my_colors)%>%
  layout(title="United States networth in all industries",
         yaxis=list(title="United States"))
#China  networth in all Industries----
China <- Industries %>% filter(Country=="China")
China_Industries <- China%>% plot_ly(x=~Industry,y=~n, type="scatter",
                                                        mode="markers",size=~n,color=~Industry,colors=my_colors)%>%
  layout(title="China networth in all industries",
         yaxis=list(title="China"))
#India  networth in all Industries---- 
India <- Industries %>% filter(Country=="India")
India_Industries <- India%>% plot_ly(x=~Industry,y=~n, type="scatter",
                                     mode="markers",size=~n,color=~Industry,colors=my_colors)%>%
  layout(title="India networth in all industries",
         yaxis=list(title="India"))
#Germany networth in all Industries---- 
Germany <- Industries %>% filter(Country=="Germany")
Germany_Industries <- Germany%>% plot_ly(x=~Industry,y=~n, type="scatter",
                                     mode="markers",size=~n,color=~Industry,colors=my_colors)%>%
  layout(title="Germany networth in all industries",
         yaxis=list(title="Germany"))
#France networth in all Industries---- 
France <- Industries %>% filter(Country=="France")
France_Industries <- France%>% plot_ly(x=~Industry,y=~n, type="scatter",
                                         mode="markers",size=~n,color=~Industry,colors=my_colors)%>%
  layout(title="France networth in all industries",
         yaxis=list(title="France"))
#Analysis according to source----
#Sources of top 10 young billionaires----
bar <- forbes %>% head(10)%>% ggplot(aes(reorder(x=Names, -Age),y=Age,fill=Names)) + 
  geom_bar(stat="identity")+
  geom_text(aes(label=Age),cex=2.5,vjust=0.05)+
  labs(title="Top 10 billionaires and their age",
       x="Names",y="Networth")+ 
  theme_classic()+scale_fill_brewer(palette="BrBG")+
  theme(axis.text.x=element_text(angle=45,vjust=0.5))
#old billionaires----
forbes_Old <- forbes[order(forbes$Age, decreasing = TRUE), ]
Forbes_Old3 <- forbes_Old %>% head(10) %>%ggplot(aes(x=Names,y=Networth,fill=Source))+ 
  geom_bar(stat="identity")+
  geom_text(aes(label=Networth),cex=2.5,vjust=0.05)+
  labs(title="Top 10 oldest billionaires and their sources",
       x="Names",y="Networth")+ 
  theme_classic()+scale_fill_brewer(palette="PuOr")+
  theme(axis.text.x=element_text(angle=45,vjust=0.5))
#youngest billionaire and their sources----
forbes_Old <- forbes[order(forbes$Age, decreasing = TRUE), ]
Tail_10 <- forbes_Old %>% tail(10)%>% ggplot(aes(x=Names,y=Networth,fill=Source))+
  geom_bar(stat="identity")+
  geom_text(aes(label=Networth),cex=2.5,vjust=0.05)+
  labs(title="Top 10 youngest billionaires and their sources", x="Names",y="Networth")+ 
  theme_classic()+scale_fill_brewer(palette="PuOr")+
  theme(axis.text.x=element_text(angle=45,vjust=0.5))
#Total networth of billionaires in top 10 sources
Sources <- forbes  %>%group_by(Source) %>% summarise(Total_Networth=sum(Networth))%>%arrange(desc(Total_Networth))
#Total networth of billionaires in top 10 sources
Top_Billionaire_sources<- Sources %>%head(10) %>% plot_ly(x=~Source,y=~Total_Networth, type="scatter",
                   mode="markers",size=10,color=~Source,colors=my_colors)%>%
  layout(title="Total networth of billionaires in top 10 sources",
         yaxis=list(title="Networth"))
#Total networth of billionaires in top 10 Industries----
Industry <- forbes  %>%group_by(Industry) %>% summarise(Total_Networth=sum(Networth))%>%arrange(desc(Total_Networth))
#Total networth of billionaires in top 10 Industries
Top_Billionaire_Industry<- Industry %>%head(10) %>% plot_ly(x=~Industry,y=~Total_Networth, type="scatter",
                                                          mode="markers",size=10,color=~Industry,colors=my_colors)%>%
  layout(title="Total networth of billionaires in top 10 Industries",
         yaxis=list(title="Networth"))
