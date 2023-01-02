#DESCRIPTIVE CHARTS

#Testing for correlation between test scores and parental income

Install.packages("gglpot2")
library(ggplot2)

#Creating a Scatter Plot with a Fitted Line
ggplot(analysisdata%>%filter(year==5),  
       aes(x=parental_lincome,y=test_score))+
  geom_smooth(color="#145c21") +
  geom_point(alpha=0.1,size=0.85,color="#63a668")+
  theme(panel.background = element_rect(fill="#ededed",color="#ededed"),
        plot.background = element_rect(fill="#ededed",color="#ededed"),
        panel.grid.major = element_line(colour="#a3a3a3",size=0.1))+
  labs(x="Log(Parental Income)",y="Test Score (Mean=0,SD=1)", title="Test scores & Parental income")

#Testing for correlation between test scores and parental schooling

install.packages("patchwork")
# Load patchwork 
library("patchwork")
# Create raw chart element
rawchart<-ggplot(analysisdata%>%filter(year==4),x=as.factor(fill))+
  theme_classic()
# Create bar chart of pre summer school test score and summer school 
p1<-rawchart+
  geom_smooth(aes(x=parental_schooling,y=test_score)) +
  geom_point(aes(x=parental_schooling,y=test_score),alpha=0.1)+
  labs(x="Parental schooling", y="Test Score Year 5")
# Create bar chart of pre summer school test score and summer school 
p2<-rawchart+
  geom_bar(aes(x=as.factor(summerschool),y=test_score),
           stat="summary",fun="mean")+
  labs(y="Test Score Year 5", x="Attended Summer School")
# Create bar chart of parental schooling and summer school attendance
p3<-rawchart+
  geom_boxplot(aes(x=as.factor(summerschool),y=parental_lincome))+
  labs(y="Parental Income (log)", x="Attended Summer School")
# Combine charts
p1/(p2+p3)
# Export chart
ggsave("fig1.png")

#Test score distribution for year 6 for those who attended summer schools vs those who did not
# create a histogram and density chart
ggplot(filter(analysisdata,year==6),
       aes(x=test_score,fill=as.factor(summerschool)))+
  geom_histogram(aes(y=..density..),bins = 50,alpha=0.5,
                 position="identity",color="white")+
  geom_density(alpha=0.0,size=1,show.legend= FALSE)+
  theme_minimal()+
  labs(y="Density",x="Test score",fill=" ")+
  scale_fill_brewer(palette="Set2",labels=c("No summer school","Summer school"))+
  theme(legend.position="top")
