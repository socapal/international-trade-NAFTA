################################################################################
################################################################################
#################### NAFTA_ US-MEX 1985 to 2005 ################################
####################     Data Visualization     ################################
################################################################################
################################################################################
################### Luis Armando Alvarado Rodr?guez ############################
###################   Sebasti?n Ocampo Palacios     ############################
################################################################################
--------------------------------------------------------------------------------
  

# 1. Libraries & data -----------------------------------------------------
rm(list = ls())
dev.off()
library(readxl)
library(xts)
library(dplyr)
library(dygraphs)
library(seasonal)
library(ggplot2)

#Fuentes
#install.packages("extrafont") #Nos permite usar Times New Roman en el doc.
library(extrafont)
# font_import() Utilizar +UNICAMENTE si hay problemas con la fuente de Times New Roman
fonts()

#setwd("C:/Users/Luis AAR/OneDrive - CIDE/Trade DV/Pa no eliminar cosas xd")
setwd("C:/Users/socap/OneDrive/Documentos/CIDE/6 - Sexto Semestre/International Trade/data-vis")

NAFTA=read_xlsx("NAFTA85-05.xlsx", col_names=FALSE, na="NA")
NAFTA=NAFTA[3:195,] #Removes headers

colnames(NAFTA)=NAFTA[1,] 
colnames(NAFTA)     #Renames columns 
     #Note that 'ExporT_sa' is not 'ExportT_sa' as intended. Do not fix and be aware.

NAFTA=NAFTA[2:193,] #Removes colname row


# TROUBLESHOOTING

#Converting columns into numeric type.
NAFTA$ExporT_sa=as.numeric(as.character(NAFTA$ExporT_sa))
NAFTA$ImportT_sa=as.numeric(as.character(NAFTA$ImportT_sa))
NAFTA$`Export %`=as.numeric(as.character(NAFTA$`Export %`))
NAFTA$`Import %`= as.numeric(as.character(NAFTA$`Import %`))
NAFTA$`Import_M%`=as.numeric(as.character(NAFTA$`Import_M%`))
NAFTA$`Export_M%`=as.numeric(as.character(NAFTA$`Export_M%`))
NAFTA$EXC_RATE=as.numeric(as.character(NAFTA$EXC_RATE))
NAFTA$`INPC_12-2005`=as.numeric(as.character(NAFTA$`INPC_12-2005`))
NAFTA$`Import WRLD`=as.numeric(as.character(NAFTA$`Import WRLD`))
NAFTA$`Export WRLD`=as.numeric(as.character(NAFTA$`Export WRLD`))
NAFTA$Exp_Mex=as.numeric(as.character(NAFTA$Exp_Mex))
NAFTA$Imp_Mex=as.numeric(as.character(NAFTA$Imp_Mex))
#Date data is not being read properly.
dates=seq(from=as.Date("1990-01-01"), to=as.Date("2005-12-01"), by="month")
NAFTA$Dates=dates #Redifines date data.

summary(NAFTA)


#Rates of Trade 

#Normalization
NAFTA_TS["1994-01-01"] #Beginning of NAFTA
#              ExporT_sa   ImportT_sa  Import WRLD Export WRLD Export %    Import % 
# 1994-01-01 " 53959.99" " 52060.16" " 50027.0"    "39843.0"  "0.10091025" "0.07753833"

#NAFTA$`Export %`= (NAFTA$`Export %`/0.10091025)-1
#NAFTA$`Import %`= (NAFTA$`Import %`/0.07753833)-1


#Obtaining real values. 

   #Price Index
NAFTA$`INPC_12-2005` #BASE Dec, 2005 (Most Recent Observation)

   #Exports
NAFTA$ExporT_sa=NAFTA$ExporT_sa*NAFTA$EXC_RATE #Pesos (n in millios of dollars * exc rate [pesos x dolar] )
NAFTA$ExporT_sa=NAFTA$ExporT_sa/NAFTA$`INPC_12-2005`*100 #Inflation Adjustment

   #Imports
NAFTA$ImportT_sa=NAFTA$ImportT_sa*NAFTA$EXC_RATE #Pesos (n in millios of dollars * exc rate [pesos x dolar] )
NAFTA$ImportT_sa=NAFTA$ImportT_sa/NAFTA$`INPC_12-2005`*100

  #Shares
#USA
NAFTA$`Import WRLD`=NAFTA$`Import WRLD`*NAFTA$EXC_RATE
NAFTA$`Import WRLD`=NAFTA$`Import WRLD`/NAFTA$`INPC_12-2005`*100
NAFTA$`Export WRLD`=NAFTA$`Export WRLD`*NAFTA$EXC_RATE
NAFTA$`Export WRLD`=NAFTA$`Export WRLD`/NAFTA$`INPC_12-2005`*100


NAFTA$`Export %`= NAFTA$ExporT_sa/NAFTA$`Export WRLD`
NAFTA$`Import %`= NAFTA$ImportT_sa/NAFTA$`Import WRLD`

#MEX
NAFTA$Exp_Mex=NAFTA$Exp_Mex*NAFTA$EXC_RATE
NAFTA$Exp_Mex=NAFTA$Exp_Mex/NAFTA$`INPC_12-2005`*100
NAFTA$Imp_Mex=NAFTA$Imp_Mex*NAFTA$EXC_RATE
NAFTA$Imp_Mex=NAFTA$Imp_Mex/NAFTA$`INPC_12-2005`*100

NAFTA$`Import_M%`=NAFTA$ExporT_sa/NAFTA$Imp_Mex
NAFTA$`Export_M%`=NAFTA$ImportT_sa/NAFTA$Exp_Mex

summary(NAFTA)

#Defining Tariffs
  #NAFTA$Anual_IC_NMF[is.na(NAFTA$Anual_IC_NMF)] = 0 
        #We asume a 0 tariff for NAs in our data.
  #NAFTA$Anual_IC_NMF=NAFTA$Anual_IC_NMF/100  #Tariffs into percentages.


# 2. Time Series  ---------------------------------------------------------
NAFTA_TS=xts(x=NAFTA, order.by = dates) #xts object
NAFTA_TS=NAFTA_TS[,2:7] #Removes redundant columns (dates)

#Deseasonalization

 Des_data <- ts(coredata(NAFTA_TS),frequency=12,start=c(1990,1), end = c(2005, 12))
 #inData <- seas(Des_data) #seas function. No me funcion? :( 
 #Des_data=as.data.frame(inData)


plot(x=dates, y=NAFTA_TS$ImportT_sa, col="red")
par(new=TRUE)
plot(x=dates, y=NAFTA_TS$ImportC_sa, col="blue")
par(new=TRUE)
plot(x=dates, y=NAFTA_TS$ExporT_sa, col="green")
Trade_rate <- NAFTA_TS[,4]
plot(x=dates, y=NAFTA_TS$`Export %`, col="green")

# 3. Dyraphs ---------------------------------------------------------------

# Finally the plot

IET=dygraph(NAFTA_TS, main= "Imports, Exports and Tariffs since NAFTA") %>%
  dySeries("ExporT_sa", label = "Mexican imports from USA",strokeWidth = 3)%>%
  dySeries("ImportT_sa", label = "Mexican exports to USA",strokeWidth = 3 )%>%
  dySeries("ImportC_sa", label = "Mexican exports of consumption goods to USA",strokeWidth = 3 )%>%
  dySeries("Anual_IC_NMF", axis = "y2", strokeWidth = 3, label = "Weighted average tariff", color = "#D8AE5A" )%>%
  dyAxis("y2", label = "Tariff porcentage", valueRange = c(0,50))%>%
  dyAxis("y", label = "Nominal amount of imports and exports (millions of dollars)")%>%
  dyOptions(labelsUTC = TRUE) %>%
  dyRangeSelector() %>%
  dyEvent("1994-1-1", "Begining of NAFTA", labelLoc = "top")%>%
  dyCrosshair(direction = "vertical")
IET

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/dygraphs318.html"))




# 4. ggplot2 - Import, Exports and Trade Rates --------------------------


# Defining color palette (https://colorhunt.co/palette/264466)
dark_green= "#184d47"
p_green= "#96bb7c"
p_yellow= "#fad586"
p_red= "#c64756"

import_public_sans()

#MEX-US Imports and Exports since NAFTA. 
IET=ggplot(NAFTA, aes(x=Dates, y=ExporT_sa/1000))+
  geom_line(data=NAFTA, mapping=aes( y= ImportT_sa/1000), color= p_red, size=1.2)+
  geom_line(data=NAFTA, mapping=aes( y= ExporT_sa/1000), color=dark_green, size=1.2)+
  #Scales
  
  scale_x_date(name="", date_labels = "%Y", date_breaks = "2 year") +
  scale_y_continuous(
    # Features of the first axis ======
    name = "",  minor_breaks = NULL, limits = c(0,180))+
  
  #Annotation
  geom_vline(xintercept=as.Date("1994-01-01"), color="black", size=.5, linetype="solid")+
  
  annotate(geom="text", x=as.Date("1994-01-01"), y=130, 
           label="Beginning of NAFTA", size=4)+ 
  ggtitle("MEX-USA Imports and Exports since NAFTA")+ 
  labs( subtitle="In millions of December 2005 pesos.")+
  
  #Themes
  theme_classic() +
    theme( 
      panel.border = element_blank(), 
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      
      text=element_text(family="Times New Roman", size=12),
      plot.title = element_text(face="bold")
      ) + 
  ggExtra::removeGrid()
IET


# Share of USA trade that takes place Under NAFTA
SUT_N=ggplot(NAFTA, aes(x=Dates, y=`Export %`*100 ))+
    geom_line(data=NAFTA, mapping=aes( y= `Export %`*100), color= p_red, size=1.2)+
    geom_line(data=NAFTA, mapping=aes( y= `Import %`*100), color=dark_green, size=1.2)+
    
    #Scales
    scale_x_date(name="", date_labels = "%Y", date_breaks = "2 year") +
    scale_y_continuous(
      # Features of the first axis ======
      name = "",  minor_breaks = NULL, limits = c(0,15.5))+
    
    #Annotation
    geom_vline(xintercept=as.Date("1994-01-01"), color="black", size=.5, linetype="solid")+
   # annotate(geom="text", x=as.Date("1994-01-01"), y=12, 
   #          label="Beginning of NAFTA", size=4)+ 
    ggtitle("Share of USA trade that takes place under NAFTA (%)")+ 
    
    #Themes
    theme_classic() +
    theme( 
      panel.border = element_blank(), 
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      
      text=element_text(family="Times New Roman", size=12),
      plot.title = element_text(face="bold")
      )+ ggExtra::removeGrid()
SUT_N



#Share of Mexican trade that takes place Under NAFTA
SMT_N=ggplot(NAFTA, aes(x=Dates, y=`Export_M%`*100 ))+
  geom_line(data=NAFTA, mapping=aes( y= `Export_M%`*100), color= p_red, size=1.2)+
  geom_line(data=NAFTA, mapping=aes( y= `Import_M%`*100), color=dark_green, size=1.2)+
  
  #Scales
  scale_x_date(name="", date_labels = "%Y", date_breaks = "2 year") +
  scale_y_continuous(
    # Features of the first axis ======
    name = "",  minor_breaks = NULL, limits = c(0,90))+
  
  #Annotation
  geom_vline(xintercept=as.Date("1994-01-01"), color="black", size=.5, linetype="solid")+
  #annotate(geom="text", x=as.Date("1994-01-01"), y=25, 
  #         label="Beginning of NAFTA", size=4)+ 
  ggtitle("Share of Mexican trade that takes place under NAFTA (%)")+ 
  
  #Themes
  theme_classic() +
  theme(  panel.border = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    text=element_text(family="Times New Roman", size=12),
    plot.title = element_text(face="bold")
  )+
  ggExtra::removeGrid()
SMT_N

#We desire a view that incorporates this structure IET + (RoT+MoT)
IET
egg::ggarrange(SUT_N, SMT_N)


# Salvar archivo
IET #MEX-USA Imports and Exports since NAFTA
ggsave("MEX-USA_IEN.png", height = 4.38,width = 6, units = "in", dpi=300, limitsize=FALSE)
SUT_N #Share of USA trade that takes place Under NAFTA
ggsave("SUT_N.png", height =2.12,width = 6, units = "in", dpi=300, limitsize=FALSE)
SMT_N #Share of Mexican trade that takes place Under NAFTA
ggsave("SMT_N.png", height = 2.12 ,width = 6, units = "in", dpi=300, limitsize=FALSE)

