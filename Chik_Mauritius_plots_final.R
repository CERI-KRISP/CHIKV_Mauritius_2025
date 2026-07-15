library(ggplot2)
library(ggtree)
library(treeio)
library(readxl)
library(lubridate)
library(cowplot)
library(raster)
library(sf)
#library(rgeos)
library(dplyr)
library(rworldmap)
library(rnaturalearth)
library(RColorBrewer)
library(ISOweek)
library(stringr)



##### Figure 1


#### Figure 1A - Epicurve + sequencing dates

#metadata<-read_excel('data/Mauritius_metadata.xlsm')
#metadata$age<-as.numeric(metadata$age)
#metadata$date<-as.Date(metadata$`Collection date`)

epi_data<-read_excel('data/Mauritius-CHIKV-2025-Metadata-March_13-June_15.xlsx')
epi_data$date<-as.Date(epi_data$`Date Coll`)
epi_data$date2<-as.Date(cut(as.Date(epi_data$date),
                            breaks = "2 weeks",
                            start.on.monday = FALSE))

epi_data$AGE<-as.numeric(epi_data$AGE)

reunion_epi_data<-read_excel('data/Reunion_weekly_cases_Aug24.xlsx')
colnames(reunion_epi_data)<-c('Year','Week','Cases')
reunion_epi_data$date<-ISOweek2date(paste0(paste0(paste0(reunion_epi_data$Year,"-W"),sprintf("%02d",reunion_epi_data$Week)), "-7"))    # Sunday


R_estimate<-read_excel('data/Mauritius_CHIKV_R_updated.xlsx')
R_estimate$date<-as.Date(R_estimate$t_end_dates)

reu_R_estimate<-read_excel('data/REU_R_estimates_Aug24.xlsx')
reu_R_estimate$date<-as.Date(reu_R_estimate$date)

epi_curve<-ggplot()+
  theme_bw()+
  geom_bar(data=epi_data, aes(date, fill='Cases'),colour='black', size=0.2)+
  #geom_bar(data=reunion_epi_data, stat='identity',aes(x=date, y=Cases, fill='Reunion Cases'),colour='black', size=0.2)+
  
  #geom_bar(data=epi_data, aes(as.Date(date), No,fill='...'),stat='identity', colour='black', size=0.2)+
  #scale_fill_manual(values=c('pink3','dodgerblue4'))+
  #theme(legend.position = c(0.3,0.8), legend.background = element_blank(), legend.title = element_blank())+
  geom_line(data=R_estimate,aes(date,as.numeric(`Mean(R)`)*10,colour='Rt'),size=1)+
  geom_ribbon(data=R_estimate, aes(x=date,ymin=as.numeric(`Quantile.0.05(R)`)*10,ymax=as.numeric(`Quantile.0.95(R)`)*10),fill='purple4',alpha=0.2)+
  geom_hline(yintercept = 1*10, colour='purple3',linetype='dashed')+
  scale_x_date(date_labels = "%d-%b\n%Y",date_breaks = "2 weeks")+
  ylab('New Cases')+xlab('Date')+
  scale_fill_manual(values=c('grey80','tan'))+
  scale_colour_manual(values=c('red3','purple4'))+
  theme(legend.title = element_blank(), legend.position = c(0.15,0.7))+
    scale_y_continuous(
      "Cases", 
      limits=c(0,40),
      sec.axis = sec_axis(~ . / 10, name = "Rt")
    )+
  geom_rug(data=subset(epi_data,!is.na(SEQUENCED)),aes(x=as.Date(date),colour='Genomes'),size=0.8,alpha=0.5,length = unit(0.05, "npc"))
  #geom_rug(data=subset(epi_data,COMMENTS=='Reunion'),aes(x=as.Date(date),colour='Genomes\n(travel history)'),size=0.8,alpha=1,length = unit(0.05, "npc"))
  #geom_rug(data=subset(metadata,date>as.Date("2025-01-01")),aes(x=as.Date(date),colour='Genomes'),size=0.8,alpha=0.5,length = unit(0.05, "npc"))
epi_curve


###Figure 1B - Demographics plot

demographics<-epi_data %>% 
  dplyr::mutate(
    # Create categories
    age_group = dplyr::case_when(
      AGE <= 4            ~ "0-4",
      AGE > 4 & AGE <= 14 ~ "5-14",
      AGE > 14 & AGE <= 29 ~ "15-29",
      AGE > 29 & AGE <= 44 ~ "30-44",
      AGE > 44 & AGE <= 64 ~ "45-65",
      AGE > 64  ~ "> 65"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("0-4", "5-14","15-29", "30-44", "45-65","> 65")
    )
  )

demographics_count<-demographics %>% dplyr::count(SEX, age_group, sort = TRUE)


demographics_plot<-apyramid::age_pyramid(data = subset(demographics_count,age_group!='NA'),
                                         age_group = "age_group",# column name for age category
                                         split_by = "SEX",   # column name for gender
                                         count = "n")   +   # column name for case counts+
  theme_minimal()+
  theme(legend.position = c(0.95,0.3))+
  ylab('Count')+xlab('Age Groups')+
  scale_fill_manual(values=c('thistle3','lightsteelblue4'))

demographics_plot

FIG1AB<-plot_grid(epi_curve,demographics_plot,ncol=1,labels=c("A","B"),rel_heights = c(0.65,0.35))
FIG1CD<-plot_grid("","",labels=c("C","D"),ncol=1)
FIG1EF<-plot_grid("","",labels=c("E","F"),ncol=1)

FIG1<-plot_grid(FIG1AB,FIG1CD,FIG1EF,ncol=3,rel_widths = c(0.5,0.25,0.25))
FIG1



### Figure 1C - map of distribution of cases and genomes


#start here
dir.create("data/gadm", recursive = TRUE)
Mauritius_gadm_data_0 <- geodata::gadm(country = "MUS", level = 0,path = "data/gadm")
Mauritius_gadm_data_1 <- geodata::gadm(country = "MUS", level = 1,path = "data/gadm")
Mauritius_gadm_data_1_sf <- sf::st_as_sf(Mauritius_gadm_data_1)
Mauritius_gadm_data_1_df<-as.data.frame(Mauritius_gadm_data_1)

Mauritius_cases_count_confirmed<- epi_data %>% group_by(DISTRICT) %>%
  summarise(count=n())

Mauritius_cases_count_confirmed$District<-str_to_sentence(Mauritius_cases_count_confirmed$DISTRICT)
Mauritius_cases_count_confirmed

Mauritius_cases_count_confirmed$District[Mauritius_cases_count_confirmed$District=='Black-River']<-'Black River'
Mauritius_cases_count_confirmed$District[Mauritius_cases_count_confirmed$District=='Black-river']<-'Black River'
Mauritius_cases_count_confirmed$District[Mauritius_cases_count_confirmed$District=='Black river']<-'Black River'

Mauritius_cases_count_confirmed$District[Mauritius_cases_count_confirmed$District=='Riviere Du rempart ']<-'Rivière du Rempart'
Mauritius_cases_count_confirmed$District[Mauritius_cases_count_confirmed$District=='Riviere du rempart']<-'Rivière du Rempart'

Mauritius_cases_count_confirmed$District[Mauritius_cases_count_confirmed$District=='Port louis']<-'Port Louis'
Mauritius_cases_count_confirmed$District[Mauritius_cases_count_confirmed$District=='Plaines wilhems']<-'Plaines Wilhems'
Mauritius_cases_count_confirmed$District[Mauritius_cases_count_confirmed$District=='Plaine wilhems']<-'Plaines Wilhems'
Mauritius_cases_count_confirmed$District[Mauritius_cases_count_confirmed$District=='Plaines']<-'Plaines Wilhems'
Mauritius_cases_count_confirmed$District[Mauritius_cases_count_confirmed$District=='Grand port']<-'Grand Port'
Mauritius_cases_count_confirmed$District[Mauritius_cases_count_confirmed$District=='Mahebourg']<-'Grand Port'

Mauritius_cases_count_confirmed$District[Mauritius_cases_count_confirmed$District=='Rodrigues island']<-'Rodriguez'

colnames(Mauritius_cases_count_confirmed)<-c("DISTRICT","case_count", "District")


Mauritius_sequence_count_district<-subset(epi_data,!is.na(SEQUENCED)) %>% group_by(DISTRICT) %>%
  summarise(count=n())
Mauritius_sequence_count_district$District<-str_to_sentence(Mauritius_sequence_count_district$DISTRICT)
colnames(Mauritius_sequence_count_district)<-c("DISTRICT","sequence_count", "District")


Mauritius_sequence_count_district

Mauritius_sequence_count_district$District[Mauritius_sequence_count_district$District=='Black-river']<-'Black River'
Mauritius_sequence_count_district$District[Mauritius_sequence_count_district$District=='Black river']<-'Black River'
Mauritius_sequence_count_district$District[Mauritius_sequence_count_district$District=='Riviere du rempart']<-'Rivière du Rempart'
Mauritius_sequence_count_district$District[Mauritius_sequence_count_district$District=='Port louis']<-"Port Louis"
Mauritius_sequence_count_district$District[Mauritius_sequence_count_district$District=='Port-louis']<-"Port Louis"
Mauritius_sequence_count_district$District[Mauritius_sequence_count_district$District=='Plaine wilhems']<-"Plaines Wilhems"
Mauritius_sequence_count_district$District[Mauritius_sequence_count_district$District=='Plaine wilhems']<-'Plaines Wilhems'
Mauritius_sequence_count_district$District[Mauritius_sequence_count_district$District=='Plaines']<-'Plaines Wilhems'
Mauritius_sequence_count_district$District[Mauritius_sequence_count_district$District=='Grand port']<-"Grand Port"
Mauritius_sequence_count_district$District[Mauritius_sequence_count_district$District=='Rodrigues']<-"Rodriguez"

Mauritius_sequence_count_district<-subset(Mauritius_sequence_count_district, District != 'Not stated')
Mauritius_sequence_count_district<-subset(Mauritius_sequence_count_district, District != 'Imported')

Mauritius_gadm_data_1_sf2<-left_join(Mauritius_gadm_data_1_sf,Mauritius_sequence_count_district, by=c("NAME_1"="District"))
Mauritius_gadm_data_1_sf2<-left_join(Mauritius_gadm_data_1_sf2,Mauritius_cases_count_confirmed, by=c("NAME_1"="District"))

Mauritius_gadm_data_1_sf2<-subset(Mauritius_gadm_data_1_sf2,ENGTYPE_1=='District')
Mauritius_gadm_data_1_sf2_centroids <- st_centroid(Mauritius_gadm_data_1_sf2)

Mauritius_epi_map<-ggplot() + 
  theme_void()+
  geom_sf(data=Mauritius_gadm_data_1_sf2,aes(fill=case_count)) +
  geom_sf(data=Mauritius_gadm_data_1_sf2,colour='black',linewidth=0.5, fill=NA) +
  geom_point(data=Mauritius_gadm_data_1_sf2_centroids, aes(geometry=geometry,size=log2(sequence_count)),
             stat = "sf_coordinates",shape=21,colour='red3',fill='red3',alpha=0.2)+
  geom_text(data=Mauritius_gadm_data_1_sf2_centroids, aes(geometry=geometry,label=sequence_count),size=3,
            stat = "sf_coordinates",shape=21,colour='white')+
  scale_size_continuous(range = c(3, 10),breaks=c(1,3),labels=c(1,20),name='Genomic\nSampling')+  # Adjust min/max point sizes
  scale_fill_distiller(palette = "PuBuGn", direction = 1,na.value = "white",
                       breaks = c(0, 50, 100,150,200,250), labels = c(0, 50, 100,150,200,250),
                       
                       name='Cases') +
  theme(legend.position = c(0.0,0.5))

#geom_point(data=Mauritius_map2_df, aes(x=longitude,y=latitude,size=count.y),colour='dodgerblue3',alpha=0.3,stroke=0.5)
#add cases
Mauritius_epi_map

## Figure 1D-F

y_min = -20.53; y_max =-19.97; x_min = 57.25; x_max = 57.82
study_area = extent(x_min, x_max, y_min, y_max)



Mauritius_population<-raster("data/Mauritius_Population.asc")
Mauritius_population<-crop(Mauritius_population,Mauritius_gadm_data_1_sf2)
Mauritius_population<-raster::mask(Mauritius_population,Mauritius_gadm_data_1_sf2)
#plot(Mauritius_population)

Mauritius_temperature<-raster("data/Mauritius_temperature.asc")
Mauritius_temperature<-crop(Mauritius_temperature,study_area)
Mauritius_temperature<-raster::mask(Mauritius_temperature,Mauritius_gadm_data_1_sf2)
#plot(Mauritius_temperature)

precipitation<-raster("data/Mauritius_precipitation.asc")
Mauritius_precipitation<-crop(precipitation,study_area)
Mauritius_precipitation<-raster::mask(Mauritius_precipitation,Mauritius_gadm_data_1_sf2)
#plot(Mauritius_precipitation)

cols1=colorRampPalette(brewer.pal(9,"YlOrRd"))(130)[16:115]
cols2=colorRampPalette(brewer.pal(9,"RdBu"))(130)[20:130]
cols3=colorRampPalette(brewer.pal(9,"YlGnBu"))(130)[20:130]
cols4=colorRampPalette(brewer.pal(9,"BrBG"))(130)[16:115]


plot(Mauritius_population,col=rev(cols4),axes=F,frame.plot=F,legend=F)
plot(Mauritius_population, legend.only=T, add=T, col=rev(cols4), legend.width=0.5, legend.shrink=0.3, smallplot=c(0.2,0.35,0.7,0.71),
     legend.args=list(text="Population density", cex=0.6, line=0.3, col="gray30"), horizontal=T,
     axis.args=list(cex.axis=0.5, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="gray30", line=0, mgp=c(0,-0.02,0)))
plot(Mauritius_gadm_data_1_sf2$geometry,add=T)


plot(Mauritius_temperature,col=rev(cols2),axes=F,frame.plot=F,legend=F)
plot(Mauritius_temperature, legend.only=T, add=T, col=rev(cols2), legend.width=0.5, legend.shrink=0.3, smallplot=c(0.2,0.35,0.7,0.71),
     legend.args=list(text="Temperature (max)", cex=0.6, line=0.3, col="gray30"), horizontal=T,
     axis.args=list(cex.axis=0.5, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="gray30", line=0, mgp=c(0,-0.02,0)))
plot(Mauritius_gadm_data_1_sf2$geometry,add=T)


plot(Mauritius_precipitation,col=cols3,axes=F,frame.plot=F,legend=F)
plot(Mauritius_precipitation, legend.only=T, add=T, col=cols3, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.2,0.35,0.7,0.71),
     legend.args=list(text="Precipitation (monthly, mm)", cex=0.6, line=0.3, col="gray30"), horizontal=T,
     axis.args=list(cex.axis=0.5, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="gray30", line=0, mgp=c(0,-0.02,0)))
plot(Mauritius_gadm_data_1_sf2$geometry,add=T)




### Figure 2

### Climate data
mru_climate <- read.csv('data/Temp_Prec_Admin1_Mauritius.csv')
mru_climate$date<-as.Date(mru_climate$date)

mru_climate_national<-mru_climate %>% group_by(date) %>%
  summarise(mean_precip=mean(Precipitation), mean_temp=mean(Temp))

mru_climate_national$Temp_rolling <- rollmean(mru_climate_national$mean_temp, k = 30, align = "center", fill = NA)
mru_climate_national$precip_rolling <- rollmean(mru_climate_national$mean_precip, k = 30, align = "center", fill = NA)


reu_climate <- read.csv('data/Temp_Prec_Admin1_Reunion.csv')
reu_climate$date<-as.Date(reu_climate$date)

reu_climate_national<-reu_climate %>% group_by(date) %>%
  summarise(mean_precip=mean(Precipitation), mean_temp=mean(Temperature))

reu_climate_national$Temp_rolling <- rollmean(reu_climate_national$mean_temp, k = 30, align = "center", fill = NA)
reu_climate_national$precip_rolling <- rollmean(reu_climate_national$mean_precip, k = 30, align = "center", fill = NA)


breteau_index_df<-read_excel("data/breteau_index.xlsx")

breteau_index_plot<-ggplot(breteau_index_df, aes(Date, Index, group=1))+
  stat_summary(fun.y=mean, colour="red", geom="line",group=1)
breteau_index_plot


mru_climate_plot<-ggplot()+
  geom_bar(data=epi_data, aes(date, fill='Mauritius - Cases'),colour='grey70', size=0.2)+
  geom_line(data=mru_climate_national, aes(date,precip_rolling*10000, colour='Mauritius - Precipitation'),size=1)+
  stat_summary(data=breteau_index_df, aes(as.Date(Date), Index*10, colour='Mauritius - Breteau Index'),fun.y=mean, geom="line",linetype='dashed',group=1,size=1)+
  geom_line(data=subset(R_estimate, date>as.Date("2025-04-05")),aes(date,`Mean(R)`*10,colour='Mauritius - Re'),size=1)+
  geom_ribbon(data=subset(R_estimate, date>as.Date("2025-04-05")), aes(x=date,ymin=`Quantile.0.05(R)`*10,ymax=`Quantile.0.95(R)`*10),fill='purple4',alpha=0.2)+
  geom_hline(yintercept = 1*10, colour='purple3',linetype='dashed')+
  
  theme_bw()+
  scale_colour_manual(values=c('cadetblue3','blue3','purple3'))+
  scale_fill_manual(values=c('grey80'))+
  theme(legend.position=c(0.2,0.7),legend.background = element_blank(),legend.title = element_blank())+
  scale_y_continuous(
    "Daily Cases / Precipitation * 10^4", 
    sec.axis = sec_axis(~ . / 10, name = "Re / Breteau Index")
  )

mru_climate_plot


reu_climate_plot<-ggplot()+
  geom_bar(data=reunion_epi_data, stat='identity',aes(date,Cases, fill='Reunion - Cases'),colour='grey70', size=0.2)+
  geom_line(data=reu_climate_national, aes(date,precip_rolling*1000000, colour='Reunion - Precipitation'), size=1)+
  geom_line(data=reu_R_estimate,aes(date,as.numeric(`Mean(R)`)*1000,colour='Reunion - Re'),size=1)+
  geom_ribbon(data=reu_R_estimate, aes(x=date,ymin=as.numeric(`Quantile.0.05(R)`)*1000,ymax=as.numeric(`Quantile.0.95(R)`)*1000),fill='red3',alpha=0.2)+
  geom_hline(yintercept = 1*1000, colour='red3',linetype='dashed')+
  
  theme_bw()+
  scale_colour_manual(values=c( "darkgreen", "red3"))+
  scale_fill_manual(values=c('tan2'))+
  theme(legend.position=c(0.2,0.7),legend.background = element_blank(),legend.title = element_blank())+
  scale_y_continuous(
    "Daily Cases / Precipitation * 10^5", 
    #limits=c(0,50),
    sec.axis = sec_axis(~ . / 1000, name = "Re")
  )

reu_climate_plot

climate_cases_fig<-plot_grid(mru_climate_plot,reu_climate_plot,ncol=1,labels=c("A","B"))
climate_cases_fig



##### Figure 3 - Phylogenetics - ECSA ML Trees

### tree

#ML_tree<-read.newick('data/ECSA_Final_dataset_aligned_trimmed_outlierremoved.nwk.tree')
ML_tree<-read.newick('data/Mauritius_ML_bestfitting.nwk')
ML_tree_data<-fortify(ML_tree)
subset_tips_mru <- ML_tree_data[ML_tree_data$isTip & grepl("Mauritius", ML_tree_data$label), ]
subset_tips_reu <- ML_tree_data[ML_tree_data$isTip & grepl("Reunion", ML_tree_data$label), ]



ML_tree <- groupClade(ML_tree,.node=c(3133,3161,1756))

p <- ggtree(ML_tree,aes(colour=group), size=0.5) + theme_tree2()+
  geom_tiplab(data = subset_tips_mru,aes(label="<"),  colour='deeppink2', size = 3,align=F,linetype = "dashed", offset = 0.001)+
  
  scale_colour_manual(values=c('ivory3','darkgreen','darkblue','goldenrod2'),
                      labels=c("Other ECSA Diversity\n","New ECSA Clade\n(Central Africa)\n", "New ECSA Clade\n(Indian Ocean)\n", "Indian Ocean Lineage\n(Emerged in 2005)\n"),
                      name="")+
  theme(legend.position = c(0.25,0.8))+
  theme(axis.line.x = element_blank(),axis.ticks = element_blank(),axis.text = element_blank())
  #expand_limits(y =350)+
  #geom_tiplab()+
  #geom_text(aes(label=node), hjust=-.3, size=2)

p
#ggsave('ECSA_tree_labels_v3.pdf', width = 50, height = 300, units = "cm",limitsize = FALSE)


NewClade_ML_tree<-read.newick('data/Mauritius_ML_bestfitting_SUBCLADE.nwk')
NewClade_ML_tree_data<-fortify(NewClade_ML_tree)

NewClade_ML_tree <- groupClade(NewClade_ML_tree,.node=c(381))

p2 <- ggtree(NewClade_ML_tree,aes(colour=group),size=0.5,show.legend=F) + theme_tree2()+
  scale_colour_manual(values=c('darkgreen','darkblue'),
                      labels=c("New ECSA Clade\n(Central Africa)\n", "New ECSA Clade\n(Indian Ocean)\n"),
                      name="")+
  
  theme(legend.position = c(0.25,0.75))+
  theme(axis.line.x = element_blank(),axis.ticks = element_blank(),axis.text = element_blank())
  #geom_tiplab()+
  #geom_text(aes(label=node), hjust=-.3, size=2)
p2

subset_tips_mru2 <- p2$data[NewClade_ML_tree_data$isTip & grepl("Mauritius", p2$data$label), ]
subset_tips_reu2 <- p2$data[NewClade_ML_tree_data$isTip & grepl("Reunion", p2$data$label), ]
subset_tips_drc2 <- p2$data[NewClade_ML_tree_data$isTip & grepl("DemocraticRepublicoftheCongo", p2$data$label), ]
subset_tips_cmr2 <- p2$data[NewClade_ML_tree_data$isTip & grepl("Cameroon", p2$data$label), ]
subset_tips_gbn2 <- p2$data[NewClade_ML_tree_data$isTip & grepl("Gabon", p2$data$label), ]
subset_tips_egn2 <- p2$data[NewClade_ML_tree_data$isTip & grepl("EquatorialGuinea", p2$data$label), ]
subset_tips_may2 <- p2$data[NewClade_ML_tree_data$isTip & grepl("Mayotte", p2$data$label), ]



p3<-p2+
  ggnewscale::new_scale_color() +
  #geom_tiplab(data = subset_tips_mru2,aes(label=sapply(strsplit(label, "_"), `[`, 1)),  colour='deeppink2', size = 1,align=F,linetype = "dashed", offset = 0.001)+
  geom_tiplab(data = subset_tips_mru2,aes(label="<",  colour='Mauritius'), fontface='bold',size = 3,align=F,linetype = "dashed", offset = 0.0005)+
  geom_tiplab(data = subset_tips_reu2,aes(label="<",  colour='Reunion'), fontface='bold',size = 3,align=F,linetype = "dashed", offset = 0.0005)+
  geom_tiplab(data = subset_tips_may2,aes(label="<",  colour='Mayotte'), fontface='bold',size = 3,align=F,linetype = "dashed", offset = 0.0005)+
  geom_tiplab(data = subset_tips_drc2,aes(label="<",  colour='DRC'), fontface='bold',size = 3,align=F,linetype = "dashed", offset = 0.0005)+
  geom_tiplab(data = subset_tips_cmr2,aes(label="<",  colour='Cameroon'), fontface='bold',size = 3,align=F,linetype = "dashed", offset = 0.0005)+
  geom_tiplab(data = subset_tips_egn2,aes(label="<",  colour='Equatorial Guinea'), fontface='bold',size = 3,align=F,linetype = "dashed", offset = 0.0005)+
  geom_tiplab(data = subset_tips_gbn2,aes(label="<",  colour='Gabon'), fontface='bold',size = 3,align=F,linetype = "dashed", offset = 0.0005)+
  scale_colour_manual(values=c('darkgreen','grey50','honeydew3','tan','deeppink2','purple3','dodgerblue3'),
                      name="Countries")+
  guides(color = guide_legend(override.aes = list(label = "<", fontface='bold',size = 4)))+
  expand_limits(y =-10)
  #geom_tiplab()
  #geom_text(aes(label=node), hjust=-.3, size=2)

p3


plot_grid(p,p3,rel_widths = c(0.5,0.5),labels=c("A","B"))


##### Figure 4 

#root to tip - Fig 4A

residuals<-read.table('data/IndianOceanClade_n324_cleaned_outliersremoved_TEMPEST.txt',header=T)
residuals$days<-as.Date(date_decimal(as.numeric(residuals$date)))

root_tip<-ggplot(residuals, aes(days, as.numeric(distance)))+
  theme_classic()+
  geom_smooth(method='lm',colour='grey60')+
  geom_point(shape=21,size=3,alpha=0.5,aes(fill=days<as.Date("2024-01-01")))+
  scale_fill_manual(values=c('navyblue','springgreen4'))+
  ylab('Root-to-tip Divergence')+xlab('Time')+
  scale_x_date(    date_labels = "%b-%Y",
                   date_breaks = "2 months")+
  annotate(geom = 'text', label='Correlation coefficient = 0.54',x=as.Date("2025-01-30"),y=0.00105)+
  annotate(geom = 'text',label='R squared = 0.29', x=as.Date("2025-01-30"),y=0.001)+
  theme(legend.position='none')


root_tip



#### Population density - Fig 4C

pop_den<-read.table('data/IO_n324.skygrid.txt',header=T)

p_skygrid<-ggplot()+
  theme_bw()+
  geom_ribbon(data=pop_den,aes(x=as.Date(date),ymin=as.numeric(lower),ymax=as.numeric(upper)), fill='navyblue',alpha=0.3)+
  geom_line(data=pop_den,aes(as.Date(date),as.numeric(median)), colour='navyblue', size=1)+
  scale_y_continuous(trans='log10')+
  ylab('Effective Population Size')+
  theme(axis.title.x = element_blank())+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "1 month")
  

p_skygrid


library(ggtree)
library(treeio)


palette6 <- c(
  "Black River"="#FAF6F0", # ivory
  "Flacq" = "maroon3", # very light pink
  "Grand Port"= "#E9BFD2", # pale pink
  "Moka" ="orchid3", # dusty pink
  "Pamplemousses"="indianred2", # rose
  "Plaine Wilhems"=  "maroon4", # deeper rose
  "Port-Louis"     = "thistle3", # muted fuchsia accent
  "Riviere du Rempart"= "#A95C9A", # soft purple
  "Reunion"="royalblue",  # standout blue
  "Savanne"  = "#F3DCE6", # muted violet
  # "#7A3E6A", # plum
  "Rodrigues" = "ivory3" # wine / maroon
  #"#5A3A2C" # subtle brown (kept minimal)
  
)

##Figure 4B
beast_tree<-read.beast("data/IO_n324.rlxd.skygrid.dscrt.500mlv2.Combined_MCC.tree")

# Get ggtree data with node info
tree_df <- ggtree(beast_tree, mrsd = "2025-06-16", as.Date = TRUE)$data

# Add tip labels to the tips (isTip == TRUE)
tree_df$label[tree_df$isTip] <- beast_tree@phylo$tip.label


p_tree <- ggtree(beast_tree, mrsd="2025-06-16",as.Date=T, color='navyblue',size=0.5) + theme_tree2()+
  geom_tippoint(
    data = subset(tree_df, grepl("Mauritius", label, fixed = TRUE)),
    size = 3, stroke = 0.1, fill='grey30',color = "black", shape = 21
  ) +
   geom_tippoint(
    data = subset(tree_df, grepl("36503", label, fixed = TRUE)),
    size = 3.2, stroke = 1.5, fill=NA,color = "red2", shape = 21
  ) +
  geom_tippoint(
    data = subset(tree_df, grepl("39529", label, fixed = TRUE)),
    size = 3.2, stroke = 1.5, fill=NA,color = "red2", shape = 21
  ) +
  #geom_tippoint()
  #geom_tippoint(aes(fill='Mauritius'),size=3, align=F, stroke=0.1,color='grey60',shape=21)+
  geom_tippoint(
    data = subset(tree_df, grepl("Reunion", label, fixed = TRUE)),
    aes(fill = "Reunion"),
    size = 3, stroke = 0.1, color = "black", shape = 21
  ) +
  geom_tippoint(
    data = subset(tree_df, grepl("PAMPLEMOUSSES", label, fixed = TRUE)),
    aes(fill = "Pamplemousses"),
    size = 3, stroke = 0.1, color = "black", shape = 21
  ) +
  geom_tippoint(
    data = subset(tree_df, grepl("MAPOU", label, fixed = TRUE)),
    aes(fill = "Riviere du Rempart"),
    size = 3, stroke = 0.1, color = "black", shape = 21
  ) +
  geom_tippoint(
    data = subset(tree_df, grepl("PLAINE", label, fixed = TRUE)),
    aes(fill = "Plaine Wilhems"),
    size = 3, stroke = 0.1, color = "black", shape = 21
  ) +
  geom_tippoint(
    data = subset(tree_df, grepl("REMPART", label, fixed = TRUE)),
    aes(fill = "Riviere du Rempart"),
    size = 3, stroke = 0.1, color = "black", shape = 21
  ) +
  geom_tippoint(
    data = subset(tree_df, grepl("RIVER", label, fixed = TRUE)),
    aes(fill = "Black River"),
    size = 3, stroke = 0.1, color = "black", shape = 21
  ) +
  geom_tippoint(
    data = subset(tree_df, grepl("NOIRE", label, fixed = TRUE)),
    aes(fill = "Black River"),
    size = 3, stroke = 0.1, color = "black", shape = 21
  ) +
  geom_tippoint(
    data = subset(tree_df, grepl("RODRIGUES", label, fixed = TRUE)),
    aes(fill = "Rodrigues"),
    size = 3, stroke = 0.1, color = "black", shape = 21
  ) +
  geom_tippoint(
    data = subset(tree_df, grepl("LOUIS", label, fixed = TRUE)),
    aes(fill = "Port-Louis"),
    size = 3, stroke = 0.1, color = "black", shape = 21
  ) +
  geom_tippoint(
    data = subset(tree_df, grepl("FLACQ", label, fixed = TRUE)),
    aes(fill = "Flacq"),
    size = 3, stroke = 0.1, color = "black", shape = 21
  ) +
  geom_tippoint(
    data = subset(tree_df, grepl("FLACQ", label, fixed = TRUE)),
    aes(fill = "Flacq"),
    size = 3, stroke = 0.1, color = "black", shape = 21
  ) +
  geom_tippoint(
    data = subset(tree_df, grepl("MOKA", label, fixed = TRUE)),
    aes(fill = "Moka"),
    size = 3, stroke = 0.1, color = "black", shape = 21
  ) +
  geom_tippoint(
    data = subset(tree_df, grepl("SAVANNE", label, fixed = TRUE)),
    aes(fill = "Savanne"),
    size = 3, stroke = 0.1, color = "black", shape = 21
  ) +
  geom_tippoint(
    data = subset(tree_df, grepl("GRAND", label, fixed = TRUE)),
    aes(fill = "Grand Port"),
    size = 3, stroke = 0.1, color = "black", shape = 21
  ) +

  #geom_tippoint(aes(
  # subset=(grepl('REU',label,fixed=TRUE)==TRUE), fill='Reunion'),size=2, align=F, stroke=0.1,color='grey60',shape=21)+
  #geom_tippoint(aes(
  #subset=(grepl('rodrigues',label,fixed=TRUE)==TRUE), fill='Rodrigues'),size=2, align=F, stroke=0.1,color='grey60',shape=21)+
  
  #scale_fill_manual(values=c("palevioletred3", 'indianred2',"maroon3", "thistle3","mistyrose1","hotpink4","ivory3",'royalblue'),
  #                  name='Location')+
  scale_fill_manual(values=palette6,
                    name='Location')+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 month")+
  
  theme(axis.text=element_text(size=10))+
  ylim(c(0,340))
p_tree

quartz.save(file = "FIG4B_MCCtree_with_travel_locations.pdf", type = "pdf")


### age density

age_density<-read.table('data/IO_n324.RootAge.txt',header=T)
age_density$date2<-as.Date(date_decimal(as.numeric(age_density$Date)))
age_density$days<-as.Date(cut(age_density$date2,breaks = "day",start.on.monday = FALSE))

tmrca<-ggplot()+
  theme_classic()+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  geom_area(data=age_density,aes(x=date2,y=as.numeric(density)), fill='navyblue',alpha=0.5)+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "1 month")
  
tmrca


########## Markov Jumps
## Fig 4D-F

library(lubridate)

all_jumps<-read.csv('data/IOn324.500mlv2.Combined.history_summary.csv') #done after combining results #zipped file provided
#above file obtained by running following command
#java -cp beast-mcmc/build/dist/beast.jar dr.app.tools.TreeMarkovJumpHistoryAnalyzer -mrsd 2025.455 IO_n324.rlxd.skygrid.dscrt.500mlv2.location.history.Combined.trees IOn324.500mlv2.Combined.history_summary.csv
all_jumps<-subset(subset(all_jumps,endLocation!='Mauritius_unknown'),endLocation!='Mauritius_reunion')
all_jumps<-subset(subset(all_jumps,startLocation!='Mauritius_unknown'),startLocation!='Mauritius_reunion')

all_jumps[all_jumps=="Black Ricker"]<-"Black River" #fix typo in the documnet

all_jumps$date<-as.Date(date_decimal(all_jumps$time),format="%YY-%mm-%dd")
all_jumps$month<-floor_date(as.Date(all_jumps$date), 'month')
head(all_jumps)

all_jumps_summarized<-
  all_jumps %>% 
  group_by(startLocation,endLocation,month) %>%
  summarize(count=n()) %>%
  mutate(mean=count/7100) %>% #7100 number of trees after burn-in, comibing history.trees file
  ungroup()


all_jumps_summarized$month_num<-as.numeric(format(all_jumps_summarized$month,'%m'))
all_jumps_summarized$month_name<-format(all_jumps_summarized$month,'%m-%b')
all_jumps_summarized$year_num<-as.numeric(format(all_jumps_summarized$month,'%Y'))

#all_jumps_summarized<-left_join(all_jumps_summarized,locations_coordinates, by=c("startLocation"="location"))
#all_jumps_summarized<-left_join(all_jumps_summarized,locations_coordinates, by=c("endLocation"="location"))


all_jumps_summarized_destinations_mru<-subset(subset(subset(all_jumps_summarized,endLocation!='Reunion'),endLocation!='Mayotte'),startLocation!='Mayotte') %>% group_by(endLocation,month)  %>% 
  mutate(summarised_count=sum(count)/7100) %>%
  select(endLocation,month,summarised_count) %>% unique()

e<-ggplot(subset(subset(all_jumps_summarized_destinations_mru,endLocation!='Reunion'),endLocation!='Mayotte'),aes(x=month))+
  theme_minimal()+
  #scale_fill_manual(values=c("palevioletred3", 'indianred2',"maroon3", "thistle3","mistyrose1","hotpink4","ivory3","orange2","darkorange","goldenrod2","salmon1","tan4","khaki3"),
  #                  name='Destination')+
  scale_fill_manual(values=palette6,
                    name='Destination')+
  geom_bar(stat='identity',aes(x=month,y=summarised_count,fill=endLocation),colour='black',size=0.2)+
  #geom_density(stat='count',size=1)+
  
  theme(legend.position = c(0.2,0.5))+
  scale_x_date(breaks='1 month', date_labels="%b\n%Y")+
  xlab("Date")+
  ylab("No. CHIKV Introductions\ninto Mauritius Districts")
e

all_jumps_summarized_origins_mru<-subset(subset(subset(all_jumps_summarized,endLocation!='Reunion'),endLocation!='Mayotte'),startLocation!='Mayotte') %>% group_by(startLocation,month)  %>% 
  mutate(summarised_count=sum(count)/7100) %>%
  select(startLocation,month,summarised_count) %>% unique()

f<-ggplot(all_jumps_summarized_origins_mru,aes(x=month))+
  theme_minimal()+
  #scale_fill_manual(values=c("palevioletred3", 'indianred2',"maroon3", "thistle3","mistyrose1",'royalblue',"hotpink4","ivory3","orange2","darkorange","goldenrod2","salmon1","tan4","khaki3"),
  #                  name='Origin')+
  scale_fill_manual(values=palette6,
                    name='Origin')+
  geom_bar(stat='identity',aes(x=month, y=summarised_count,fill=startLocation),colour='black',size=0.2)+
  #geom_density(stat='count',size=1)+
  
  theme(legend.position = c(0.2,0.5))+
  scale_x_date(breaks='1 month', date_labels="%b\n%Y")+
  xlab("Date")+
  ylab("No. CHIKV Introductions\ninto Mauritius Districts")
f
plot_grid(e,f,ncol=2)




library(tidyverse)
library(countrycode)
library(migest)

# use dictionary to get region to region flows
d <- subset(subset(all_jumps_summarized,endLocation!='Mayotte'),startLocation!='Mayotte') %>%
  group_by(month, startLocation, endLocation) %>%
  summarise_all(mean) %>%
  ungroup()
d

pb <- d %>%
  #filter(dest == 'Africa') %>%
  mutate(flow = mean) %>%
  dplyr::select(startLocation, endLocation, flow)
pb

D<-mig_chord(x = pb, 
          # order of regions
          #order = rev(unique(pb$orig)),
          # spacing for labels
          preAllocateTracks = list(track.height = 0.3),
          # colours
          grid.col = palette6) 

D

plot_grid(D,e,f,ncol=3,labels=c("D","E","F"))



pal <- c("palevioletred3", "royalblue", "thistle3", 'indianred2', "hotpink4","mistyrose1","ivory3","maroon3")

palette <- c(
  "#FFF0F5", # very light pink (lavender blush)
  "#FFE4EC",
  "#FFD6E5",
  "#FFC7DD",
  "#FFB8D5",
  "#F9A8CC",
  "#F095C1",
  "#E681B5",
  "#D96DA8",
  "#C95A98",
  "#B84A87",
  "#9E3B73", # deep rose
  "#2B6CB0"  # standout blue
)

palette6 <- c(
  "Black River"="#FAF6F0", # ivory
  "Flacq" = "maroon3", # very light pink
  "Grand Port"= "#E9BFD2", # pale pink
  "Moka" ="orchid3", # dusty pink
  "Pamplemousses"="indianred2", # rose
  "Plaine Wilhems"=  "maroon4", # deeper rose
  "Port-Louis"     = "thistle3", # muted fuchsia accent
  "Riviere du Rempart"= "#A95C9A", # soft purple
  "Reunion"="royalblue",  # standout blue
  "Savanne"  = "#F3DCE6", # muted violet
 # "#7A3E6A", # plum
  "Rodrigues" = "ivory3" # wine / maroon
  #"#5A3A2C" # subtle brown (kept minimal)

)
swatch(palette6)

        
               
               