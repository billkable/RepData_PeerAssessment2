library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)

# Load and pre-process data
## Load Reference Data
### State List
ref.state.list.df <- fread("./ref.state.list.csv")
### Event Type Catalog
ref.evtype.list.df <- fread("./ref.evtype.list.csv")
### Event Type Clean Up Mapping List
mapped.evtypes.dt <- fread("./cleaned.evtypes.dt.csv") %>%
        filter(EVTYPE.MAPPED != "")

# # Load Raw CSV

#stormdata.raw.df <- read.csv(bzfile("./repdata-data-StormData.csv.bz2"))
# 
# # Vertical filtering 
# stormdata.df <- filter(stormdata.raw.df,STATE %in% ref.state.list.df$STATE) %>%
#                 select(REFNUM,
#                        BGN_DATE,
#                        EVTYPE,
#                        FATALITIES,
#                        INJURIES,
#                        PROPDMG, 
#                        PROPDMGEXP,
#                        CROPDMG,
#                        CROPDMGEXP)

# Clean conversion to data table
# write.csv(stormdata.df,"./stormdata.csv")
stormdata.cleaned.df <- fread("./stormdata.csv")

stormdata.cleaned.df$BGN_DATE <- as.Date(stormdata.cleaned.df$BGN_DATE, "%m/%d/%Y")
stormdata.cleaned.df$EVTYPE <- toupper(stormdata.cleaned.df$EVTYPE)

# Preprocess Property Damage 
## Clean Crop impact
stormdata.cleaned.df$CROPDMGEXPNUM <- 0
stormdata.cleaned.df$CROPDMGEXPNUM[stormdata.cleaned.df$CROPDMGEXP %in% c("k","K")] <- 1000
stormdata.cleaned.df$CROPDMGEXPNUM[stormdata.cleaned.df$CROPDMGEXP %in% c("m","M")] <- 1000000
stormdata.cleaned.df$CROPDMGEXPNUM[stormdata.cleaned.df$CROPDMGEXP %in% c("b","B")] <- 1000000000
## Clean Prop impact
stormdata.cleaned.df$PROPDMGEXPNUM <- 0
stormdata.cleaned.df$PROPDMGEXPNUM[stormdata.cleaned.df$PROPDMGEXP %in% c("k","K")] <- 1000
stormdata.cleaned.df$PROPDMGEXPNUM[stormdata.cleaned.df$PROPDMGEXP %in% c("m","M")] <- 1000000
stormdata.cleaned.df$PROPDMGEXPNUM[stormdata.cleaned.df$PROPDMGEXP %in% c("b","B")] <- 1000000000
stormdata.cleaned.df$PROPDMGDOL <- stormdata.cleaned.df$PROPDMG * stormdata.cleaned.df$PROPDMGEXPNUM

## Build Economic impact data set
stormdata.econ.dt <- select(stormdata.cleaned.df, REFNUM,
                            BGN_DATE,
                            EVTYPE,
                            PROPDMGDOL) %>%
                        filter(PROPDMGDOL > 0) %>%
                        arrange(desc(PROPDMGDOL))

stormdata.econ.summary.dt <- group_by(stormdata.econ.dt,EVTYPE) %>%
        summarize(TOTAL = sum(PROPDMGDOL)) %>%
        data.table

### Clean Event Data by replacing dirty records with mapped event reference types
stormdata.econ.summary.cleaned.dt <- inner_join(mapped.evtypes.dt,stormdata.econ.summary.dt, by = c("EVTYPE")) %>%
        select(EVTYPE.MAPPED,TOTAL)
setnames(stormdata.econ.summary.cleaned.dt,c("EVTYPE","TOTAL"))
stormdata.econ.summary.orig.dt <- inner_join(ref.evtype.list.df,stormdata.econ.summary.dt, by = c("EVTYPE"))

stormdata.econ.summary.dt <- rbind(stormdata.econ.summary.cleaned.dt,
                                 stormdata.econ.summary.orig.dt) %>%
                                arrange(desc(TOTAL))

# Preprocess Human Health Impacts
stormdata.cleaned.df$HHIMPACT <- stormdata.cleaned.df$FATALITIES + stormdata.cleaned.df$INJURIES

# Build Human Health Impact Dataset
stormdata.hh.dt <- select(stormdata.cleaned.df, REFNUM,
                                                BGN_DATE,
                                                EVTYPE,
                                                HHIMPACT) %>%
                        filter(HHIMPACT > 0) 

# Build Human Health Impact Summary
stormdata.hh.summary.dt <- group_by(stormdata.hh.dt,EVTYPE) %>%
        summarize(TOTAL = sum(HHIMPACT)) %>%
        data.table %>%
        arrange(desc(TOTAL))

### Clean Event Data by replacing dirty records with mapped event reference types

stormdata.hh.summary.cleaned.dt <- inner_join(mapped.evtypes.dt,stormdata.hh.summary.dt, by = c("EVTYPE")) %>%
                                select(EVTYPE.MAPPED,TOTAL)
setnames(stormdata.hh.summary.cleaned.dt,c("EVTYPE","TOTAL"))
stormdata.hh.summary.orig.dt <- inner_join(ref.evtype.list.df,stormdata.hh.summary.dt, by = c("EVTYPE"))

stormdata.hh.summary.dt <- rbind(stormdata.hh.summary.cleaned.dt,
                                 stormdata.hh.summary.orig.dt) %>%
                                arrange(desc(TOTAL))

                   
# Build aggregrate summary for events types
stormdata.hh.finalsummary.dt <- stormdata.hh.summary.dt[stormdata.hh.summary.dt$EVTYPE %in% ref.evtype.list.df$EVTYPE]
stormdata.econ.finalsummary.dt <- stormdata.econ.summary.dt[stormdata.econ.summary.dt$EVTYPE %in% ref.evtype.list.df$EVTYPE]

hh.report.df <- stormdata.hh.finalsummary.dt[rownames(stormdata.hh.finalsummary.dt) %in% c(1:10) ]
hh.report.df$ID <- 1:10
hh.report.df$LABEL <- paste(str_pad(hh.report.df$ID,2,pad = "0"),"-",hh.report.df$EVTYPE)


# Top 10 reports
econ.report.df <- stormdata.econ.finalsummary.dt[rownames(stormdata.econ.finalsummary.dt) %in% c(1:10) ]
econ.report.df$ID <- 1:10
econ.report.df$LABEL <- paste(str_pad(econ.report.df$ID,2,pad = "0"),"-",econ.report.df$EVTYPE)

hh.p <- ggplot(hh.report.df,aes(x = LABEL, y = TOTAL)) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("Weather Event Human Health Impact (fatalities and injuries)") +
        xlab("Event Types") + ylab("Fatality or Injury recorded")

econ.p <- ggplot(econ.report.df,aes(x = LABEL, y = TOTAL)) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("Weather Event Economic Impact (Property + Crop Damage)") +
        xlab("Event Types") + ylab("Damage Impact (USD)")

print(hh.p)
print(econ.p)