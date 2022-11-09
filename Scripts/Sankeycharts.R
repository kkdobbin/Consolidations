#sankey diagram from gov categories
library(networkD3)

# ALL CONSOLIDATIONS
## https://www.data-to-viz.com/graph/sankey.html

Data_long <- as.data.frame(Data %>% count(Data$govcat_cons, Data$govcat_rec))
colnames(Data_long) <- c("source", "target", "value")
Data_long$target <- paste(Data_long$target, " ", sep="") #this step isn't doing anything could be an issue

nodes <- data.frame(name=c(as.character(Data_long$source), as.character(Data_long$target)) %>% unique()) #don't seem to me unique

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
Data_long$IDsource=match(Data_long$source, nodes$name)-1 
Data_long$IDtarget=match(Data_long$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
Sankey_all <- sankeyNetwork(Links = Data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)

# CWS CONSOLIDATIONS
CWS$govcat_rec <- as.factor(CWS$govcat_rec)
CWS$govcat_cons <- as.factor(CWS$govcat_cons)
CWS$govcat_rec <- droplevels(CWS$govcat_rec)
CWS$govcat_cons <- droplevels(CWS$govcat_cons)

Data_longCWS <- as.data.frame(CWS %>% count(CWS$govcat_cons, CWS$govcat_rec))
colnames(Data_longCWS) <- c("source", "target", "value")
Data_longCWS$target <- paste(Data_longCWS$target, " ", sep="") #this step isn't doing anything could be an issue

nodesCWS <- data.frame(name=c(as.character(Data_longCWS$source), as.character(Data_longCWS$target)) %>% unique()) #don't seem to me unique

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
Data_longCWS$IDsource=match(Data_longCWS$source, nodes$name)-1 
Data_longCWS$IDtarget=match(Data_longCWS$target, nodes$name)-1

# Make the Network
Sankey_CWS <- sankeyNetwork(Links = Data_longCWS, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)

attach(mtcars)
par(mfrow=c(1,2))
Sankey_all
Sankey_CWS

library(patchwork)
Sankey_all + Sankey_CWS
