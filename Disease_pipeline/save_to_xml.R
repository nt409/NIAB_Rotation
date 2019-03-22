library(XML)
library(xml2)

data<- t(c(params$input_labels,params$annot_file))
data<-as.data.frame(data)
colnames(data)<- c("xml_path","json_file")

xml <- xmlTree()
xml$addTag("document", close=FALSE)
for (i in 1:nrow(data)) {
  xml$addTag("row", close=FALSE)
  for (j in names(data)) {
    xml$addTag(j, data[i, j])
  }
  xml$closeTag()
}
xml$closeTag()

saveXML(xml,file = paste(params$folder_containing_annotations,"xml_pathnames.xml",sep="/"))

