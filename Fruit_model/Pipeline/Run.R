setwd("C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Pipeline")

# with set.seed(142) we should get
# train_indicies = [77 60 81 47 67 71 23 54 34 78 66 82 57 41 38 40 11 37 84 12 62 61  4 29 56]


run_xml_to_json<-0
run_im_classifier<-0
run_analyse_CNN_output<-1
run_analyse_SVM_output<-1

if(run_xml_to_json==1){
  system("python xmltojson.py")
}

if(run_im_classifier==1){
source('CNN_image_classifier.R') # sources 'Image_classifier_functions.R', 'parameters.R', feeds into
}else{
  source('Image_classifier_functions.R')
  source('parameters.R')
}

if(run_analyse_CNN_output==1){
  source('CNN_output_analysis.R') # feeds into
  class_preds
  corners
}

source('Disease_fake_data.R') # feeds into
source('SVM.R') # feeds into

if(run_analyse_SVM_output==1){
  source('SVM_output_analysis.R') # feeds into
  head(attr(predz,"probabilities")) # predicted class
}
