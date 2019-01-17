Data_labelled <- function(){

class_names <- c('Apple_Braeburn',
              'Apple_Golden_1',
              'Apple_Golden_2',
              'Apple_Golden_3',
              'Apple_Granny_Smith',
              'Apple_Red_1',
              'Apple_Red_2',
              'Apple_Red_3',
              'Apple_Red_Delicious',
              'Apple_Red_Yellow',
              'Apricot',
              'Avocado',
              'Avocado_ripe',
              'Banana',
              'Banana_Red',
              'Cactus_fruit',
              'Cantaloupe_1',
              'Cantaloupe_2',
              'Carambula',
              'Cherry_1',
              'Cherry_2',
              'Cherry_Rainier',
              'Cherry_Wax_Black',
              'Cherry_Wax_Red',
              'Cherry_Wax_Yellow',
              'Clementine',
              'Cocos',
              'Dates',
              'Granadilla',
              'Grape_Pink',
              'Grape_White',
              'Grape_White_2',
              'Grapefruit_Pink',
              'Grapefruit_White',
              'Guava',
              'Huckleberry',
              'Kaki',
              'Kiwi',
              'Kumquats',
              'Lemon',
              'Lemon_Meyer',
              'Limes',
              'Lychee',
              'Mandarine',
              'Mango',
              'Maracuja',
              'Melon_Piel_de_Sapo',
              'Mulberry',
              'Nectarine',
              'Orange',
              'Papaya',
              'Passion_Fruit',
              'Peach',
              'Peach_Flat',
              'Pear',
              'Pear_Abate',
              'Pear_Monster',
              'Pear_Williams',
              'Pepino',
              'Physalis',
              'Physalis_with_Husk',
              'Pineapple',
              'Pineapple_Mini',
              'Pitahaya_Red',
              'Plum',
              'Pomegranate',
              'Quince',
              'Rambutan',
              'Raspberry',
              'Salak',
              'Strawberry',
              'Strawberry_Wedge',
              'Tamarillo',
              'Tangelo',
              'Tomato_1',
              'Tomato_2',
              'Tomato_3',
              'Tomato_4',
              'Tomato_Cherry_Red',
              'Tomato_Maroon',
              'Walnut')


labeller <- function(n){
  pathname <- paste("C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Fruit_data/fruits-360/Training",class_names[n],sep = '/')
  files <- list.files(path = pathname, pattern=".jpg",all.files=T, full.names=F, no.. = T) 
  array <- cbind(files,b=n)
  return(array)
}

Fruit_train_data <- labeller(1)
for( i in 2:(length(class_names))){
  new_data <- labeller(i)
  Fruit_train_data <- rbind(Fruit_train_data,new_data)
}


return(Fruit_train_data)
}

# to check a random image from the data set
image_number <- 40000
N <- as.numeric(Fruit_train_data[image_number,2])
N
class_names[N]

path <- paste("C:/Users/Administrator/Documents/GitHub/NIAB_Rotation/Fruit_model/Fruit_data/fruits-360/Training",class_names[N],as.character(Fruit_train_data[image_number,1]),sep = '/')
im <- readImage(path)
dim(im) # im is the array
imageShow(im)