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



img_width <- 20
img_height <- 20
target_size <- c(img_width, img_height)

# RGB = 3 channels
channels <- 3

# path to image folders
train_image_files_path <- "/Users/shiringlander/Documents/Github/DL_AI/Tutti_Frutti/fruits-360/Training/"
valid_image_files_path <- "/Users/shiringlander/Documents/Github/DL_AI/Tutti_Frutti/fruits-360/Validation/"



train_data_gen = image_data_generator(
  rescale = 1/255 #,
  #rotation_range = 40,
  #width_shift_range = 0.2,
  #height_shift_range = 0.2,
  #shear_range = 0.2,
  #zoom_range = 0.2,
  #horizontal_flip = TRUE,
  #fill_mode = "nearest"
)

train_image_array_gen <- flow_images_from_directory("C:/Users/Administrator/Documents/Rotation/fruits-360/Training/", 
                                                    train_data_gen,
                                                    target_size = target_size,
                                                    class_mode = "categorical",
                                                    classes = class_names,
                                                    seed = 42)