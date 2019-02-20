library(tensorflow)
# install_tensorflow(method = "auto")
a = tf$constant(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), shape=c(2, 3), name='a')
b = tf$constant(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), shape=c(3, 2), name='b')
c = tf$matmul(a,b)
sess = tf$Session(config=tf$ConfigProto(log_device_placement=TRUE))


use_condaenv("r-tensorflow")
sess <- tf$Session()

sessionInfo()
reticulate::conda_list()
reticulate::py_config()

###
#trying to check gpu and python working fine

library(keras)
use_condaenv("r-tensorflow")


library(keras)
system("conda config --set ssl_verify false")
install_keras(tensorflow = "gpu")
install_keras()
mnist <- dataset_mnist()

reticulate::py_discover_config("keras")
reticulate::py_discover_config("tensorflow")

###
