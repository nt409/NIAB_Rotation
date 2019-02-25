from google_images_download import google_images_download
 
response = google_images_download.googleimagesdownload()
arguments = {"keywords":"black sigatoka","limit":20,"print_urls":True}
path = response.download(arguments)
