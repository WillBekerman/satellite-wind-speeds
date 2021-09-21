
loadRData <- function(fileName){
  # loads an RData file, and returns it (allows us to load file and assign variable name)
  # taken from https://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
  load(fileName)
  get(ls()[ls() != "fileName"])
}
