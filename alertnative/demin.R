#install.packages("reticulate") #install package reticulate for use Python univers
library("reticulate") #run library reticulate



# Check the version of Python and localisation in computer
#py_config()

# Set the path to the Python executable file

#WARNING : it's important to change the address with the address of your computer where you stored conda environment
python_path <- "C:/Users/dallo/AppData/Local/r-miniconda/envs/r-reticulate/python.exe"

#WARNING : it's important to change the address with the address of your computer where you stored the demin.py file
script_path <- "C:/Users/dallo/Downloads/demin.py"


###  Python file calling  ###

cat("Enter difficulty level: ")
level <- readLines("stdin",n=1)
cat("You entered difficulty level")
str(level)
cat( "\n" )

if (level =="low" || level =="medium" || level =="hard"){
    system2(python_path, args = c(script_path, level))} else{ print("Invalid input, try again ...")}
