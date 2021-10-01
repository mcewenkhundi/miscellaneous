#project: epir chapter6
#task: downloading epir data
#author: McEwen Khundi 2021-09-10
#Modefied by: name date of modification


#Refer to chapter 2

# install the latest version of the Epi R Handbook package
pacman::p_install_gh("appliedepi/epirhandbook")


# load the package for use
pacman::p_load(epirhandbook)

# download only the linelist example data into a folder on your computer
get_data(file = "linelist_cleaned.rds")
