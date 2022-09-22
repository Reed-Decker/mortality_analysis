# Working Directory
When using RStudio to run the Rmd file for this project, the setup cell should automatically locate the directory the file is saved to and set that as your working directory. The rstudioapi package is required for this to work properly.

If you are not using RStudio, you'll need to go to line 10 of the Rmd file, in the initial setup chunk, and replace "setwd(dirname(rstudioapi::getActiveDocumentContext()$path))" with "setwd("[Filepath to the local repository]")

# Environment control
This project uses the renv package for environmental control. When running the .Rmd file, the command to restore the renv library, included as part of the repository, is in the setup chunk. You may need to install renv before running the file. When opening either the .Rproj or Rmd file directly from desktop, the renv library should be loaded automatically.
