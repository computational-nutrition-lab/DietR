### Project website can be found here:
https://riesadohara.github.io/test-Rmarkdown3/

## This test-Rmarkdown3 repo is one for practicing building GitHub pages with R markdown files as its pages.  
This GitHub page template was copied from https://crumplab.github.io/LabJournalWebsite/index.html.
See the README file of their repo for an excellent tutorial.

## Memorandom of how to update my GitHub page.

1. Open up the R project file in the folder for your repo on your local computer (.rproj file)
2. Edit the .rmd files in R-studio3
3. Recompile website (build website when index.rmd is loaded), or knit individual .rmd files. This will produce a .html file in the /docs folder. 
4. Send your changes back to the online github repository. Wait a couple seconds, your changes should now be served on your website.

### To add pages or tabs... 
Edit "_site.yml" file.

name: "DietR Tutorial name"
navbar:
  title: "DietR Tutorial title"
  left:
  - text: Home             # Tab name
    href: index.html
  - text: ASA24            # Tab name
    menu:
    - text: ASA24_1            # Page name inside the tab ASA24
      href: ASA24_1.html       # html name for this page
    - text: ASA24_2             
      href: ASA24_2.html       
  - text: NHANES           # Tab name
    menu:
    - text: NHANES_1           # Page name inside the tab NHANES
      href: Links.html         # html name for this page
    - text: NHANES_2           
      href: thing1.html
output_dir: "docs"
