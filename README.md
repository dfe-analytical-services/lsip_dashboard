<h1 align="center">
  <br>
Local Skills Dashboard  <br>
</h1>

<p align="center">
  <a href="#introduction">Introduction</a> |
  <a href="#requirements">Requirements</a> |
  <a href="#how-to-use">How to use</a> |
  <a href="#how-to-contribute">How to contribute</a> |
  <a href="#contact">Contact</a>
</p>

---

## Introduction 

The Local Skills dashboard provides published local data from a variety of sources in an easy to navigate format. To support local skills planning, the dashboard covers topics such as employment, qualifications, and education outcomes across England.

- Production - https://department-for-education.shinyapps.io/local-skills-dashboard/
- Pre-production - https://department-for-education.shinyapps.io/local-skills-dashboard-preprod/


---

## Requirements

### i. Software requirements (for running locally)

- Installation of R Studio 1.2.5033 or higher

- Installation of R 3.6.2 or higher

- Installation of RTools40 or higher

### ii. Programming skills required (for editing or troubleshooting)

- R at an intermediate level, [DfE R training guide](https://dfe-analytical-services.github.io/r-training-course/)

- Particularly [R Shiny](https://shiny.rstudio.com/)
  
---

## How to use

### Running the app locally

1. Clone or download the repo. 

2. Open the R project in R Studio.

3. Run `renv::restore()` to install dependencies.

4. Run `shiny::runApp()` to run the app locally.


### Packages

Package control is handled using renv. As in the steps above, you will need to run `renv::restore()` if this is your first time using the project.

### Deployment

- The app is deployed to the department's shinyapps.io subscription using GitHub actions, to [https://department-for-education.shinyapps.io/local-skills-dashboard/](https://department-for-education.shinyapps.io/local-skills-dashboard). The yaml file for this can be found in the .github/workflows folder.

### Navigation

In general all .r files will have a usable outline, so make use of that for navigation if in RStudio: `Ctrl-Shift-O`.

### Code styling 

The function tidy_code() is created in the Rprofile script and therefore is always available in the RStudio console to tidy code according to tidyverse styling using the styler package. This function also helps to test the running of the code and for basic syntax errors such as missing commas and brackets.

### Key files
- /Data/2_...: Geographical lookup and shape files
- /Data/2_...: All the raw data files imported for use in the dashboard
- /Data/3_...: Text used within the dashboard
- /Data/AppData: The datafiles the app runs off
- /DeveloperGuide: Guide to updating the data in the dashboard
- /R: scripts run when the app runs
- /renv: package alignment
- /tests: testing changes to the app (not currently used)
- /www: web specific files (cookies, css)
- /ExtractLoadData.R: extract and load raw data
- /TransformData.R: converts raw data into the data shape used by the app
- /server.R: calculates the data shown in the dashboard reacting to user input
- /ui.R: controls the layout of the dashboard
- /global.R: loads R package libraries and reusable functions


---

## How to contribute

### Flagging issues

If you spot any issues with the application, please flag it in the "Issues" tab of this repository, and label as a bug.

### Merging pull requests

Only members of the Statistics Development and UFS team can merge pull requests. Add lauraselby, cjrace and pjames as requested reviewers, and the team will review before merging.

---

## Contact

If you have questions about the dashboard or data within it, please contact us at ufs.contact@education.gov.uk.
