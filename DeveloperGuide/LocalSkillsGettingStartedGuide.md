# Local skills getting started guide

- [Introduction](#introduction)

## Introduction

The Local Skills dashboard is an R shiny dashboard displaying published
local data from a variety of sources in an easy to navigate format. To
support local skills planning, the dashboard covers topics such as
employment, qualifications, and education outcomes across England.

This guide is for developers who are new to the dashboard and would like 
to set up the project and run the dashboard in R for the first time.

## View the live dashboard

Anyone can view the public dashboard, here:

<https://department-for-education.shinyapps.io/local-skills-dashboard/>

We also have a 'pre production dashboard' where we test the dashboard 
before publishing. The URL is below but you will need to contact the project 
team for access:

<https://department-for-education.shinyapps.io/local-skills-dashboard-preprod/>

## Prepare to run the dashboard in R

### Step 1 - Download the required software

- Open Software Center on your laptop. You can navigate to it by searching 
“Software Center” in the search section of the task bar.

- Download the following software:

  - R for Windows 4.4 or higher
  - RStudio
  - RTools 4.4 or higher

> [!CAUTION]
> It is important that all your software is the same version. 
For example, if R is 4.4, your RTools also needs to be 4.4.

> You can find the version number under the name of the software in the
Software Center or by clicking on the software and looking for “Version:” 
in the information.

> You can also check the R version you’re using in RStudio by checking 
under the console tab (1) or the information shown in 
the console on start up (2) as shown in the image below. Or, you can 
use the function getRversion() in the console.

![R version is shown under the console tab](./Images/R_version_number.png)

> [!NOTE]
> If you have the latest version of R downloaded but RStudio still shows an 
older one, use [this guide to help you switch to the version of R you need](https://bioinformatics.ccr.cancer.gov/docs/rtools/R%20and%20RStudio/2.6_switching_r_version/).

- You will also need to install git, which is a version control software. You 
can downloaded this directly from the [git for windows website](https://gitforwindows.org/). 
Once the installer has started, follow the instructions as provided 
in the Git Setup wizard screen until the installation is complete.

### Step 2 - Clone the repository

The code for the dashboard is stored in [a repository on GitHub](https://github.com/dfe-analytical-services/lsip_dashboard). 
This allows us to collaborate on the project using version control. 

You will need to 'clone' the repository ('repo'), so that you can
work on the code on your own machine. To do this, navigate to a folder where
you would like to store your copy of the repo. This should be outside of your 
OneDrive area. To do this, click on documents, then click in the file path
and delete everything up to your username (1). This should take you to your user 
folder outside of OneDrive. You can make a folder there to store all your 
repos (2).

![Creat a Repo folder within your user folder](./Images/Repo_folder.png)

One you have done this, right click in your new repos folder and click 'Open Git
Bash here'. If this doesn't appear as an option, go back to step one and install
Git.

![Right click to open Git Bash](./Images/Gitbash.png)

Next, go to the [repository](https://github.com/dfe-analytical-services/lsip_dashboard),
click on code in the top right hand corner and then copy the URL in the
HTTPS section.

![Click on the code button to copy the HHTPS address](./Images/Clone.png)

In the Git Bash console window you opened earlier, after the $ type `git clone`,
leave a space and then paste the URL. Press Enter. This should start cloning 
the repository. You only need to do this once, unless you later delete your 
cloned folder. 

> [!CAUTION]
> You can't paste into the Git Bash using Ctrl + V, instead you can right 
click and choose paste, or you can click the middle roller button on your
mouse if you have one.

There are more details about using git and github, and a link to full training,
in the main developer guide.

# Run the dashboard in R
