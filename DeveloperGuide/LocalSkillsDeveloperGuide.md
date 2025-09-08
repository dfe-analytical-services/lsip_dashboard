# Local skills developer guide

-   [Introduction](#introduction)
-   [Working practices](#working-practices)
    -   [Use of Git](#use-of-git)
    -   [Project management](#project-management)
    -   [workflow and quality assurance](#workflow-and-quality-assurance)
-   [Contributing](#contributing)
    -   [Run the dashboard in R](#run-the-dahsboard-in-r)
    -   [Routine data updates](#routine-data-updates)
    -   [Adding new features](#adding-new-features)
    -   [Fixing bugs](#fixing-bugs)
-   [Troubleshooting](#troubleshooting)
-   [Monitoring dashboard usage](#monitoring-dashboard-usage)
- ANNEX??? Check lists???

## Introduction {#introduction}

The Local Skills dashboard is an R Shiny dashboard displaying published
local data from a variety of sources, in an easy to navigate format. To
support local skills planning, the dashboard covers topics such as
employment, qualifications, and education outcomes across England.

This guide is for developers to update data or change the dashboard.

> [!IMPORTANT] If you are new to the project, you should follow the 
[Getting started guide](https://github.com/dfe-analytical-services/lsip_dashboard/blob/main/DeveloperGuide/LocalSkillsDeveloperGuide.md) 
before returning here.

## Working practices {#working-practices}

This section describes some important ways of working employed on this
project. You should read this section before you work on the dashboard
for the first time, and refer back to it regularly to ensure you are
following agreed practice.

### Use of Git {#use-of-git}

This project uses Git, a version control software package, and GitHub,

These tools allow all project members to collaborate effectively on the
project, and record the project history.

> [!IMPORTANT] If you have not used Git before, you should undertake
some basic Git training before you read on. For example, 
[Introduction to Git on the ONS Learning Hub](https://analysisfunction.civilservice.gov.uk/training/introduction-to-git/) 
or [Git and GitHub courses on datacamp](https://www.datacamp.com/courses-all?q=git)

#### Use of branches

There are two core branches in the project repository:

  -   **Main** is the public facing branch - any pushes to this will be
      live on the web. Commits should never be made directly to this
      branch.
  
  -   **Development** is the pre-production (preprod) branch - any pushes
      to this will be live on the private preprod site. This allows us to
      test changes before they go live. Commits should never be made
      directly to this branch.

All other branches should follow this naming convention:

  -   **For data updates:** ‘update/nameOfDatasetMMYY’ (where MMYY is
      numeric month and year of the data release, e.g.
      update/onlineJobAds0825)
  -   **For problems that need fixing:** bug/summaryOfBug (e.g.
      bug/brokenMapAxis)
  -   **For new features:** feature/summaryOfFeature (e.g.
      feature/downloadableGraphs)

When working with branches, follow these best practice tips:

-   Branch names should be meaningful, and related to the content of the
    branch (as in the examples above), never 'qaChanges' or 'testBranch'
    etc.
-   Work on a branch should be limited to one coherent feature or change
    (more about this in the [project management](#project-management)
    section).
-   As a norm, only one person should be working on a single branch.
-   Don’t let the branch get too out of date with the development
    branch. If it does, consider updating it with merge or rebase
    (guidance on the two options
    [here](https://www.atlassian.com/git/tutorials/merging-vs-rebasing)
    and on resolving any merge conflicts
    [here](https://www.atlassian.com/git/tutorials/using-branches/merge-conflicts))

#### Committing

When committing work to a branch, follow these best practice tips:

  -   Keep commits small and commit frequently
  -   Don’t change lots of files in the same commit
  -   Keep your commit messages short and just specific enough that you or
      someone else can understand what changed (fewer than 50 character)
  -   Use the imperative tense (as if giving an order), e.g. ‘Fix typos in
      developer guide’ or ‘Update APS data to Jan 25 release’

### Project management {#project-management}

Project management is recorded on GitHub as follows:

1. New work is recorded [on the project board as a GitHub issue](https://github.com/orgs/dfe-analytical-services/projects/10) 
for the project, and added to the project board as either an issue to be scoped (ideas that need to be translated
into clearly defined changes to the dashboard) or an issue for the backlog (a 
clear and coherent feature or change that can be made on the dashboard.)

2. Once an issue has been scoped, the existing issue should be marked as complete
and new issue(s) added to backlog. Scoped issues may result in several new issues
on the backlog, in order to break up the work into coherent features or changes.

3. The project board is reviewed weekly by the project team and work from
the backlog is assigned to team members for the next 'sprint' 
(a period of work with a target end data for the updates to be completed and live
on the dashboard). The length of sprints may vary, for example to align with
the release of a new data update.

4. Each issue on the backlog should be completed on one branch. The branch should
be linked to the issue once created.

5. A new [milestone](https://github.com/dfe-analytical-services/lsip_dashboard/milestones) 
is created for each sprint, named with the next dashboard 
version number, which loosely follows the major.minor.patch system, i.e.

  v1.0.0 Launch of the dashboard
  v.1.1.0 Minor re-design of the dashboard or substantial new feature
  v.1.1.1 Routine data update, bug fix or small new feature
  v2.0.0 Major re-design of the dashboard

All issues and pull requests relating to the sprint are tagged with the milestone. 

6. As sprint the is in progress, issues should be moved across the planner board 
columns to show their progress. 

7. Once a sprint is complete (updates live on the dashboard), the milestone 
should be checked to ensure all associated issues and pull requests have been 
closed (or unassigned from the milestone), and the milestone should be closed. 
Any merged or redundant branches should be deleted.

TO VIEW PAST QA....???

### Workflow and quality assurance {#workflow-and-quality-assurance}

#### Deploying changes into pre-production (our test site)

For this stage, the analysis should be... the QA'er should be...

2.	Analyst: Create a new branch for your work, link it to the issue on the planner board and tag it with the next milestone. Remember to follow branch naming convention and best practice [link]
3.	Analyst: Start work on your branch. If multiple updates are made to the development branch while you are still working on your branch, use rebase or merge [link to guidance] to update your branch with the latest changes as you go. This will make it easier to merge your changes into development when you are finished.
4.	Analyst: When you have completed your work, create a pull request. Add a description to the pull request summarising the change and including a list of QA tasks, using the checkbox formatting (see annex for example QA checklists for different types of analysis, which you can paste into the description).
5.	Analyst: Assign a reviewer to QA the work. This should be someone trained to work on the dashboard, who has had minimal involvement with the work to be QA’d. Speak to Jane or Paul if you are not sure who can QA.
6.	QA’er: Checkout the branch and complete the QA tasks on the list. When you have complete each task, check it off on the checklist. 
7.	QA’er: If you identify issues as part of the QA process, add a comment on the most relevant script. To do this, go to the ‘Files changed’ tab on the pull request, click on the relevant file, find the line of code and click the plus icon next to that line of code.
NOTE: you should always add a comment on a script, even if the comment is not clearly linked to a specific line of code. This is because allows helpful features, like marking the comment as resolved. If you’re not sure, add you’re comment to the first line of the ui.R script.
NOTE: you can post comments as you go through the QA, or when you write a comment you can click ‘start a review’ /  ’add to review’ and post them all at the end. This can sometimes be helpful, as you may want to go back and edit comments as you complete further QA.
8.	QA’er: When you have completed your QA, finish your review. To do this, go to the changes tab, click the green ‘Review changes’ button in the top right hand corner, add a comment and choose whether to approve the PR (if not issues identified) or request changes (if issues identified). In your comment you should confirm that you have completed all listed QA, note any additional QA you have done, confirm the methods you used (e.g. ran dashboard locally for visual inspection, ran code to check for errors, ran changed code line by line to sense check). Also confirm here if there are issues to resolve before you are happy to approve. 
NOTE: This step will release all other comments, if you have been adding them to your review instead of posting them as you go.
9.	Analyst: Address any issues identified. It will usually be easiest to address each issue as a separate commit (unless they are connected). Once you have addressed each issue, reply to the comment confirming what you have done and linking to the commit that addresses the problem.
10.	QA’er: Review each change that has been made in response to issues you have raised. If you are happy with the resolution, reply to or thumbs up the comment to confirm, and mark the comment as resolved. If you think the change does not resolve the issue, reply to say so.
11.	Analyst and QA’er: repeat steps 9 and 10 until all issues have been resolved. 
12.	QA’er: When you are happy that all issues have been resolved, go again to the ‘review changes’ button on the changes tab, click to approve the PR and add a brief comment to confirm. 
NOTE: if there are any merge conflicts on the PR, these will need to be resolved and a final QA completed before you approve the changes. Where possible it is best to resolve merge conflicts before QA begins for this reason.
13.	QA’er: Click the merge button to merge the changes into the development branch. 

#### Deploying changes to the live site

## Contributing {#contributing}

### Run the dashboard in R

1.  Connect to the git hub branch you want to run and pull to ensure you
    have the latest version:

    -   **Main** is the public facing branch - any pushes to this will
        be live on the web.

    -   **Development** is the pre-production (preprod) branch - any
        pushes to this will be live on the private preprod site.

    -   Or any other branch you would like to run.

The github page is here:
<https://github.com/dfe-analytical-services/lsip_dashboard>.

2.  Align your packages with the dashboards packages in your branch
    using renv: `renv::restore()`

3.  Run the dashboard. The dashboard will open in a new window:
    `runApp()`

The dashboard runs off the data held within /Data/AppData, so will just
run without having to have data within your local Environment.

There may be times when you want to look at the dashboard in a browser
(checking formatting across browsers or window sizes, or checking
download functions). To do this click on the “Open in Browser” button at
the top of the dashboard pop up window.

### Routine data updates {#routine-data-updates}

The Data Sources tab of the dashboard has a list of all the datasets and
their next release date.

**Update schedule**

These are the common release points, however be aware that there may be
revisions to any of the datasets throughout the year.

-   *Annual population survey* is released four times a year: 1) Jan 2)
    Apr 3) Jul/Aug/Sept 4) Oct. We update employment volumes and split
    by industry at every release. However:

    -   Highest qualification is only updated in the APS once a year in
        the Apr release.

-   *Individualised learner record* is updated quarterly but we only
    update with the final data, usually release in November (the in year
    data is provisional and subject to lag).

-   *UK business count* is released and updated in the dashboard once a
    year in Sept/Oct.

-   *Business demography* is released and updated in the dashboard once
    a year in Nov.

-   *Destination measures* are released and updated in the dashboard
    once a year in Oct.

-   *Job adverts* were released for the first time in Feb 23. No release
    schedule has been announced.

-   *Skills Imperative 2035* was released in Mar 23. No release schedule
    has been announced.

| Month | Release        |             |              |
|-------|----------------|-------------|--------------|
| Jan   | APS emp vols   | APS emp ind |              |
| Feb   |                |             |              |
| Mar   |                |             |              |
| Apr   | APS emp vols   | APS emp ind | APS emp qual |
| May   |                |             |              |
| Jun   |                |             |              |
| Jul   | APS emp vols   | APS emp ind |              |
| Aug   |                |             |              |
| Sep   | Business count |             |              |
| Oct   | APS emp vols   | APS emp ind | Destinations |
| Nov   | ILR            |             |              |
| Dec   |                |             |              |

**Update process**

1.  Make yourself a new github branch off (probably) the latest public
    version, so off Main. Pull and run `renv::restore()` to align
    packages with the dashboard.

*For non-NOMIS datasets:*

2a. Download the latest version of the dataset. If you need help finding
the dataset source, have a look at the “Data Sources” tab in the
dashboard - this gives info and links to data sources.

2b. Navigate to /Data folder (either in R Files or in Windows Explorer).
All raw datasets are held in /Data. They are the numbered datasets.

-   1-x are geographical files (look ups and boundary files)

-   2-x are the raw local skills data files

-   3-x are excel files which populate tables within the dashboard and a
    3-2_dataText helps populates the dynamic text in the Local Skills
    tab

2c. Once you have figured out where the dataset you want to update is
currently kept, navigate to that folder

2d. Delete the currently held dataset(s) in that folder. The folder
should now be empty.

2e. Paste the new dataset in that folder.

*For Nomis Datasets:*

You should just be able to skip to stage (4) as ExtractLoadData.R in
step (5) will pull all the latest Nomis data. However, please take note
that the highest qualifications data currently includes data from two
timeseries (NVQ and RQF), as RQF replaced NVQ in January 2022. The code
for this section will need to be updated at each release (yearly) to use
all available RQF data, and NVQ data for the remaining quarters. Once
there have been five releases of the RQF data the code can be changed to
use only the five latest releases of RQF data.

4.  Dashboard admin:

<!-- -->

-   You need to update the dashboard’s Data Sources tab with information
    about your new data. Open /Data/3-1_DataTable/DataTable.xlsx in
    Excel. Update the relevant row with new dates, links and
    information.

-   Update the dashboard’s Version Log. This is done in ui.R under the
    “2.1.3 Version control” header. Move the current Latest Version text
    down into the Previous Version area, and repopulate the Current
    Version with new details.

-   You may need to update the Latest Period and/or the data caveats in
    the /Data/3-2_dataText/dataText.xlsx file as well. These are used to
    populate dynamic text on the dashboard’s Local Skills tab.

5.  Run the ExtractLoadData.R script. This runs all the scripts in
    /importData and goes through all the numbered folders in /Data and
    loads the datasets in each folder as a R object. As we have datasets
    from a wide range of sources that come in all kinds of formats, we
    do a lot of data cleaning, manipulating and formatting to get into a
    format the dashboard can work with.

`source("~/RProjects/lsip_dashboard/ExtractLoadData.R", echo=TRUE)`

This takes about 20 mins. You should see your environment is populated
with new, clean datasets and your Data/AppData folder has been updated
with the updated versions of the files the dashboard uses.

6.  Check any changes to the data used in the dashboard. As a step in
    the QA run this file and have a look at the outputs:

    `source("~/RProjects/lsip_dashboard/checkUpdateChanges.R", echo=TRUE)`

    This will show you if there has been any changes to the geography
    names, the metric names, the time periods and also check for any
    changes in the NAs within the data (a sign that something has gone
    awry).

7.  Run the dashboard: `runApp()`

8.  Update the shiny tests by running `shinytest2::test_app()` and
    checking that any newly generated snapshots look as expected.

9.  QA. Create a new QA log in “Analytical Projects\Analysis\S005 LSIP
    dashboard\Documentation\QA”. Follow the structure of one already in
    there. Some checks you may do:

-   Check that your dataset has been updated correctly (by comparing
    with the version you branched off of and cross checking with the raw
    data).
-   Check the Data Sources table has been updated correctly
-   Check the text in the Local Skills tab makes sense for your updated
    metric.
-   Get sign off Hannah Cox and/or Harris (see
    [here](https://educationgovuk.sharepoint.com/:w:/r/sites/UnitforFutureSkills/Shared%20Documents/Analytical%20Projects/Analysis/S005%20LSIP%20dashboard/Documentation/Local%20skills%20dashboard%20update%20principles.docx?d=wbd95fecb97144a348a73a34520212d75&csf=1&web=1&e=InX9xM)
    and
    [here](https://educationgovuk.sharepoint.com/:w:/r/sites/UnitforFutureSkills/Shared%20Documents/Analytical%20Projects/Analysis/S005%20LSIP%20dashboard/Documentation/QA/QA%20plan_Local%20Skills%20dashboard_UFS.docx?d=wa17c64605c7f4a3fb2b8bf35f4bec670&csf=1&web=1&e=Z66Jas)
    for process)

10. If everything looks ok, commit, push and merge to Development. Wait
    for that to deploy (can take 15mins or more - you can track this in
    [Actions · dfe-analytical-services/lsip_dashboard
    (github.com)](https://github.com/dfe-analytical-services/lsip_dashboard/actions)).
    Again check everything looks ok in the preprod environment:
    <https://department-for-education.shinyapps.io/local-skills-dashboard-preprod/>
    13 If you have done sufficient QA and you are satisfied, merge to
    Main and wait for that to deploy. Check again everything looks ok in
    the live environment:
    <https://department-for-education.shinyapps.io/local-skills-dashboard/>

11. Consider communicating the change to the wider Team or other
    stakeholders.

### Adding new features {#adding-new-features}

Make yourself a new github branch off (probably) the latest public
version, so off Main. Pull and run `renv::restore()` to align packages
with the dashboard.

#### It's a metric that already exists in the data

We currently have a fair few metrics that exist in the data but aren’t
being shown in the dashboard:

“all”, “economicallyactive”, “employees”,
“starts_rate_per_100000_population”, “starts”, “active”, “births”,
“deaths”, “qualNone”, “qualL1”, “qualL2”, “qualApp”, “qualL3”, “qualL4”,
“qualOther”,“employmentProjection”

To add one of these into the dashboard:

1.  Remove the metric from the unused list in importData/combineData.R
    under the “4.1 Unused metrics” header.

2.  Add your metric into metricChoices in global.R and assign it a more
    user friendly name for use in the dashboard.

3.  Add a row into /Data/3-2_dataText/dataText.xlsx for your metric
    assigning it the relevant text.

4.  Run extractLoadData.R

5.  Run `runApp()` and check the metric is available in the Local Skills
    tab.

6.  Update the shiny tests by running `shinytest2::test_app()` and
    checking that any newly generated snapshots look as expected.

7.  QA. Create a new QA log in “Analytical Projects\Analysis\S005 LSIP
    dashboard\Documentation\QA”. Follow the structure of one already in
    there. Some checks you may do:

    -   Check that your dataset has been added correctly (by comparing
        with the version you branched off of and cross checking with the
        raw data).
    -   Check the Data Sources table has been updated correctly
    -   Check the text in the Local Skills tab makes sense for your
        updated metric.
    -   Check the charts are in the format you want especially looking
        at whether it needs to be a percentage or volume

8.  If everything looks ok, commit, push and merge to Development. Wait
    for that to deploy (can take 15mins or more - you can track this in
    [Actions · dfe-analytical-services/lsip_dashboard
    (github.com)](https://github.com/dfe-analytical-services/lsip_dashboard/actions)).
    Again check everything looks ok in the preprod environment:
    <https://department-for-education.shinyapps.io/local-skills-dashboard-preprod/>

9.  If you have done sufficient QA and you are satisfied, merge to Main
    and wait for that to deploy. Check again everything looks ok in the
    live environment:
    <https://department-for-education.shinyapps.io/local-skills-dashboard/>

10. Consider communicating the change to the wider Team or other
    stakeholders.

**Optional extras/troubleshooting**

-   You might need to make some adjustments to how it is presented in
    the charts ie percentage vs vol. If the problem is in the time chart
    or map, check server.R where there are a few if statements to assign
    a metric to either % or vol. If the problem is in the breakdown
    chart check the server and combineData.R where some metrics are
    split into proportions and some are not.

-   If you want it to appear in the Data Explorer tab, remove it from
    the exclusion list in combineData.r/4.4 C_dataHub and give it a good
    name in there as well.

-   You may want to add to the Overview page. If so, add to ui.R
    wherever you want and create KPIs and charts in server.R using the
    following function: createOverviewKPI, createOverviewChart,
    renderOverviewChart.

#### It’s a brand new metric

1.  Create a new folder for your new dataset in /Data. Labels the folder
    2-x_datasetName.

2.  Paste your data into this folder.

3.  Dashboard admin:

    1.  You need to update the dashboard’s Data Sources tab with
        information about your new data. Open
        /Data/3-1_DataTable/DataTable.xlsx in Excel. Add a new row with
        new dates, links and information. You also need to add some
        information about the dataset on this page. You can do this in
        ui.R/2.5.2 Data details text.

    2.  Update the dashboard’s Version Log. This is done in ui.R under
        the “2.1.3 Version control” header. Move the current Latest
        Version text down into the Previous Version area, and repopulate
        the Current Version with new details.

    3.  Populate a new row in /Data/3-2_dataText/dataText.xlsx file.
        These are used to populate dynamic text on the dashboard’s Local
        Skills tab.

    4.  Add some overview information about the dataset on the User
        Guide page of the dashboard. an do this in ui.R/2.1.2 Contents

4.  Add a new script to /importData to extract and clean the data and
    add the script to the run list in ExtractLoadData.R (copy one of the
    examples in there). This file should direct to your new folder and
    import the data. It will also need to format your data so that it
    matches the form the dashboard is prepared for.

    a\. Firstly clean the raw data so it is a usable format with
    headings and data rows. There are some functions within the
    functions folder that might help (formatNomis cleans Nomis data,
    formatLong puts data into long format)

    b\. Ensure you have data for all the geographic areas used in the
    dashboard: LADUs, LSIPs, LEPs, MCAs, and national.

    If your data only comes in LADU form, you can groups these up to get
    the bigger geographcal areas. The function addGeos will help with
    this, but might not always be exacly what you want, so check. If you
    are grouping up, there will likely be some rounding errors so make
    sure to add that caveat to the Data Sources tab.

    c\. You then need to manipulate your now clean data into the
    following form:

| Column name | Description | Format | Example |
|----------|-------------------------------------------|----------|----------|
| geogConcat | Area name and geography | character | Black Country LEP |
| metric | Variable of interest | character | inemployment |
| breakdown | *If* the dataset has a breakdown they are listed here. Every metric **must** have a “Total” as well as any breakdowns. | character | Age |
| subgroup | Any subgroups of the breakdown. If breakdown is “Total” this is also “Total” | character | 19-24 |
| chartPeriod | The period which the data relates to in plain English. | character | AY20/21 |
| timePeriod | The start date of the period. | character | 01/08/2020 |
| latest | 1 if this is the most recent period in the data. -1 is one year before that. 0 for any other. | numeric | -1 |
| valueText | The value of the metric in this subgroup. Includes any supression from the source. | character | 5000 |
| value | The value of the metric in this subgroup in numeric terms. NA if suppressed. | numeric | 5000 |

7.  Give the now clean and formatted dataset a name like C_dataName and
    add into importData/combineData.R file.

8.  Run ExtractLoadData.R. You should see some new files in your local
    environment and the data in Data/AppData will have updated to
    include your new metric.

9.  Add your metric into metricChoices in global.R and assign it a more
    user friendly name for use in the dashboard.

10. Give your metric a good name for the Data Explorer tab in
    combineData.r/4.4 C_dataHub.

11. Run the app `runApp()`.

12. Update the shiny tests by running `shinytest2::test_app()` and
    checking that any newly generated snapshots look as expected.

13. QA. Create a new QA log in “Analytical Projects\Analysis\S005 LSIP
    dashboard\Documentation\QA”. Follow the structure of one already in
    there. Some checks you may do:

    -   Check that your dataset has been added correctly (by comparing
        with the version you branched off of and cross checking with the
        raw data).
    -   Check the Data Sources table has been updated correctly
    -   Check the text in the Local Skills tab makes sense for your
        updated metric.
    -   Check the charts are in the format you want especially looking
        at whether it needs to be a percentage or volume

14. Take a renv snapshot to help future users allign to any packages you
    have changed/added:`renv::snapshot()`

15. If everything looks ok, commit, push and merge to Development. Wait
    for that to deploy (can take 15mins or more - you can track this in
    [Actions · dfe-analytical-services/lsip_dashboard
    (github.com)](https://github.com/dfe-analytical-services/lsip_dashboard/actions)).
    Again check everything looks ok in the preprod environment:
    <https://department-for-education.shinyapps.io/local-skills-dashboard-preprod/>

16. If you have done sufficient QA and you are satisfied, merge to Main
    and wait for that to deploy. Check again everything looks ok in the
    live environment:
    <https://department-for-education.shinyapps.io/local-skills-dashboard/>

17. Consider communicating the change to the wider Team or other
    stakeholders.

**Optional extras/troubleshooting**

-   You might need to make some adjustments to how it is presented in
    the charts ie percentage vs vol. If the problem is in the time chart
    or map, check server.R where there are a few if statements to assign
    a metric to either % or vol. If the problem is in the breakdown
    chart check the server and combineData.R where some metrics are
    split into proportions and some are not.

-   You may want to add to the Overview page. If so, add to ui.R
    wherever you want and create KPIs and charts in server.R using the
    following function: createOverviewKPI, createOverviewChart,
    renderOverviewChart

### Fixing bugs {#fixing-bugs}

[CONTENT]

## Troubleshooting {#troubleshooting}

The most common problems occur when something changes in an input data
set. That might be :

-   renaming of a column

-   moving of data within an excel input

-   renaming geographical areas

-   adding or removing or changing the definition of some data

Another source of problem is changing geographical areas:

-   updating boundaries (pages like this are good to keep an eye on
    <https://en.wikipedia.org/wiki/2019%E2%80%932023_structural_changes_to_local_government_in_England>
    but changes to LSIPs, MCAs and LEPs you are better off looking at
    the latest data on <https://geoportal.statistics.gov.uk/>)

-   area name changes. These can be random eg typos, or genuine updates
    to names. In the /importData files there are a number of bits of
    correcting code to allign names. We attempt to use codes where
    possible but some data sets do not have them.

-   some data sets use the boundaries at the time in their historical
    data. Some project the current projections back. In the dashboard we
    apply the latest boundaries to all files.

The best way to check for all of these is to try and spot any NAs in the
final data (that in /Data/AppData). This will catch most, but may not
catch all so it is also good to check the final dashboard for errors in
the charts and maps.

We are looking to implement more QA tests to check these things
automatically.

## Monitoring dashboard usage {#monitoring-dashboard-usage}

We use Google Analytics to see a range of metrics on dashboard users.
Log in here (you may need access):
<https://analytics.google.com/analytics/web/>.

As well as the more generic metrics (users, new users, event clicks,
time spent), we have been collecting specific events from the dashboard.
You can see the things we monitor in /google-analytics.html. You can add
tracking for whatever dashboard event you want and then monitor that
event within Google Analytics.

In Google Analytics we have started creating custom reports to collect
some interesting data. You can see these (and create these) by clicking
the “Explore” tab in the left of the Google Analytics page.
