<!-- badges: start -->
  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/mabafaba/researchcyclematrix?branch=master&svg=true)](https://ci.appveyor.com/project/mabafaba/researchcyclematrix)
  <!-- badges: end -->
  
  
# researchcyclematrix
managing the research cycle matrix



## related google drive sheets 
all data is stored on google drive.
There's two sheets the package interacts with:

- the [research cycle matrix](https://docs.google.com/spreadsheets/d/1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk/edit)
- the [submitted validations sheet](https://docs.google.com/spreadsheets/d/1iNt__-uMMBTbLEsJkiIXglPJ4GK-9UCVqC7awhMTXF8/)


## downloading

There are three essential objects, each pulled from google drive:


- rcm_download(): downloads the researchcyclematrix and "standardises" it, which means:
	- keeping only relevant columns
	- simplified column names
	- converting date formats etc.
	- standardise the status column values
	- few other things, see rcm_download() code
	- *if you need the rows to match the actual google sheet, and/or the exact columns as they are online, use rcm_download(raw = T)*

- subs_download() downloads the submissions sheet as is (converts dates and a few things)

- todo_download() downloads _both_ sheets and creates a "todo" class data frame
	- filters only items "with HQ" in the RCM
	- keeps/creates relevant columns from both sheets such as "date.hqsubmission.actual", "submitter.emergency" (was emergency ticked?), "in.country.deadline"
	- "todo" objects are printed with the `todo_next()` function (in the code you'll see below the todo_next(), `print.todo <- todo_next` which does the trick)
	- *the todo data frame only contains stuff that has been moved to the research cycle matrix with rcm_update_from_subs(). It also does not contain any items where the 'new.file.id' has not been changed to a real file id and moved to the rcm.*


## checking for inconsistencies
	- obvious; see `rcm_check()` for details


## changes to the research cycle matrix / google docs

- the submissions sheet is never changed from this package. By design that sheet is meant is "incoming" only (but maybe should break that principle eventually to add stuff to deal with new ids)
- the reserach cycle matrix status can be changed with `rcm_set_to_validated()`, `rcm_set_to_withHQ()`  `rcm_set_to_withField()`
	- internally: these also set the date with rcm_set_validation_date(), rcm_set_withfield_date(), rcm_set_withHQ_date() 
	- internally: rcm_set_to_validated uses `rcm_set_hours_worked()` as well. This writes to column BB just the number you put.
- the reserach cycle matrix comment with `rcm_comment()`. adds to the existing comment, and also appends a timestamp
- for status update, I mostly  use `todo_validate_next()` which will _set the status of the "top" item in a todo data frame to validated, remove that item from the todo data frame and return the updated todo item._. See the [validation repository](https://github.com/mabafaba/validation/) for details
internally all these functions use the more generic `rcm_change_value()` function that takes a file id, column, and value. It will change the row of the _first instance of the file.id_ so if they're not unique we gonna have trouble. If the column order changes we're also in trouble.

## rcm dashboard

- rcm_dashboard() renders an rmarkdown document stored in ./inst/md_templates/



## visualisations

- unstable & under construction, but in rcm_plots.R there's some approaches to start visualising the rcm

## 


