<!-- badges: start -->
  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/mabafaba/researchcyclematrix?branch=master&svg=true)](https://ci.appveyor.com/project/mabafaba/researchcyclematrix)
  <!-- badges: end -->
  
  
# Core Components

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

obvious; see `rcm_check()` for details


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


# List of currently exported functions

(see also the "Overview" vignette inside the package)


## The Research Cycle Matrix

### Get the RCM

`rcm_download` download the RCM. This function performs a lot of standardisation such as renaming and filtering relevant columns, converting dates etc.; it also has a bunch of options (including/excluding archived and validated items etc.)


### View the RCM

`rcm_dashboard` create the html dashboard

`researchcyclematrix:::rcm_under_review_with_data_unit_html` a simple list with data unit items under review. useful for the monthly meeting. It's experimental and not exported yet, needs some further tweaking to work correctly.

`validation_timeline` plot a timeline of validations by status (as found on the shiny dashboard). Once the matrix is up to date this should be super useful to immediately spot any items that are delayed, stuck with us or the field as well as volume of upcoming submission 

`rcm_browse` opens the research cycle matrix google sheet in the default browser


### Search the RCM

`rcm_find_file.id` returns the file id of the entry that is most similar to the search term (searches a whole bunch of columns inlcuding the RC name, round,...) Might ask you to select one from a list, so not good to use in other functions. Separate search terms with space

`rcm_find_row_by_file.id` same as above, but returns the entire RCM row rather than just the file id

### Analyse RCM

`rcm_check` return a tibble with inconsistencies in the matrix

`subs_status` look up submission entries in the RCM and get their status. I don't use this directly anymore (but it's called within other functions)

### Edit the RCM

`rcm_set_to_withHQ`: usually not needed, as it's run automatically by `rcm_update_from_subs`.

`rcm_set_to_withField` once feedback is sent to the field, use this to change the status to "with field" in the rcm

`rcm_set_to_validated` once validated, use this to change the status in the rcm. Edits a bunch of other columns as well (validation date, comment, hq focal point, ..)

`rcm_comment` add a comment to an RCM entry with a time stamp. Called by `rcm_set_to_validated`.

`rcm_set_hours_worked` set the number in the hours worked. Called by `rcm_set_to_validated`.

`todo_validate_next` if you pass a `todo` item, this will 
	1. set the top item to validated on the remote RCM
	2. remove the item from the `todo` object and return that
This is really useful if you're coherently using the `todo` item system to decide what to validate next. This function might be broken since we added a more required parameters to `rcm_set_to_validated`.

## The submission form entries ("subs")

### Get subs
`subs_download` download the table with validation submission form entries. Usually you don't need this directly but some other functions will ask you for it. 

### Edit subs
You can not edit the submission form google sheet table from here. This is on purpose - that one is supposed to be the pure input, and any corrections should be made in the google sheet directly - use:

`subs_browse` open the submission for entries in the browser



### Analyse subs

`subs_with_new_id` get only the `new id` submissions from the downloaded submissions. 


### Managing validation submissions via the form

`rcm_update_from_subs`: parses the submission form entries and edits the RCM. Some other functions, for example `rcm_dashboard` run this function by default. This has to be run ideally daily after manually checking the submissions & giving "new id" submissions a real id.




## Combined subs and rcm: The `todo` class

The RCM and form submissions ("subs") can be combined into  a "todo" item which helps get an immediate idea what needs to be done next.

### get the todo object

`todo_download` create a todo item from freshly downloaded rcm and subs

`todo_create` create a todo item from existing rcm and subs objects

### View the todo object

The default view is now included in it's basic print function, so you can just print it to the console like any other object.

### analyse the todo object

`todo_delayed` find out which RCM items have are not submitted but the date is passed


## Exports


`rcm_prefill_research_tracker` create the prefilled monthly trackers. For more details see the special vignette on this (`browseVignettes("researchcyclematrix")`)


## Depreciated functions

`rcm_check_csv` same as rcm_check, but write to csv by HQ focal point. Replaced by department monthly tracker

`rcm_delayed_to_google_sheet`: append a list of delayed items to a google sheet. Replaced by the department wide monhtly tracker

`todo_delayed_to_csv` write a csv file for each country with delayed items . Was first replaced by the google sheet and later by the department wide tracker. Might still be useful sometimes?


`todo_delayed_browse`: open the google sheet with a a list of delayed items in the browser. Replaced by the department wide monthly tracker 

`todo_next` pretty print a todo item. This is now the default print function for `todo` objects (called when you write a  `todo` item to the console like any other object)

