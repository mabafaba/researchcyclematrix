# researchcyclematrix 0.2.0

# researchcyclematrix 0.1.1

* New Features
** Added a `NEWS.md` file to track changes to the package.
** easy prefilling the monthly tracker with rcm_prefill_research_tracker()
*** not exported but already working to directly put together the whole tracker package with folders etc.: researchcyclematrix:::rcm_prefill_research_tracker_zip

* Changes
** updated rcm checks:
  - removed less relevant checks (i.e. hq/field status but no received date)
  - delays based on status not based on missing received date
** new sorting for  todo by: 1. status; 2. emergency; 3. days since submission; 4. days until in country deadline (moving "days since submission" before "deadline")

* Fixes
** gdrive links pointing at the correct sheet
