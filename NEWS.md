# researchcyclematrix 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* updated rcm checks:
  - removed less relevant checks (i.e. hq/field status but no received date)
  - delays based on status not based on missing received date
* easy prefilling the monthly tracker with rcm_prefill_research_tracker()
* not exported but already working to directly put together the whole tracker package with folders etc.: researchcyclematrix:::rcm_prefill_research_tracker_zip
