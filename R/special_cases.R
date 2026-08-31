# Countries that use their own Part 1 report skeleton instead of the
# standard `scidata-report` one, because their report structure
# genuinely differs (currently: no flag-state reporting section, since
# neither has a foreign-flagged licensed fleet). Each entry here must
# have a matching `inst/rmarkdown/templates/scidata-report-<cc>/`
# template folder.
#
# To add a new special case (e.g. once Wallis & Futuna's skeleton is
# ready): add its lowercase code below, and add the matching
# `scidata-report-wf` template folder. No other code changes needed —
# both `new_scidata_report()` and `render_all_countries()` pick it up
# from here.
special_case_countries <- c("nu", "tk")
