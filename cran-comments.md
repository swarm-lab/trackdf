## Test environments
* local OS X install, R 3.6.1
* ubuntu 16.04 (on travis-ci), R 3.6.1
* win-builder (release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* Possibly mis-spelled words in DESCRIPTION:
  GPS (11:60)

  GPS is not mis-spelled.

## Downstream dependencies

There are currently no downstream dependencies for this package.

## CRAN team comments

"\dontrun{} should be only used if the example really cannot be executed 
(e.g. because of missing additional software, missing API keys, ...) by 
the user. That's why wrapping examples in \dontrun{} adds the comment 
("# Not run:") as a warning for the user.
Please unwrap the examples if they are executable in < 5 sec, or create 
additionally small toy examples to allow automatic testing (then replace 
\dontrun with \donttest)."

\dontrun{} calls were eliminated. New examples were created that run < 5 sec. 

"Please add \value to .Rd files and explain the functions results in the 
documentation. f.i.: sub-.track.Rd"

Fixed
