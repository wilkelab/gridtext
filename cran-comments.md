Remove unnecessary dependency on systemfonts, per request by Brian Ripley:

"Version 0.1.2 has added a requirement of package systemfonts.  The latter is not easy to install because of its (mis-stated) system requirements.  Clearly systemfonts is not needed for all the package users of gridtext as 0.1.1 sufficed, so it should be in Suggests and used conditionally.  Please do so ASAP."

## Test environments
* ubuntu 20.04, devel and release
* windows, release
* macOS, release

## R CMD check results

0 errors | 0 warnings | 0 notes
