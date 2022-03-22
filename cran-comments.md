## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

GitHub Actions:
windows-latest (release)
macOS-latest (release)
ubuntu-20.04 (release)
ubuntu-20.04 (devel)

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Sarah Endicott <sarah.endicott@ec.gc.ca>'
  
  New submission

> On fedora-clang-devel (r-devel)
  checking examples ... NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                user system elapsed
  projectRoads 9.622  0.235   1.776

0 errors √ | 0 warnings √ | 2 notes x

* This is a new release.

* I can not reproduce the long example times for fedora and I don't know how to go about fixing it or if it is necessary to. It only takes 1.68 seconds on windows. Any feedback would be appreciated

## Resubmission

* Fixed the error in one of the tests and confirmed it worked by running rhub::check_on_debian()