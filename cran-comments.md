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
❯ On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release, 
  fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [10s] NOTE
  Maintainer: 'Sarah Endicott <sarah.endicott@ec.gc.ca>'
  
  New submission
  
  Package was archived on CRAN
  
  Possibly misspelled words in DESCRIPTION:
    Kruskal's (32:6)
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2022-11-09 as requires archived package
      'Require'.

❯ On windows-x86_64-devel (r-devel)
  checking examples ... [14s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                    user system elapsed
  getDistFromSource 6.92   0.17    8.28

❯ On ubuntu-gcc-release (r-release)
  checking examples ... [14s/48s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                     user system elapsed
  getDistFromSource 7.781   0.13  28.545

❯ On fedora-clang-devel (r-devel)
  checking examples ... [14s/56s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                     user system elapsed
  getDistFromSource 8.019  0.149  31.359

0 errors ✔ | 0 warnings ✔ | 4 notes ✖

* I can not reproduce the long example times for the getDistFromSource function.
  It is a tiny example with only 25 raster cells and takes 0.16 seconds on my 
  computer. Examples were also fine with rhub::check_on_debian()   

https://builder.r-hub.io/status/roads_1.1.0.tar.gz-1b545ba8da804b4fa31841d2711d2543