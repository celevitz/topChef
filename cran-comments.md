## R CMD check results

0 errors | 0 warnings | 0 notes

* Note from submitter: I could not install miktext since it isn't available 
for the version of R that I have (4.4.0). Same with pdflatex. Additionally, I 
have reviewed the items under the spellcheck and am comfortable with the words
as they are listed. Most of them are the names of the datasets, and the others
are package names or content-specific words that make sense.

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... [15s] NOTE
  Maintainer: 'Levitz Carly E <celevitz@gmail.com>'
  
  New submission

❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
    
    Note from submitter: I could not install miktext since it isn't available 
    for the version of R that I have (4.4.0). Same with pdflatex.

❯ On ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... [6s/13s] NOTE
  Maintainer: ‘Levitz Carly E <celevitz@gmail.com>’
  
  New submission

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [6s/16s] NOTE
  Maintainer: ‘Levitz Carly E <celevitz@gmail.com>’
  
  New submission

0 errors ✔ | 0 warnings ✔ | 6 notes ✖
