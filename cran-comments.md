* This is a new package (first release).


I submitted this earlier today but it failed an automatic check.
I received automated email at 12:55 Pacific June 29 2023 saying that:
  Authors@R field gives persons with invalid ORCID identifiers:
  Levitz Carly <celevitz@gmail.com> [cre, aut, cph] (000-0003-3094-411X)
  
In this submission, I removed the reference to my ORCID. I received the same 
notes as last time, except that I added my middle initial to my maintainer name.

0 errors ✔ | 0 warnings ✔ | 6 notes ✖

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
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
    for the version of R that I have (4.2.3). Same with pdflatex.

❯ On ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... [6s/11s] NOTE
  Maintainer: ‘Levitz Carly E <celevitz@gmail.com>’
  
  New submission

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [6s/17s] NOTE
  Maintainer: ‘Levitz Carly E <celevitz@gmail.com>’
  
  New submission
