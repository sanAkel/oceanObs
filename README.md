# oceanObs
to prepare a database of ocean observations

DATA Download URL:
        http://www.usgodae.org/cgi-bin/datalist.pl?dset=fnmoc_obs_profile&summary=Go#
--------------------

TEST
data in [/test_data/profile] comes from:

wget            http://www.usgodae.org/pub/outgoing/fnmoc/data/ocn/profile/2000/2000061800.profile.Z
uncompress      2000061800.profile.Z
--------------------

SOURCE CODE to decode and CHECK decoder:
[test_data/docs/]
                http://www.usgodae.org/ftp/outgoing/fnmoc/data/ocn/docs/
wget            http://www.usgodae.org/ftp/outgoing/fnmoc/data/ocn/docs/report.profile
mv              report.profile          report.profile_2000061800
** reproduced above, see decoder/ocn_obs.x; additional columns do NOT MEAN NON-ZERO diff
--------------------


--------------------
Data availability (note: ARGO starts sometime in 2000), so all good from USGODAE!

Before 1998, there's is NO data from the USGODAE- fnmoc.
For profiles, I suggest one gets the best possible (QC'ed and bias corrected) obs
from EN4
https://www.metoffice.gov.uk/hadobs/en4/download-en4-2-1.html             <-- data URL
https://www.metoffice.gov.uk/hadobs/en4/en4-0-2-profile-file-format.html  <-- format info
which goes back to 1900s.

For ships, buoys, etc, Please go to ICOADS:
http://icoads.noaa.gov/products.html
most of the surface obs have been processed into IQUAM, so that could be a easier/quicker way to get the SURFACE obs.

TO-DOs:
-------
option to python plot- needs to READ netcdf output:
horizontal spatial map of locations, vertical profile map of T (and S)-- total of 3 plots, like from Coriolis
