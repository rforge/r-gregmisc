#!/bin/sh
cd /home/warnes/src/R/r-gregmisc/OpenSourceInPharma/rwn-wiki 
wget -q -N -r -nd ftp://research.warnes.net:8021/statcomp/projects/CommercialR/
svn add *
svn commit -m "Automatic update of wiki information"
