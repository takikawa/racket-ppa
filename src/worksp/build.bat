cd mzscheme
devenv mzscheme.sln /Build Release
cd ..\mred
devenv mred.sln /Build Release
cd ..

cd gc2
..\..\..\mzschemecgc -cu make.ss
cd ..

cd mzcom
devenv mzcom.sln /Build Release
cd ..\libmysterx
devenv libmysterx.sln /Build Release
cd ..

cd libmysterx
..\..\..\mzscheme -cu xform.ss
cd ..

cd mzcom
..\..\..\mzscheme -cu xform.ss
cd ..

cd mzcom
devenv mzcom.sln /Build 3m
cd ..\libmysterx
devenv libmysterx.sln /Build 3m
cd ..

cd mzstart
devenv mzstart.sln /Build Release
cd ..\mrstart
devenv mrstart.sln /Build Release
cd ..

..\..\mzscheme -l setup
