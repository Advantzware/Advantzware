ubin
----

These programs are free tools collected from various places (many, but not
all come from "gnuwin32") that support unix-like commands in a Windows
environment.

This is *much* lighter weight than something like Cygwin.

I find these tools to be very helpful when I am working on Windows.

You do not need these if you are already running Unix or Linux. But you
might find them handy if you have Windows machines elsewhere in your
environment.

You can use the character ProTop 3 without these tools if you modify
bin\protop.bat to not use grep and gawk.


Actually used in bin\*.bat:
===========================
blat.exe
curl.exe
gawk.exe
grep.exe
nssm64.exe
ptime.exe
sleep.exe
tail.exe
tee.exe
unzip.exe


Where used by scripts in bin:
=============================

nssm64 - dbmonitor.bat

curl   - pt3upd.bat, maintenance.bat
unzip  - pt3upd.bat

grep   - protop.bat, getdbpath.bat, hc.bat, sendalert.bat, picamon.bat
gawk   - protop.bat, getdbpath.bat, hc.bat, sendalert.bat, syncio.bat

ptime  - syncio.bat
tail   - syncio.bat
tee    - syncio.bat

blat   - mailx.bat

sleep  - protop.bat, pt3upd.bat, restart.bat


Unused in BAT scripts but often used in proenv:
===============================================
cat.exe
cp.exe
head.exe
ls.exe
pwd.exe
rm.exe
wc.exe

putty.exe
plinkk.exe
pscp.exe
psftp.exe

metapad.exe
me32.exe


Occasionally used in proenv:
============================
dd.exe
od.exe
find.exe
zip.exe
tar.exe
gunzip.exe
gzip.exe


May also be used internally
===========================

wget.exe

