input from C:\ASIgui\Databases\Customer\BackupTest\backupReport.txt.
def var cLine as char.
def var rCurRec as rowid.
def var lFound as log.
def var iDayOfToday as int.
def var iLookbackdays as int init 2.
def temp-table ttReport
field sysName as char
field FileDate as char
field fileName as char
field lastOrder as char
.

iDayOfToday = weekday(today).
/* If this is monday, look back 5 days */
if iDayofToday LE 2 then 
  iLookBackDays = 6.

repeat:
 import unformatted cLine.
 if cLine begins "MBS" then do:
   create ttREport.
   ttReport.sysName = substring(cline, r-index(cline, "/") + 1).
   rCurRec = rowid(ttReport).
   lfound = true.
 end.
 else if lfound then do:
   find first ttReport where rowid(ttReport) = rCurRec .
   if cLine begins "BackupFileDate" then 
     assign ttReport.fileDate = substring(cline, index(cline, "=") + 1).
   if cLine begins "BackupFileName" then 
     assign ttReport.fileName = substring(cline, index(cline, "=") + 1).     
   if cLine begins "LastOrder" and ttReport.lastOrder EQ ""  then 
     assign ttReport.lastOrder = substring(cline, index(cline, "=") + 1).     
     
 end.
  
end.

output to c:\temp\backupReport.txt.
for each ttreport.
 
  disp sysname format "x(30)" filedate format "x(10)" lastorder format "x(80)" with width 159 stream-io.
end.
output close.

find first ttreport where ttreport.fileDate EQ "" 
  or date(entry(1, ttreport.fileDate, " ")) lt today - iLookBackDays no-error.
  
if avail ttreport then do:
    output to c:\temp\backupErrors.txt.
    
    for each ttreport where ttreport.fileDate EQ ""
      or date(entry(1, ttreport.fileDate, " ")) lt today - iLookbackdays.
      if not index(sysname, "shamrock") gt 0 then 
      disp sysname format "x(30)" filedate format "x(10)" lastorder format "x(70)" with width 132 stream-io.
    end.
    output close.    
end.
else
  os-delete c:\temp\backupErrors.txt.

QUIT.
