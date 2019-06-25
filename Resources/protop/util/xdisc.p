{lib/v9.i}
{lib/tt_dblist.i}

define variable i as integer no-undo.
define variable n as integer no-undo.

define variable dirList  as character no-undo format "x(40)".
define variable nameList as character no-undo.
define variable pathList as character no-undo.

update dirList.

run lib/discover.p ( dirList, output table tt_dbList by-reference ).

/***
for each tt_dbList by tt_dblist.xvalid:

  display
    tt_dbList.friendlyName format  "x(20)" label "Friendly Name"
    ( if tt_dbList.dbPath = "" then "unknown" else tt_dbList.dbPath )      format  "x(60)" label "DB Path"
    not( xvalid )          format "yes/no" label "New?"
    tt_dbList.monitorDB    format "yes/no" label "Monitor?"
    tt_dbList.serverName   format  "x(30)" label "Server"
   with
    frame showDiscovery
    title " Database List "
    centered
    row 4
    20 down
  .

  down with frame showDiscovery.

end.

pause.
 ***/

quit.
