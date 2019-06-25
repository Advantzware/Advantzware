/* lib/writedblist.p
 *
 */

{lib/v9.i}
{lib/tt_dblist.i}

define input parameter table for tt_dbList.

define new global shared variable logLevel as integer no-undo initial 5.

define variable n as integer no-undo.

publish "logMsg" ( 0, substitute( "&1", "lib/writedblist" )).

output to value( "etc/dblist.cfg" ) unbuffered.

put unformatted "# etc/dblist.cfg" skip.
put unformatted "#" skip.
put unformatted "# note: please do NOT append .db to /dbpath/dbName" skip.
put unformatted "#" skip.
put unformatted "# friendlyName|/dbpath/dbName|serverName|monitor[|dlc[|type]]" skip.
put unformatted "# s2k|/db/s2k|myserver|no" skip.
put unformatted "#" skip.
put skip(1).

for each tt_dbList:

  /* remove old GUID field if it exists
   */

  if num-entries( tt_dbList.serverName, "-" ) = 5 and length( tt_dbList.serverName ) = 36 then tt_dbList.serverName = "".

  /* remove ".db" if it somehow got through to this point
   */

  if r-index( tt_dbList.dbPath, ".db" ) = ( length( tt_dbList.dbPath ) - 2 ) then
    tt_dbList.dbPath = substring( tt_dbList.dbPath, 1, length( tt_dbList.dbPath ) - 3 ).

  put unformatted
    tt_dbList.friendlyName "|"
    tt_dbList.dbPath "|"
    tt_dbList.serverName "|"
    tt_dbList.monitorDB "|"
    tt_dbList.dlcPath "|"
    tt_dbList.resrcType
    skip
  .

  n =  n + 1.

end.

output close.

publish "logMsg" ( 0, substitute( "&1: &2 entries written", "lib/writedblist", 2 )).

return.
