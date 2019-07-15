/* lib/readdblist.p
 *
 */

{lib/v9.i}
{lib/tt_dblist.i}

define output parameter table for tt_dbList.

define new global shared variable logLevel as integer no-undo initial 5.

define variable dbListLine as character no-undo.

define variable n as integer no-undo.

define stream inStrm.

publish "logMsg" ( 0, substitute( "&1", "lib/readdblist" )).

file-info:file-name = search( "etc/dblist.cfg" ).
  
if file-info:full-pathname <> ? then
  do:

    input stream inStrm from value( file-info:full-pathname ).

    repeat:

      dbListLine = "".
      import stream inStrm unformatted dbListLine.

      if dbListLine = "" or dbListLine begins "#" then next.
      if num-entries( dbListLine, "|" ) < 4 then next.

      find tt_dbList where tt_dbList.friendlyName = entry( 1, dbListLine, "|" ) no-error.

      if not available tt_dbList then
        do:
          n = n + 1.
          create tt_dbList.
        end.

      assign
        tt_dbList.xvalid       = yes
        tt_dbList.friendlyName = entry( 1, dbListLine, "|" ) 
        tt_dbList.dbPath       = entry( 2, dbListLine, "|" ) 
        tt_dbList.serverName   = entry( 3, dbListLine, "|" ) 
        tt_dbList.monitorDB    = logical( entry( 4, dbListLine, "|" ))
      no-error.

      /* sites that have multiple servers sharing a custId and etc/dblist.cfg can use the serverName
       * to ensure that agents only run on the server that they are supposed to run on
       */

      if num-entries( tt_dbList.serverName, "-" ) = 5 and length( tt_dbList.serverName ) = 36 then tt_dbList.serverName = "".

      if tt_dbList.monitorDB = ? then tt_dbList.monitorDB = no.

      if num-entries( dbListLine, "|" ) >= 5 then
        tt_dbList.dlcPath = entry( 5, dbListLine, "|" ).
      if num-entries( dbListLine, "|" ) >= 6 then
        tt_dbList.resrcType = entry( 6, dbListLine, "|" ).

    end.

    input stream inStrm close.

  end.

publish "logMsg" ( 0, substitute( "&1: &2 entries found", "lib/readdblist", n )).

return.
