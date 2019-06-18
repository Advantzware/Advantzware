/* lib/discover.p
 *
 * discover running databases
 *
 * use "find" to identify *.db files, confirm that they are running Progress databases with "mbpro dbname -p lib/quit.p"
 *
 *
 */

{lib/v9.i}
{lib/tt_dblist.i}

define input  parameter p1 as character no-undo.
define output parameter table for tt_dbList.

define new global shared variable logLevel as integer no-undo initial 5.

define variable i        as integer no-undo.
define variable n        as integer no-undo.
define variable ok       as logical no-undo.
define variable dupName  as logical no-undo.
define variable inLine   as character no-undo format "x(30)".
define variable xdbName  as character no-undo format "x(20)".
define variable xdbPath  as character no-undo format "x(50)".
define variable dirList  as character no-undo format "x(50)".
define variable dName    as character no-undo format "x(50)".
define variable srvName  as character no-undo.
define variable xquit    as character no-undo.
define variable dirSep   as character no-undo.
define variable xeditor  as character no-undo.

define stream inStrm.
define stream inStrm2.

if opsys = "unix" then
  dirSep = "/".
 else
  dirSep = "~\".

if p1 = "" or p1 = ? then p1 = "/".
p1 = replace( p1, ",", " " ).

publish "logMsg" ( 0, substitute( "&1: &2", "lib/discover", p1 )).

n = num-entries( p1, " " ).
do i = 1 to n:

  dName = entry( i, p1, " " ).

  if opsys begins "win" then
    do:
      dName = dName + "~\*.db".
    end.

  dirList = substitute( "&1 &2", dirList, dName ).

end.

dirList = trim( dirList ).

/*** ***/

if dirList = "" or dirList = ? then dirList = "/".

if opsys = "unix" then
  do:
    input stream inStrm through value( "uname -a" ).
    import stream inStrm ^ srvName.
    input stream inStrm close.
  end.
 else
  do:
    input stream inStrm through value( "hostname" ).
    import stream inStrm srvName.
    input stream inStrm close.
  end.

publish "logMsg" ( 0, substitute( "&1: srvName = &2", "lib/discover", srvName )).

/* get the initial, known values from etc/dblist.cfg
 */

run lib/readdblist.p ( output table tt_dbList by-reference ).		/*** if we only discover when this doesn't exist... ***/

message "Searching " replace( dirList, " ", "," ) "for .db files.".
message "This may take some time...".

if opsys = "unix" then
  input stream inStrm through value( substitute( 'find &1 -name "*.db" 2>/dev/null', dirList )).
 else
  input stream inStrm through value( substitute( 'dir /s /b &1 2>NUL', dirList )).

  /* input stream inStrm through value( substitute( 'find &1 -name "*.db" 2>NUL',       dirList )). */

repeat:

  xdbPath = "".
  import stream inStrm unformatted xdbPath.

  publish "logMsg" ( 0, substitute( "&1: candidate = &2", "lib/discover", xdbPath )).

  file-info:file-name = replace( xdbPath, ".db", ".lk" ).

  if file-info:full-pathname = ? then					/* this might be a running Progress database	*/
    do:
      publish "logMsg" ( 0, substitute( "&1: no .lk found &2, therefore not a running Progress db", "lib/discover", xdbPath )).
    end.
   else
    do:

      publish "logMsg" ( 0, substitute( "&1: .lk found &2", "lib/discover", xdbPath )).

      xdbPath = replace( file-info:full-pathname, ".lk", "" ).

      /* "** The database /db/s2k is in use in multi-user mode. (276)"	*/

      /* useless -- proutil reports stale l.lk files as "in use..."
       *
       * input stream inStrm2 through value( substitute( 'proutil &1 -C busy', xdbPath )).
       *
       */

      /* There is no server for database /db/s2k. (1423)	*/
      /* connected: /db/s2k					*/

      ok = no.

      if opsys  = "unix" then
        input stream inStrm2 through value( substitute(    '$DLC/bin/_progres -b &1 -p lib/quit.p 2>/dev/null', xdbPath )).
       else
        input stream inStrm2 through value( substitute( '%DLC%~\bin~\_progres -b &1 -p lib/quit.p 2>NUL', xdbPath )).

      mpro_loop: repeat:

        import stream inStrm2 unformatted xquit.

        if xquit = trim( substitute( "connected: &1", xdbPath )) then
          do:
            ok = yes.
            leave mpro_loop.
          end.

      end.

      input stream inStrm2 close.

      if ok = yes then
        publish "logMsg" ( 0, substitute( "&1: &2 is a running Progress db", "lib/discover", xdbPath )).
       else
        publish "logMsg" ( 0, substitute( "&1: &2 is NOT a running Progress db, connection attempt failed", "lib/discover", xdbPath )).

      if ok = yes then
        do:

          assign
            i = 0
            xdbName = entry( num-entries( xdbPath, dirSep ), xdbPath, dirSep )
          .

          create_dbList: do while true:
            i = i + 1.
            find tt_dbList where tt_dbList.friendlyName = xdbName no-error.
            if available( tt_dbList ) then
              do:
                dupName = yes.
                xdbName = xdbName + string( i, "_9999" ).
              end.
             else
              do:
                create tt_dbList.
                assign
                  tt_dbList.friendlyName = xdbName
                  tt_dbList.dbPath       = xdbPath
                  tt_dbList.serverName   = srvName
                  tt_dbList.monitorDB    = yes
                  tt_dbList.dlcPath      = os-getenv( "DLC" )
                .
                if tt_dbList.dlcPath = ? then tt_dbList.dlcPath = "".
                leave create_dbList.
              end.

          end.

        end.
      
    end.

end.

input stream inStrm close.

assign
  i = 0
  n = 0
.

for each tt_dbList:
  if xvalid = yes then
    i = i + 1.
   else
    n = n + 1.
end.

run lib/editdblist.p ( input-output table tt_dbList by-reference ).

n = 0.
for each tt_dbList:
  n = n + 1.
end.

if n > 0 then
  run lib/writedblist.p ( input table tt_dblist by-reference ).

if n > 0 and dupName = yes and session:batch = no then
  do:

    xeditor = os-getenv( "EDITOR" ).

    if xeditor = ? or xeditor = "" then
      xeditor = ( if opsys = "unix" then "vi" else "notepad" ).

    message
        skip(1)
        "You may have some awkwardly named databases - would you like to edit them?"
        skip(1)
      view-as alert-box question buttons yes-no-cancel
      title " Edit friendly names? "
      update ok
    .

    if  ok = yes then
      publish "logMsg" ( 0, substitute( "&1: awkward names found, editing with &2", "lib/discover", xeditor )).
     else
      publish "logMsg" ( 0, substitute( "&1: awkward names found but not editing", "lib/discover" )).

    if  ok = yes then
      do:
        os-command silent value( xeditor + " etc" + dirSep + "dblist.cfg" ).
        for each tt_dbList:
          delete tt_dbList.
        end.
        run lib/readdblist.p ( output table tt_dbList by-reference ).		/* get an accurate tt_dbList after manual updates	*/
      end.

  end.

return.
