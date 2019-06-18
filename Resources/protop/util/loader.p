/* loader.p
 *
 * monitor the staging directory; when files appear select the largest and
 *  - move it to the load directory
 *  - launch a _proutil to load it
 *  - when the load finishes move it to archive directory 
 * rinse and repeat
 *
 * pro -p util/loader.p
 *
 * consider putting the load processes in the background so that the clock & status can be updated
 * in real time as the load runs -- from a UI perspective that would be nice although it seems
 * overly complex to implement for a fairly small beneift.
 *
 *
 */

define variable testMode as logical no-undo initial false.

define temp-table tt_fileList no-undo
  field fileSize   as decimal
  field fileName   as character
  field baseName   as character format "x(40)"
  field sizeGB     as decimal   format ">,>>>,>>9.9999999" label "GB"

  field loadStatus as character format "x(16)" label "Status"
  field loadStart  as datetime  label "Start"
  field loadWait   as integer
  field loadTime   as integer
  field loadRate   as decimal   format ">,>>>,>>9.999999999" label "MB/sec"

  index fileSize-idx is primary fileSize descending
  index fileName-idx is unique fileName
.

define variable numTbls      as integer   no-undo.
define variable dumpComplete as integer   no-undo initial 3.
define variable justDoIt     as character no-undo.
define variable dbEnvName    as character no-undo.

define variable targetDB   as character no-undo format "x(60)".
define variable dlDir      as character no-undo format "x(60)".

define variable stageDir   as character no-undo format "x(60)".
define variable loadDir    as character no-undo format "x(60)".
define variable arcDir     as character no-undo format "x(60)".
define variable logDir     as character no-undo format "x(60)".

define variable dotdCmd    as character no-undo format "x(60)" initial '_progres &1 -b -1 -p util/loaddotd.p -param "&2|&3" >> &4 2>&&1'.

define variable binlCmd    as character no-undo format "x(60)" initial 'yes | _proutil &1 -C load &2 -r >> &3 2>&&1'.

if os-getenv( "IDXBUILD" ) = "inline" then
  binlCmd = 'yes | _proutil &1 -C load &2 build indexes -TB 31 -TM 32 -r >> &3 2>&&1'.

define variable osFileName as character no-undo.
define variable cmd        as character no-undo format "x(65)".
define variable xcmd       as character no-undo format "x(60)".
define variable n          as integer   no-undo.

define variable t          as integer   no-undo format "->,>>9"            label "Tables Loaded".
define variable w          as integer   no-undo format "->,>>9"            label "      Waiting".
define variable z          as integer   no-undo format "->,>>9"            label "       Queued".
define variable f          as integer   no-undo format "->,>>9"            label "       Failed".
define variable gb         as decimal   no-undo format ">,>>>,>>>,>>9.99"  label "    GB Loaded".
define variable tw         as integer   no-undo format ">,>>>,>>>,>>9"     label "    Wait Time".

define variable dt         as character no-undo format "x(8)"              label "Start".
define variable q          as character no-undo format "x(8)"              label "Wait Time".
define variable x          as character no-undo format "x(8)"              label "Load Time".

define variable dirSep     as character no-undo.

define variable dlStartTS as datetime-tz no-undo.
define variable currTS    as datetime-tz no-undo.

define variable currTime    as character no-undo format "x(19)".  /* 2018/09/29@15:09:32.551+0000 */
define variable elapsedTime as character no-undo format "x(8)".

define stream logStrm.
define stream errStrm.
define stream inStrm.

form
  t gb tw             currTime to 132 skip
  w z  space(11) f elapsedTime to 132 skip
 with
  frame showSummary
  no-box
  width 132
  row 1
  side-labels
.

form
  dt tt_fileList.sizeGB tt_fileList.baseName tt_fileList.loadStatus q x tt_fileList.loadRate skip
 with
  frame showWork
  no-box
  width 132
  row 4
  down
.

define variable dlStart as datetime no-undo initial ?.
define variable lastDT  as datetime no-undo initial ?.

procedure loadData:

  define input parameter stageFile as character no-undo.

  define variable tblName  as character no-undo.
  define variable loadFile as character no-undo.
  define variable arcFile  as character no-undo.
  define variable logFile  as character no-undo.

  define variable inLine   as character no-undo.

  define variable startDT  as datetime  no-undo.

  define variable n as integer no-undo.

  if dlStart = ? then dlStart = now.

  n = num-entries( stageFile, "." ).

  if lastDT <> ? then tt_fileList.loadWait  = interval( now, lastDT, "seconds" ).

  assign
    loadFile = replace( stageFile, stageDir, loadDir )
    arcFile  = replace( stageFile, stageDir, arcDir )
    startDT  = now
    tt_fileList.loadStart = now
    tw = tw + tt_fileList.loadWait
  .

  if entry( n, loadFile, "." ) = "d" then
    do:
      tblName = substring( tt_fileList.baseName, 1, length( tt_fileList.baseName ) - 2 ).
      logFile = replace( replace( stageFile, stageDir, logDir ), ".d", ".load.log" ).
      cmd = substitute( dotdCmd, targetDB, tblName, loadFile, logFile ).
    end.
   else if entry( n, loadFile, "." ) begins "bd" then
    do:
      logFile = replace( replace( stageFile, stageDir, logDir ), ".bd", ".load.log" ).
      cmd = substitute( binlCmd, targetDB, loadFile, logFile ).
    end.
   else
    do:
      f = f + 1.
      tt_fileList.loadStatus = "unknown type".
      display tt_fileList.loadStatus with frame showWork.
      down with frame showWork.
      return.
    end.

  xcmd = entry( 1, cmd, ">" ).

  tt_fileList.loadStatus = "launch".
  display substring( string( now ), 12, 8 ) @ dt tt_fileList.sizeGB tt_fileList.baseName tt_fileList.loadStatus with frame showWork.

  os-rename value( stageFile ) value( loadFile ).

  file-info:file-name = loadFile.
  if file-info:full-pathname = ? then
    do:

      f = f + 1.
      tt_fileList.loadStatus = "move failed!".
      display tt_fileList.loadStatus with frame showWork.
      down with frame showWork.

    end.
   else
    do:

      output stream logStrm to value( logFile ).
      put stream logStrm unformatted now " " cmd skip.
      output stream logStrm close.

      tt_fileList.loadStatus = "loading...".

      q = string( tt_fileList.loadWait, "hh:mm:ss" ).

      display substring( string( now ), 12, 8 ) @ dt tt_fileList.sizeGB tt_fileList.baseName tt_fileList.loadStatus q with frame showWork.

      if testMode = false then
        do:

          message tt_fileList.baseName ':' cmd.

          os-command silent value( cmd ).

          n = num-entries( tt_fileList.baseName, "." ).
          if entry( n, tt_fileList.baseName, "." ) = "d" then
            assign
              t = t + 1
              tt_fileList.loadStatus = "complete"
            .
           else
            do:

              inLine = "".
              input stream inStrm through value( substitute( "tail -1 &1", logFile )).
              import stream inStrm unformatted inLine.
              input stream inStrm close.

              if inLine matches "*(6256)*" then
                assign
                  t = t + 1
                  tt_fileList.loadStatus = "complete"
                .
               else
                assign
                  f = f + 1
                  tt_fileList.loadStatus = "failed"
                .

            end.

        end.

      assign
        gb = gb + tt_fileList.sizeGB
        tt_fileList.loadTime   = interval( now, tt_fileList.loadStart, "seconds" )
        tt_fileList.loadRate   = ( tt_fileList.sizeGB * 1024 ) / tt_fileList.loadTime
        x  = string( integer( tt_fileList.loadTime ), "hh:mm:ss" )
        lastDT = now
      no-error.

      currTime = substring( string( now ), 1, 19 ).
      elapsedTime = string( interval( now, dlStartTS, "seconds" ), "hh:mm:ss" ).

      display t gb tw f currTime elapsedTime with frame showSummary.

      display tt_fileList.loadStatus q x tt_fileList.loadRate with frame showWork.
      down 1 with frame showWork.

      output stream logStrm to value( logFile ) append.
      put stream logStrm unformatted now " -- run time: " x " -- " tt_fileList.loadRate "  MB/sec" skip.
      output stream logStrm close.

      os-rename value( loadFile )  value( arcFile ).

    end.

  output stream errStrm to value( substitute( "&1/&2", logDir, "status.log" )) append.
  put stream errStrm unformatted now space(1) string( tt_fileList.baseName, "x(40)" ) space(1) string( tt_fileList.loadStatus, "x(12)" ) cmd skip.
  output stream errStrm close.

  return.

end.

/* main block
 *
 */

dirSep = ( if opsys = "unix" then "/" else "~\" ).

assign
  justDoIt  = os-getenv( "JUSTDOIT" )
  dbEnvName = os-getenv( "DB" )
  stageDir  = os-getenv( "STGDIR" )
  loadDir   = os-getenv( "LOADDIR" )
  arcDir    = os-getenv( "ARCDIR" )
  logDir    = os-getenv( "DL_LGDIR" )
  targetDB  = os-getenv( "TGTDIR" ) + dirSep + os-getenv( "DB" )
  numTbls   = integer( os-getenv( "NUMTBLS" ))
no-error.

if justDoIt <> "yes" then
  do:
    update
      skip(1)
      "       Stage:" stageDir skip
      "        Load:"  loadDir skip
      "     Archive:"   arcDir skip
      "        Logs:"   logDir skip
      skip(1)
      "  Target  DB:" targetDb skip
      skip(1)
      "    # Tables:" numTbls  skip
      skip(1)
     with
      frame updPaths
      no-labels
    .
  end.

pause 0 before-hide.

dlStartTS = now.

watcher: do while lastkey <> 4 and lastkey <> asc( 'q' ):		/* and t < numTbls */ 

  /*** empty temp-table tt_fileList. ***/

  input from os-dir( stageDir ).

  dirLoop: repeat:

    import ^ osFileName.

    if osFileName begins "." then next dirLoop.						/* skip hidden files				*/

    n = num-entries( osFileName, "." ).
    if n <> 2 then next dirLoop.							/* tblName.d, tblName.bd or tblName.bd#		*/

    if not (( entry( n, osFileName, "." ) = "d" ) or ( entry( n, osFileName, "." ) begins "bd" )) then
      next dirLoop.									/* only process dot-d or binary dump files 	*/

    file-info:file-name = osFileName.

    if file-info:full-pathname <> ? then
      do:

        find tt_fileList where tt_fileList.fileName = file-info:full-pathname no-error.
        if not available tt_fileList then
          create tt_fileList.

        assign
          n = num-entries( file-info:full-pathname, dirSep )
          tt_fileList.fileSize = file-info:file-size
          tt_fileList.fileName = file-info:full-pathname
          tt_fileList.baseName = entry( n, file-info:full-pathname, dirSep )
          tt_fileList.sizeGB   = file-info:file-size / ( 1024 * 1024 * 1024 )
          tt_fileList.loadStatus = "ready"
        .

      end.

  end.  /* dirLoop */

  w = 0.
  for each tt_filelist where loadStatus = "ready":
    w = w + 1.
  end.

  if dumpComplete < 0 and w = 0 then leave watcher.

  currTime = substring( string( now ), 1, 19 ).
  elapsedTime = string( interval( now, dlStartTS, "seconds" ), "hh:mm:ss" ).

  z = numTbls - t.
  display w z f tw currTime elapsedTime with frame showSummary.

  for each tt_fileList where tt_fileList.loadStatus = "ready" by tt_fileList.sizeGB descending:

    z = numTbls - t.
    w = w - 1.

    currTime = substring( string( now ), 1, 19 ).
    elapsedTime = string( interval( now, dlStartTS, "seconds" ), "hh:mm:ss" ).

    display w z f tw currTime elapsedTime with frame showSummary.

    run loadData( tt_fileList.fileName ).

    readkey pause 0.1.
    next watcher.		/* just do one at a time -- then check for more work!	*/

  end.

  if lastkey = 4 or lastkey = asc( 'q' ) then next watcher.

  display "waiting..." @ tt_fileList.loadstatus with frame showWork.

  readkey pause 0.05.

  file-info:file-name = substitute( "&1&2&3.dump.complete", logdir, dirSep, dbEnvName ).
  if file-info:full-pathname <> ? then dumpComplete = -1.

  /*** the table count is unreliable!            use the dump.complete flag instead ***/
  /*** if justDoIt = "yes" and numTbls > 0 and t >= numTbls then dumpComplete = -1. ***/

end. /* watcher */

output to value( logdir + dirSep + "loader.log" ).

currTime = substring( string( now ), 1, 19 ).
elapsedTime = string( interval( now, dlStartTS, "seconds" ), "hh:mm:ss" ).

display t gb tw w z f currTime elapsedTime with frame showSummary.

for each tt_fileList by tt_fileList.loadStart:

  display
    tt_fileList.loadStart
    tt_fileList.baseName
    tt_fileList.sizeGB
    tt_fileList.loadStatus
    tt_fileList.loadWait
    tt_fileList.loadTime
    tt_fileList.loadRate
  .

end.

output close.

quit.
