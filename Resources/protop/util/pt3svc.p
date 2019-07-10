/* pt3svc.p
 *
 * service to make sure that monitoring jobs are up and running - deprecated, replaced by util/dbmonitor.p
 *
 */

{lib/protop.i}

define variable ptVerStr as character no-undo initial "{lib/ptversion.i}".

ptVerStr = trim( entry( 1, ptVerStr, "~n" )).

define variable monint as decimal no-undo initial 5.

define new global shared variable custId as character no-undo.

run lib/windows.p persistent.

/* {lib/windows.i} */
{ssg/sausage17.i}

/* main block
 */

run lib/protop-cfg.p persistent.					/* initialize protop environment                	*/

file-info:file-name = pt_tmpdir.					/* make certain that we have a temp directory!		*/
if file-info:full-pathname = ? then
  os-command silent value( "mkdir " + pt_tmpdir ).

file-info:file-name = pt_logdir.					/* make certain that we have a log directory!		*/
if file-info:full-pathname = ? then
  os-command silent value( "mkdir " + pt_logdir ).

run ssg/sausage02.p persistent.

display 
  ' w = start "waiting" and "down" ' skip
  ' s = stop  "still running"      ' skip
  ' k = kill all                   ' skip
  ' a = auto monitor               ' skip
  skip(1)
  " q = quit " skip
  skip(1)
 with
  no-box
.

do while ( lastkey <> 3 and lastkey <> 4 and lastkey <> 81 and lastkey <> 113 and lastkey <> 88 and lastkey <> 120 ):

  if session:batch = yes then
    do:
      run autoMonitor.
      pause monint no-message.
    end.
  if session:batch = no then
    do:

      run chkDBList.

      for each tt_dbList:
        run chkDB( tt_dbList.friendlyName ).
        display
          tt_dbList.friendlyName
          tt_dbList.dbPath
          tt_dbList.monitorDB
          tt_dbList.monitorPID
          tt_dbList.monitorStat
          tt_dbList.statusInfo
         with
          no-box
        .
      end.

      readkey pause monint.
      if       lastkey = 115 /* "s" */ then run stopRunning.
       else if lastkey = 119 /* "w" */ then run startWaiting.
       else if lastkey = 107 /* "k" */ then run killAll.
       else if lastkey =  97 /* "a" */ then run autoMonitor.

    end.

end.

quit.
