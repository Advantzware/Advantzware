/* dbmaint.p
 *
 * manage etc/dblist.cfg
 *
 */

{lib/protop.i}

define variable ptVerStr as character no-undo initial "{lib/ptversion.i}".

ptVerStr = trim( entry( 1, ptVerStr, "~n" )).

define new global shared variable dbgMode as integer   no-undo initial 3.
define new global shared variable custId  as character no-undo.

define variable monint as decimal no-undo initial 3.

define variable mergeStat as logical no-undo.

run lib/chkscreen.p.

run lib/windows.p persistent.

/* {lib/windows.i} */
{ssg/sausage17.i}

define stream inStrm.

/* setup
 */

pause 0 before-hide.

if "{&window-system}" <> "tty" then                                     /* look reasonable if someone fires up prowin32         */
  assign
    current-window:hidden       = true
    session:v6display           = true
    current-window:height-chars = 48
    current-window:width-chars  = 132
    current-window:bgcolor      = 15
    current-window:hidden       = false
    current-window:visible      = true
  .

run lib/protop-cfg.p persistent.					/* initialize protop environment                	*/

file-info:file-name = pt_tmpdir.
if file-info:full-pathname = ? then os-command silent value( "mkdir " + pt_tmpdir ).

file-info:file-name = pt_logdir.
if file-info:full-pathname = ? then os-command silent value( "mkdir " + pt_logdir ).

file-info:file-name = "etc/custid.cfg".
if file-info:full-pathname = ? then
  custId = "".
 else
  do on error undo, leave
     on endkey undo, leave:
    input stream inStrm from value( file-info:full-pathname ).
    import stream inStrm unformatted custId.
    input stream inStrm close.
  end.

run ssg/sausage02.p persistent.
run ssg/sausage06.p persistent.

define variable changeStatus as character no-undo.

define variable numFound as integer no-undo.

define variable c as handle no-undo.
define variable f as handle no-undo.
define variable x as handle no-undo.
define variable z as handle no-undo.

define variable r as rowid no-undo.

define query  q for tt_dbList.

define browse b
  query q
  display			/* over-ride width for updates in the upd_tt_dblist frame	*/
    tt_dbList.friendlyName	/* x(20) */
    tt_dbList.dbPath		/* x(60) */
    tt_dbList.monitorDB		/* x(4)	 */
    tt_dbList.resrcType		/* x(10) */
    tt_dbList.serverName	/* x(20) */
/*
    tt_dbList.statusInfo format "x(30)"
 */
   with
    /* no-box */
    /* width 132 */
    /* 20 down */
    size 132 by 20
    no-scrollbar-vertical
.

form
  " <insert>, ^T = insert new record." skip
  " <delete>, ^D = delete record." skip
  " <enter>      = update a record." skip
  skip(1)
  " m = toggle 'Mon?' flag." skip
  skip(1)
  " s = save changes." skip
  " x = save and exit." skip
  " q = quit without saving." skip
 with
  frame info
  row 1
  column 1
  width 132
.

form b with frame dbList column 1 no-box width 132.

form changeStatus with frame chgStatus row 1 column 1 overlay top-only no-box no-labels.

form
  tt_dbList.friendlyName      format "x(128)" view-as fill-in size 20 by 1
  tt_dbList.dbPath            format "x(128)" view-as fill-in size 60 by 1
  tt_dbList.monitorDB
  tt_dbList.resrcType at 88	/* "no-labels" messes up the alignment because "Mon?" (above) is wider than "yes" */
  tt_dbList.serverName        format "x(128)" view-as fill-in size 20 by 1
 with
  frame upd_tt_dbList
  no-box
  no-labels
  overlay
  column 4
  width 128 /* must fit inside container with space... */
.

c = frame chgStatus:handle.
f = frame upd_tt_dbList:handle.
x = frame dbList:handle.

z = browse b:handle.

function showChanges returns logical:

  assign
    c:row     = x:row + x:height
    c:column  = x:width - 9
    c:visible = ( changeStatus > "" )
    c:hidden  = not c:visible
  .

  display changeStatus with frame chgStatus.

  return true.

end.

procedure new_dbList:

  define buffer tt_dbList for tt_dbList.

  create tt_dbList.
  assign
    changeStatus          = "unsaved"
    tt_dbList.monitorStat = "new"
    tt_dbList.serverName  = pt_server
  .

  r = rowid( tt_dbList ).

/* query q:handle:reposition-to-rowid( r ). */

  return.

end.

on "q", "ctrl-e" of b in frame dbList do:

  define variable ok as logical no-undo.

  ok = no.

  if changeStatus <> "unsaved" then
    ok = yes.
   else
    message
        skip
        "There are unsaved changes." skip
        "Do you really want to quit without saving?"
        skip(1)
      view-as alert-box question buttons yes-no-cancel
      title " Quit Without Saving? "
      update ok
    .

  if ok then apply "close" to this-procedure.

  return no-apply.

end.

on "leave" of frame upd_tt_dbList do:			/* this prevents the tab order from overflowing to the browse widget	*/

  file-info:file-name = tt_dbList.dbPath + ".db".
  if file-info:full-pathname = ? then
    assign
      tt_dbList.monitorPID = "1"
      tt_dbList.statusInfo = "invalid dbPath"
    .
   else
    tt_dbList.monitorPID = "2".

  apply "entry" to b in frame dbList.

  return no-apply.

end.

on "go" of frame upd_tt_dbList do:

  if ( tt_dbList.friendlyName <> input tt_dbList.friendlyName ) or
     ( tt_dbList.dbPath       <> input tt_dbList.dbPath ) or
     ( tt_dbList.resrcType    <> input tt_dbList.resrcType ) or
     ( tt_dbList.monitorDB    <> input tt_dbList.monitorDB ) or
     ( tt_dbList.serverName   <> input tt_dbList.serverName ) then
    assign
      changeStatus = "unsaved"
      tt_dbList.monitorStat = ( if tt_dbList.monitorStat begins "new" then "new" else "changed" )
    .

  if tt_dbList.monitorPID = "1" then
    tt_dbList.statusInfo = ( if tt_dbList.monitorStat > "" then tt_dbList.monitorStat + ", " else "" ) + "invalid dbPath".
   else
    tt_dbList.statusInfo = tt_dbList.monitorStat.

  assign
    tt_dbList.friendlyName = input tt_dbList.friendlyName
    tt_dbList.dbPath       = input tt_dbList.dbPath
    tt_dbList.resrcType    = input tt_dbList.resrcType
    tt_dbList.monitorDB    = input tt_dbList.monitorDB
    tt_dbList.serverName   = input tt_dbList.serverName
  .

  if r-index( tt_dbList.dbPath, ".db" ) = ( length( tt_dbList.dbPath ) - 2 ) then
    assign
      tt_dbList.dbPath = substring( tt_dbList.dbPath, 1, length( tt_dbList.dbPath ) - 3 )
      changeStatus = "unsaved"
      tt_dbList.monitorStat = ( if tt_dbList.monitorStat begins "new" then "new" else "changed" )
    .

  z:refresh().
  apply "entry" to b in frame dbList.

  return no-apply.

end.

on "s", "x" of b in frame dbList do:

  define variable lk as integer no-undo.

  lk = lastkey.

  message "saving data".

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

  end.

  output close.

  if custId <> "" and custId <> ? and pt_backend <> "" and pt_backend <> ? then
    run lib/mergedblist.p ( input-output custId, pt_backend, "POST", output mergeStat ).

  for each tt_dbList where tt_dbList.statusInfo begins "new" and tt_dbList.monitorPID  = "2":

    assign
      tt_dbList.monitorPID  = ""
      tt_dbList.monitorStat = ""
      tt_dbList.statusInfo  = ""
    .

  end.

  changeStatus = "".
  showChanges().

  if lk = 88 or lk = 120 then apply "close" to this-procedure.

  return no-apply.

end.

on "m" of b in frame dbList do:

  assign
    tt_dbList.monitorDB   = not( tt_dbList.monitorDB )
    changeStatus          = "unsaved"
    tt_dbList.monitorStat = ( if tt_dbList.monitorStat begins "new" then "new" else "changed" )
  .

  if tt_dbList.monitorPID = "1" then
    tt_dbList.statusInfo = tt_dbList.monitorStat + ", invalid dbPath".
   else
    tt_dbList.statusInfo = tt_dbList.monitorStat.

  z:refresh().
  showChanges().

  return no-apply.

end.

on "entry", "value-changed" of b in frame dbList do:

  z:refresh().
  assign
    f:row     = x:row + z:focused-row + 2
    f:column  = x:column + 3
  .
  showChanges().

  if tt_dbList.monitorPID = "1" then
    tt_dbList.statusInfo = ( if tt_dbList.monitorStat > "" then tt_dbList.monitorStat + ", " else "" ) + "invalid dbPath".
   else
    tt_dbList.statusInfo = tt_dbList.monitorStat.

  hide message no-pause.

  return.

end.

on "insert-mode", "ctrl-t" of b in frame dbList do:

  message "insert row!".

  find first tt_dbList where tt_dbList.friendlyName = "" no-error.
  if available tt_dbList then
    r = rowid( tt_dbList ).
   else
    run new_dbList.

  close query q.
  open query q for each tt_dbList.
  z:refresh().

  apply "enter" to b in frame dbList.

  /* for reasons that are currently mysterious to me an extra copy of the
   * most recently inserted row appears in the browse widget.  this does
   * not seem to hurt anything and both copies can be editied - but it is
   * very odd.  and somewhat annoying.
   *
   */

  close query q.
  open query q for each tt_dbList.

  z:refresh().

  return no-apply.

end.

on "delete-character", "delete-line", "ctrl-d" of b in frame dbList do:

  message "delete row!".
  changeStatus = "unsaved".
  delete tt_dbList.
  z:delete-current-row().

  return no-apply.

end.

on "enter" of b in frame dbList do:

  enable
    tt_dbList.friendlyName
    tt_dbList.dbPath
    tt_dbList.monitorDB
    tt_dbList.resrcType
    tt_dbList.serverName
    /* tt_dbList.dlcPath  */
   with frame upd_tt_dbList
  .

  if "{&window-system}" <> "tty" then				/* look reasonable if someone fires up prowin32         */
    assign
      f:row = x:row + z:focused-row /* + 2 */	/* 1 if no-box, 2 if there are boxes... */
      f:column = x:column /* + 3 */
    .
   else
    assign
      f:row = x:row + z:focused-row + 2				/* 1 if no-box, 2 if there are boxes... */
      f:column = x:column + 3
    .

  if available tt_dbList then
    do:
      display tt_dbList.friendlyName tt_dbList.dbPath tt_dbList.monitorDB tt_dbList.resrcType tt_dbList.serverName with frame upd_tt_dbList.
      apply "entry" to tt_dbList.friendlyName in frame upd_tt_dbList.
    end.
  z:refresh().

  return no-apply.

end.


/* main block
 */

view frame info.

os-copy value( "etc/dblist.cfg" ) value( "etc/dblist.bak" ).
os-copy value( "etc/dblist.cfg" ) value( pt_tmpdir + "/dblist.save" ).

dbgMode = 3.

/***
message "Checking portal:" pt_backend custId dbgMode.
pause.
 ***/

if custId <> "" and custId <> ? and pt_backend <> "" and pt_backend <> ? then
  run lib/mergedblist.p ( input-output custId, pt_backend, "", output mergeStat ).

run chkDBList.

for each tt_dbList:

  file-info:file-name = tt_dbList.dbPath + ".db".
  if file-info:full-pathname = ? then
    assign
      tt_dbList.monitorPID = "1"
      tt_dbList.statusInfo = "invalid dbPath"
    .
   else
    tt_dbList.monitorPID = "2".

  numFound = numFound + 1.

end.

if numFound = 0 then run new_dbList.

open query q for each tt_dbList.

enable b with frame dbList.

apply "entry" to b in frame dbList.

if numFound = 0 then
  do:
    find first tt_dbList.
    apply "entry" to tt_dbList.friendlyName in frame upd_tt_dbList.
  end.

do on error undo, leave on endkey undo, leave on stop undo, leave:

  wait-for "close" of this-procedure.

end.

quit.
