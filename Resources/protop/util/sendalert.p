/* sendalert.p
 *
 * send an alert (from an external script)
 *
 * _progres -1 -b -p util/sendalert.p -param "friendlyName|alertType|alertMsg" > tmp/sendalert.err
 *
 */

{lib/protop.i}

/* dbgMode = 1  minimal
 * dbgMode = 3  errors only
 * dbgMode = 4  + success
 * dbgMode = 5  verbose details (usually overkill)
 */

dbgMode = 3.

define new global shared variable custId as character no-undo.

define variable monName      as character no-undo.
define variable XID          as character no-undo.
define variable logFileName  as character no-undo.
define variable flgFileName  as character no-undo.
define variable dbgFileName  as character no-undo.

define variable alertType    as character no-undo.
define variable alertMetric  as character no-undo.
define variable alertMsg     as character no-undo.
define variable alertBody    as character no-undo.

define variable alertTempl   as character no-undo.
define variable alertText    as character no-undo.

alertTempl = "&1 &2~n[&3 &4]~n[cfgname: &5]~n[server: &6]~n".


/* main body
 *
 */

if num-entries( session:parameter, "|" ) >= 1 then pt_shortname = entry( 1, session:parameter, "|" ).
if num-entries( session:parameter, "|" ) >= 2 then alertType    = entry( 2, session:parameter, "|" ).
if num-entries( session:parameter, "|" ) >= 3 then alertMetric  = entry( 3, session:parameter, "|" ).
if num-entries( session:parameter, "|" ) >= 4 then alertMsg     = entry( 4, session:parameter, "|" ).

if num-entries( session:parameter, "|" ) <> 4 then
  do:
    message {&NOW} 'usage: _progres -1 -b -p util/sendalert.p -param "friendlyName|alertType|alertMetric|alertMsg"'.
    quit.
  end.

run lib/protoplib.p persistent.						/* load protop infrastructure library                   */
run lib/protop-cfg.p persistent.					/* initialize protop environment                        */

file-info:file-name = pt_tmpdir.					/* make certain that we have a temp directory!          */
if file-info:full-pathname = ? then
  os-command silent value( "mkdir " + pt_tmpdir ).

file-info:file-name = pt_logdir.					/* make certain that we have a log directory!           */
if file-info:full-pathname = ? then
  os-command silent value( "mkdir " + pt_logdir ).

{ssg/sausage28.i}

if dbgMode >= 5 then
  do:
    output to value( logFileName ) unbuffered append.
    message {&NOW} "==Quit==".
    output close.
  end.

quit.
