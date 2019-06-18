/* util/dbanalys.p
 *
 * mpro dbName -p util/dbanalys.p -param friendlyName
 *
 */

{lib/protop.i}

dbgMode = 3.

define variable monName      as character   no-undo.
define variable logFileName  as character   no-undo.
define variable flgFileName  as character   no-undo.
define variable dbgFileName  as character   no-undo.

if num-entries( session:parameter, "|" ) >= 1 then pt_shortname = entry( 1, session:parameter, "|" ).

run lib/protoplib.p persistent.						/* load protop infrastructure library                   */
run lib/protop-cfg.p persistent.					/* initialize protop environment                        */

run getMonName ( input-output monName ).

file-info:file-name = pt_tmpdir.					/* make certain that we have a temp directory!          */
if file-info:full-pathname = ? then
  os-command silent value( "mkdir " + pt_tmpdir ).

file-info:file-name = pt_logdir.					/* make certain that we have a log directory!           */
if file-info:full-pathname = ? then
  os-command silent value( "mkdir " + pt_logdir ).

run lib/vstlib.p persistent.						/* db related infrastructure				*/

message now "Starting" monName pt_shortname "pdbname:" pdbname(1).

assign
  logFileName = substitute( "&1/&2.&3.&4", pt_logdir, monName, pt_shortname, "log" )
  flgFileName = substitute( "&1/&2.&3.&4", pt_tmpdir, monName, pt_shortname, "flg" )
  dbgFileName = substitute( "&1/&2.&3.&4", pt_tmpdir, monName, pt_shortname, "dbg" )
.

output to value( logFileName ) unbuffered append.			/* if a 2nd copy starts, try to make it obvious		*/

message now "Starting" monName pt_shortname "pdbname:" pdbname(1).

publish "dba_report".

output close.

return.

