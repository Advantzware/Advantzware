/* zippyng.p
 *
 * (see lib/zippy.p for more details)
 *
 * run a constant workload against the db and record the time that it takes
 *
 * ideally 3 instances of this monitor will be run:
 *
 *  1) connect on the db server with shared memory
 *
 *	_progres dbname -b -p util/zippyng.p -param "shm" > tmp/zippyng.shm.err
 *
 *  2) connect on the db server client/server thru localhost
 *
 *	_progres dbname -b -p util/zippyng.p -param "lh"  -H localhost -S port > tmp/zippyng.lh.err
 *
 *  3) connect from a remote client over a network
 *
 *	_progres dbname -b -p util/zippyng.p -param "cs"  -H dbserver  -S port > tmp/zippyng.cs.err
 *
 * if everything is operating correctly the times should be consistent and the
 * ratio between them stable as the workload increases or decreases
 *
 * if the shared memory timings vary then you likely have a CPU bottleneck or VM
 * configuration issue
 *
 * if there are not enough "remote servers" being started then you would see the
 * ratio between shared memory and localhost vary
 *
 * if there is a bottleneck in the network layer you should see that as variation
 * in the ratios between all 3 tests
 *
 */

define new global shared variable pt_logdir as character no-undo.
define new global shared variable pt_tmpdir as character no-undo.

define variable i as integer no-undo.
define variable r as integer no-undo.
define variable t as integer no-undo.

define variable p1 as character no-undo.
define variable DS as character no-undo.

define variable logFileName  as character no-undo.
define variable flgFileName  as character no-undo.
define variable monName      as character no-undo.
define variable zipsync      as character no-undo.

DS = ( if opsys = "unix" then "/" else "~\" ).

if session:parameter <> "" then p1 = "." + session:parameter.

run lib/protoplib.p  persistent.
run lib/vstlib.p     persistent.
run lib/protop-cfg.p persistent.                                        /* initialize protop environment                	*/

run getMonName ( input-output monName ).

assign
  logFileName = substitute( "&1/&2&3.&4", pt_logdir, monName, p1, "log" )
  flgFileName = substitute( "&1/&2&3.&4", pt_tmpdir, monName, p1, "flg" )
.

run mkFlag ( flgFileName ).

run lib/zippy.p persistent.

do for _myconnection transaction:
  find first _myconnection exclusive-lock no-error.
  buffer _myConnection:handle:buffer-field( "_myconn-userMisc" ):buffer-value = substitute( "pt3:zippy&1", p1 ) no-error.
end.

zipsync = os-getenv( "ZIPSYNC" ).
if zipsync = ? then zipsync = "".

output to value( logFileName ) append unbuffered.

do while true:

  run zippy ( output r, output t ).

  put now (( r / t ) * 1000 ) format ">>>>>>>>9" skip.

  if zipsync <> "" then
    os-command silent value( zipsync ).

  do i = 1 to 60:
    file-info:file-name = flgFileName.
    if file-info:full-pathname = ? then quit.
    pause 1 no-message.
  end.

end.

output close.

return.
