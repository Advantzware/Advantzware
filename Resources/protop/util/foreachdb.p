/* foreachdb.p
 *
 * execute administrative commands against each monitored db
 *
 * trivial example:
 *
 *    bpro -p util/foreachdb.p -param echo > tmp/foreachdb.err 2>&1
 *
 * the provided -param will be spawned with dbPath & friendlyName as arguments
 * in the example above:
 *
 *	echo dbPath friendlyName
 *
 * will be executed for each monitored database
 *
 */

{lib/protop.i}

define variable ptVerStr as character no-undo initial "{lib/ptversion.i}".

ptVerStr = trim( entry( 1, ptVerStr, "~n" )).

define new global shared variable custId as character no-undo.

define variable cmd as character no-undo.
define variable i   as integer   no-undo.
define variable n   as integer   no-undo.

n = num-entries( session:parameter ).

run lib/windows.p persistent.

/* {lib/windows.i} */
{ssg/sausage17.i}

/* main block
 */

run lib/protop-cfg.p persistent.					/* initialize protop environment                	*/

file-info:file-name = pt_tmpdir.
if file-info:full-pathname = ? then os-command silent value( "mkdir " + pt_tmpdir ).

file-info:file-name = pt_logdir.
if file-info:full-pathname = ? then os-command silent value( "mkdir " + pt_logdir ).

run ssg/sausage02.p persistent.

output to value( substitute( "&1/&2", pt_logdir, "foreachdb.log" )) append.

run chkDBList.

for each tt_dbList where tt_dbList.monitorDB = yes and tt_dbList.serverName = pt_server:

  cmd = "".
  file-info:file-name = tt_dbList.dbPath + ".db".
  if file-info:full-pathname <> ? then
    do i = 1 to n:
      cmd = substitute( "&1 &2 &3", entry( i, session:parameter ), tt_dbList.dbPath, tt_dbList.friendlyName ).
      message {&NOW} cmd.
      os-command silent value( cmd ).
    end.

end.

output close.

return.
