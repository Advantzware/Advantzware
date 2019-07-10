/* pt3upd.p
 *
 * update protop 3
 *
 */

{lib/protop.i}

/* dbgMode = 1	minimal
 * dbgMode = 3	errors
 * dbgMode = 4	errors + success
 * dbgMode = 5	verbose socket communications details (overkill)
 */

dbgMode = 4.

run lib/protop-cfg.p persistent.					/* initialize protop environment                	*/

file-info:file-name = pt_tmpdir.					/* make certain that we have a temp directory!		*/
if file-info:full-pathname = ? then
  os-command silent value( "mkdir " + pt_tmpdir ).

file-info:file-name = pt_logdir.					/* make certain that we have a log directory!		*/
if file-info:full-pathname = ? then
  os-command silent value( "mkdir " + pt_logdir ).

run ssg/sausage02.p persistent.
run ssg/sausage04.p persistent.
run ssg/sausage03.p persistent.

output to value( pt_logdir + "/pt3upd.log" ) append unbuffered.

message now "Checking" pt_backend "for updates".

run autoUpdater( pt_backend ).

output close.

quit.
