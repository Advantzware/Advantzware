/* util/syncdblist.p
 *
 * sync local etc/dblist.cfg with portal resources
 *
 */

{lib/protop.i}

define new global shared variable dbgMode as integer   no-undo initial 5.

define variable custId  as character no-undo.
define variable mergeStat as logical no-undo.

define stream inStrm.


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

if custId <> "" and custId <> ? and pt_backend <> "" and pt_backend <> ? then
  run lib/mergedblist.p ( input-output custId, pt_backend, "POST", output mergeStat ).

return.
