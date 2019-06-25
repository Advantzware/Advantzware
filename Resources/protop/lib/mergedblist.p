/* lib/mergedblist.p
 *
 * merge local etc/dblist.cfg with portal resources
 *
 */

define input-output parameter custId  as character   no-undo.
define input        parameter host    as character   no-undo.
define input        parameter opMode  as character   no-undo.		/* "POST" = send merged list back to portal		*/
define output       parameter mergeStatus as logical no-undo.

define new global shared variable logLevel as integer no-undo initial 5.


/* main body
 */

publish "logMsg" ( 0, substitute( "&1: &2 &3", "lib/mergelist", custId, host )).

run getDBList ( custId, host ).

run readDBList.
run mergeDBList ( custId ).

mergeStatus = true.

if index( opMode, "POST" ) > 0 and custId <>  "" and custId <> ? and custId <> "local" then
  do:
    publish "logMsg" ( 0, substitute( "&1: &2 host = &3 custId = &4", "lib/mergedblist.p", "posting etc/dblist.cfg to portal", host, custId )).
    run postdblist ( custId, host, output mergeStatus ).
  end.

if mergeStatus = false then
  do:
    publish "logMsg" ( 0, substitute( "&1: &2 &3 &4", "lib/mergedblist.p", "failed", custId, host )).
  end.
 else
  do:
    publish "logMsg" ( 0, substitute( "&1: &2", "lib/mergedblist.p", "saving etc/dblist.cfg" )).
    run saveDBList.								/* do this *after* the post to ensure that the server timestamp is later than the portal timestamp	*/
  end.

return.
