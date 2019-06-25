/* dbmonitor.p
 *
 * monitor databases, runs as a service in the background
 *
 */

{lib/protop.i}
{lib/protoplib.i}

/* dbgMode = 1	minimal
 * dbgMode = 3	errors
 * dbgMode = 4	errors + success
 * dbgMode = 5	verbose socket communications details (overkill)
 */

dbgMode = 2.

define variable updCheck as integer   no-undo initial 300.		/* how long between checking for updates?		*/
define variable monName  as character no-undo.

run lib/windows.p.

{ssg/sausage13.i}

return.
