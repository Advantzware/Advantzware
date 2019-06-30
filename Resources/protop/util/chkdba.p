/* util/chkdba.p
 *
 * parse dbanalys oputput
 *
 * [m]pro dbname -p util/chkdba.p -param dbname.dba
 *
 * you must be connected to some database -- if it is not the same db that was used
 * to generate the dbanalys then some features (b2 status of an object, rm chain
 * analysis) will not be available but the basic parsing should still run
 *
 */

define new global shared variable dbgMode as integer no-undo initial 1.

run lib/parsedba.p persistent.

file-info:filename = session:parameter.
if file-info:full-pathname <> ? then
  run parseDBA( file-info:full-pathname ).
 else
  do:
    message "Cannot find file to parse:" session:parameter view-as alert-box.
    return.
  end.

if program-name( 4 ) = ? then
  quit.
 else
  return.
