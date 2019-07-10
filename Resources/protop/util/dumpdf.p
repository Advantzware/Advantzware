/* dumpdf.p
 *
 * dump schema
 *
 * 08/05/02     tom     created
 *
 * August 5, 2002 */

define variable dfxname as character no-undo.
define variable dfzname as character no-undo.

dfxname = session:parameter.
if dfxname = "" or dfxname = ? then dfxname = ldbname(1) + ".dfx".

dfzname = replace( dfxname, "dfx", "df.noarea" ).

run prodict/dump_df.p ( "ALL", input dfxname, "" ).

if dfxname <> dfzname then
  os-command value( substitute( 'grep -v "^  AREA " &1 > &2', dfxname, dfzname )).

quit.
