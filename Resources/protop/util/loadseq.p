/* loadseq.p 
 *
 * bpro dbname -p util/loadseq.p -param "_seqvals|/loadDir/_seqvals.d" > logfile 2>&1
 *
 */

define variable tblName as character no-undo.
define variable loadFile as character no-undo.

define variable seqNum     as integer   no-undo.
define variable seqName    as character no-undo.
define variable seqCurrVal as int64     no-undo.

define stream inStrm.

assign
  tblName = entry( 1, session:parameter, "|" )
  loadFile = entry( 2, session:parameter, "|" )
no-error.

if tblName <> "_seqvals" then return.

if loadFile = "" or loadFile = ? then loadFile = "./_seqvals.d".

file-info:file-name = loadFile.
if file-info:full-pathname = ? then
  do:
    message "cannot find" file-info:file-name.
    return.
  end.

input stream inStrm from value( file-info:full-pathname ).
repeat:
  seqName = ?.
  import stream inStrm seqNum seqName seqCurrVal.
  if seqName <> ? then
    do:
      display seqNum seqName seqCurrVal format ">,>>>,>>>,>>>,>>>,>>>,>>9".
      dynamic-current-value( seqName, "dictdb" ) = seqCurrVal no-error.
    end.
end.
input stream inStrm close.

return.
