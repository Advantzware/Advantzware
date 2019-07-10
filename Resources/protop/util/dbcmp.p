/* dbcmp.p
 *
 */

define temp-table tt_dbx no-undo
  field tblName  as character format "x(40)"
  field newRecs  as int64 label "new"
  field oldRecs  as int64 label "old"
  field numBytes as int64
  field inBoth   as logical
  index tblName-idx is primary unique tblName
.

define variable oldDBX as character no-undo format "x(70)".
define variable newDBX as character no-undo format "x(70)".

define variable t as character no-undo label "table" format "x(40)".
define variable r as int64 label "old".
define variable x as int64.

define stream inStrm.
define stream outStrm.

if num-entries( session:parameter ) >= 1 then oldDBX = entry( 1, session:parameter ).
if num-entries( session:parameter ) >= 2 then newDBX = entry( 2, session:parameter ).

if session:batch = no then
  update
    oldDBX skip
    newDBX skip
   with
    side-labels
  .

input stream inStrm from value( newDBX ).
repeat:
  create tt_dbx.
  import stream inStrm ^ ^ tblName ^ newRecs numBytes.
end.
input stream inStrm close.

output stream outStrm to value( "rpt/dbcmp.rpt" ). 
input stream inStrm from value( oldDBX ).
repeat:
  import stream inStrm ^ ^ t ^ r x.
  find tt_dbx where tt_dbx.tblName = t no-error.
  if available tt_dbx then
    assign
      inBoth = true
    .
   else
    do:
      create tt_dbx.
      assign
        tt_dbx.tblName = t
      .
    end.
  oldRecs = r.
  display stream outStrm t oldRecs newRecs (newRecs - oldRecs) label "delta".
end.
input stream inStrm close.

for each tt_dbx where tblName <> "" and (( inBoth = false ) or ( newRecs <> oldRecs )):
  display stream outStrm tblName oldRecs newRecs (newRecs - oldRecs) label "delta".
end.

output stream outStrm close.

quit.
