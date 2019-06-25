/* qryInfo.p
 *
 * this only handles single table queries -- joins are (obviously) much more complicated
 *
 * to do: actually run the query and add table and index usage stats
 *
 *
 */

define variable tblName   as character no-undo label "Table"        format "x(255)" view-as fill-in size 15 by 1.
define variable qryString as character no-undo label "WHERE Clause" format "x(255)" view-as fill-in size 40 by 1.

define variable idxInfo   as character no-undo label "Index Info"   format "x(20)".
define variable idxUsed   as character no-undo label "Index Used"   format "x(70)".
define variable idxFields as character no-undo label "Index Fields" format "x(40)".

define variable qh as handle no-undo.
define variable bh as handle no-undo.

define variable i as integer no-undo.
define variable j as integer no-undo.

update
  "FOR EACH" tblName "NO-LOCK WHERE" qryString skip
  skip(1)
 with
  frame querySetup
  no-labels
  no-box
.

create buffer bh for table tblName.
create query qh.
qh:set-buffers( bh ).
qh:query-prepare( "for each " + tblName + " no-lock where " + qryString ).
qh:query-open.

/* show the index(es) that have actually been selected
 *
 * if the criteria require a table scan WHOLE-INDEX will be shown first and only one index will be used
 *
 */

idxInfo = qh:index-information( 1 ).		/* 1 = 1st join level, with a single table we don't need to go deeper	*/

do i = 1 to num-entries( idxInfo ):

  display entry( i, idxInfo ) format "x(30)"
   with
    frame indexUsed
    title "Index(es) Selected"
    num-entries( idxInfo ) down
    /* row 5 */
  .

  down with frame indexUsed.

end.

/* show the indexes that are available for this table
 */

i = 1.
do while bh:index-information( i ) <> ?:

  idxInfo = bh:index-information( i ).

  idxFields = "".

  /* after the field name a 0 means "ascending", 1 means "descending"
   */

  do j = 5 to num-entries( idxInfo ):
    idxFields = idxFields + " " + entry( j, idxInfo ).
  end.

  display
    entry( 1, idxInfo ) label "idxName" format "x(30)"
    entry( 2, idxInfo ) label "U" format "x"
    entry( 3, idxInfo ) label "P" format "x"
    entry( 4, idxInfo ) label "W" format "x"
    idxFields
   with
    frame idxList
    10 down
    title " Available Indexes "
  .

  down 1 with frame idxList.
  i = i + 1.

end.

delete object bh.
delete object qh.

return.
