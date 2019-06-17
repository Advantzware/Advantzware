/* userstats.i
 *
 * support for lib/usertablestats.p
 *
 */

define temp-table tt_usrTblInfo no-undo
  field tblName as character format "x(30)"
  field tblRd   as int64
  field tblCr   as int64
  field tblUp   as int64
  field tblDl   as int64
  index tblName-idx is unique tblName
.

define temp-table tt_usrIdxInfo
  field idxName as character format "x(41)"
  field idxRd   as int64
  field idxCr   as int64
  field idxDl   as int64
  index idxName-idx is unique idxName
.
