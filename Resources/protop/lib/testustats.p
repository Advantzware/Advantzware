/* testustats.p
 *
 */

{lib/userstats.i}

run usertablestats.p persistent.

define variable t as integer no-undo.
define variable i as integer no-undo.

/*
for each dictdb.order no-lock:
end.
 */

run getUStats ( output table tt_usrTblInfo by-reference, output table tt_usrIdxInfo by-reference ).


/***

/* show the top 10 tables and indexes (by reads)
 */

t = 0.
for each tt_usrTblInfo by tt_usrTblInfo.tblRd descending:
  t = t + 1.
  display
    tt_usrTblInfo.tblName
    tt_usrTblInfo.tblRd
    tt_usrTblInfo.tblCr
    tt_usrTblInfo.tblUp
    tt_usrTblInfo.tblDl
   with
    10 down
    row 1
  .
  if t >= 10 then leave.
end.

i = 0.
for each tt_usrIdxInfo by tt_usrIdxInfo.idxRd descending:
  i = i + 1.
  display
    tt_usrIdxInfo.idxName
    tt_usrIdxInfo.idxRd
    tt_usrIdxInfo.idxCr
    tt_usrIdxInfo.idxDl
   with
    10 down
    row 15
  .
  if i >= 10 then leave.
end.
 */
return.
