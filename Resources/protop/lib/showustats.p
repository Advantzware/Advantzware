/* showustats.p
 *
 */

{lib/userstats.i}

define variable t as integer no-undo.
define variable i as integer no-undo.

display
  skip(1)
  '        The point of this is to demonstrate the gathering of user table and index stats from  ' skip
  '        within a running session.  It isn~'t really all that insightful with regards to your   ' skip
  '        specific application.                                                                 ' skip
  skip(1)
  '        Unless you have modified protop.p to generate some test traffic you probably won~'t   ' skip
  '        see much happening.  If your table and index ranges are set to look at meta-schema    ' skip
  '        activity you will mostly see the "user experience" queries running.                   ' skip
  skip(1)
  '        The ProTop session should not be doing any IO against your application tables unless  ' skip
  '        you have implemented an "application specific monitoring" plugin.                     ' skip
  skip(32)
  '        The sample code is in: lib/userstats.i, lib/usertablestats.p and lib/showustats.p     ' skip
  skip(2)
 with frame bg
  overlay
  width 98
  row 3
  centered
  no-box
.

run getUStats ( output table tt_usrTblInfo by-reference, output table tt_usrIdxInfo by-reference ).

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
    frame show_usrTblInfo
    title " Top 10 Tables Used by My Session "
    overlay
    centered
    10 down
    row 16
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
    frame show_usrIdxInfo
    title " Top 10 Indexes Used by My Session "
    overlay
    centered
    10 down
    row 30
  .
  if i >= 10 then leave.
end.

pause.

hide frame show_usrTblInfo.
hide frame show_usrIdxInfo.
hide frame bg.

return.
