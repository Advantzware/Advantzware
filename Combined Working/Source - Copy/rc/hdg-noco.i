/*  hdg-noco.i - default heading fields when no driving File.Co
09.23.92 by CH:
1.  Implemented calendar/calenper.

12.18.91 by CH:
1.  Change from find first company to find company where = ws_co.
    This was required to allow for xglco considerations, where the desired
    company record (not necessarily the first) has already been retrieved.
2.  Made company find conditional upon company record not already available.


02.09.91 by CH:
1.  Removed hard-wired find of TBCO - Application may be undefined.

*/

if connected("rpro") then run rc/hdg-set.p.
else assign
hdg_perdate = (if ws_perdate = ? then today else ws_perdate)
hdg_name = ws_co_name
hdg_rpt_code = program-name(1)
.
