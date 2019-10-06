/*

02.13.97 by CAH on \\ricky\rprorel Log#0000:
1.  Changed ws_time to stat_time, conflicts with rc/timev.i declaration.

02.12.94 by CH on TI4000 Log#0000:
1.  Added {1} to allow creation of variables as "[NEW] SHARED".
    This allows called procedures to adjust stats based on processing
    which they do and return results to caller.

2.  Note that frames are not shared; caller does the screen IO.
3.  To facilitate this, variables carved out into rc/statvars.i.

04.26.93 by CH on TI4000:
1.  Changed white to black - white on green too little contrast.

02.06.93 BY CH:
1.  Added color white/green to frame f-stats.

12.24.91 by CH:
1.  Consolidated tps.
2.  Added amount buckets corresponding to record counts.

05.15.91 by hs:
1.   eliminated no-undo on added, changed and deleted variables
*/
{rc/statvars.i " {1} " }

def {1} var start as integer                          NO-UNDO.
def var done            as integer                          NO-UNDO.
def var transactions    as integer                          NO-UNDO.
def var elapsed         as integer      format ">>>>9"      NO-UNDO.
def var tps             as decimal      format ">>>.9"      NO-UNDO.

FORM
    "Records read           :"    ws_recs_read     ws_amt_read skip
    "Selected for processing:"    ws_recs_selected ws_amt_selected skip
    "Additions              :"    ws_recs_added    ws_amt_added skip
    "Changes                :"    ws_recs_changed  ws_amt_changed skip
    "Deletions              :"    ws_recs_deleted  ws_amt_deleted skip
    "Errors encountered     :"    ws_recs_inerror  ws_amt_inerror skip
    "Transactions processed :"    transactions                      SKIP
    "Elapsed time           :"    elapsed                           SKIP
    "Transactions per second:"    tps
WITH FRAME f-stats TITLE "PROCESSING STATISTICS" CENTERED NO-LABELS
    COLOR value(c_add) stream-io.
