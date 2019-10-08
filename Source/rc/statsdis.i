/*  statsdis.i - display processing statistics
04.02.93 by CH on TI4000 @111:
1.  Added check for passing midnight.  Since time is always seconds since
    midnight, back 24 hours out of start to compensate.

02.28.92 by CH:
1.  Modified to use ws_recs_selected as procedures do not yet pass
    required transaction count parameter.

*/

done = time.
if start > done then start = start - (24 * 3600).
elapsed = done - start.
transactions = /* {1} */ ws_recs_selected.
tps = transactions / elapsed.

DISPLAY
    ws_recs_read            ws_amt_read
    ws_recs_selected        ws_amt_selected
    ws_recs_added           ws_amt_added
    ws_recs_changed         ws_amt_changed
    ws_recs_deleted         ws_amt_deleted
    ws_recs_inerror         ws_amt_inerror
    elapsed
    transactions
    tps
WITH FRAME F-STATS.
