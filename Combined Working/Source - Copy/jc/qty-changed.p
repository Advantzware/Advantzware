
DEF PARAM BUFFER io-job FOR job.
DEF OUTPUT PARAM op-qty-changed AS LOG NO-UNDO.

IF AVAIL io-job THEN
   op-qty-changed = io-job.qty-changed.
