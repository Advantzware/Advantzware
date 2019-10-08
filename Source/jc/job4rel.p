DEF PARAM BUFFER io-job FOR job.
DEF PARAM BUFFER io-rel FOR oe-rel.

RELEASE io-rel.

IF AVAIL io-job THEN
FIND FIRST io-rel NO-LOCK
     WHERE io-rel.company eq io-job.company
       AND io-rel.job eq io-job.job
     no-error.

