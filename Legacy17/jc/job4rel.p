DEF PARAM BUFFER io-job FOR job.
DEF PARAM BUFFER io-rel FOR oe-rel.

DEF BUFFER oe-rel-job FOR reftable.


RELEASE io-rel.

FIND FIRST oe-rel-job NO-LOCK
    WHERE oe-rel-job.reftable EQ "oe-rel.job"
      AND oe-rel-job.code2    EQ STRING(io-job.job,"9999999999")
    USE-INDEX code2 NO-ERROR.

IF AVAIL oe-rel-job THEN
FIND FIRST io-rel NO-LOCK
    WHERE io-rel.r-no EQ INT(oe-rel-job.code)
    NO-ERROR.
