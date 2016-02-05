
DEF PARAM BUFFER io-jobh FOR job-hdr.
DEF PARAM BUFFER io-rel  FOR oe-rel.
DEF PARAM BUFFER io-ordl FOR oe-ordl.


RELEASE io-ordl.

IF AVAIL io-rel THEN
FIND FIRST io-ordl NO-LOCK
    WHERE io-ordl.company EQ io-rel.company
      AND io-ordl.ord-no  EQ io-rel.ord-no
      AND io-ordl.i-no    EQ io-rel.i-no
      AND io-ordl.line    EQ io-rel.line
    NO-ERROR.

ELSE
FIND FIRST io-ordl NO-LOCK
    WHERE io-ordl.company  EQ io-jobh.company
      AND ((io-ordl.ord-no EQ io-jobh.ord-no AND
            io-jobh.ord-no NE 0) OR
           io-jobh.ord-no  EQ 0)
      AND io-ordl.job-no   EQ io-jobh.job-no
      AND io-ordl.job-no2  EQ io-jobh.job-no2
      AND io-ordl.i-no     EQ io-jobh.i-no
      AND io-ordl.est-no   EQ io-jobh.est-no
    NO-ERROR.
