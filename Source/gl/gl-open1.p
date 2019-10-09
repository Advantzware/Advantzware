
DEF INPUT  PARAM ip-recid    AS   RECID.
DEF INPUT  PARAM ip-curr-yr  AS   INT.
DEF INPUT  PARAM ip-date     AS   DATE.
DEF INPUT  PARAM ip-period   AS   INT.
DEF OUTPUT PARAM op-open-bal AS   DEC.

{sys/inc/var.i shared}
{sys/form/s-top.f}

DEF NEW SHARED VAR udate AS DATE NO-UNDO.
DEF NEW SHARED VAR uperiod AS INT NO-UNDO.


ASSIGN
 udate   = ip-date
 uperiod = ip-period.

RUN gl/gl-open.p (ip-recid, ip-curr-yr, OUTPUT op-open-bal).
