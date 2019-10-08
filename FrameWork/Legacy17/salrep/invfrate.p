
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-slsm1 AS CHAR  NO-UNDO.
DEF INPUT PARAM ip-slsm2 AS CHAR  NO-UNDO.

{salrep/ttreport.i}

DEF BUFFER b-ar-invl FOR ar-invl.

DEF VAR li AS INT NO-UNDO.
DEF VAR ld-frt AS DEC DECIMALS 10 NO-UNDO.
DEF VAR lv-sman AS CHAR NO-UNDO.
DEF VAR ld-spct AS DEC NO-UNDO.
DEF VAR ld-frt-tot AS DEC EXTENT 3 NO-UNDO.
DEF VAR ll-all-slsm AS LOG NO-UNDO.


ll-all-slsm = ip-slsm1 EQ "allsalesmen".

FIND ar-inv WHERE ROWID(ar-inv) EQ ip-rowid NO-LOCK NO-ERROR.

IF NOT AVAIL ar-inv THEN
FIND ar-invl WHERE ROWID(ar-invl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL ar-invl THEN
FIND FIRST ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK NO-ERROR.

IF AVAIL ar-inv THEN
FIND FIRST cust
    WHERE cust.company EQ ar-inv.company
      AND cust.cust-no EQ ar-inv.cust-no
    NO-LOCK NO-ERROR.

IF AVAIL cust THEN
FOR EACH b-ar-invl
    WHERE b-ar-invl.x-no EQ ar-inv.x-no
      AND (NOT AVAIL ar-invl OR
           ROWID(b-ar-invl) EQ ROWID(ar-invl))
    NO-LOCK:

  ld-frt-tot[2] = 0.

  RUN ar/getfrtcs.p (ROWID(b-ar-invl), b-ar-invl.inv-qty, OUTPUT ld-frt).

  IF ll-all-slsm THEN DO:
    CREATE tt-report2.

    ASSIGN
     tt-report2.inv-no  = ar-inv.inv-no
     tt-report2.term-id = ""
     tt-report2.rec-id  = RECID(b-ar-invl)
     tt-report2.key-01  = "FREIGHT"
     tt-report2.key-09  = cust.cust-no
     tt-report2.key-10  = "ar-invf"
     ld-frt-tot[1]      = ld-frt
     ld-frt-tot[2]      = ld-frt-tot[2] + ld-frt-tot[1]
     tt-report2.freight = ld-frt-tot[1].
  END.

  ELSE DO:
    DO li = 1 TO 3:
      IF b-ar-invl.sman[li] EQ "" AND li EQ 1 THEN
        ASSIGN
         lv-sman = cust.sman
         ld-spct = 100.

      ELSE
        ASSIGN
         lv-sman = b-ar-invl.sman[li]
         ld-spct = b-ar-invl.s-pct[li].

      IF lv-sman LT ip-slsm1               OR
         lv-sman GT ip-slsm2               OR
         (li NE 1 AND
          (lv-sman EQ "" OR ld-spct EQ 0)) THEN NEXT.

      FIND FIRST tt-report2
          WHERE tt-report2.key-10 EQ "ar-invf"
            AND tt-report2.inv-no EQ ar-inv.inv-no
            AND tt-report2.sman   EQ lv-sman
            AND tt-report2.rec-id EQ RECID(b-ar-invl)
          NO-ERROR.

      IF NOT AVAIL tt-report2 THEN DO:
        CREATE tt-report2.

        ASSIGN
         tt-report2.inv-no  = ar-inv.inv-no
         tt-report2.sman    = lv-sman
         tt-report2.term-id = ""
         tt-report2.rec-id  = RECID(b-ar-invl)
         tt-report2.key-01  = "FREIGHT"
         tt-report2.key-09  = cust.cust-no
         tt-report2.key-10  = "ar-invf".
      END.

      IF ld-spct EQ 0 THEN ld-spct = 100.

      ASSIGN
       ld-frt-tot[1]      = ld-frt * ld-spct / 100
       ld-frt-tot[2]      = ld-frt-tot[2] + ld-frt-tot[1]
       tt-report2.freight = tt-report2.freight + ld-frt-tot[1].
    END.

    IF ld-frt-tot[2] NE ld-frt THEN
    FOR EACH tt-report2
        WHERE tt-report2.key-10 EQ "ar-invf"
          AND tt-report2.inv-no EQ ar-inv.inv-no
          AND tt-report2.rec-id EQ RECID(ar-invl):
      tt-report2.freight = tt-report2.freight + (ld-frt - ld-frt-tot[2]).
      LEAVE.
    END.
  END.

  ld-frt-tot[3] = ld-frt-tot[3] + ld-frt-tot[2].
END.

IF NOT AVAIL ar-invl AND AVAIL ar-inv AND ld-frt-tot[3] NE ar-inv.freight THEN
FOR EACH tt-report2
    WHERE tt-report2.key-10 EQ "ar-invf"
      AND tt-report2.inv-no EQ ar-inv.inv-no:
  tt-report2.freight = tt-report2.freight + (ar-inv.freight - ld-frt-tot[3]).
  LEAVE.
END.
