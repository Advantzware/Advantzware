/* period.i */

DEF VAR i AS INT NO-UNDO.
DEF VAR period_year AS INT NO-UNDO.
DEF VAR old-yr LIKE period_year NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.

DEF BUFFER old-period FOR period.
DEF BUFFER new-period FOR period.
DEF BUFFER b-company FOR company.


{methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT op-company)"}

FIND FIRST b-company WHERE b-company.company EQ op-company NO-LOCK NO-ERROR.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
   period_year    = INT(period.yr:SCREEN-VALUE IN FRAME {&FRAME-NAME})
   period.company = op-company
   period.pstat   = YES.

  RELEASE old-period.

  IF NOT CAN-FIND(FIRST old-period
                  WHERE old-period.company EQ op-company
                    AND old-period.yr      EQ period_year) THEN
  FIND LAST old-period
      WHERE old-period.company EQ op-company
        AND old-period.yr      LT period_year
      NO-LOCK NO-ERROR.
  IF AVAIL old-period THEN DO:
    old-yr = old-period.yr.

    FOR EACH old-period
        WHERE old-period.company EQ op-company
          AND old-period.yr      EQ old-yr
        NO-LOCK:
      
      CREATE new-period.
      BUFFER-COPY old-period to new-period
      ASSIGN
       new-period.yr    = period_year
       new-period.pstat = YES
       lv-rowid         = ROWID(new-period).

      DO i = 1 TO 31:
        new-period.pst   = DATE(MONTH(old-period.pst),
                                DAY(old-period.pst) + (i - 1),
                                period_year + (YEAR(old-period.pst) - old-period.yr)) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN LEAVE.
      END.
       
      DO i = 1 TO 31:
        new-period.pend  = DATE(MONTH(old-period.pend),
                                DAY(old-period.pend) - (i - 1),
                                period_year + (YEAR(old-period.pend) - old-period.yr)) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN LEAVE.
      END.
    END.
  END.

  ELSE
  DO i = 1 TO b-company.num-per:
    FIND FIRST new-period
        WHERE new-period.company EQ op-company
          AND new-period.yr      EQ period_year
          AND new-period.pnum    EQ i
          AND ROWID(new-period)  NE ROWID(period)
        NO-LOCK NO-ERROR.

    IF NOT AVAIL new-period THEN DO:
      CREATE new-period.
      ASSIGN
       new-period.company = op-company
       new-period.yr      = period_year
       new-period.pnum    = i
       new-period.pstat   = TRUE
       lv-rowid           = ROWID(new-period).

      IF (b-company.yend-off + i) GE 13 AND b-company.yend-off NE 12 THEN
        period_year = period_year + 1.

        IF i LE 12 THEN DO:
          IF b-company.yend-off LT 12 THEN DO:
            IF (b-company.yend-off + i) MODULO 12 EQ 0 THEN
              ASSIGN
               new-period.pst  = DATE(12,1,period_year - 1)
               new-period.pend = DATE(12,31,period_year - 1).

            ELSE
              ASSIGN
               new-period.pst  =
                   DATE(((b-company.yend-off + i) MODULO 12),1,period_year - 1)
               new-period.pend =
                   DATE(((b-company.yend-off + i) MODULO 12) + 1,1,period_year - 1) - 1.
        END.

        ELSE DO:
          IF (company.yend-off + i) MODULO 12 EQ 0 THEN
            ASSIGN
             new-period.pst  = DATE(12,1,period_year)
             new-period.pend = DATE(12,31,period_year).

          ELSE
            ASSIGN
             new-period.pst  =
                  DATE(((company.yend-off + i) modulo 12),1,period_year)
             new-period.pend =
                  DATE(((company.yend-off + i) modulo 12) + 1,1,period_year) - 1.
    
        END.
      END.

      ELSE
      IF i GT 12 THEN
        ASSIGN
         new-period.pst = ?
         new-period.pend = ?.

      IF (b-company.yend-off + i) GE 13 AND b-company.yend-off NE 12 THEN
        period_year = period_year - 1.
    END.
  END.

  FIND new-period WHERE ROWID(new-period) EQ lv-rowid NO-ERROR.
      /*WHERE new-period.company EQ op-company
        AND new-period.yr      EQ period_year
        AND ROWID(new-period)  NE ROWID(period)
      NO-ERROR.*/

  IF AVAIL new-period THEN DO:
    ASSIGN
     period.pnum:SCREEN-VALUE  = STRING(new-period.pnum)
     period.pnum               = new-period.pnum
     period.pst:SCREEN-VALUE   = STRING(new-period.pst)
     period.pend:SCREEN-VALUE  = STRING(new-period.pend)
     period.pstat:SCREEN-VALUE = "YES".

    DELETE new-period.
  END.

  ELSE DO:
    RUN dispatch ("cancel-record").
    RETURN "ADM-ERROR".
  END.
END.
