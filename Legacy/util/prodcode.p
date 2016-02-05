
DEF VAR li AS INT NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF itemfg.
DISABLE TRIGGERS FOR LOAD OF oe-ordl.


FOR EACH company NO-LOCK,
    EACH itemfg
    WHERE itemfg.company EQ company.company
    BY company.company
    BY i-no:
  DISPLAY company.company
          i-no FORMAT "x(20)".

  /*RUN fg/prodcode.p (ROWID(itemfg)).*/

  IF TRIM(itemfg.type-code) EQ ""      OR
     INDEX("NR",itemfg.type-code) GT 0 THEN itemfg.type-code = "N".

  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ itemfg.company
        AND oe-ordl.i-no    EQ itemfg.i-no
      BY oe-ordl.ord-no:

    IF TRIM(oe-ordl.type-code) EQ ""      OR
       INDEX("NR",oe-ordl.type-code) GT 0 THEN DO:
      IF TRIM(itemfg.type-code) NE "" THEN oe-ordl.type-code = itemfg.type-code.

      FOR EACH job-hdr NO-LOCK
          WHERE job-hdr.company EQ itemfg.company
            AND job-hdr.i-no    EQ itemfg.i-no
          BREAK BY job-hdr.job:

        IF (job-hdr.job-no  EQ oe-ordl.job-no AND
            job-hdr.job-no2 EQ oe-ordl.job-no2)   OR
            LAST(job-hdr.job)                     THEN DO:
          IF NOT FIRST(job-hdr.job) THEN oe-ordl.type-code = "R".
          LEAVE.
        END.
      END.
    END.

    IF TRIM(itemfg.type-code) EQ ""      OR
       INDEX("NR",itemfg.type-code) GT 0 THEN itemfg.type-code = oe-ordl.type-code.
  END.
END.

HIDE ALL NO-PAUSE.

MESSAGE "Utility Completed..." VIEW-AS ALERT-BOX.
