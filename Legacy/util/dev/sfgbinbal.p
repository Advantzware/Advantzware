/* sfgbinbal.p - batch procedure for util/fgbinbal.w - rstark - 11.8.2013 */

&SCOPED-DEFINE s-name sfgbinbal
{spoolrpt/s-defs.i}

parm-var-list = 'begin_i-no,end_i-no,begin_cust,end_cust,lbl_del-zer,tb_del-zer,'
              + 'lbl_del-neg,tb_del-neg,tbIncludeInactive'.

DEFINE VARIABLE begin_cust AS CHARACTER NO-UNDO.
DEFINE VARIABLE begin_i-no AS CHARACTER NO-UNDO.
DEFINE VARIABLE end_cust AS CHARACTER NO-UNDO.
DEFINE VARIABLE end_i-no AS CHARACTER NO-UNDO.
DEFINE VARIABLE lbl_del-neg AS CHARACTER NO-UNDO.
DEFINE VARIABLE lbl_del-zer AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_del-neg AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_del-zer AS LOGICAL NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-font-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE td-show-parm AS LOGICAL NO-UNDO.
DEFINE VARIABLE tbIncludeInactive AS LOGICAL NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER NO-UNDO.

PROCEDURE run-report:
  DEF VAR fcus        LIKE cust.cust-no.
  DEF VAR tcus        LIKE fcus               INIT "zzzzzzzz".
  DEF VAR fitm        LIKE itemfg.i-no.
  DEF VAR titm        LIKE fitm               INIT "zzzzzzzzzzzzzzz".
  DEF VAR vzer        AS   LOG                INIT NO.
  DEF VAR vneg        AS   LOG                INIT NO.
  DEF VAR vInclInact   AS   LOG                INIT NO.

  ASSIGN
   fitm = begin_i-no
   titm = end_i-no
   fcus = begin_cust
   tcus = end_cust
   vzer = tb_del-zer
   vneg = tb_del-neg
   vInclInact = tbIncludeInactive.
   

  FOR EACH itemfg
      WHERE itemfg.company    EQ cocode
        AND itemfg.cust-no    GE fcus
        AND itemfg.cust-no    LE tcus
        AND itemfg.i-no       GE fitm
        AND itemfg.i-no       LE titm
        AND TRIM(itemfg.i-no) NE ""
        AND (itemfg.stat EQ "A" OR vInclInact)
      USE-INDEX customer NO-LOCK
      TRANSACTION:

/*     FIND FIRST reftable                                                    */
/*       WHERE reftable.reftable EQ "FGSTATUS"                                */
/*       AND reftable.company  EQ itemfg.company                              */
/*       AND reftable.loc      EQ ""                                          */
/*       AND reftable.code     EQ itemfg.i-no NO-LOCK NO-ERROR.               */
/*     IF AVAIL reftable AND vInclInact = FALSE AND reftable.code2 = "I" THEN */
/*           NEXT.                                                            */
    RUN fg/fg-mkbin.p (RECID(itemfg)).

    IF vzer THEN
    FOR EACH fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ itemfg.i-no
          AND fg-bin.qty     EQ 0:
      DELETE fg-bin.
    END.

    IF vneg THEN
    FOR EACH fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ itemfg.i-no
          AND fg-bin.qty     LT 0:

      RUN fg/cre-pchr.p (ROWID(fg-bin), "C", 0, 0,"").

      DELETE fg-bin.
    END.

    RUN fg/fg-reset.p (RECID(itemfg)).  
  END. /* each itemfg */
END PROCEDURE.

{spoolrpt/s-proc.i}
