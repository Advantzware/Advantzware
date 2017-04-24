/*File: cec/get-exclude-vend.p*/

DEF INPUT PARAMETER v-rowid as ROWID no-undo.
DEF INPUT PARAMETER v-cebrowse-exclude-cat AS CHAR NO-UNDO.
DEF INPUT PARAMETER v-cebrowse-exclude-vend AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER op-vend-no as cha no-undo.

{sys/inc/var.i shared}

DO TRANSACTION:
   {sys\inc\ceboard.i}
END.

DEF VAR v-forms AS INT EXTENT 2 NO-UNDO.
DEF VAR li AS INT NO-UNDO.

DEF TEMP-TABLE tt-report
    FIELD key-01 AS CHAR
    FIELD rec-id AS RECID.

FIND est WHERE ROWID(est) EQ v-rowid NO-LOCK NO-ERROR.

IF NOT AVAIL est OR
   NOT CAN-FIND(FIRST eb OF est
                WHERE eb.form-no GT 0
                  AND eb.pur-man EQ NO) THEN LEAVE.

FOR EACH ef 
    WHERE ef.company EQ est.company
      AND ef.est-no  EQ est.est-no
      AND CAN-FIND(FIRST eb OF ef WHERE NOT eb.pur-man)
    NO-LOCK:

  v-forms[1] = v-forms[1] + 1.

  /*for combos, category for each form has to be v-cebrowse-exclude-cat*/
  
  IF NOT CAN-FIND(FIRST eb OF ef WHERE
     eb.procat EQ v-cebrowse-exclude-cat) THEN NEXT.

  FOR EACH e-item-vend FIELDS(vend-no)
      WHERE e-item-vend.company EQ ef.company
        AND e-item-vend.i-no    EQ ef.board
        AND e-item-vend.vend-no EQ v-cebrowse-exclude-vend        
        AND ef.gsh-wid          GE e-item-vend.roll-w[27]
        AND ef.gsh-wid          LE e-item-vend.roll-w[28]
        AND ef.gsh-len          GE e-item-vend.roll-w[29]
        AND ef.gsh-len          LE e-item-vend.roll-w[30]
      NO-LOCK:

    CREATE tt-report.
    ASSIGN
     tt-report.key-01  = e-item-vend.vend-no
     tt-report.rec-id  = RECID(e-item-vend).
  END.
END.

FOR EACH tt-report BREAK BY tt-report.key-01:

  IF FIRST-OF(tt-report.key-01) THEN v-forms[2] = 0.

  v-forms[2] = v-forms[2] + 1.

  IF NOT LAST-OF(tt-report.key-01) OR v-forms[1] NE v-forms[2] THEN
     DELETE tt-report.
END.

adder-blok:
FOR EACH tt-report.

  FOR EACH ef
      WHERE ef.company EQ est.company
        AND ef.est-no  EQ est.est-no
        AND CAN-FIND(FIRST eb OF ef WHERE NOT eb.pur-man)
      NO-LOCK:
    DO li = 1 TO 6:
      IF ef.adder[li] NE "" AND
         NOT CAN-FIND(FIRST e-item-vend
                      WHERE e-item-vend.company EQ cocode
                        AND e-item-vend.i-no    EQ ef.adder[li]
                        AND e-item-vend.vend-no EQ tt-report.key-01) THEN DO:
        DELETE tt-report.
        NEXT adder-blok.
      END.
    END.
  END.
END.

FIND FIRST tt-report NO-ERROR.

IF AVAIL tt-report THEN
   op-vend-no = tt-report.key-01.
