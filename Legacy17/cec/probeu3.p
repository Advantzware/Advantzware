/* -------------------------------------------------- cec/probeu2.p 03/97 JLF */
/* 'What if' UPDATE                                                           */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAMETER v-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

DEF SHARED BUFFER xest FOR est.
DEF SHARED BUFFER xef  FOR ef.
DEF SHARED BUFFER xeb  FOR eb.

DEF SHARED VAR tmp-dir AS cha NO-UNDO.

{cec/print4.i SHARED SHARED}
{cec/print42.i SHARED}

DEF VAR num-probeit AS INT NO-UNDO.
DEF VAR li-seq AS INT INIT 0 NO-UNDO.
DEF VAR v-est-list AS CHAR FORMAT "x(54)" INIT "" NO-UNDO.
DEF VAR cerunc AS CHAR NO-UNDO.
DEF VAR v-line AS CHAR FORMAT "X(100)" NO-UNDO.
DEF VAR v-probe-line-fmt AS CHAR NO-UNDO.

DEF NEW SHARED FRAME probe.
DEF NEW SHARED FRAME probe-peach.

FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.
    
{cec/probe.f}
{cec/box/probeit.f}

FIND probe WHERE ROWID(probe) EQ v-rowid NO-LOCK NO-ERROR.

IF probe.LINE LT 100 THEN
   v-probe-line-fmt = "99".
ELSE
   v-probe-line-fmt = "999".

OUTPUT TO VALUE(tmp-dir + TRIM(xest.est-no) + ".s" +
                STRING(probe.line,v-probe-line-fmt)) APPEND /*PAGE-SIZE 60*/.

PUT SKIP(1).

DEF VAR cerunc-dec AS DEC NO-UNDO.
DEFINE VARIABLE cCerun-char AS CHARACTER NO-UNDO.
FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ xest.company
      AND sys-ctrl.NAME EQ "CERUN" 
     NO-ERROR.
ASSIGN cCerun-char = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "" .

FIND FIRST sys-ctrl WHERE
     sys-ctrl.company EQ xest.company AND
     sys-ctrl.NAME EQ "CERUNC"
     NO-LOCK.

ASSIGN
   cerunc-dec = sys-ctrl.dec-fld
   cerunc     = sys-ctrl.char-fld.

IF cCerun-char NE "PEACHTRE" THEN
DO:  
    IF cerunc-dec EQ 0 THEN
    DO:
       put unformatted
           "     E S T I M A T E  " + "#" + trim(xest.est-no) +
           "   A N A L Y S I S   P e r  T h o u s a n d     " format "x(78)" skip.
    
       find first sys-ctrl where sys-ctrl.company eq cocode
                            and sys-ctrl.name eq "CEPrint" no-lock no-error.
       put unformatted
           (if avail sys-ctrl and sys-ctrl.char-fld ne 'Text' then "<P10>" else "")
           "            Tot.Fact      Full"
           space(17)
           "     Sell    Price  Total   Total" skip
           "    Qty  R      Cost      Cost"
           fill(" ",8 - length(trim(ce-ctrl.hd-gross))) + trim(ce-ctrl.hd-gross) format "x(8)"
           fill(" ",8 - length(trim(ce-ctrl.hd-net))) + trim(ce-ctrl.hd-net)     format "x(8)"
           "     Price     /BSF Sheets     MSF"
           skip.
    END.
    ELSE
    DO:
       put unformatted
           "     E S T I M A T E  " + "#" + trim(xest.est-no) +
           "   A N A L Y S I S   P e r  T h o u s a n d     " format "x(78)" skip.
    
       find first sys-ctrl where sys-ctrl.company eq cocode
                            and sys-ctrl.name eq "CEPrint" no-lock no-error.
       put unformatted
        (if avail sys-ctrl and sys-ctrl.char-fld ne 'Text' then "<P10>" else "")
         "               Tot.Fact         Full"
         space(17)
         "         Sell          Price  Total   Total" skip
         "    Qty  R         Cost         Cost"
         fill(" ",8 - length(trim(ce-ctrl.hd-gross))) + trim(ce-ctrl.hd-gross) format "x(8)"
         fill(" ",8 - length(trim(ce-ctrl.hd-net))) + trim(ce-ctrl.hd-net)     format "x(8)"
         "         Price           /BSF Sheets     MSF"
         skip.
    END.
END.

IF cerunc-dec EQ 0 THEN
FOR EACH probe
    WHERE probe.company EQ xest.company
      AND probe.est-no  EQ xest.est-no
    NO-LOCK
    BY probe.probe-date
    BY probe.est-qty
    WITH FRAME probe:
  {cec/probe.v}
  PUT SKIP.
END.
ELSE
FOR EACH probe
    WHERE probe.company EQ xest.company
      AND probe.est-no  EQ xest.est-no
    NO-LOCK
    BY probe.probe-date
    BY probe.est-qty
    WITH FRAME probe-big:
  {cec/probe.v}
  PUT SKIP.
END.

FIND probe WHERE ROWID(probe) EQ v-rowid NO-LOCK NO-ERROR.

IF xest.est-type NE 5 THEN DO:
  PUT SKIP(1).
  FOR EACH probeit
      WHERE probeit.company EQ probe.company
        AND probeit.est-no  EQ probe.est-no
        AND probeit.line    EQ probe.line
      NO-LOCK:
     num-probeit = num-probeit + 1.
  END.

IF cerunc NE "Protagon" THEN
  IF cerunc-dec EQ 0 THEN DO:
    FOR EACH eb WHERE
        eb.company EQ xest.company AND
        eb.est-no EQ xest.est-no
        NO-LOCK,
        EACH probeit
        WHERE probeit.company EQ probe.company
          AND probeit.est-no  EQ probe.est-no
          AND probeit.line    EQ probe.line
          AND probeit.part-no EQ eb.part-no
        NO-LOCK
        BY eb.form-no
        WITH FRAME probeit:
      {cec/box/probeit.v}
      DOWN.
    END.
  END.
  ELSE DO:
    FOR EACH eb WHERE
        eb.company EQ xest.company AND
        eb.est-no EQ xest.est-no
        NO-LOCK,
        EACH probeit
        WHERE probeit.company EQ probe.company
          AND probeit.est-no  EQ probe.est-no
          AND probeit.line    EQ probe.line
          AND probeit.part-no EQ eb.part-no
        NO-LOCK
        BY eb.form-no
        WITH FRAME probeit-large:
      {cec/box/probeit.v}
      DOWN.
    END.
 END.
ELSE
IF cerunc-dec EQ 0 THEN
    FOR EACH eb WHERE
        eb.company EQ xest.company AND
        eb.est-no EQ xest.est-no
        NO-LOCK,
        EACH probeit
        WHERE probeit.company EQ probe.company
          AND probeit.est-no  EQ probe.est-no
          AND probeit.line    EQ probe.line
          AND probeit.part-no EQ eb.part-no
        NO-LOCK
        BY eb.form-no
        BY eb.part-no
        WITH FRAME probeit-protagon:
      {cec/box/probeitp.v}
      DOWN.
    END.
  ELSE
     FOR EACH eb WHERE
        eb.company EQ xest.company AND
        eb.est-no EQ xest.est-no
        NO-LOCK,
        EACH probeit
        WHERE probeit.company EQ probe.company
          AND probeit.est-no  EQ probe.est-no
          AND probeit.line    EQ probe.line
          AND probeit.part-no EQ eb.part-no
        NO-LOCK
        BY eb.form-no
        BY eb.part-no
        WITH FRAME probeit-protagon-large:
      {cec/box/probeitp.v}
      DOWN.
    END.

  PUT SKIP(1).
END.

IF cerunc EQ "Protagon" AND SEARCH(tmp-dir + TRIM(xest.est-no) + ".z" +
                                   STRING(probe.line,v-probe-line-fmt)) NE ? THEN
DO:
   INPUT FROM VALUE(tmp-dir + TRIM(xest.est-no) + ".z" +
                    STRING(probe.line,v-probe-line-fmt)) NO-ECHO.

   repeat:
      v-line = "".
      IMPORT UNFORMATTED v-line.
   
      if v-line eq "" then put skip(1).
      else put unformatted v-line skip.
   end.
   input close.
   
END.

FIND FIRST reftable
    WHERE reftable.reftable EQ "est/getqty.w"
      AND reftable.company  EQ xest.company
      AND reftable.loc      EQ xest.loc
      AND reftable.code     EQ xest.est-no
    NO-LOCK NO-ERROR.
IF AVAIL reftable THEN li-seq = INT(reftable.code2).

IF li-seq NE 0 THEN
FOR EACH reftable
    WHERE reftable.reftable EQ "est/getqty.w"
      AND reftable.code2    EQ STRING(li-seq,"9999999999")
      AND reftable.code     NE xest.est-no
    USE-INDEX code2 NO-LOCK
    BREAK BY reftable.code:

  IF FIRST-OF(reftable.code) THEN DO:
    IF LENGTH(TRIM(v-est-list)) + LENGTH(TRIM(reftable.code)) GT 54 OR
       FIRST(reftable.code)                                         THEN DO:

      IF FIRST(reftable.code) THEN PUT SKIP(1) "Estimates for Board Cost:".
      ELSE PUT v-est-list AT 27 SKIP.

      v-est-list = reftable.code.
    END.

    ELSE v-est-list = TRIM(v-est-list) + "," + TRIM(reftable.code).
  END.

  IF LAST(reftable.code) THEN PUT v-est-list AT 27 SKIP(1).
END.

OUTPUT CLOSE.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
