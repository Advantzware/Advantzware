
DEF INPUT PARAMETER v-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

DEF SHARED BUFFER xest FOR est.
DEF SHARED BUFFER xef  FOR ef.
DEF SHARED BUFFER xeb  FOR eb.

DEF VAR lv-cebrowse-dir AS cha NO-UNDO.
DEFINE VARIABLE cCeBrowseBaseDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmp-dir AS CHARACTER NO-UNDO.
{ce/print4.i SHARED SHARED}
{ce/print42.i SHARED}

find first sys-ctrl where
    sys-ctrl.company eq cocode AND
    sys-ctrl.name    eq "CEBROWSE"
    no-lock no-error.

if not avail sys-ctrl then DO TRANSACTION:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CEBROWSE"
   sys-ctrl.descrip = "# of Records to be displayed in browser"
   sys-ctrl.log-fld = YES
   sys-ctrl.char-fld = "CE"
   sys-ctrl.int-fld = 30.
end.

RUN est/EstimateProcsOld.p (cocode, OUTPUT cCEBrowseBaseDir, OUTPUT tmp-dir ).

lv-cebrowse-dir = tmp-dir.

IF LOOKUP(SUBSTRING(lv-cebrowse-dir,LENGTH(lv-cebrowse-dir)),"\,/") EQ 0 THEN
   lv-cebrowse-dir = lv-cebrowse-dir + "\".

lv-cebrowse-dir = REPLACE(lv-cebrowse-dir,"/","\").

DEF BUFFER probe-ref FOR reftable.

DEF VAR num-probeit AS INT NO-UNDO.

DEF NEW SHARED FRAME probe.

FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.
    
{ce/probe.f}
{ce/probeit.f}
{sys/inc/cerun.i F}

     
FIND probe WHERE ROWID(probe) EQ v-rowid NO-LOCK.

IF NOT AVAIL xest THEN
FIND FIRST xest NO-LOCK
    WHERE xest.company EQ probe.company
      AND xest.est-no  EQ probe.est-no.

IF NOT AVAIL xeb THEN
FOR EACH xeb NO-LOCK
    WHERE xeb.company EQ probe.company
      AND xeb.est-no  EQ probe.est-no
      AND xeb.form-no NE 0
    BY xeb.comm DESC:
  LEAVE.
END.

IF probe.LINE LT 100 THEN
   OUTPUT TO VALUE(lv-cebrowse-dir + TRIM(xest.est-no) + ".s" +
                   STRING(probe.line,"99")) APPEND /*PAGE-SIZE 60*/.
ELSE
   OUTPUT TO VALUE(lv-cebrowse-dir + TRIM(xest.est-no) + ".s" +
                  STRING(probe.line,"999")) APPEND /*PAGE-SIZE 60*/.

PUT SKIP(1).

FOR EACH probe
    WHERE probe.company EQ xest.company
      AND probe.est-no  EQ xest.est-no
    NO-LOCK
    BY probe.probe-date
    BY probe.est-qty
    BY probe.probe-time
    WITH FRAME probe:
  {ce/probe.v}
  DOWN.
END.

FIND probe WHERE ROWID(probe) EQ v-rowid NO-LOCK NO-ERROR.

IF xest.est-type NE 1 THEN DO:
  PUT SKIP(1).
  FOR EACH probeit
      WHERE probeit.company EQ probe.company
        AND probeit.est-no  EQ probe.est-no
        AND probeit.line    EQ probe.line
      NO-LOCK:
     num-probeit = num-probeit + 1.
  END.

  FOR EACH probeit
      WHERE probeit.company EQ probe.company
        AND probeit.est-no  EQ probe.est-no
        AND probeit.line    EQ probe.line
      NO-LOCK,
      FIRST eb WHERE
            eb.company EQ xest.company AND
            eb.est-no EQ xest.est-no AND
            eb.part-no EQ probeit.part-no
            NO-LOCK
            BY eb.form-no
      WITH FRAME probeit:
    {ce/probeit.v}
    DOWN.
  END.
  PUT SKIP(1).
END.

OUTPUT CLOSE.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
