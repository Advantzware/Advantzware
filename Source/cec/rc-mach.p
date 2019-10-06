
DEF PARAM BUFFER io-mach FOR mach.
DEF INPUT PARAM ip-on-f AS INT NO-UNDO.
DEF INPUT PARAM ip-defr AS LOG NO-UNDO.

{sys/inc/var.i SHARED}

DEF SHARED BUFFER xest FOR est.
DEF SHARED BUFFER xef  FOR ef.
DEF SHARED BUFFER xeb  FOR eb.

DEF SHARED VAR qty     AS INT NO-UNDO.
DEF SHARED VAR maxco   AS INT NO-UNDO.
DEF SHARED VAR xcal    AS DEC NO-UNDO.
DEF SHARED VAR sh-wid  AS DEC NO-UNDO.
DEF SHARED VAR sh-len  AS DEC NO-UNDO.

{est/d-machex.i}

DEF VAR v-run AS DEC NO-UNDO.
DEF VAR v-on-f AS INT NO-UNDO.


IF AVAIL io-mach AND AVAIL xeb THEN
FIND FIRST style NO-LOCK
   {sys/ref/styleW.i}
      AND style.style EQ xeb.style
   NO-ERROR.

IF AVAIL style THEN DO:
/*  {sys/inc/cepanel.i} - Deprecated with 17756*/

  v-on-f = ip-on-f.

  FIND mach WHERE ROWID(mach) EQ ROWID(io-mach) NO-LOCK NO-ERROR.

  {cec/mach-seq.i xef.gsh-len xef.gsh-wid xcal}

  IF NOT AVAIL mach THEN DO:
    IF AVAIL tt-mach-exc THEN DELETE tt-mach-exc.

    FIND mach WHERE ROWID(mach) EQ ROWID(io-mach) NO-LOCK NO-ERROR.

    {cec/mach-seq.i xef.gsh-wid xef.gsh-len xcal}
  END.

  IF AVAIL tt-mach-exc THEN DO:
    tt-mach-exc.defr = ip-defr.
    RELEASE mach.
  END.
END.

