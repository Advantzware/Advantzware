
DEF PARAM BUFFER io-mach FOR mach.
DEF INPUT PARAM ip-on-f AS INT NO-UNDO.
DEF INPUT PARAM ip-defr AS LOG NO-UNDO.

/*{sys/inc/var.i SHARED}*/
def SHARED var cocode     as   char  format "x(3)"  no-undo.
def SHARED var locode     as   char  format "x(5)"  no-undo.

def  var  x  as   int no-undo.
def  var  y  as   int no-undo.
DEF  VAR  k  as   int no-undo.

def var i          as   int no-undo.
def var j          as   int no-undo.

def var z          as   int no-undo.
def var xxx        as   dec no-undo.
def var yyy        as   dec no-undo.
def var zzz        as   dec no-undo.
def var tmpstore   as   cha no-undo.

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
   where (style.company eq cocode)
      AND style.style EQ xeb.style
   NO-ERROR.

IF AVAIL style THEN DO:
  {sys/inc/cepanel.i}

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

