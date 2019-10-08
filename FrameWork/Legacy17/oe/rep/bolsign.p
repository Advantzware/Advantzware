/* ---------------------------------------------- oe/rep/bolsign.p 06/06 YSK */
/* PRINT Signed BOL                                                           */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}
def buffer xxreport     for report.
{oe/rep/oe-lad.i}

DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(50)" NO-UNDO.
DEF VAR ls-pdf-image AS cha NO-UNDO.
DEF VAR lv-cmd AS cha NO-UNDO.
DEF VAR lv-return AS INT NO-UNDO.
DEF VAR tInt AS INT NO-UNDO.

PROCEDURE ShellExecuteA EXTERNAL "shell32":u :
      define input parameter hwnd as long.
      define input parameter lpOperation as char.
      define input parameter lpFile as char.
      define input parameter lpParameters as char.
      define input parameter lpDirectory as char.
      define input parameter nShowCmd as long.
      define return parameter hInstance as long.
END PROCEDURE.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "BOLSIGN" no-lock no-error.
ASSIGN
ls-image1 = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE ""
ls-image2 = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".

for each xxreport NO-LOCK where xxreport.term-id eq v-term-id,
    first oe-bolh NO-LOCK where recid(oe-bolh)   eq xxreport.rec-id
    break by oe-bolh.bol-no:
  
    if first-of(oe-bolh.bol-no) then do:
       ls-image1 = ls-image2.
       IF ls-image1 <> "" AND 
          SUBSTRING(ls-image1,LENGTH(ls-image1),1) <> "\" AND
          SUBSTRING(ls-image1,LENGTH(ls-image1),1) <> "/"
       THEN ls-image1 = ls-image1 + "\".
      
       ls-pdf-image = ls-image1 + STRING(oe-bolh.bol-no) + ".pdf".
       IF SEARCH(ls-pdf-image) <> ? THEN DO:
          RUN ShellExecuteA(0, "open", ls-pdf-image, "", "", 0, OUTPUT tInt).
          IF tInt LE 32 THEN
          DO:
             RUN custom/runapdf.p (OUTPUT lv-cmd).
             lv-cmd = lv-cmd + chr(32) + ls-pdf-image.
             RUN WinExec (INPUT lv-cmd, INPUT 1,OUTPUT lv-return).
          END.
       END.
       ELSE DO:
          ASSIGN
             ls-image1 = ls-image1 + STRING(oe-bolh.bol-no) + ".jpg"
             FILE-INFO:FILE-NAME = ls-image1
             ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">"
             FILE-INFO:FILE-NAME = ls-image2
             ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".
         PUT "<R1><C1><#1><R+60><C+80><IMAGE#1=" ls-full-img1 SKIP.
         PAGE.
       END.
    end. /* first-of(oe-bolh.bol-no) */
  
end. /* for each oe-bolh */

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

PROCEDURE WinExec EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT PARAMETER programname AS cha.
    DEFINE INPUT PARAMETER visualstyle AS long.
    DEFINE RETURN PARAM statuscode AS LONG.
END.
