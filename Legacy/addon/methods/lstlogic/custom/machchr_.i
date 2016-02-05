/* machchr_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'machchr_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="machchrg" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="machchrg" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="machchrg" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="machchrg" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

DEFINE VARIABLE machine_code AS CHARACTER NO-UNDO.

IF machine_code NE machchrg.machine THEN
DO:
  FIND mach WHERE mach.company = selected-company
              AND mach.m-code = machchrg.machine NO-LOCK NO-ERROR.
  DISPLAY
    machchrg.machine
    mach.m-dscr WHEN AVAILABLE(mach).
  machine_code = machchrg.machine.
END.
FIND job-code WHERE job-code.code = machchrg.charge_code NO-LOCK NO-ERROR.
FIND machseq OF machchrg NO-LOCK NO-ERROR.
DISPLAY
  machseq.machseq WHEN AVAILABLE(machseq)
  machchrg.charge_code
  job-code.cat WHEN AVAILABLE(job-code)
  job-code.dscr WHEN AVAILABLE(job-code).

{methods/lstlogic/shownote.i &db_table="machchrg" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="machchrg" &col="5" &frame-name="f-miscflds"}
