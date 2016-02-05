/* machshf_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'machshf_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="machshft" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="machshft" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="machshft" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="machshft" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

DEFINE VARIABLE machine_code AS CHARACTER NO-UNDO.

IF machine_code NE machshft.machine THEN
DO:
  FIND mach WHERE mach.company = selected-company
              AND mach.m-code = machshft.machine NO-LOCK NO-ERROR.
  DISPLAY
    machshft.machine
    mach.m-dscr WHEN AVAILABLE(mach) COLUMN-LABEL 'Description'.
  machine_code = machshft.machine.
END.
FIND shifts OF machshft NO-LOCK NO-ERROR.
DISPLAY
  machshft.shift
  shifts.description WHEN AVAILABLE(shifts)
  STRING(machshft.start_time,'HH:MM:SS am') LABEL 'Start Time'
  STRING(machshft.end_time,'HH:MM:SS am') LABEL 'End Time'.

{methods/lstlogic/shownote.i &db_table="machshft" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="machshft" &col="5" &frame-name="f-miscflds"}
