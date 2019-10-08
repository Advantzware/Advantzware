/* shifts_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'shifts_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="shifts" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="shifts" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="shifts" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="shifts" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

IF shifts.company = selected-company THEN
DO:
  DISPLAY
    shifts.shift
    shifts.description
    STRING(shifts.start_time,'HH:MM am') LABEL 'Start Time'
    STRING(shifts.end_time,'HH:MM am') LABEL 'End Time'.

  {methods/lstlogic/shownote.i &db_table="shifts" &col="5" &frame-name="f-notes"}
  {methods/lstlogic/showmisc.i &db_table="shifts" &col="5" &frame-name="f-miscflds"}
END.
