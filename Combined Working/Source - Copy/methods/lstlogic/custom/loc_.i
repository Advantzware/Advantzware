/* loc_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'loc_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="loc" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="loc" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="loc" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="loc" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

DEFINE VARIABLE fg_bin AS CHARACTER NO-UNDO.
DEFINE VARIABLE rm_bin AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

ASSIGN
  fg_bin = ''
  rm_bin = ''.
IF show-fg-bins THEN
FOR EACH fg-bin FIELDS(loc-bin) OF loc  WHERE
    fg-bin.i-no EQ "" NO-LOCK:
  fg_bin = fg_bin + fg-bin.loc-bin + ','.
END.
IF show-rm-bins THEN
FOR EACH rm-bin FIELDS(loc-bin) OF loc  WHERE
    rm-bin.i-no EQ "" NO-LOCK:
  rm_bin = rm_bin + rm-bin.loc-bin + ','.
END.

DISPLAY
  loc.loc
  loc.dscr.
IF fg_bin NE '' OR rm_bin NE '' THEN
DO i = 1 TO MAX(NUM-ENTRIES(fg_bin),NUM-ENTRIES(rm_bin)) - 1
     WITH FRAME {&FRAME-NAME} STREAM-IO DOWN:
  IF NUM-ENTRIES(fg_bin) GT i THEN
  DISPLAY ENTRY(i,fg_bin) LABEL 'F/G Bins'.
  IF NUM-ENTRIES(rm_bin) GT i THEN
  DISPLAY ENTRY(i,rm_bin) LABEL 'R/M Bins'.
  DOWN.
END.

{methods/lstlogic/shownote.i &db_table="loc" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="loc" &col="5" &frame-name="f-miscflds"}
