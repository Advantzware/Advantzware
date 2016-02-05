/* account_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'account_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="account" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="account" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="account" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="account" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

DEFINE VARIABLE i AS INTEGER FORMAT '99' LABEL 'Month' NO-UNDO.

DISPLAY
  account.actnum
  account.dscr LABEL 'Description'
  account.type
  account.cyr-open
  account.lyr-open.
IF show-history THEN
DO i = 1 TO MIN(EXTENT(account.cyr),MIN(EXTENT(account.lyr),EXTENT(account.bud)))
    WITH FRAME history STREAM-IO TITLE '----- HISTORY -----' DOWN COLUMN 27 WIDTH 72:
  DISPLAY i
    account.cyr[i] LABEL 'Current Year'
    account.lyr[i] LABEL 'Prior Year'
    account.bud[i] LABEL 'Budget'.
  DOWN.
END.
IF show-history THEN
PUT UNFORMATTED SKIP(1).

{methods/lstlogic/shownote.i &db_table="account" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="account" &col="5" &frame-name="f-miscflds"}
