/* prod_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'prod_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="prod" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="prod" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="prod" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="prod" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

DEFINE VARIABLE account-name AS CHARACTER FORMAT 'X(30)' NO-UNDO LABEL 'Account Name'.

account-name = IF prod.aa-fo NE '' THEN '' ELSE ''.
DISPLAY
  prod.prolin
  prod.dscr
  account-name
  prod.aa-fo LABEL 'Account Number'.
IF prod.aa-lab NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.aa-lab @ prod.aa-fo.
END.
IF prod.aa-mat NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.aa-mat @ prod.aa-fo.
END.
IF prod.aa-vo NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.aa-vo @ prod.aa-fo.
END.
IF prod.cgs-dlv NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.cgs-dl @ prod.aa-fo.
END.
IF prod.cgs-dlv NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.cgs-dlv @ prod.aa-fo.
END.
IF prod.cgs-fo NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.cgs-fo @ prod.aa-fo.
END.
IF prod.cgs-fov NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.cgs-fov @ prod.aa-fo.
END.
IF prod.cgs-mat NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.cgs-mat @ prod.aa-fo.
END.
IF prod.cgs-mu NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.cgs-mu @ prod.aa-fo.
END.
IF prod.cgs-vo NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.cgs-vo @ prod.aa-fo.
END.
IF prod.cgs-vov NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.cgs-vov @ prod.aa-fo.
END.
IF prod.fg-fo NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.fg-fo @ prod.aa-fo.
END.
IF prod.fg-lab NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.fg-lab @ prod.aa-fo.
END.
IF prod.fg-mat NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.fg-mat @ prod.aa-fo.
END.
IF prod.fg-vo NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.fg-vo @ prod.aa-fo.
END.
IF prod.wip-fo NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.wip-fo @ prod.aa-fo.
END.
IF prod.wip-lab NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.wip-lab @ prod.aa-fo.
END.
IF prod.wip-mat NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.wip-mat @ prod.aa-fo.
END.
IF prod.wip-vo NE '' THEN
DO:
  account-name = ''.
  DOWN.
  DISPLAY
    account-name
    prod.wip-vo @ prod.aa-fo.
END.
IF show-catagories THEN
FOR EACH prodl OF prod NO-LOCK WITH STREAM-IO COLUMN 5:
  FIND procat WHERE procat.procat = prodl.procat NO-LOCK NO-ERROR.
  DISPLAY prodl.procat procat.dscr WHEN AVAILABLE procat.
END.
IF show-catagories THEN
PUT UNFORMATTED SKIP(1).

{methods/lstlogic/shownote.i &db_table="prod" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="prod" &col="5" &frame-name="f-miscflds"}
