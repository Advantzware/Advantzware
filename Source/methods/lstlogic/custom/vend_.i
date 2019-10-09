/* vend_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'vend_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="vend" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="vend" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="vend" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="vend" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

DISPLAY
  vend.vend-no LABEL 'Vendor'
  vend.name.
DEFINE VARIABLE taglabel AS CHARACTER FORMAT 'X(12)' NO-UNDO.
{custom/gperiod.i}
{custom/getperd.i}
IF show-totals THEN
DO WITH FRAME totals STREAM-IO TITLE '----- TOTALS -----' DOWN COLUMN 5 WIDTH 70:
  taglabel = '  Purchases:'.
  DISPLAY
    taglabel NO-LABEL
    vend.purch[gperiod] LABEL 'Period To Date'
    vend.purch[13] LABEL 'Year To Date'
    vend.last-year LABEL 'Prior Year'.
  DOWN.
  taglabel = '  Total MSF:'.
  DISPLAY
    taglabel
    vend.ptd-msf[gperiod] @ vend.purch[gperiod]
    vend.ytd-msf @ vend.purch[13]
    vend.lyytd-msf @ vend.last-year.
  DO WITH FRAME totals2 STREAM-IO COLUMN 5 SIDE-LABELS WIDTH 90:
    DISPLAY
    vend.hibal LABEL 'High Balance' COLON 20
    vend.hibal-date LABEL 'On'
    vend.num-inv LABEL 'Total# of Inv. Paid' SKIP
    vend.lpay COLON 20
    vend.lpay-date LABEL 'On'
    vend.avg-pay LABEL 'Avg# Days to Pay' COLON 69 SKIP
    vend.ord-bal
    vend.acc-bal COLON 69.
  END.
  PUT UNFORMATTED SKIP(1).
END.

{methods/lstlogic/shownote.i &db_table="vend" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="vend" &col="5" &frame-name="f-miscflds"}
