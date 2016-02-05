/* carrier_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'carrier_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="carrier" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="carrier" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="carrier" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="carrier" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

DISPLAY
  carrier.carrier
  carrier.dscr
  carrier.loc
  carrier.by-pallet.
IF show-matrix THEN
FOR EACH carr-mtx OF carrier NO-LOCK WITH STREAM-IO COLUMN 5:
  DISPLAY
    carr-mtx.del-zone
    carr-mtx.del-dscr
    carr-mtx.del-zip
    carr-mtx.min-rate.
  DISPLAY
    '                     -1-     -2-     -3-     -4-     -5-     -6-  ' AT 1
    '  -7-     -8-     -9-     -10-'
    '                   ------- ------- ------- ------- ------- -------' AT 1
    '------- ------- ------- -------'
    'Weight(lbs) Up To:' AT 1
    carr-mtx.weight[1]
    carr-mtx.weight[2]
    carr-mtx.weight[3]
    carr-mtx.weight[4]
    carr-mtx.weight[5]
    carr-mtx.weight[6]
    carr-mtx.weight[7]
    carr-mtx.weight[8]
    carr-mtx.weight[9]
    carr-mtx.weight[10]
    '    Rate / 100lbs:' AT 1
    carr-mtx.rate[1]
    carr-mtx.rate[2]
    carr-mtx.rate[3]
    carr-mtx.rate[4]
    carr-mtx.rate[5]
    carr-mtx.rate[6]
    carr-mtx.rate[7]
    carr-mtx.rate[8]
    carr-mtx.rate[9]
    carr-mtx.rate[10]
        WITH FRAME matrix STREAM-IO NO-BOX COLUMN 5 WIDTH 132 NO-LABELS.
END.

{methods/lstlogic/shownote.i &db_table="carrier" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="carrier" &col="5" &frame-name="f-miscflds"}
