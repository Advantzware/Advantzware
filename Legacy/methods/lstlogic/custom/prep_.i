/* prep_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'prep_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="prep" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="prep" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="prep" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="prep" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

DISPLAY
  prep.code FORMAT "x(15)"
  prep.dscr
  prep.mkup
  prep.cost
  prep.ml
  prep.amtz
  prep.mat-type COLUMN-LABEL 'MType'
  prep.dfault COLUMN-LABEL 'Use w/ Est'
  prep.uom
  prep.simon
  prep.cost-type COLUMN-LABEL 'CType'
  prep.actnum
  prep.vend-no FORMAT "X(8)".

{methods/lstlogic/shownote.i &db_table="prep" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="prep" &col="5" &frame-name="f-miscflds"}
