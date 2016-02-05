/* stax_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'stax_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="stax" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="stax" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="stax" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="stax" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/
define variable li-tax-index as integer no-undo.

INDEXLOOP: DO li-tax-index = 1 to extent(stax.tax-code1):
  if li-tax-index <> 1 and stax.tax-rate1[li-tax-index] = 0 then next INDEXLOOP.
  If li-tax-index = 1 then
     DISPLAY
	  stax.tax-group when li-tax-index = 1
	  stax.tax-code1[li-tax-index] LABEL 'Tax Code'           
	  stax.tax-dscr1[li-tax-index] LABEL 'Description'        
	  stax.tax-rate1[li-tax-index] LABEL 'Rate'                
	  stax.tax-frt1 [li-tax-index] COLUMN-LABEL 'Tax Freight?' 
	  stax.tax-acc1 [li-tax-index] LABEL 'Account'.
  else DISPLAY
	  stax.tax-code1[li-tax-index] @ stax.tax-code1[1]
	  stax.tax-dscr1[li-tax-index] @ stax.tax-dscr1[1]
	  stax.tax-rate1[li-tax-index] @ stax.tax-rate1[1]
	  stax.tax-frt1 [li-tax-index] @ stax.tax-frt1[1]
	  stax.tax-acc1 [li-tax-index] @ stax.tax-acc1[1].
  DOWN.
end. /* each extent. */
{methods/lstlogic/shownote.i &db_table="stax" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="stax" &col="5" &frame-name="f-miscflds"}
