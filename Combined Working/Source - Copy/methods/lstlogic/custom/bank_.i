/* bank_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'bank_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="bank" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="bank" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="bank" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="bank" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

DISPLAY
  bank.bank-code
  bank.bank-name
  bank.addr[1] LABEL 'Address/Phone/Contact'
  bank.bk-area LABEL 'Area Code'
  bank.bk-act LABEL 'Bank Account'
  bank.actnum
  bank.dep-tr LABEL 'Dep in Trans'
  bank.last-chk LABEL 'Last Check'
  bank.bal LABEL 'Balance'
  bank.o-chk LABEL 'Outstanding'
  bank.serv LABEL 'Serv Chrg'
   WITH WIDTH 200.
IF bank.addr[2] NE '' THEN
DO:
  DOWN.
  DISPLAY bank.addr[2] @ bank.addr[1].
END.
IF bank.city + bank.state + bank.zip NE '' THEN
DO:
  DOWN.
  DISPLAY bank.city + ', ' + bank.state + ' ' + bank.zip @ bank.addr[1].
END.
IF bank.phone NE '' THEN
DO:
  DOWN.
  DISPLAY 'Phone: ' + bank.phone @ bank.addr[1].
END.
IF bank.contact NE '' THEN
DO:
  DOWN.
  DISPLAY 'Contact: ' + bank.contact @ bank.addr[1].
END.

{methods/lstlogic/shownote.i &db_table="bank" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="bank" &col="5" &frame-name="f-miscflds"}
