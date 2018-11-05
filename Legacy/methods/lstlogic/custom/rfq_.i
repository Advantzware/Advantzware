/* rfq_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'rfq_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="rfq" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="rfq" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="rfq" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="rfq" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/
def var v-start-compress as char init "" no-undo.
def var v-end-compress as char init "" no-undo.
DEF VAR i as INT NO-UNDO.
&SCOPED-DEFINE setfirstinteger 0
&SCOPED-DEFINE setlastinteger 99999
assign v-start-compress = ""
       v-end-compress = "".
      
find first printer where printer.company eq gcompany
                     and printer.loc     eq gloc
                     and printer.pr-no   eq selected-printer no-lock no-error.
if avail printer then
do:
  if printer.pr-cmd ne "" then
  do i = 1 to num-entries(printer.pr-cmd):
    if entry(i,printer.pr-cmd) ne ? then
      v-start-compress = v-start-compress +
			  chr(int(entry(i,printer.pr-cmd))).
  end.
end.

put control v-start-compress.

           PUT "test" begin_rfq-no "," end_rfq-no "," begin_rfq_cust-no end_rfq_cust-no
             SKIP.

FOR EACH rfq WHERE rfq.rfq-no >= begin_rfq-no and
                   rfq.rfq-no <= end_rfq-no and
                   rfq.cust-no >= begin_rfq_cust-no and
                   rfq.cust-no <= end_rfq_cust-no
                   NO-LOCK
                   ,
    EACH rfqitem OF rfq no-lock
                   BREAK BY rfq.rfq-no BY rfqitem.seq:

           PUT "in" rfq.rfq-no rfqitem.part-no SKIP.
    if FIRST-OF(rfq.rfq-no) then
       DISPLAY    "RFQ#:" rfq.rfq-no
                  "Customer:" rfq.cust-no
                  rfq.ship-name
                  "Req-Date:" rfq.req-date
                  with frame rfqhd no-labels.
    DISP rfqitem.seq label "Seq" form ">>9"
         rfqitem.part-no rfqitem.i-name rfqitem.style rfqitem.procat  rfqitem.LEn rfqitem.wid
         rfqitem.dep
         SKIP(1).
END.

put control v-end-compress.
/*
{methods/lstlogic/shownote.i &db_table="rfq" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="rfq" &col="5" &frame-name="f-miscflds"}
	Last change:  YSK  26 Feb 2001   10:12 am
*/
