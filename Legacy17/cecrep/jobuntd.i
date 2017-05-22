/* ---------------------------------------------- cec/rep/jobuntd.i */
/* factory ticket                                                             */
/* -------------------------------------------------------------------------- */

def {1} var v-lines         as   int init 39                            no-undo.
def {1} var save_id as recid.

def {1} buffer xest     for est.
def {1} buffer xef      for ef.
def {1} buffer xeb      for eb.
def {1} buffer xoe-ord  for oe-ord.
def {1} buffer xoe-ordl for oe-ordl.
def {1} buffer xoe-rel  for oe-rel.
def {1} buffer xstyle   for style.
def {1} buffer xxprep   for prep.

def {1} var v-break         as   log                                    no-undo.
def {1} var v-job-prt       as   char format "x(9)"                     no-undo.
def {1} var v-ord-no        as   char format "x(8)"                     no-undo.
def {1} var v-ord-date      as   char format "x(10)"                     no-undo.
def {1} var v-est-no        as   char format "x(6)"                     no-undo.
def {1} var v-fg            as   char format "x(37)"                    no-undo.
def {1} var v-cp            as   char format "x(37)"                    no-undo.
def {1} var v-due-date      as   char format "x(13)"                    no-undo.

def {1} var v-joint-dscr    like item.i-name                            no-undo.
def {1} var v-cus           as   char format "x(30)" extent 4           no-undo.
def {1} var v-shp           like v-cus                                  no-undo.
def {1} var v-standards     as   log                                    no-undo.
def {1} var v-adders        as   char format "x(31)"                    no-undo.
def {1} var v-sht-qty       as   int  format ">,>>>,>>9"                no-undo.
def {1} var v-dc-qty        as   int  format ">,>>>,>>>,>>9"            no-undo.
def {1} var v-1st-dc        as   log                                    no-undo.
def {1} var v-out           as   int  format ">9"                       no-undo.
def {1} var v-outw          as   int  format ">9"                       no-undo.
def {1} var v-outl          as   int  format ">9"                       no-undo.
def {1} var v-upw           as   int  format ">9"                       no-undo.
def {1} var v-upl           as   int  format ">9"                       no-undo.
def {1} var v-letter        as   char format "x"                        no-undo.
def {1} var v-form-code     as   char format "x(32)"                    no-undo.
def {1} var v-form-dscr     as   char format "x(32)"                    no-undo.
def {1} var v-form-len      like job-mat.len                            no-undo.
def {1} var v-form-wid      like job-mat.wid                            no-undo.
def {1} var v-form-sqft     as   dec decimals 3 format ">>9.9<<"        no-undo.
def {1} var v-len-score     as   char format "x(27)"                    no-undo.
def {1} var v-inst          as   char                                   no-undo.
def {1} var v-ship          as   char                                   no-undo.
def {1} var v-cas-pal       like eb.cas-pal                             no-undo.
def {1} var v-tr-cnt        like eb.tr-cnt                              no-undo.
def {1} var v-numstacks     as   int                                    no-undo.
def {1} var v-stackcode     as   char                                   no-undo.
def {1} var v-error         as   log                                    no-undo.
def {1} var v-line          as   char format "x(130)" extent 8          no-undo.
def {1} var v-form-hdr      as   char format "x(16)"                    no-undo.
def {1} var v-net-shts      as   log                                    no-undo.
def {1} var v-iso           as   char format "x(14)"                    no-undo.
def {1} var v-loc           as   char format "x(14)"                    no-undo.
def {1} var v-loc-bin       as   char format "x(14)"                    no-undo.
def {1} var v-set-hdr       as   char format "x(25)"                    no-undo.
def {1} var v-board-code    as   char format "x(15)"                    no-undo.
def {1} var v-board-dscr    as   char format "x(32)"                    no-undo.

def {1} workfile w-m no-undo
  FIELD m-code LIKE mach.m-code
  field dseq like mach.d-seq
  field dscr like mach.m-dscr
  field s-hr like job-mch.mr-hr
  field r-sp like job-mch.speed
  field r-hr like job-mch.run-hr format ">>>9.99".

def {1} workfile w-i no-undo
  field i-code as ch format "x(10)"
  field i-dscr as ch format "x(19)"
  field i-qty  as de format ">>>9.99"
  field i-code2 as ch format "x(10)"
  field i-dscr2 as ch format "x(19)"
  field i-qty2  as de format ">>>9.99".

def {1} workfile w-ef no-undo
  field frm  like ef.form-no.

/*def {1} frame head. */

FORM HEADER
       /*"<=1> <B>Customer Name:</B>" AT 2 
       "<B>Special Requirements:</B>" AT 58  
       " <B>Estimate#:</B>" AT 115 v-est-no SKIP
       "Contact:" v-contact  at 2
       "Label/Perf Part #s:" AT 58 v-perf
       "Order Qty Assd Sets:" AT 115 v-set-qty SKIP
       "Tel:" AT 2 v-tel 
       "Boxed:" AT 58 v-cas-no
       "Order Qty U/A Longs:" AT 115 v-qty-1 SKIP
       "Fax:" AT 2 v-fax 
       "Reverse Corrugation:" AT 58 v-reversed
       "Order Qty U/A Shorts:" v-qty-1 skip
       "E-mail:" AT 2 v-email
       "# of Load Tags" AT 58 v-num-tags 
       "Overrun:"  AT 115 lv-over-run format "x(7)"  SKIP
       "<B>D/C Machine Set-up:</B>" AT 2
       "<B>D/C Set-up:</B>"         AT 58
       "<B>Type Dies:</B>"   AT 115 SKIP
       "Form #1:"  
       "Form #1A:"  AT 58
       "Form #1:"   AT 115 SKIP
       "Form #1:"  
       "Form #2A:"  AT 58
       "Form #1:"   AT 115 SKIP
       "Form #2:"  
       "Form #1B:"  AT 58
       "Form #2:"   AT 115 SKIP
       "Form #2:"  
       "Form #2B:"  AT 58
       "Form #2:"   AT 115 SKIP
       "<B>Board:" AT 2 "Board:</B>" AT 58 SKIP
       "Type:" AT 2 "Type:" AT 58     SKIP
       "Size:" AT 2 "Size:" AT 58     SKIP
       "Qty:" AT 2 "Qty:" AT 58       SKIP
       "Due Date:" AT 2 v-due-date "Due Date:" AT 58 v-due-date2 SKIP
        */

    "<=1><C2><B>Customer Name:</B>" 
       "<C36><B>Special Requirements:</B>" 
       "<C70><B>Estimate#:</B>" v-est-no SKIP
       "<C2>Contact:" v-contact 
       "<C36>Label/Perf Part #s:"  v-perf
       "<C70>Order Qty Assd Sets:" v-set-qty SKIP
       "<C2>Tel:"  v-tel 
       "<C36>Boxed:" v-cas-no
       "<C70>Order Qty U/A Longs:" v-qty-1 SKIP
       "<C2>Fax:" v-fax 
       "<C36>Reverse Corrugation:" v-reversed
       "<C70>Order Qty U/A Shorts:" v-qty-2 skip
       "<C2>E-mail:" v-email
       "<C36># of Load Tags" v-num-tags 
       "<C70>Overrun:"  lv-over-run format "x(7)"  SKIP
       "<C2><B>D/C Machine Set-up:</B>"
       "<C36><B>D/C Set-up:</B>" 
       "<C70><B>Type Dies:</B>"   SKIP
       "<C2>Form #1:"  v-dc-msetup[1] 
       "<C36>Form #1A:" v-dc-setup[1]
       "<C70>Form #1:"  v-dies[1] SKIP
       "<C2>Form #1:"  v-dc-msetup[2]
       "<C36>Form #2A:" v-dc-setup[2]
       "<C70>Form #1:" v-dies[2] SKIP
       "<C2>Form #2:" v-dc-msetup[3] 
       "<C36>Form #1B:" v-dc-setup[3]  
       "<C70>Form #2:" v-dies[3]  SKIP
       "<C2>Form #2:"  v-dc-msetup[4]
       "<C36>Form #2B:" v-dc-setup[4]
       "<C70>Form #2:" v-dies[4] SKIP
       "<C2><B>Board: Form #1"  "<C36>Board: Form#2</B>" SKIP
       "<C2>Type:" v-board-type-1 "<C36>Type:" v-board-type-2 SKIP
       "<C2>Size:" v-board-size-1 "<C36>Size:" v-board-size-2 SKIP
       "<C2>Qty:" v-board-qty-1 "<C36>Qty:" v-board-qty-2  SKIP
       "<C2>Due Date:"  v-due-date1 "<C36>Due Date:" v-due-date2 SKIP
       

/*
       "Our Order#:" AT 40 v-ord-no
       "Ord Date:" AT 80  v-ord-date
       "Whs:" AT 109 v-loc 
       
        "Underrun:" AT 17 lv-under-run FORMAT "x(7)"
       "FG#:" AT 40 v-fg FORM "x(15)"
       "Due Date: " AT 80 v-due-date
       "Bin:" AT 109 v-loc-bin  SKIP
       v-form-hdr AT 2 "CustPart#:" AT 40 v-cp FORM "x(15)" "<P7>" SKIP
       "Sold To:" AT 2 "Order Information:" AT 55 "Item Description:" AT 115
       "<P10>"        
*/       
    with no-box frame head no-labels width 170 NO-ATTR-SPACE STREAM-IO.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
