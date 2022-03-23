/* ---------------------------------------------- cec/rep/jobtick.i 04/97 JLF */
/* factory ticket                                                             */
/* -------------------------------------------------------------------------- */
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No). */
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
def {1} var v-job-prt       as   char format "x(13)"                     no-undo.
def {1} var v-ord-no        as   char format "x(8)"                     no-undo.
def {1} var v-ord-date      as   char format "x(10)"                     no-undo.
def {1} var v-est-no        as   char format "x(8)"                     no-undo.
def {1} var v-fg            as   char format "x(37)"                    no-undo.
def {1} var v-cp            as   char format "x(37)"                    no-undo.
def {1} var v-due-date      as   char format "x(8)"                    no-undo.

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
def {1} var v-form-code     as   char format "x(15)"                    no-undo.
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
def {1} var v-board-dscr     as   char format "x(32)"                    no-undo.

def {1} workfile w-m no-undo
  FIELD m-code LIKE mach.m-code
  field dseq like mach.d-seq
  field dscr like mach.m-dscr
  field s-hr like job-mch.mr-hr
  field r-sp like job-mch.speed
  field r-hr like job-mch.run-hr format ">>>9.99".

def {1} workfile w-i no-undo
  field i-code as ch format "x(10)"
  field i-dscr as ch format "x(18)"
  field i-qty  as de format ">>>9.99"
  field i-code2 as ch format "x(10)"
  field i-dscr2 as ch format "x(18)"
  field i-qty2  as de format ">>>9.99".

def {1} workfile w-ef no-undo
  field frm  like ef.form-no.

FORM HEADER
       "</PROGRESS><P8><=1>           <U>Job Information:</U>" AT 2 
       "<U>Production ID:</U>" AT 92
       "<U>Date:</U>" AT 148
       "<U>Location:</U>" AT 181 "</U><P12><B>"       
       "Job#:"                         at 1
       v-job-prt 
       "</B><P10>Overrun:"  AT 20 lv-over-run format "x(7)"       
       "Order#:" at 45 v-ord-no "CAD#:" v-cad-no
       "<P12><B>Due Date:" AT 85 v-due-code  v-due-date  "</B><P10>" 
       "Loc:" AT 128 v-loc 
       "Est#:" AT 2 v-est-no
        "Underrun:" AT 23 lv-under-run FORMAT "x(7)"
       "FG#:" AT 40 v-fg FORM "x(15)"  v-blk-per-frm    
       "<C67>Ord Date:"   v-ord-date   
       "  Bin:" AT 110  v-loc-bin  SKIP
       "Salesman:" AT 2 v-sman
       "<P12><B>CustPart#:" AT 40 v-cp FORM "x(15)"
       "</B><P10>" "<C67>Rel Date:" /*AT 91*/  v-rel-date "Qt:" + string(v-rel-qty,">>>>9") FORM "x(12)" "<P8>" SKIP
       "<U>Sold To:</U>" AT 15 " <U>Order Information:</U>" AT 70 " <U>Item Description:</U>" AT 133
       "<P10>"
    with no-box frame head no-labels width 230 NO-ATTR-SPACE STREAM-IO.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
