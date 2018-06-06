/* ---------------------------------------------- cec/rep/jobtick2.i 04/97 JLF */
/* factory ticket                                                             */

/* Also update other includes that call cecrep/jobtick2.p for example*/

/* -------------------------------------------------------------------------- */

                                                                  
                                                                  
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
def {1} var v-job-prt       as   char format "x(10)"                    no-undo.
def {1} var v-ord-no        as   char format "x(8)"                     no-undo.
def {1} var v-ord-date      as   char format "x(8)"                     no-undo.
def {1} var v-est-no        as   char format "x(6)"                     no-undo.
def {1} var v-fg            as   char format "x(37)"                    no-undo.
def {1} var v-due-date      as   char format "x(13)"                    no-undo.

def {1} var v-joint-dscr    like item.i-name                            no-undo.
def {1} var v-cus           as   char format "x(39)" extent 4           no-undo.
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
def {1} var v-board-dscr     as   char format "x(32)"                    no-undo.
DEF {1} VARIABLE cShpNote    AS CHARACTER FORMAT 'x(47)' EXTENT 4 NO-UNDO.

def {1} workfile w-m no-undo
  FIELD m-code LIKE mach.m-code
  field dseq like mach.d-seq
  field dscr like mach.m-dscr
  field s-hr like job-mch.mr-hr
  field r-sp like job-mch.speed
  field r-hr like job-mch.run-hr format ">>>9.99"
  .

def {1} workfile w-i no-undo
  field i-code  as ch format "x(10)"
  field i-dscr  as ch format "x(19)"
  field i-qty   as de format ">>>9.99"
  field i-code2 as ch format "x(10)"
  field i-dscr2 as ch format "x(19)"
  field i-qty2  as de format ">>>9.99".

def {1} workfile w-ef no-undo
  field frm  like ef.form-no.

/*def {1} frame head. */

format header
       /*"F a c t o r y   T i c k e t"    at 2 */
       "<P7><FCourier New>JOB  TICKET" AT 2
       space(3)
       v-iso
       space(3)
       "[  ] Edit"                      space(4)
       "[  ] Production"                space(4)
       "[  ] Plate Room"                space(4)
       "[  ] Die Room"                  space(4)
       "[  ] Purchasing"
             /* was 196*/
       /*fill(chr(95),130)               at 2    format "x(130)"         */      
      "<#1><C+80><LINE#1><|3>" 
      SKIP
       "Job #:"                         at 3
       v-job-prt
       v-form-hdr                       at 20
       "Our Order #:"                   at 47
       v-ord-no
       "Ord Date:"                      at 93
       v-ord-date
       v-loc                            at 118

       "Est #:"                         at 3
       v-est-no
       v-set-hdr                        at 18
       "FG#:"                           at 47
       v-fg
       "Due Date:"                      at 93
       v-due-date
       v-loc-bin                        at 118

    with no-box frame head no-labels width 146 /*132*/  STREAM-IO.

/*{cec/msfcalc.i} */

/*help-id = program-name(1). */

/* end ---------------------------------- copr. 1997  advanced software, inc. */
