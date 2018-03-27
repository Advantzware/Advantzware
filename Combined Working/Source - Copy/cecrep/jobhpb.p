/* ---------------------------------------------- */
/*  cecrep/jobfibre.p  factory ticket  for Fibre landscape */
/* -------------------------------------------------------------------------- */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

def input parameter v-format as char.
DEF SHARED VAR s-prt-fgimage AS LOG NO-UNDO.
DEF VAR prt-copies AS INT  NO-UNDO.
DEF VAR v-start-compress AS cha NO-UNDO.
DEF VAR v-end-compress AS cha NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR v-ink-1 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-2 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-3 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-4 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-5 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-6 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-7 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-8 AS cha FORM "X(30)" NO-UNDO.
DEF var v-dept-note AS cha FORM "x(48)" EXTENT 50 NO-UNDO.
DEF var v-spec-note AS cha FORM "x(124)" EXTENT 10 NO-UNDO.
DEF VAR v-deptnote AS cha NO-UNDO.
DEF VAR v-dept-length AS DEC NO-UNDO.
DEF VAR lv-under-run AS cha NO-UNDO.
DEF VAR lv-over-run AS cha NO-UNDO.
DEF VAR lv-part-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-fg-name AS cha NO-UNDO.
DEF VAR lv-status AS cha FORM "x(20)" NO-UNDO.
DEF VAR lv-sts-code AS cha INIT "O,R,C,T,N,X,Q" NO-UNDO.
DEF VAR lv-sts-desc AS cha INIT "O-Original,R-Repeat,C-Change,T-Transfer,N-New Customers,X-Complete Re-run,Q-Quality/Re-work" NO-UNDO.

DEF VAR v-sman AS cha FORM "x(25)" NO-UNDO.
DEF VAR v-blk-per-frm AS cha FORM "x(15)" NO-UNDO.
DEF VAR ls-fgitem-img AS cha FORM "x(50)" NO-UNDO.
DEF VAR lv-ord-po LIKE oe-ord.po-no NO-UNDO.
DEF VAR lv-part-no AS cha FORM "x(15)" NO-UNDO.
DEF VAR lv-cust-set AS cha FORM "x(15)" NO-UNDO.
DEF VAR lv-cas-cnt AS INT FORM ">>>>>>" NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg.

RUN sys/ref/ordtypes.p (OUTPUT lv-sts-code, OUTPUT lv-sts-desc).

{jcrep/r-ticket.i "shared"}

{cecrep/jobfibre.i "new shared"}
{cecrep/jc-fibre.i }
DEF WORK-TABLE w-i2 LIKE w-i FIELD i-ext AS INT.
DEF BUFFER b-w-i2 FOR w-i2.
DEF VAR ld-i-qty LIKE w-i2.i-qty NO-UNDO.

{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}

def new shared var v-out1-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
def new shared var v-out2-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
 
def var laser           as   log    init no     format "Y/N"            no-undo.
def var v-vend-no       like oe-ordl.vend-no                            no-undo.
def var v-po-no         like oe-ordl.po-no-po                           no-undo.
def var v-qty-or-sup    as   char               format "x(38)"          no-undo.
def var v-i-line        as   char   extent 4    format "x(38)"          no-undo.
def var v-flag          as   log    init no                             no-undo.
def var v-local-copies  as   int                                        no-undo.
def var v-local-loop    as   int    init 1                              no-undo.
def var v-print-score   as   log    init yes                            no-undo.
def var v-pqty          as   dec                                        no-undo.

DEF VAR lv-rt-num AS INT NO-UNDO.
def stream ctl.
DEF VAR lv-add-entry AS INT NO-UNDO.
DEF VAR v-loop-cnt AS INT NO-UNDO.
DEF VAR v-note-cnt AS INT NO-UNDO.
DEF VAR v-note-length AS INT NO-UNDO.
DEF VAR v-die-loc AS cha FORM "x(15)" NO-UNDO.

DEF VAR v-prev-ext-gap AS INT NO-UNDO.
DEF VAR v-coldscr LIKE eb.i-coldscr NO-UNDO.
DEF VAR vpono     AS CHAR NO-UNDO.

DEF VAR v-oecount AS LOG NO-UNDO.
DEF VAR v-cont-string AS cha NO-UNDO.  
DEF VAR v-prev-k AS INT NO-UNDO.
DEF VAR v-tmp-note-length AS INT NO-UNDO.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR v-cas-desc AS cha NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
DEF SHARED VAR s-prt-ship-split AS LOG NO-UNDO.
DEF BUFFER b-eb FOR eb.
DEF VAR lv-spattern-img AS cha FORM "x(50)" NO-UNDO.
DEF VAR lv-split AS cha FORM "x(60)" EXTENT 4 NO-UNDO.
DEF VAR lv-au AS cha FORM "x(20)" NO-UNDO.
DEF VAR lv-est-type AS cha FORM "x(35)" NO-UNDO.
DEF VAR v-qa-text AS cha FORM "x(30)" INIT "6/05 Job Ticket QF-119 Rev.A" NO-UNDO.

{custom/formtext.i NEW}
{sys/inc/notes.i}
DO TRANSACTION:
   {sys/inc/tspostfg.i}
END.
{cecrep/jc-prem.i}
{custom/notesdef.i}
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 15 NO-UNDO.
DEF VAR v-inst2 AS cha EXTENT 6 NO-UNDO.    
DEF VAR v-tmp-line AS INT NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER b-ef FOR ef.
DEF workfile tt-wm LIKE w-m.
DEF VAR lv-spec-qty LIKE ef.spec-qty FORMAT ">>>,>>9.9<<<<" NO-UNDO.
DEF SHARED VAR s-prt-set-header AS LOG NO-UNDO.
DEF VAR v-managed-order AS cha FORM "x(30)" NO-UNDO.


assign /*v-local-copies = if lookup(printer.pr-port, "{&PR-PORT}") eq 0 then 
                              prt-copies
                            else 1  */
           v-local-copies = 1
           prt-copies = 1.


       PUT "<P10></PROGRESS>" SKIP "<FCourier New><C2>" SKIP      
       "<#1><C1><FROM><C106><R+45><RECT><||3><C80><P10>" 
       "<=1><R-2><C33><B><P12>JOB #: "  "    FORM #: " "</B><P10>"  cocode
       "<=1><C2>DATE:" TODAY "<C20>CUSTOMER:" v-cus[1] "<C70>SHIPPING DATE:" SKIP
       "<=1><R+1><C2>PO: " vpono "<C20>ADDRESS :" v-cus[2] "<C70>SHIP TO:" v-shp[1] SKIP
       "<=1><R+2><C28>" v-cus[3] "<C77>" v-shp[2] SKIP 
       "<=1><R+3><C28>" v-cus[4] "<C77>" v-shp[3] SKIP
       "<=1><R+4><C77>" v-shp[4] SKIP(1)    .


FOR EACH job-hdr NO-LOCK
        where job-hdr.company               eq cocode
          and (job-hdr.ftick-prnt           eq reprint OR
              PROGRAM-NAME(2) MATCHES "*r-tickt2*"  )
          and job-hdr.job-no                ge substr(fjob-no,1,6)
          and job-hdr.job-no                le substr(tjob-no,1,6)

          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  ge fjob-no

          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  le tjob-no
        USE-INDEX job-no,

        first job
        where job.company                   eq cocode
          and job.job                       eq job-hdr.job
          and job.job-no                    eq job-hdr.job-no
          and job.job-no2                   eq job-hdr.job-no2
          and job.stat                      ne "H"
        USE-INDEX job NO-LOCK,
        
        first est
        where est.company = job.company
          AND est.est-no                    eq job.est-no
          and est.est-type                  gt 4
        USE-INDEX est-no2 NO-LOCK,

        first cust
        where cust.company                  eq cocode
          and cust.cust-no                  eq job-hdr.cust-no
        no-lock,

        first itemfg
        where itemfg.company                eq cocode
          and itemfg.i-no                   eq job-hdr.i-no
        no-lock

        break by job.job-no
              by job.job-no2:


/* Set Customer Data - Begin */
assign
 i      = 0
 v-cus  = "".

if cust.name ne "" then
  assign
   i        = i + 1
   v-cus[i] = cust.name.
   
if cust.addr[1] ne "" then
  assign
   i        = i + 1
   v-cus[i] = cust.addr[1].
   
if cust.addr[2] ne "" then
  assign
   i        = i + 1
   v-cus[i] = cust.addr[2].
   
assign
 i        = i + 1
 v-cus[i] = trim(cust.city) + ", " + cust.state + "  " + cust.zip.
 
/* Set Customer Data - End */


/* Find Ship To - Begin */
        v-shp = "".
        find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq job-hdr.ord-no
                  and oe-ordl.job-no  eq job-hdr.job-no
                  and oe-ordl.job-no2 eq job-hdr.job-no2
                  and oe-ordl.i-no    eq job-hdr.i-no
                no-lock no-error.
        IF AVAIL oe-ordl THEN 
            find first oe-rel
            where oe-rel.company eq cocode
              and oe-rel.ord-no  eq oe-ordl.ord-no
              and oe-rel.i-no    eq oe-ordl.i-no
              and oe-rel.line    eq oe-ordl.line
            no-lock no-error.
        IF AVAILABLE oe-rel THEN DO:
           find first shipto
              where shipto.company eq cocode
                and shipto.cust-no eq oe-rel.cust-no
                and shipto.ship-id eq oe-rel.ship-id
              no-lock no-error.  
          if avail shipto then DO:
             ASSIGN i = 0
                    v-shp = "".
             IF shipto.ship-name <> "" THEN
                ASSIGN i = i + 1
                       v-shp[i] = shipto.ship-name.
             IF shipto.ship-addr[1] <> "" THEN
                ASSIGN i = i + 1
                       v-shp[i] = shipto.ship-addr[1].
             IF shipto.ship-addr[2] <> "" THEN
                ASSIGN i = i + 1
                       v-shp[i] = shipto.ship-addr[2].                
             ASSIGN i = i + 1
                    v-shp[i] = TRIM(oe-rel.ship-city) + ", " +
                               oe-rel.ship-state + "  " + oe-rel.ship-zip. 
          END. /* if avail shipto then DO: */                                           
        end. /* IF AVAILABLE oe-rel THEN DO: */

/* Find Ship To - End */


       ASSIGN vpono = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE "".

       PUT "<P10></PROGRESS>" SKIP "<FCourier New><C2>" SKIP      
       "<#1><C1><FROM><C106><R+45><RECT><||3><C80><P10>" 
       "<=1><R-2><C33><B><P12>JOB #: " job-hdr.job-no "-" STRING(job.job-no2,"99") "    FORM #: " job-hdr.frm "</B><P10>"  cocode
       "<=1><C2>DATE:" TODAY "<C20>CUSTOMER:" v-cus[1] "<C70>SHIPPING DATE:" SKIP
       "<=1><R+1><C2>PO: " vpono "<C20>ADDRESS :" v-cus[2] "<C70>SHIP TO:" v-shp[1] SKIP
       "<=1><R+2><C28>" v-cus[3] "<C77>" v-shp[2] SKIP 
       "<=1><R+3><C28>" v-cus[4] "<C77>" v-shp[3] SKIP
       "<=1><R+4><C77>" v-shp[4] SKIP(1)                     
  
       "<#10><C1><FROM><C106><LINE><||3>" SKIP           
       "<=10><C2>F/B<C10>FG ITEM#<C25>ORDER QTY<C45>JOB QTY<C55>DESCRIPTION<C75>STYLE<C85>CARTON SIZE   #UP COLOUR STANDARD" SKIP(3)

       "<#20><C1><FROM><C106><LINE><||3>"  
       "<=20><C2><B>BOARD</B>" 
       "<=20><C50><FROM><R+8><LINE><||3>"          
       "<=20><R+1><C5>P.O.: _______________________"
       "<=20><R+1><C52>GUILLOTINE: _______________________"         
       "<=20><R+2><C5>DESCRIPTION: _______________________" 
       "<=20><R+2><C52>SHEETS: _______________________"          
       "<=20><R+3><C5>SUPPLIER: _______________________"  
       "<=20><R+3><C52>PREVIOUS JOB#: _______________________"        
       "<=20><R+4><C5>SHEETS SUPPLIED TO PRESS: _______________________"  
       "<=20><R+5><C5>SHEETS RETURNED TO STOCK: _______________________"  
       "<=20><R+6><C5>WASTE: _______________________" SKIP(1) 

       "<#30><C1><FROM><C106><LINE><||3>" SKIP
       "<=30><C49><B><U>PRINTING</U></B>" 
       "<=30><R+1><C50><FROM><R+5><LINE><||3>"         
       "<=30><R+1><C2>PLATE LOCATION :"         
       "<=30><R+2><C2>COLOUR:       ITEMS:" 
        
       "<=30><R+1><C52>BLANKET LOCATION:"         
       "<=30><R+2><C52>BOARD:"  
       "<=30><R+3><C52>TOTAL #UP:"  
       "<=30><R+4><C52>INSTRUCTIONS:" SKIP(1)   

       "<#40><C1><FROM><C106><LINE><||3>" SKIP               
       "<=40><C47><B><U>OUTSIDE WORK</U></B>" SKIP(1) 

       "<#50><C1><FROM><C106><LINE><||3>" SKIP         
       "<=50><C48><B><U>DIECUTTING</U></B>" 
       "<=50><R+1><C50><FROM><R+5><LINE><||3>"        
       "<=50><R+1><C2>DIE #:"  
       "<=50><R+1><C52>INSTRUCTIONS:"               
       "<=50><R+2><C2>STRIPPING DIE#:" 
       "<=50><R+3><C2>MAKE READY:"  
       "<=50><R+4><C2>FRAME/BLANKER #:" SKIP(1) 
              
       "<#55><C1><FROM><C106><LINE><||3>" SKIP              
       "<=55><C44><B><U>SPECIAL PROCESSING</U></B>" SKIP(1)  
            
       "<#60><C1><FROM><C106><LINE><||3>" SKIP
       "<=60><C48><B><U>FINISHING</U></B>" SKIP
       "<=60><R+1><C2>INSTRUCTIONS:" SKIP(1)
                     
       "<#70><C1><FROM><C106><LINE><||3>" SKIP 
       "<=70><C50><FROM><R+7><LINE><||3>"       
       "<=70><R+1><C2>PACKING:" SKIP 
       "<=70><R+2><C2>CORRUGATED CASE:"  
       "<=70><R+2><C52>NOTES:"       
       "<=70><R+3><C2>PADS:" 
       "<=70><R+4><C2>PACK/CASE:"                      
       "<=70><R+5><C2>SKID TYPE:" 
       "<=70><R+6><C2>PALLET PATTERN:" SKIP 
     
                                                                       
       .
       
PAGE.
     
  


END. /* for each job-hdr NO-LOCK */
        hide all no-pause.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
