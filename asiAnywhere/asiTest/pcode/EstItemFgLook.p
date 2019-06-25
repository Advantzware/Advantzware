
/*------------------------------------------------------------------------
    File         : EstItemFgLook.p
    Purpose     :  estimate lookup

    Syntax      :

    Description : Return a Dataset of all Order Entry

    Author(s)   : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttEstItemFgLook NO-UNDO 

        FIELD vItenname     AS CHARACTER
        FIELD vItemno       AS CHARACTER
        FIELD vpartdscr1    AS CHARACTER
        FIELD vpartdscr2    AS CHARACTER
        FIELD vprice        AS DECIMAL
        FIELD vpruom        AS CHARACTER
        FIELD vcascnt       AS INTEGER
        FIELD vcasesunit    AS INTEGER
        FIELD vpartno       AS CHARACTER
        FIELD vqty          AS INTEGER
        FIELD vdis          AS DECIMAL  
        FIELD vjob          AS CHARACTER
        FIELD vpo           AS CHAR  
        FIELD vcost         AS DECIMAL 
        FIELD sman1         AS CHARACTER 
        FIELD sman2         AS CHARACTER
        FIELD sman3         AS CHARACTER
        FIELD vtax          AS LOGICAL
        FIELD vtype         AS CHAR
        FIELD company       AS CHAR
        FIELD ord-no        AS INT
        FIELD cust-no       AS CHAR
        FIELD po-no         AS CHAR
        FIELD req-code      AS CHAR
        FIELD req-date      AS DATE  
        FIELD prom-code     AS CHAR
        FIELD prom-date     AS DATE
        FIELD over-pct      AS DEC
        FIELD under-pct     AS DEC.

DEFINE DATASET dsEstItemFgLook FOR ttEstItemFgLook .


DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrder      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmEst      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEstItemFgLook.
 
IF prmUser = ?  THEN ASSIGN prmUser = "".
IF prmOrder = ?  THEN ASSIGN prmOrder = 0.
IF prmEst = ?  THEN ASSIGN prmEst = "".
DEFINE VAR prmComp AS CHAR NO-UNDO.
DEFINE SHARED VAR cocode AS CHARACTER  NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN
cocode = prmComp.
  DEFINE BUFFER   xest FOR est.
def new shared buffer xoe-ord for oe-ord.    /* BUFFER WITH ORDER HEADER */

def var li-cnt LIKE oe-ordl.cas-cnt no-undo.
  def var li-unit LIKE oe-ordl.cases-unit no-undo.
  def var lv-out-cost as dec no-undo.
  def var x as int no-undo.
  def var li-alloc as int no-undo.
  def var lv-recid as recid no-undo.
  def var lv-price as dec no-undo.
  def var lv-pr-uom as CHAR no-undo.
  def var v-tmp-part as CHAR no-undo.
  DEF VAR lv-cost-uom AS CHAR NO-UNDO.
  DEF VAR ll-tax LIKE oe-ordl.tax NO-UNDO.
  DEF VAR lv-new-i-no LIKE oe-ordl.i-no NO-UNDO.
DEF VARIABLE setFromHistory AS LOGICAL NO-UNDO.
DEF VAR cp-part-no LIKE itemfg.part-no NO-UNDO.
DEF VAR cp-rowid AS ROWID NO-UNDO.
def new shared var price-ent as log NO-UNDO.
def new shared var v-qty-mod as log no-undo.
def new shared var save_id as recid no-undo.  /* RECORD ID FOR ORDER LINE */

def new shared var v-i-item like oe-ordl.i-no no-undo. /* INPUT ITEM */
def new shared var v-i-qty like oe-ordl.qty no-undo. /* INPUT QUANTITY */

def var ld-prev-price as dec no-undo.
DEF VAR ll-new-file AS LOG NO-UNDO.
DEF VAR lv-q-no LIKE quotehd.q-no NO-UNDO.
def var v-upd-comm as log init yes no-undo.
DEF VAR lv-add-mode AS LOG NO-UNDO. 

def var ll-got-qtprice as log no-undo.
 FIND FIRST oe-ord WHERE oe-ord.company = prmComp AND oe-ord.ord-no = prmOrder NO-LOCK NO-ERROR.

  DO WHILE NOT AVAIL itemfg:
    FIND FIRST itemfg
        WHERE itemfg.company EQ prmComp
          AND itemfg.i-no    EQ prmItem
        NO-LOCK NO-ERROR.
    IF NOT AVAIL itemfg THEN DO:
        RUN custom/getobitm.p (prmComp, prmItem,
                               OUTPUT lv-new-i-no).
            prmItem = lv-new-i-no.
  END.
  if /*ip-type <> "add" and */ 
      prmEst <> "" then do:
      if oe-ord.est-no eq "" then do:
          if not avail xest then
              find first xest where xest.company eq prmComp and
              xest.est-no eq prmEst no-lock no-error.
          if avail xest  and
              (ld-prev-price = 0 or ld-prev-price = dec(ttEstItemFgLook.vprice) )
              /* to allow user's overriding price */  
              then do:
              assign lv-price = dec(ttEstItemFgLook.vprice)
                     lv-pr-uom = ttEstItemFgLook.vpruom
                     v-tmp-part = prmItem
                     .
              ll-got-qtprice = YES.
              run oe/getqpric.p (recid(xest), ttEstItemFgLook.vpartno,
                                    v-tmp-part, int(ttEstItemFgLook.vqty),
                                    input-output lv-price,
                                    input-output lv-pr-uom,
                                    OUTPUT lv-q-no).
              assign 
               ttEstItemFgLook.vprice  = lv-price
               ttEstItemFgLook.vpruom = lv-pr-uom.

              /*{oe/ordltot.i oe-ordl qty oe-ordl}*/
           end.           
        end. 
     end.     /* oe-ordl.est-no <> "" */

     /*run validate-fgitem no-error.
     find first itemfg where itemfg.company = prmComp and
                          itemfg.i-no = prmItem
                          no-lock no-error.
     if not avail itemfg then return error.*/
    end.  /* update and est-no <> "" */

  /*run validate-fgitem no-error.*/
  
  FIND FIRST cust
     /* {sys/ref/cust.w}*/WHERE  cust.cust-no EQ oe-ord.cust-no
      USE-INDEX cust
      NO-LOCK NO-ERROR.
  ASSIGN
   ttEstItemFgLook.vdis      = cust.disc .
  ll-tax = AVAIL cust AND cust.sort EQ "Y" AND oe-ord.tax-gr NE "" AND itemfg.taxable.

  IF NOT ll-tax THEN DO:
   {custom/shptotax.i oe-ord.cust-no oe-ord.sold-id ll-tax}
  END.
   ttEstItemFgLook.vtax = ll-tax .
assign
    ttEstItemFgLook.company   = prmComp
    ttEstItemFgLook.ord-no    = oe-ord.ord-no
    ttEstItemFgLook.vtype     = oe-ord.type
    ttEstItemFgLook.cust-no   = oe-ord.cust-no
    ttEstItemFgLook.po-no     = oe-ord.po-no
    ttEstItemFgLook.req-code  = oe-ord.due-code
    ttEstItemFgLook.req-date  = oe-ord.due-date
    ttEstItemFgLook.prom-code = oe-ord.due-code
    ttEstItemFgLook.prom-date = oe-ord.due-date
    ttEstItemFgLook.over-pct  = oe-ord.over-pct   
    ttEstItemFgLook.under-pct = oe-ord.under-pct
    .

  if not avail oe-ord then find oe-ord where oe-ord.company = prmComp and
                                oe-ord.ord-no = prmOrder no-lock. 

  {custom/fgexempt.i itemfg ttEstItemFgLook.disc}

  if ttEstItemFgLook.vtype  eq "O" and prmEst ne "" then
     
                   if itemfg.i-name <> "" then ASSIGN ttEstItemFgLook.vItenname = itemfg.i-name .
                   if itemfg.i-no <> "" then ASSIGN ttEstItemFgLook.vItemno =   itemfg.i-no. 
                   if itemfg.part-dscr2 <> "" THEN ASSIGN ttEstItemFgLook.vpartdscr2 = itemfg.part-dscr2 
            .
               else  
                    if itemfg.i-name <> "" THEN ASSIGN ttEstItemFgLook.vItenname = itemfg.i-name .
                    if itemfg.i-no <> ""  THEN ASSIGN ttEstItemFgLook.vItemno = itemfg.i-no .
                  /*  IF setFromHistory THEN historyPrice ELSE
                                            if itemfg.sell-price <> 0 THEN ASSIGN ttEstItemFgLook.vprice = itemfg.sell-price .
                    IF setFromHistory THEN historyPrUOM ELSE
                                            if itemfg.sell-uom <> ""  THEN ASSIGN ttEstItemFgLook.vpruom = itemfg.sell-uom .*/
                    if itemfg.case-count <> 0 THEN ASSIGN ttEstItemFgLook.vcascnt =  itemfg.case-count.
                    if itemfg.case-pall <> 0 THEN ASSIGN ttEstItemFgLook.vcasesunit = itemfg.case-pall.
                    if itemfg.part-dscr2 <> "" THEN ASSIGN ttEstItemFgLook.vpartdscr2 = itemfg.part-dscr2 .
  
  if prmEst eq "" THEN DO:
                   if itemfg.part-no <> "" then ASSIGN ttEstItemFgLook.vpartno = itemfg.part-no .
                   if itemfg.part-dscr1 <> "" THEN ASSIGN ttEstItemFgLook.vpartdscr1 =  itemfg.part-dscr1 .
                   if itemfg.case-count <> 0 THEN ASSIGN ttEstItemFgLook.vcascnt = itemfg.case-count.
                    if itemfg.case-pall <> 0 THEN ASSIGN ttEstItemFgLook.vcasesunit = itemfg.case-pall .
               

     
       ASSIGN
        cp-part-no = ""
        cp-rowid   = ROWID(itemfg).
       RUN custom/getcpart.p (prmComp, oe-ord.cust-no,
                              INPUT-OUTPUT cp-part-no, INPUT-OUTPUT cp-rowid).
       IF cp-part-no NE "" THEN ttEstItemFgLook.vpartno = cp-part-no.
     END.
  
  /*else do:
      run oe/oe-cnt.p(recid(oe-ordl), output li-cnt, OUTPUT li-unit).
      assign
       ttEstItemFgLook.vcascnt    = li-cnt
       ttEstItemFgLook.vcasesunit = li-unit.
  end.*/
 
  IF INT(ttEstItemFgLook.vqty) GT 0 THEN
    IF INT(ttEstItemFgLook.vqty) LT INT(ttEstItemFgLook.vcascnt) THEN
      ttEstItemFgLook.vcascnt = ttEstItemFgLook.vqty.
    ELSE
    IF INT(ttEstItemFgLook.vcascnt) EQ 0 AND prmItem NE "0" THEN
      ttEstItemFgLook.vcascnt = 1.

  IF INT(ttEstItemFgLook.vcasesunit) EQ 0 THEN
    ttEstItemFgLook.vcasesunit = 1.

  /*ASSIGN
   oe-ordl.cases:SCREEN-VALUE   = STRING(TRUNC(INT(vqty) / INT(ttEstItemFgLook.vcascnt),0))
   oe-ordl.partial:SCREEN-VALUE = STRING(INT(vqty) MOD INT(ttEstItemFgLook.vcascnt)). */

  IF /* v-foamdate-log                                             AND*/
     itemfg.style NE ""                                         AND
     CAN-FIND(FIRST style WHERE style.company EQ itemfg.company
                            AND style.style   EQ itemfg.style
                            AND style.type    EQ "F")           THEN DO:
    ttEstItemFgLook.req-date = oe-ord.ord-date . 
  END. 

  RUN itemfg-cost.
 if itemfg.isaset and itemfg.t-sqft eq 0 AND
     CAN-FIND(FIRST fg-set WHERE fg-set.company EQ prmComp
                             AND fg-set.set-no  EQ itemfg.i-no
                             AND fg-set.part-no NE fg-set.set-no) then
   run fg/updsetdm.p (recid(itemfg)).
/*
 if prmEst eq "" then do:
    if v-upd-comm then do:               
       {oe/oescomm.i oe-ordl.ttEstItemFgLook.sman1 1}
       {oe/oescomm.i oe-ordl.ttEstItemFgLook.sman2 2}
       {oe/oescomm.i oe-ordl.ttEstItemFgLook.sman3 3}              
    end.
 end. */

 RUN get-price.
/* END.*/
/*------------------------------------------------------------------------------*/
PROCEDURE get-price :

  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR lv-price-ent LIKE price-ent NO-UNDO.

    IF NOT price-ent                           AND
       AVAIL oe-ordl                           AND
       TRIM(prmEst) EQ "" THEN DO:

      lv-price-ent = price-ent.
      IF NOT lv-add-mode THEN price-ent = YES.
  
      IF NOT AVAIL xoe-ord THEN
      FIND FIRST xoe-ord
          WHERE xoe-ord.company EQ prmComp
            AND xoe-ord.ord-no  EQ oe-ordl.ord-no
          NO-LOCK NO-ERROR.

      ASSIGN
       save_id   = RECID(oe-ordl)
       lv-rowid  = ROWID(oe-ordl)
       v-i-item  = prmItem
       v-i-qty   = ttEstItemFgLook.vqty
      /* v-qty-mod = oe-ordl.qty:MODIFIED  */.  

      FIND FIRST itemfg
          WHERE itemfg.company EQ prmComp
            AND itemfg.i-no    EQ v-i-item
          NO-LOCK NO-ERROR.
      IF AVAIL itemfg THEN DO:
        RUN oe/oe-price.p.
        FIND oe-ordl WHERE ROWID(oe-ordl) EQ lv-rowid NO-ERROR.
        

       /* {oe/ordltot.i oe-ordl qty oe-ordl}  */
        RUN ordltot.
      END.

      price-ent = lv-price-ent.
    END.
  
END PROCEDURE.
/*------------------------------------------------------*/
PROCEDURE itemfg-cost :
/*------------------------------------------------------------------------------*/
  DEF VAR lv-uom LIKE oe-ordl.pr-uom NO-UNDO.
  DEF VAR lv-cost AS DEC DECIMALS 10 NO-UNDO.
  FIND FIRST ttEstItemFgLook NO-LOCK NO-ERROR.
  if ttEstItemFgLook.vjob = "" then do:
      find first itemfg
          where itemfg.company = prmComp
            and itemfg.i-no = prmItem
          NO-LOCK NO-ERROR.
      find first po-ordl where po-ordl.company   eq prmComp
                           and po-ordl.i-no      eq prmItem
                           and po-ordl.po-no     eq int(ttEstItemFgLook.vpo)
                           and po-ordl.item-type eq no
                           use-index item-ordno no-lock no-error.
      if AVAIL po-ordl AND int(ttEstItemFgLook.vpo) NE 0 then
        IF ttEstItemFgLook.vpruom = "" THEN ASSIGN ttEstItemFgLook.vpruom = po-ordl.cons-uom .

                if po-ordl.cons-uom NE "" THEN ASSIGN lv-uom = po-ordl.cons-uom .
               ttEstItemFgLook.vcost   = po-ordl.cons-cost.
      
      IF AVAIL itemfg THEN
        if itemfg.prod-uom NE "" AND ttEstItemFgLook.vpruom = "" THEN assign ttEstItemFgLook.vpruom =  itemfg.prod-uom .
                                    if itemfg.prod-uom NE "" THEN ASSIGN lv-uom =  itemfg.prod-uom  .
               ttEstItemFgLook.vcost   = itemfg.total-std-cost.
      
      if lv-uom ne "M" THEN do:
        run sys/ref/convcuom.p(lv-uom, "M", 0, 0, 0, 0,
                               dec(ttEstItemFgLook.vcost), output lv-cost).
        assign ttEstItemFgLook.vcost = lv-cost
            ttEstItemFgLook.vpruom = itemfg.sell-uom .                       
      end. /*if lv-uom ne "M" THEN do:*/

      if AVAIL po-ordl AND int(ttEstItemFgLook.vpo) NE 0 THEN DO:
         FIND FIRST po-ord WHERE
             po-ord.company EQ po-ordl.company AND
             po-ord.po-no   EQ po-ordl.po-no NO-LOCK NO-ERROR.
         IF AVAIL po-ord THEN DO:
            FIND FIRST reftable WHERE
                 reftable.reftable EQ 'e-itemfg-vend.markup' AND
                 reftable.company EQ po-ordl.company AND
                 reftable.loc EQ po-ordl.i-no AND
                 reftable.code EQ po-ord.vend-no
                 NO-LOCK NO-ERROR.
            
           IF AVAIL reftable THEN DO:
               ttEstItemFgLook.vcost = DEC(ttEstItemFgLook.vcost) * (1 + (reftable.val[1]/ 100.0 )).
               
            END.
            
         END.
      END.
    END PROCEDURE.



/*********************************************************************************************************************/
PROCEDURE ordltot:
FIND FIRST ttUpdateEstItem NO-LOCK NO-ERROR.
MESSAGE "jyoti" ttUpdateEstItem.i-no  ttUpdateEstItem.pr-uom   ttUpdateEstItem.cas-cnt ttUpdateEstItem.qty.
  find first itemfg   WHERE itemfg.company = prmComp and
       itemfg.i-no eq ttUpdateEstItem.i-no
      no-lock no-error.
      
  assign
   v-tmp-price = if ttUpdateEstItem.pr-uom begins "L" AND ttUpdateEstItem.pr-uom NE "LB" then
                   if ttUpdateEstItem.qty lt 0 then -1 else 1
                 else
                 if ttUpdateEstItem.pr-uom eq "CS" then
                   ttUpdateEstItem.qty / (if ttUpdateEstItem.cas-cnt ne 0 then ttUpdateEstItem.cas-cnt else
                                    if avail itemfg and itemfg.case-count ne 0
                                                   then itemfg.case-count else
                                                        1)
                 else
                 if ttUpdateEstItem.pr-uom eq "C" then
                    ttUpdateEstItem.qty / 100
                 else
                 if  ttUpdateEstItem.pr-uom eq "M" then
                    ttUpdateEstItem.qty / 1000
                 else
                    ttUpdateEstItem.qty.
                            
    lv-t-price = v-tmp-price * ttUpdateEstItem.price.
    /*{1}.t-price:screen-value = string(round(lv-t-price - (lv-t-price * INPUT {1}.disc / 100),2)).*/
    ttUpdateEstItem.t-price = (lv-t-price - ROUND(lv-t-price * ttUpdateEstItem.disc / 100,2)).
          

        /*STRING(
          IF v-print-fmt EQ "Dayton" THEN 
        ELSE
            ROUND(lv-t-price * (1 - (ttUpdateEstItem.disc / 100)),2)).*/

/* end ---------------------------------- copr. 1992  advanced software, inc. */
  END PROCEDURE.
