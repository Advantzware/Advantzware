


/*------------------------------------------------------------------------
    File        : ItemLookall.p
    Purpose     : item

    Syntax      :

    Description : Return a Dataset of all item

    Author(s)   : Jyoti
    Created     : dec 04 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemLookallone NO-UNDO 
    FIELD itemno        AS CHAR
    FIELD iname         AS CHAR
    FIELD idscr         AS CHAR
    FIELD consuom       AS CHAR
    FIELD prqtyuom      AS CHAR
    FIELD conscost      AS DECIMAL
    FIELD part-dscr1    AS CHAR
    FIELD part-dscr2    AS CHAR
    FIELD item-type     AS CHAR 
    FIELD tlen          AS DECIMAL
    FIELD twid          AS DECIMAL
    FIELD tdep          AS DECIMAL
    FIELD pruom         AS CHAR
    FIELD cost          AS DECIMAL
    FIELD swid          AS DECIMAL
    FIELD slen          AS DECIMAL
    FIELD sdep          AS DECIMAL
    FIELD v-wid-frac    AS CHAR 
    FIELD v-len-frac    AS CHAR
    FIELD v-dep-frac    AS CHAR
    FIELD actnum        AS CHAR
    FIELD actdesc       AS CHAR
    FIELD vend-i-no     AS CHAR
    FIELD tax           AS CHAR
    FIELD cust          AS CHAR
    FIELD partno        AS CHAR
    FIELD qtyonhand     AS DECIMAL
    FIELD a-hdr         AS CHAR
    FIELD fi-uom        AS CHAR
    FIELD q-onh         AS DECIMAL
    FIELD q-ono         AS DECIMAL
    FIELD q-alloc       AS DECIMAL
    FIELD q-back        AS DECIMAL
    FIELD q-avail       AS DECIMAL
  
    FIELD fi_m-onh      AS DECIMAL 
    FIELD fi_m-ono      AS DECIMAL 
    FIELD fi_m-comm     AS DECIMAL 
    FIELD fi_m-back     AS DECIMAL 
    FIELD fi_m-avail    AS DECIMAL 
    FIELD poNxtPgBQty   AS DEC 
    FIELD posetup       AS DEC 
    FIELD pototcost     AS DEC
    FIELD ext_lodtg       AS CHAR.

DEFINE DATASET dsItemLookallone FOR ttItemLookallone.

DEFINE INPUT PARAMETER prmAction          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmIndu            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmMatType         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPoNo            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmJob             AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmJob2            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp            AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmType            AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmpoline          AS INT       NO-UNDO.
DEFINE INPUT PARAMETER prmino             AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmsno             AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmbno             AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmqty             AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmpoitemtype      AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmCust            AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmcost            AS DEC        NO-UNDO.
DEFINE INPUT PARAMETER prmconsuom         AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmprqtyuom        AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmpruom           AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmdiscount        AS DEC        NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemLookallone .

DEF VAR v-ord-qty AS dec NO-UNDO.

DEF BUFFER buf-item FOR ITEM.
DEF BUFFER b-vend FOR vend.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD std-uom AS CHAR.

DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20
    FIELD rec_key AS CHAR.

DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-setup FOR reftable.

DEF TEMP-TABLE tt-eiv-2 NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20
    FIELD rec_key AS CHAR.
       
IF prmAction     = ? THEN ASSIGN prmAction    = "".
IF prmUser       = ? THEN ASSIGN prmUser      = "".
IF prmField      = ? THEN ASSIGN prmField     = "".
IF prmCondition  = ? THEN ASSIGN prmCondition = "".
IF prmText       = ? THEN ASSIGN prmText      = "".     
IF prmIndu       = ? THEN ASSIGN prmIndu      = "".     
IF prmMatType    = ? THEN ASSIGN prmMatType   = "".     
IF prmPoNo       = ? THEN ASSIGN prmPoNo      = "".     
IF prmJob        = ? THEN ASSIGN prmJob       = "".     
IF prmJob2       = ? THEN ASSIGN prmJob2      = "".     
IF prmComp       = ? THEN ASSIGN prmComp      = "".     
IF prmType       = ? THEN ASSIGN prmType      = "".
IF prmpoline     = ? THEN ASSIGN prmpoline       = 0.
IF prmino        = ? THEN ASSIGN prmino       = "".
IF prmsno        = ? THEN ASSIGN prmsno       = 0.
IF prmbno        = ? THEN ASSIGN prmbno       = 0.
IF prmqty        = ? THEN ASSIGN prmqty       = 0.
IF prmpoitemType      = ? THEN ASSIGN prmpoitemType       = "".
IF prmcust      = ? THEN ASSIGN prmcust       = "".
IF prmcost            = ? THEN ASSIGN prmcost             = 0.
IF prmconsuom         = ? THEN ASSIGN prmconsuom          = "".
IF prmprqtyuom        = ? THEN ASSIGN prmprqtyuom         = "". 
IF prmpruom           = ? THEN ASSIGN prmpruom            = "".
IF prmdiscount        = ? THEN ASSIGN prmdiscount         = 0. 

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
DEF VAR v-charge LIKE surcharge.charge.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF VAR fg-uom-list AS CHAR NO-UNDO.
DEF VAR lv-cost LIKE po-ordl.cost NO-UNDO.
DEF NEW SHARED VAR v-len LIKE po-ordl.s-len NO-UNDO.
DEF NEW SHARED VAR v-wid LIKE po-ordl.s-wid NO-UNDO.
DEF NEW SHARED VAR v-dep LIKE po-ordl.s-len NO-UNDO.
 DEFINE VAR custcount AS CHAR NO-UNDO.
 DEF NEW SHARED VAR v-basis-w AS DEC NO-UNDO. /* for po/po-adder2.p */
 DEF VAR ld-roll-len AS DEC NO-UNDO.
 def NEW shared var factor# as decimal no-undo.
 def new shared var v-pocost1 as char.
def NEW shared var v-default-gl-log as log no-undo.
def NEW shared var v-default-gl-cha as cha no-undo.
DEF VAR lv-cust-no LIKE itemfg.cust-no NO-UNDO.
DEF VAR lv-part-no LIKE itemfg.part-no NO-UNDO.
DEF VAR ll-new-file AS LOG NO-UNDO.
def NEW shared var v-po-qty as log initial true no-undo.
def new shared var v-hold-op1 as log.
def NEW shared var v-po-msf like sys-ctrl.int-fld no-undo.
DEFINE VARIABLE fi_q-avail AS DECIMAL NO-UNDO.
DEFINE VARIABLE fi_q-back AS DECIMAL NO-UNDO. 
DEFINE VARIABLE fi_q-comm AS DECIMAL NO-UNDO. 
DEFINE VARIABLE fi_q-onh AS DECIMAL NO-UNDO.  
DEFINE VARIABLE fi_q-ono AS DECIMAL NO-UNDO.  
DEFINE VARIABLE fi_uom AS CHARACTER NO-UNDO.
    
ASSIGN
    cocode = prmComp .
{fg/fullset.i NEW}

DO TRANSACTION:
  {sys/inc/pocostq.i}
  {sys/inc/poqty.i}
  {sys/inc/pouom.i}
  {sys/inc/aptax.i}
END.


FIND FIRST rm-ctrl WHERE rm-ctrl.company EQ cocode NO-LOCK.
  FIND FIRST fg-ctrl WHERE fg-ctrl.company EQ cocode NO-LOCK.

  RUN po/po-sysct.p.
  {sys/ref/pocost.i}
  assign
   v-pocost1  = v-pocost
   v-hold-op1 = v-hold-op.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

ll-new-file = CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part").

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.
              
 FIND FIRST po-ord WHERE 
       po-ord.company eq prmComp and 
           po-ord.po-no EQ int(prmPoNo) NO-LOCK NO-ERROR. 




IF prmType = "ItemFG" THEN do:
if prmAction <> "search" then do:
    FOR EACH itemfg WHERE itemfg.company = prmComp NO-LOCK BY itemfg.i-no BY itemfg.i-name:
        
        create ttItemLookallone.
            assign                                         
               ttItemLookallone.itemno   =  itemfg.i-no  
               ttItemLookallone.partno   =  itemfg.part-no  
               ttItemLookallone.cust     =  itemfg.cust-no
               ttItemLookallone.qtyonhand    =  itemfg.q-onh
               ttItemLookallone.iname    =  itemfg.i-name                         
               ttItemLookallone.idscr    =  itemfg.i-no 
               ttItemLookallone.consuom       =  itemfg.prod-uom
               ttItemLookallone.prqtyuom      =  IF pouom-chr EQ "Purchase" THEN itemfg.pur-uom
                                                                         ELSE itemfg.prod-uom                       
               ttItemLookallone.conscost      =  (itemfg.last-cost)
               ttItemLookallone.part-dscr1    =  itemfg.part-dscr1   
               ttItemLookallone.part-dscr2    =  itemfg.part-dscr2                          
               ttItemLookallone.item-type     =  "FG"
               ttItemLookallone.tlen          =  itemfg.t-len   
               ttItemLookallone.twid          =  itemfg.t-wid                          
               ttItemLookallone.tdep          =  0
               ttItemLookallone.pruom         =  itemfg.prod-uom 
               ttItemLookallone.a-hdr         =  "Allocated"           
               ttItemLookallone.fi-uom        =  "EA"                 
               ttItemLookallone.q-onh         =  itemfg.q-onh         
               ttItemLookallone.q-ono         =  itemfg.q-ono         
               ttItemLookallone.q-alloc       =  itemfg.q-alloc       
               ttItemLookallone.q-back        =  itemfg.q-back        
               ttItemLookallone.q-avail       =  itemfg.q-avail.      
                                                    
                                               
                IF itemfg.taxable AND aptax-chr EQ "Item" THEN
       ttItemLookallone.tax = "yes".
                

     find first e-itemfg where e-itemfg.company eq cocode 
                          and e-itemfg.i-no eq itemfg.i-no
                          no-lock no-error.

     IF AVAIL e-itemfg THEN
       FIND FIRST e-itemfg-vend OF e-itemfg NO-LOCK
         /* gdm - 06040918 
           WHERE e-itemfg-vend.vend-no EQ po-ord.vend-no
         */  
           NO-ERROR.
     
     IF AVAIL e-itemfg-vend AND AVAIL e-itemfg THEN
       ttItemLookallone.pruom = e-itemfg.std-uom.

     
     
     IF LOOKUP(ttItemLookallone.consuom ,fg-uom-list) EQ 0 OR
        LOOKUP(ttItemLookallone.pruom,fg-uom-list)   EQ 0 THEN
       RUN sys/ref/convcuom.p(ttItemLookallone.consuom , ttItemLookallone.pruom,
                              0, v-len, v-wid, v-dep,
                              DEC(ttItemLookallone.cost), OUTPUT lv-cost).
   
     ASSIGN
      ttItemLookallone.cost = (lv-cost)
      ttItemLookallone.swid = (v-wid)
      ttItemLookallone.slen = (v-len)
      ttItemLookallone.sdep = (v-dep).

      RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.swid), INPUT 32, OUTPUT v-wid-frac).
      RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.slen), INPUT 32, OUTPUT v-len-frac).
      RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.sdep), INPUT 32, OUTPUT v-dep-frac).
      ASSIGN
        ttItemLookallone.v-wid-frac = v-wid-frac
        ttItemLookallone.v-len-frac = v-len-frac
        ttItemLookallone.v-dep-frac = v-dep-frac.
         
     /* populate GL# from reftable if it exists using itemfg AH 02-23-10*/
     ASSIGN v-charge = "".
     FIND FIRST surcharge WHERE surcharge.company = prmComp
                            AND surcharge.charge <> "" NO-LOCK NO-ERROR.
     IF AVAIL surcharge THEN
     ASSIGN v-charge = surcharge.charge.
     FIND FIRST reftable WHERE reftable.reftable EQ "chargecode"
            AND reftable.company  EQ itemfg.company
            AND reftable.loc      EQ itemfg.procat
            AND reftable.code     EQ v-charge
          /* AND reftable.code2 = "" */
            NO-LOCK NO-ERROR.
     IF AVAIL reftable AND reftable.dscr <> "" THEN 
        ASSIGN ttItemLookallone.actnum = reftable.dscr.
     /* AH */
     ELSE 
     FOR EACH prodl
         WHERE prodl.company EQ cocode
           AND prodl.procat  EQ itemfg.procat
         NO-LOCK,
         FIRST prod
         WHERE prod.company EQ cocode
           AND prod.prolin  EQ prodl.prolin
         NO-LOCK:

       ttItemLookallone.actnum = prod.fg-mat.
       LEAVE.
     END.

     RELEASE reftable.

     find first account where account.company eq cocode and
                              account.actnum eq ttItemLookallone.actnum no-lock no-error.
     ttItemLookallone.actdesc = if avail account then account.dscr else ''.

      IF AVAIL e-itemfg AND AVAIL po-ord THEN
       FIND FIRST e-itemfg-vend OF e-itemfg NO-LOCK
         WHERE e-itemfg-vend.vend-no EQ po-ord.vend-no NO-ERROR.

     IF AVAIL e-itemfg-vend AND e-itemfg-vend.vend-item NE "" THEN ttItemLookallone.vend-i-no = e-itemfg-vend.vend-item.
     ELSE if itemfg.vend-no eq po-ord.vend-no THEN ttItemLookallone.vend-i-no = itemfg.vend-item.
     ELSE if itemfg.vend2-no eq po-ord.vend-no THEN ttItemLookallone.vend-i-no = itemfg.vend2-item.

   /*  RUN fg-qtys (ROWID(itemfg)).*/
  
  find first account where account.company eq cocode and
                           account.actnum eq ttItemLookallone.actnum no-lock no-error.
  ttItemLookallone.actdesc = if avail account then account.dscr else ''.                    
              


                
               
    END.	 /* FOR EACH item */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
     
    if prmField = "i-no"  then do:
        if prmCondition = "EQUAL" then do:
              FOR EACH itemfg WHERE itemfg.company = prmComp AND (itemfg.i-no = prmText OR prmText = "") NO-LOCK:
        
        create ttItemLookallone.
            assign                                         
               ttItemLookallone.itemno   =  itemfg.i-no  
               ttItemLookallone.partno   =  itemfg.part-no  
               ttItemLookallone.cust     =  itemfg.cust-no
               ttItemLookallone.qtyonhand    =  itemfg.q-onh
               ttItemLookallone.iname    =  itemfg.i-name                         
               ttItemLookallone.idscr    =  itemfg.i-no 
               ttItemLookallone.consuom       =  itemfg.prod-uom
               ttItemLookallone.prqtyuom      =  IF pouom-chr EQ "Purchase" THEN itemfg.pur-uom
                                                                         ELSE itemfg.prod-uom                       
               ttItemLookallone.conscost      =  (itemfg.last-cost)
               ttItemLookallone.part-dscr1    =  itemfg.part-dscr1   
               ttItemLookallone.part-dscr2    =  itemfg.part-dscr2                          
               ttItemLookallone.item-type     =  "FG"
               ttItemLookallone.tlen          =  itemfg.t-len   
               ttItemLookallone.twid          =  itemfg.t-wid                          
               ttItemLookallone.tdep          =  0
               ttItemLookallone.pruom         =  itemfg.prod-uom  
               ttItemLookallone.a-hdr         =  "Allocated"           
               ttItemLookallone.fi-uom        =  "EA"                 
               ttItemLookallone.q-onh         =  itemfg.q-onh         
               ttItemLookallone.q-ono         =  itemfg.q-ono         
               ttItemLookallone.q-alloc       =  itemfg.q-alloc       
               ttItemLookallone.q-back        =  itemfg.q-back        
               ttItemLookallone.q-avail       =  itemfg.q-avail.  

                ASSIGN 
                     
                    v-len              = itemfg.t-len
                    v-wid              = itemfg.t-wid
                    v-dep              = 0
                    
                    {po/calc16.i v-wid}.

                IF itemfg.taxable AND aptax-chr EQ "Item" THEN
       ttItemLookallone.tax = "yes".

     find first e-itemfg where e-itemfg.company eq cocode 
                          and e-itemfg.i-no eq itemfg.i-no
                          no-lock no-error.

     IF AVAIL e-itemfg THEN
       FIND FIRST e-itemfg-vend OF e-itemfg NO-LOCK
         /* gdm - 06040918 
           WHERE e-itemfg-vend.vend-no EQ po-ord.vend-no
         */  
           NO-ERROR.
     
     IF AVAIL e-itemfg-vend AND AVAIL e-itemfg THEN
       ttItemLookallone.pruom = e-itemfg.std-uom.

     
     
     IF LOOKUP(ttItemLookallone.consuom ,fg-uom-list) EQ 0 OR
        LOOKUP(ttItemLookallone.pruom,fg-uom-list)   EQ 0 THEN
       RUN sys/ref/convcuom.p(ttItemLookallone.consuom , ttItemLookallone.pruom,
                              0, v-len, v-wid, v-dep,
                              DEC(ttItemLookallone.conscost), OUTPUT lv-cost).
            
     ASSIGN
      ttItemLookallone.cost = (lv-cost)
      ttItemLookallone.swid = (v-wid)
      ttItemLookallone.slen = (v-len)
      ttItemLookallone.sdep = (v-dep).
        
      RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.swid), INPUT 32, OUTPUT v-wid-frac).
      RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.slen), INPUT 32, OUTPUT v-len-frac).
      RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.sdep), INPUT 32, OUTPUT v-dep-frac).
      ASSIGN
        ttItemLookallone.v-wid-frac = v-wid-frac
        ttItemLookallone.v-len-frac = v-len-frac
        ttItemLookallone.v-dep-frac = v-dep-frac.

     /* populate GL# from reftable if it exists using itemfg AH 02-23-10*/
     ASSIGN v-charge = "".
     FIND FIRST surcharge WHERE surcharge.company = prmComp
                            AND surcharge.charge <> "" NO-LOCK NO-ERROR.
     IF AVAIL surcharge THEN
     ASSIGN v-charge = surcharge.charge.
     FIND FIRST reftable WHERE reftable.reftable EQ "chargecode"
            AND reftable.company  EQ itemfg.company
            AND reftable.loc      EQ itemfg.procat
            AND reftable.code     EQ v-charge
          /* AND reftable.code2 = "" */
            NO-LOCK NO-ERROR.
     IF AVAIL reftable AND reftable.dscr <> "" THEN 
        ASSIGN ttItemLookallone.actnum = reftable.dscr.
     /* AH */
     ELSE 
     FOR EACH prodl
         WHERE prodl.company EQ cocode
           AND prodl.procat  EQ itemfg.procat
         NO-LOCK,
         FIRST prod
         WHERE prod.company EQ cocode
           AND prod.prolin  EQ prodl.prolin
         NO-LOCK:

       ttItemLookallone.actnum = prod.fg-mat.
       LEAVE.
     END.

     RELEASE reftable.

     find first account where account.company eq cocode and
                              account.actnum eq ttItemLookallone.actnum no-lock no-error.
     ttItemLookallone.actdesc = if avail account then account.dscr else ''.

      IF AVAIL e-itemfg AND AVAIL po-ord THEN
       FIND FIRST e-itemfg-vend OF e-itemfg NO-LOCK
         WHERE e-itemfg-vend.vend-no EQ po-ord.vend-no NO-ERROR.

     IF AVAIL e-itemfg-vend AND e-itemfg-vend.vend-item NE "" THEN ttItemLookallone.vend-i-no = e-itemfg-vend.vend-item.
     ELSE if itemfg.vend-no eq po-ord.vend-no THEN ttItemLookallone.vend-i-no = itemfg.vend-item.
     ELSE if itemfg.vend2-no eq po-ord.vend-no THEN ttItemLookallone.vend-i-no = itemfg.vend2-item.

   /*  RUN fg-qtys (ROWID(itemfg)).*/
  
  find first account where account.company eq cocode and
                           account.actnum eq ttItemLookallone.actnum no-lock no-error.
  ttItemLookallone.actdesc = if avail account then account.dscr else ''.                    
              
FIND FIRST po-ordl WHERE 
       po-ordl.company eq cocode and 
           po-ordl.po-no EQ INT(prmpoNo)  and
           po-ordl.LINE EQ INT(prmpoLine) NO-LOCK NO-ERROR.
        
        FIND FIRST po-ord WHERE 
            po-ord.company eq cocode and 
            po-ord.po-no EQ INT(prmpoNo)  NO-LOCK NO-ERROR.

  RUN vend-cost(TRUE).

                
               
    END.	 /* FOR EACH item */  
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
           FOR EACH itemfg WHERE itemfg.company = prmComp AND (itemfg.i-no BEGINS prmText OR prmText = "") NO-LOCK:
        
        create ttItemLookallone.
            assign                                         
               ttItemLookallone.itemno   =  itemfg.i-no  
               ttItemLookallone.partno   =  itemfg.part-no  
               ttItemLookallone.cust     =  itemfg.cust-no
               ttItemLookallone.qtyonhand    =  itemfg.q-onh
               ttItemLookallone.iname    =  itemfg.i-name                         
               ttItemLookallone.idscr    =  itemfg.i-no 
               ttItemLookallone.consuom       =  itemfg.prod-uom
               ttItemLookallone.prqtyuom      =  IF pouom-chr EQ "Purchase" THEN itemfg.pur-uom
                                                                         ELSE itemfg.prod-uom                       
               ttItemLookallone.conscost      =  (itemfg.last-cost)
               ttItemLookallone.part-dscr1    =  itemfg.part-dscr1   
               ttItemLookallone.part-dscr2    =  itemfg.part-dscr2                          
               ttItemLookallone.item-type     =  "FG"
               ttItemLookallone.tlen          =  itemfg.t-len   
               ttItemLookallone.twid          =  itemfg.t-wid                          
               ttItemLookallone.tdep          =  0
               ttItemLookallone.pruom         =  itemfg.prod-uom  
               ttItemLookallone.a-hdr         =  "Allocated"           
               ttItemLookallone.fi-uom        =  "EA"                 
               ttItemLookallone.q-onh         =  itemfg.q-onh         
               ttItemLookallone.q-ono         =  itemfg.q-ono         
               ttItemLookallone.q-alloc       =  itemfg.q-alloc       
               ttItemLookallone.q-back        =  itemfg.q-back        
               ttItemLookallone.q-avail       =  itemfg.q-avail.  


                IF itemfg.taxable AND aptax-chr EQ "Item" THEN
       ttItemLookallone.tax = "yes".

     find first e-itemfg where e-itemfg.company eq cocode 
                          and e-itemfg.i-no eq itemfg.i-no
                          no-lock no-error.

     IF AVAIL e-itemfg THEN
       FIND FIRST e-itemfg-vend OF e-itemfg NO-LOCK
         /* gdm - 06040918 
           WHERE e-itemfg-vend.vend-no EQ po-ord.vend-no
         */  
           NO-ERROR.
     
     IF AVAIL e-itemfg-vend AND AVAIL e-itemfg THEN
       ttItemLookallone.pruom = e-itemfg.std-uom.

     
     
     IF LOOKUP(ttItemLookallone.consuom ,fg-uom-list) EQ 0 OR
        LOOKUP(ttItemLookallone.pruom,fg-uom-list)   EQ 0 THEN
       RUN sys/ref/convcuom.p(ttItemLookallone.consuom , ttItemLookallone.pruom,
                              0, v-len, v-wid, v-dep,
                              DEC(ttItemLookallone.cost), OUTPUT lv-cost).

     ASSIGN
      ttItemLookallone.cost = (lv-cost)
      ttItemLookallone.swid = (v-wid)
      ttItemLookallone.slen = (v-len)
      ttItemLookallone.sdep = (v-dep).

      RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.swid), INPUT 32, OUTPUT v-wid-frac).
      RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.slen), INPUT 32, OUTPUT v-len-frac).
      RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.sdep), INPUT 32, OUTPUT v-dep-frac).
      ASSIGN
        ttItemLookallone.v-wid-frac = v-wid-frac
        ttItemLookallone.v-len-frac = v-len-frac
        ttItemLookallone.v-dep-frac = v-dep-frac.

     /* populate GL# from reftable if it exists using itemfg AH 02-23-10*/
     ASSIGN v-charge = "".
     FIND FIRST surcharge WHERE surcharge.company = prmComp
                            AND surcharge.charge <> "" NO-LOCK NO-ERROR.
     IF AVAIL surcharge THEN
     ASSIGN v-charge = surcharge.charge.
     FIND FIRST reftable WHERE reftable.reftable EQ "chargecode"
            AND reftable.company  EQ itemfg.company
            AND reftable.loc      EQ itemfg.procat
            AND reftable.code     EQ v-charge
          /* AND reftable.code2 = "" */
            NO-LOCK NO-ERROR.
     IF AVAIL reftable AND reftable.dscr <> "" THEN 
        ASSIGN ttItemLookallone.actnum = reftable.dscr.
     /* AH */
     ELSE 
     FOR EACH prodl
         WHERE prodl.company EQ cocode
           AND prodl.procat  EQ itemfg.procat
         NO-LOCK,
         FIRST prod
         WHERE prod.company EQ cocode
           AND prod.prolin  EQ prodl.prolin
         NO-LOCK:

       ttItemLookallone.actnum = prod.fg-mat.
       LEAVE.
     END.

     RELEASE reftable.

     find first account where account.company eq cocode and
                              account.actnum eq ttItemLookallone.actnum no-lock no-error.
     ttItemLookallone.actdesc = if avail account then account.dscr else ''.

      IF AVAIL e-itemfg AND AVAIL po-ord THEN
       FIND FIRST e-itemfg-vend OF e-itemfg NO-LOCK
         WHERE e-itemfg-vend.vend-no EQ po-ord.vend-no NO-ERROR.

     IF AVAIL e-itemfg-vend AND e-itemfg-vend.vend-item NE "" THEN ttItemLookallone.vend-i-no = e-itemfg-vend.vend-item.
     ELSE if itemfg.vend-no eq po-ord.vend-no THEN ttItemLookallone.vend-i-no = itemfg.vend-item.
     ELSE if itemfg.vend2-no eq po-ord.vend-no THEN ttItemLookallone.vend-i-no = itemfg.vend2-item.

   /*  RUN fg-qtys (ROWID(itemfg)).*/
  
  find first account where account.company eq cocode and
                           account.actnum eq ttItemLookallone.actnum no-lock no-error.
  ttItemLookallone.actdesc = if avail account then account.dscr else ''.                    
              


                
               
    END.	 /* FOR EACH item */  
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = est  */
    
  END.  /* IF prmAction = search then do: */
END.  /*prmtype fgitem*/



IF prmType = "RMItem" THEN do:
if prmAction <> "search" then do:
    FOR EACH item WHERE item.company = prmComp and 
        (item.industry = prmIndu or prmIndu = "") NO-LOCK:
        
        create ttItemLookallone.
            assign                                         
               ttItemLookallone.itemno   =  item.i-no  
               ttItemLookallone.iname    =  item.i-name                         
               ttItemLookallone.idscr    =  item.i-no  
               ttItemLookallone.consuom       =  item.cons-uom
               ttItemLookallone.prqtyuom      =  IF pouom-chr EQ "Purchase" THEN item.pur-uom
                                                                         ELSE item.cons-uom                      
               ttItemLookallone.conscost      =  (item.last-cost)
               ttItemLookallone.part-dscr1    =  item.i-dscr 
               ttItemLookallone.part-dscr2    =  item.est-dscr                         
               ttItemLookallone.item-type     =  "RM"
               ttItemLookallone.tlen          =  item.s-len  
               ttItemLookallone.twid          =  IF item.s-wid NE 0 THEN item.s-wid ELSE item.r-wid                          
               ttItemLookallone.tdep          =  item.s-dep
               ttItemLookallone.pruom         =  item.pur-uom  .

                 RUN rm-qtys . 

                IF item.tax-rcpt AND aptax-chr EQ "Item" THEN
                    ttItemLookallone.tax = "yes".

     
     FIND FIRST e-item-vend WHERE
       e-item-vend.company EQ cocode AND
       e-item-vend.i-no    EQ item.i-no AND
       e-item-vend.vend-no EQ po-ord.vend-no
       NO-LOCK NO-ERROR.

  IF AVAIL e-item-vend AND e-item-vend.vend-item NE "" THEN ttItemLookallone.vend-i-no = e-item-vend.vend-item.
  ELSE IF item.vend-no eq po-ord.vend-no THEN ttItemLookallone.vend-i-no = item.vend-item.
  ELSE if item.vend2-no eq po-ord.vend-no THEN ttItemLookallone.vend-i-no = item.vend2-item.


     ASSIGN
   v-basis-w = 0
   v-dep     = item.s-dep.   

  IF CAN-DO("B,P,1,2,3,4",item.mat-type) THEN DO:
    ASSIGN
     v-basis-w = item.basis-w
     v-len     = item.s-len
     v-wid     = IF item.s-wid NE 0 THEN item.s-wid ELSE item.r-wid.

   IF v-len EQ 0 AND item.mat-type EQ "P"       AND
      ttItemLookallone.prqtyuom EQ "ROLL" THEN v-len = ld-roll-len.
  END.

  ELSE
  IF CAN-DO("C,5,6,D",item.mat-type) THEN
    ASSIGN
     v-len = item.case-l
     v-wid = item.case-w
     v-dep = item.case-d
     {po/calc16.i v-dep}.
  ELSE
    ASSIGN
     v-len = 0
     v-wid = 0
     v-dep = 0.
  
  ASSIGN
   {po/calc16.i v-len}
   {po/calc16.i v-wid}
   {po/calc16.i v-dep}.

  find first e-item where e-item.company eq cocode 
                      and e-item.i-no eq item.i-no
                      no-lock no-error.

  if avail e-item then 
     assign 
      ttItemLookallone.consuom = if lookup(e-item.std-uom, "MSF,EA,M,MSH") > 0 then "EA"
                                      else e-item.std-uom
      ttItemLookallone.pruom   = IF v-pocost1 EQ "Vendor/MSH" AND
                                         po-ord.type NE "S"        AND
                                         e-item.std-uom EQ "TON"   AND
                                         v-basis-w NE 0            AND
                                         v-wid NE 0                THEN "MSH"
                                      ELSE e-item.std-uom .

  IF pouom-int EQ 1 AND item.mat-type EQ "P" THEN
     ASSIGN
        ttItemLookallone.consuom = "TON".
  
  ASSIGN ttItemLookallone.swid = (v-wid)
         ttItemLookallone.slen = (v-len)
         ttItemLookallone.sdep      = (v-dep).
  
  RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.swid), INPUT 32, OUTPUT v-wid-frac).
  RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.slen), INPUT 32, OUTPUT v-len-frac).
  RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.sdep), INPUT 32, OUTPUT v-dep-frac).
  ASSIGN
     ttItemLookallone.v-wid-frac = v-wid-frac
     ttItemLookallone.v-len-frac = v-len-frac
     ttItemLookallone.v-dep-frac = v-dep-frac.


  FIND FIRST costtype NO-LOCK
      WHERE costtype.company   EQ cocode
        AND costtype.loc       EQ po-ord.loc
        AND costtype.cost-type EQ item.cost-type
      NO-ERROR.
  IF AVAIL costtype THEN
    ttItemLookallone.actnum = costtype.inv-asset .
        /*IF v-default-gl-cha EQ "Asset"   THEN costtype.inv-asset
        ELSE
        IF v-default-gl-cha BEGINS "Exp"  AND
           (v-default-gl-cha EQ "Expense" OR costtype.cons-exp NE "")
                                         THEN costtype.cons-exp
        ELSE                                  ttItemLookallone.actnum.      */   
  find first account where account.company eq cocode and
                           account.actnum eq ttItemLookallone.actnum no-lock no-error.
  ttItemLookallone.actdesc = if avail account then account.dscr else ''.  
              


                
               
    END.	 /* FOR EACH item */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
     
    if prmField = "i-no"  then do:
        if prmCondition = "EQUAL" then do:
               
                FOR EACH item WHERE item.company = prmComp and 
        (item.industry = prmIndu or prmIndu = "") AND ITEM.i-no = prmText  NO-LOCK:
        
        create ttItemLookallone.
            assign                                         
               ttItemLookallone.itemno   =  item.i-no  
               ttItemLookallone.iname    =  item.i-name                         
               ttItemLookallone.idscr    =  item.i-no  
               ttItemLookallone.consuom       =  item.cons-uom
               ttItemLookallone.prqtyuom      =  IF pouom-chr EQ "Purchase" THEN item.pur-uom
                                                                         ELSE item.cons-uom                      
               ttItemLookallone.conscost      =  (item.last-cost)
               ttItemLookallone.part-dscr1    =  item.i-dscr 
               ttItemLookallone.part-dscr2    =  item.est-dscr                         
               ttItemLookallone.item-type     =  "RM"
               ttItemLookallone.tlen          =  item.s-len  
               ttItemLookallone.twid          =  IF item.s-wid NE 0 THEN item.s-wid ELSE item.r-wid                          
               ttItemLookallone.tdep          =  item.s-dep
               ttItemLookallone.pruom         =  item.pur-uom  .


                IF item.tax-rcpt AND aptax-chr EQ "Item" THEN
                    ttItemLookallone.tax = "yes".

     
     FIND FIRST e-item-vend WHERE
       e-item-vend.company EQ cocode AND
       e-item-vend.i-no    EQ item.i-no AND
       e-item-vend.vend-no EQ po-ord.vend-no
       NO-LOCK NO-ERROR.

    
  IF AVAIL e-item-vend AND e-item-vend.vend-item NE "" THEN ttItemLookallone.vend-i-no = e-item-vend.vend-item.
  ELSE IF item.vend-no eq po-ord.vend-no THEN ttItemLookallone.vend-i-no = item.vend-item.
  ELSE if item.vend2-no eq po-ord.vend-no THEN ttItemLookallone.vend-i-no = item.vend2-item.


     ASSIGN
   v-basis-w = 0
   v-dep     = item.s-dep.   

  IF CAN-DO("B,P,1,2,3,4",item.mat-type) THEN DO:
    ASSIGN
     v-basis-w = item.basis-w
     v-len     = item.s-len
     v-wid     = IF item.s-wid NE 0 THEN item.s-wid ELSE item.r-wid.

   IF v-len EQ 0 AND item.mat-type EQ "P"       AND
      ttItemLookallone.prqtyuom EQ "ROLL" THEN v-len = ld-roll-len.
  END.

  ELSE
  IF CAN-DO("C,5,6,D",item.mat-type) THEN
    ASSIGN
     v-len = item.case-l
     v-wid = item.case-w
     v-dep = item.case-d
     {po/calc16.i v-dep}.
  ELSE
    ASSIGN
     v-len = 0
     v-wid = 0
     v-dep = 0.
  
  ASSIGN
   {po/calc16.i v-len}
   {po/calc16.i v-wid}
   {po/calc16.i v-dep}.

  find first e-item where e-item.company eq cocode 
                      and e-item.i-no eq item.i-no
                      no-lock no-error.

  if avail e-item then 
     assign 
      ttItemLookallone.consuom = if lookup(e-item.std-uom, "MSF,EA,M,MSH") > 0 then "EA"
                                      else e-item.std-uom
      ttItemLookallone.pruom   = IF v-pocost1 EQ "Vendor/MSH" AND
                                         po-ord.type NE "S"        AND
                                         e-item.std-uom EQ "TON"   AND
                                         v-basis-w NE 0            AND
                                         v-wid NE 0                THEN "MSH"
                                      ELSE e-item.std-uom .

  IF pouom-int EQ 1 AND item.mat-type EQ "P" THEN
     ASSIGN
        ttItemLookallone.consuom = "TON".
  
  ASSIGN ttItemLookallone.swid = v-wid
         ttItemLookallone.slen = (v-len)
         ttItemLookallone.sdep      = (v-dep).

  
  RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.swid), INPUT 32, OUTPUT v-wid-frac).
  RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.slen), INPUT 32, OUTPUT v-len-frac).
  RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.sdep), INPUT 32, OUTPUT v-dep-frac).
  ASSIGN
     ttItemLookallone.v-wid-frac = v-wid-frac
     ttItemLookallone.v-len-frac = v-len-frac
     ttItemLookallone.v-dep-frac = v-dep-frac.
  
  RUN rm-qtys .

  FIND FIRST costtype NO-LOCK
      WHERE costtype.company   EQ cocode
        AND costtype.loc       EQ po-ord.loc
        AND costtype.cost-type EQ item.cost-type
      NO-ERROR.
   IF AVAIL costtype THEN
    ttItemLookallone.actnum = costtype.inv-asset .
        /*IF v-default-gl-cha EQ "Asset"   THEN costtype.inv-asset
        ELSE
        IF v-default-gl-cha BEGINS "Exp"  AND
           (v-default-gl-cha EQ "Expense" OR costtype.cons-exp NE "")
                                         THEN costtype.cons-exp
        ELSE                                  ttItemLookallone.actnum.      */   
  find first account where account.company eq cocode and
                           account.actnum eq ttItemLookallone.actnum no-lock no-error.
  ttItemLookallone.actdesc = if avail account then account.dscr else ''.               
              


FIND FIRST po-ordl WHERE 
       po-ordl.company eq cocode and 
           po-ordl.po-no EQ INT(prmpoNo)  and
           po-ordl.LINE EQ INT(prmpoLine) NO-LOCK NO-ERROR.
        
        FIND FIRST po-ord WHERE 
            po-ord.company eq cocode and 
            po-ord.po-no EQ INT(prmpoNo)  NO-LOCK NO-ERROR.

RUN vend-cost(TRUE).
  
               
    END.	 /* FOR EACH item */  
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
              FOR EACH item WHERE item.company = prmComp and 
        (item.industry = prmIndu or prmIndu = "") AND ITEM.i-no BEGINS prmText NO-LOCK:
        
        create ttItemLookallone.
            assign                                         
               ttItemLookallone.itemno   =  item.i-no  
               ttItemLookallone.iname    =  item.i-name                         
               ttItemLookallone.idscr    =  item.i-no  
               ttItemLookallone.consuom       =  item.cons-uom
               ttItemLookallone.prqtyuom      =  IF pouom-chr EQ "Purchase" THEN item.pur-uom
                                                                         ELSE item.cons-uom                      
               ttItemLookallone.conscost      =  (item.last-cost)
               ttItemLookallone.part-dscr1    =  item.i-dscr 
               ttItemLookallone.part-dscr2    =  item.est-dscr                         
               ttItemLookallone.item-type     =  "RM"
               ttItemLookallone.tlen          =  item.s-len  
               ttItemLookallone.twid          =  IF item.s-wid NE 0 THEN item.s-wid ELSE item.r-wid                          
               ttItemLookallone.tdep          =  item.s-dep
               ttItemLookallone.pruom         =  item.pur-uom  .


                IF item.tax-rcpt AND aptax-chr EQ "Item" THEN
                    ttItemLookallone.tax = "yes".

     
     FIND FIRST e-item-vend WHERE
       e-item-vend.company EQ cocode AND
       e-item-vend.i-no    EQ item.i-no AND
       e-item-vend.vend-no EQ po-ord.vend-no
       NO-LOCK NO-ERROR.

     
  IF AVAIL e-item-vend AND e-item-vend.vend-item NE "" THEN ttItemLookallone.vend-i-no = e-item-vend.vend-item.
  ELSE IF item.vend-no eq po-ord.vend-no THEN ttItemLookallone.vend-i-no = item.vend-item.
  ELSE if item.vend2-no eq po-ord.vend-no THEN ttItemLookallone.vend-i-no = item.vend2-item.


     ASSIGN
   v-basis-w = 0
   v-dep     = item.s-dep.   

  IF CAN-DO("B,P,1,2,3,4",item.mat-type) THEN DO:
    ASSIGN
     v-basis-w = item.basis-w
     v-len     = item.s-len
     v-wid     = IF item.s-wid NE 0 THEN item.s-wid ELSE item.r-wid.

   IF v-len EQ 0 AND item.mat-type EQ "P"       AND
      ttItemLookallone.prqtyuom EQ "ROLL" THEN v-len = ld-roll-len.
  END.

  ELSE
  IF CAN-DO("C,5,6,D",item.mat-type) THEN
    ASSIGN
     v-len = item.case-l
     v-wid = item.case-w
     v-dep = item.case-d
     {po/calc16.i v-dep}.
  ELSE
    ASSIGN
     v-len = 0
     v-wid = 0
     v-dep = 0.
  
  ASSIGN
   {po/calc16.i v-len}
   {po/calc16.i v-wid}
   {po/calc16.i v-dep}.

  find first e-item where e-item.company eq cocode 
                      and e-item.i-no eq item.i-no
                      no-lock no-error.

  if avail e-item then 
     assign 
      ttItemLookallone.consuom = if lookup(e-item.std-uom, "MSF,EA,M,MSH") > 0 then "EA"
                                      else e-item.std-uom
      ttItemLookallone.pruom   = IF v-pocost1 EQ "Vendor/MSH" AND
                                         po-ord.type NE "S"        AND
                                         e-item.std-uom EQ "TON"   AND
                                         v-basis-w NE 0            AND
                                         v-wid NE 0                THEN "MSH"
                                      ELSE e-item.std-uom .

  IF pouom-int EQ 1 AND item.mat-type EQ "P" THEN
     ASSIGN
        ttItemLookallone.consuom = "TON".
  
  ASSIGN ttItemLookallone.swid = (v-wid)
         ttItemLookallone.slen = (v-len)
         ttItemLookallone.sdep      = (v-dep).

  
  RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.swid), INPUT 32, OUTPUT v-wid-frac).
  RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.slen), INPUT 32, OUTPUT v-len-frac).
  RUN sys\inc\decfrac2.p(INPUT DEC(ttItemLookallone.sdep), INPUT 32, OUTPUT v-dep-frac).
  ASSIGN
     ttItemLookallone.v-wid-frac = v-wid-frac
     ttItemLookallone.v-len-frac = v-len-frac
     ttItemLookallone.v-dep-frac = v-dep-frac.
  
  RUN rm-qtys .

  FIND FIRST costtype NO-LOCK
      WHERE costtype.company   EQ cocode
        AND costtype.loc       EQ po-ord.loc
        AND costtype.cost-type EQ item.cost-type
      NO-ERROR.
   IF AVAIL costtype THEN
    ttItemLookallone.actnum = costtype.inv-asset .
        /*IF v-default-gl-cha EQ "Asset"   THEN costtype.inv-asset
        ELSE
        IF v-default-gl-cha BEGINS "Exp"  AND
           (v-default-gl-cha EQ "Expense" OR costtype.cons-exp NE "")
                                         THEN costtype.cons-exp
        ELSE                                  ttItemLookallone.actnum.      */   
  find first account where account.company eq cocode and
                           account.actnum eq ttItemLookallone.actnum no-lock no-error.
  ttItemLookallone.actdesc = if avail account then account.dscr else ''.  
                
               
    END.	 /* FOR EACH item */  
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = est  */
    
  END.  /* IF prmAction = search then do: */
END.  /*prmtype fgitem*/


FOR EACH ttItemLookallone NO-LOCK:
        IF INDEX(ttItemLookallone.idscr ,'"',1) > 0 THEN ASSIGN
            ttItemLookallone.idscr  = REPLACE(ttItemLookallone.idscr ,'"',":").
END.


PROCEDURE rm-qtys :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.*/

  DEF VAR li AS INT NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO.

 /* FIND item WHERE ROWID(item) EQ ip-rowid NO-LOCK NO-ERROR.*/

  IF AVAIL item THEN DO :
    ASSIGN
     ttItemLookallone.a-hdr     = "Committed"
     ttItemLookallone.fi-uom    = item.cons-uom
     ttItemLookallone.q-onh     = item.q-onh  
     ttItemLookallone.q-ono     = item.q-ono  
     ttItemLookallone.q-alloc   = item.q-comm 
     ttItemLookallone.q-back    = item.q-back 
     ttItemLookallone.q-avail   = item.q-avail
     fi_uom     = item.cons-uom
     fi_q-onh   = item.q-onh
     fi_q-ono   = item.q-ono
     fi_q-comm  = item.q-comm
     fi_q-back  = item.q-back
     fi_q-avail = item.q-avail.
    
    IF item.i-code EQ "R"   AND
       item.mat-type EQ "B" AND
       fi_uom NE "MSF"      THEN DO:
      DO li = 1 TO 5:
        ld = IF li EQ 1 THEN fi_q-onh  ELSE
             IF li EQ 2 THEN fi_q-ono  ELSE
             IF li EQ 3 THEN fi_q-comm ELSE
             IF li EQ 4 THEN fi_q-back ELSE fi_q-avail.

        RUN sys/ref/convquom.p(item.cons-uom, "MSF",
                               item.basis-w,
                               (IF item.r-wid NE 0 THEN 0          ELSE item.s-len),
                               (IF item.r-wid NE 0 THEN item.r-wid ELSE item.s-wid),
                               0,
                               ld, OUTPUT ld).

        CASE li:
          WHEN 1 THEN ttItemLookallone.fi_m-onh    = ld.
          WHEN 2 THEN ttItemLookallone.fi_m-ono    = ld.
          WHEN 3 THEN ttItemLookallone.fi_m-comm   = ld.
          WHEN 4 THEN ttItemLookallone.fi_m-back   = ld.
          WHEN 5 THEN ttItemLookallone.fi_m-avail  = ld.
        END CASE.
      END.

      
    END.

   
  END.

END PROCEDURE.

PROCEDURE vend-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  PARAMs:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-calc-cost AS LOG NO-UNDO.  

  DEF VAR v-qty  AS DEC NO-UNDO.
  DEF VAR v-cost AS DEC NO-UNDO.
  DEF VAR v-pb-qty AS DEC NO-UNDO.
  DEF VAR v-pb-stp AS DEC NO-UNDO.
  DEF VAR v-pb-cst AS DEC NO-UNDO.
  DEF VAR v-pb-cns AS DEC NO-UNDO.
  DEF VAR v-save-qty AS DEC NO-UNDO.
  DEF VAR v-setup AS DEC NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-added-cost AS DEC NO-UNDO.
  DEF VAR lv-added-cons-cost AS DEC NO-UNDO.
  DEF VAR lv-adder-setup AS DEC NO-UNDO.
  DEF VAR lv-recid AS RECID NO-UNDO.
  DEF VAR lv-t-cost AS DEC NO-UNDO.
  DEF VAR ld-dim-charge AS DEC NO-UNDO.
  DEF VAR v-index AS INT NO-UNDO.

  EMPTY TEMP-TABLE tt-ei.
  EMPTY TEMP-TABLE tt-eiv.

  
    RUN set-dims.

    /* for adders */
    RELEASE job-mat.
    FIND FIRST job NO-LOCK
        WHERE job.company EQ cocode
          AND job.job-no  EQ prmJob
          AND job.job-no2 EQ INT(prmJob2)
        NO-ERROR.
    IF AVAIL job THEN
    FIND FIRST job-mat NO-LOCK
        WHERE job-mat.company  EQ job.company
          AND job-mat.job      EQ job.job
          AND job-mat.job-no   EQ job.job-no
          AND job-mat.job-no2  EQ job.job-no2
          AND job-mat.frm      EQ INT(0)
          AND job-mat.blank-no EQ INT(0) 
        USE-INDEX seq-idx NO-ERROR.
        
    IF AVAIL job-mat THEN lv-recid = RECID(job-mat).

    v-ord-qty = DEC(1).

    IF ttItemLookallone.item-type EQ "RM" THEN DO:
       FIND FIRST e-item NO-LOCK
           WHERE e-item.company EQ cocode
             AND e-item.i-no    EQ prmtext
           NO-ERROR.
      
       IF AVAIL e-item THEN DO:
          CREATE tt-ei.
          ASSIGN tt-ei.std-uom = e-item.std-uom.
      
          FIND FIRST e-item-vend NO-LOCK
              WHERE e-item-vend.company EQ e-item.company
                AND e-item-vend.i-no    EQ e-item.i-no
                AND e-item-vend.vend-no EQ po-ord.vend-no
              NO-ERROR.
      
          IF AVAIL e-item-vend THEN DO:

             CREATE tt-eiv.
             tt-eiv.rec_key = e-item-vend.rec_key.
             DO v-index = 1 TO 10:
                ASSIGN
                   tt-eiv.run-qty[v-index] = e-item-vend.run-qty[v-index]
                   tt-eiv.run-cost[v-index] = e-item-vend.run-cost[v-index]
                   tt-eiv.setups[v-index] = e-item-vend.setups[v-index].
             END.

             FIND FIRST b-qty WHERE
                  b-qty.reftable = "vend-qty" AND
                  b-qty.company = e-item-vend.company AND
                          b-qty.CODE    = e-item-vend.i-no AND
                  b-qty.code2   = e-item-vend.vend-no
                  NO-LOCK NO-ERROR.
         
             IF AVAIL b-qty THEN
             DO:
                FIND FIRST b-cost WHERE
                     b-cost.reftable = "vend-cost" AND
                     b-cost.company = e-item-vend.company AND
                             b-cost.CODE    = e-item-vend.i-no AND
                     b-cost.code2   = e-item-vend.vend-no
                     NO-LOCK NO-ERROR.

                FIND FIRST b-setup WHERE
                     b-setup.reftable = "vend-setup" AND
                     b-setup.company = e-item-vend.company AND
                             b-setup.CODE    = e-item-vend.i-no AND
                     b-setup.code2   = e-item-vend.vend-no
                     NO-LOCK NO-ERROR.
             
                DO v-index = 1 TO 10:
                   ASSIGN
                      tt-eiv.run-qty[v-index + 10] = b-qty.val[v-index]
                      tt-eiv.run-cost[v-index + 10] = b-cost.val[v-index]
                      tt-eiv.setups[v-index + 10] = b-setup.val[v-index].
                END.
             END.
          END.
       END.
    END.

    ELSE DO:
      FIND FIRST e-itemfg NO-LOCK
          WHERE e-itemfg.company EQ cocode
            AND e-itemfg.i-no    EQ prmtext
          NO-ERROR.

      IF AVAIL e-itemfg THEN DO:
        CREATE tt-ei.
        ASSIGN tt-ei.std-uom = e-itemfg.std-uom.

        IF prmcust NE "" THEN
           FIND FIRST e-itemfg-vend NO-LOCK
               WHERE e-itemfg-vend.company EQ e-itemfg.company
                 AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                 AND e-itemfg-vend.vend-no EQ po-ord.vend-no
                 AND e-itemfg-vend.cust-no EQ ""
               NO-ERROR.

        /* gdm - 06040918 - check for vendor */
        IF NOT AVAIL e-itemfg-vend THEN
           FIND FIRST e-itemfg-vend NO-LOCK
               WHERE e-itemfg-vend.company EQ e-itemfg.company
                 AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                 AND e-itemfg-vend.vend-no EQ po-ord.vend-no
               NO-ERROR.

        /* gdm - check for blank vendor */
        IF NOT AVAIL e-itemfg-vend THEN
           FIND FIRST e-itemfg-vend NO-LOCK
               WHERE e-itemfg-vend.company EQ e-itemfg.company
                 AND e-itemfg-vend.i-no    EQ e-itemfg.i-no 
                 AND e-itemfg-vend.vend-no EQ "" NO-ERROR.

        IF AVAIL e-itemfg-vend THEN DO:            
          CREATE tt-eiv.
          tt-eiv.rec_key = e-itemfg-vend.rec_key.
          DO v-index = 1 TO 10:
             ASSIGN
                tt-eiv.run-qty[v-index] = e-itemfg-vend.run-qty[v-index]
                tt-eiv.run-cost[v-index] = e-itemfg-vend.run-cost[v-index]
                tt-eiv.setups[v-index] = e-itemfg-vend.setups[v-index].
          END.
          RELEASE e-itemfg-vend.
        END.
      END.
    END.

    IF AVAIL tt-eiv THEN DO:                
      ASSIGN
       v-cost = DEC(ttItemLookallone.cost)
       v-qty  = DEC(1)
       .
    
      IF tt-ei.std-uom NE ttItemLookallone.prqtyuom          AND
        (ttItemLookallone.item-type EQ "RM"                                       OR
         LOOKUP(tt-ei.std-uom,fg-uom-list)                  EQ 0 OR
         LOOKUP(ttItemLookallone.prqtyuom,fg-uom-list) EQ 0)  THEN
        RUN sys/ref/convquom.p(ttItemLookallone.prqtyuom,
                               tt-ei.std-uom, v-basis-w,
                               v-len, v-wid, v-dep,
                               v-qty, OUTPUT v-qty).
     
      IF v-qty EQ ? THEN
          ASSIGN v-qty = 1.
      

      v-save-qty = v-qty.
      IF prmjob NE "" THEN
        RUN po/groupcst.p (prmjob,
                           INT(prmjob2),
                           prmtext,
                           INT(0),
                           INT(0),
                           INPUT-OUTPUT v-qty).
      IF v-qty EQ ? THEN
          ASSIGN v-qty = 1.

      ASSIGN
       v-save-qty = v-qty - v-save-qty
       v-setup    = 0
       v-pb-qty   = 0.
            
      RUN est/dim-charge.p (tt-eiv.rec_key,
                            v-wid,
                            v-len,
                            INPUT-OUTPUT ld-dim-charge).
     
      DO li = 1 TO EXTENT(tt-eiv.run-qty):
        IF tt-eiv.run-qty[li] LT v-qty THEN NEXT.
        ASSIGN
         v-cost   = (tt-eiv.run-cost[li] + ld-dim-charge) * v-qty
         v-setup  = tt-eiv.setups[li]
         v-pb-qty = tt-eiv.run-qty[li] - v-save-qty.
        IF li LT EXTENT(tt-eiv.run-qty) THEN
          ASSIGN
           v-pb-cst = tt-eiv.run-cost[li + 1] + ld-dim-charge
           v-pb-stp = tt-eiv.setups[li + 1].
        LEAVE.
      END.

      IF poqty-log THEN DO:
        IF v-pb-qty GE 9999999 THEN v-pb-qty = 0.

        IF v-pb-qty EQ 0 THEN v-pb-cst = 0.
        ELSE DO:
          v-pb-qty = v-pb-qty + .001.

          v-pb-cst = v-pb-cst * v-pb-qty.

          IF v-pb-qty NE 0 THEN v-pb-cst = (v-pb-cst /*+ v-pb-stp*/) / v-pb-qty.  
          ELSE v-pb-cst = (v-pb-cst /*+ v-pb-stp*/).
        END.

        IF tt-ei.std-uom NE ttItemLookallone.prqtyuom           AND
           (ttItemLookallone.item-type EQ "RM"                                        OR
            LOOKUP(tt-ei.std-uom,fg-uom-list)                  EQ 0 OR
            LOOKUP(ttItemLookallone.prqtyuom,fg-uom-list) EQ 0)  THEN
          RUN sys/ref/convquom.p(tt-ei.std-uom,
                                 ttItemLookallone.prqtyuom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-pb-qty, OUTPUT v-pb-qty).
        

        IF tt-ei.std-uom NE ttItemLookallone.pruom           AND
           (ttItemLookallone.item-type EQ "RM"                                   OR
            LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
            LOOKUP(ttItemLookallone.pruom,fg-uom-list) EQ 0)  THEN
          RUN sys/ref/convcuom.p(tt-ei.std-uom,
                                 ttItemLookallone.pruom, v-basis-w,
                                 v-len, v-wid, v-dep,
                                 v-pb-cst, OUTPUT v-pb-cst).

        IF ttItemLookallone.pruom NE ttItemLookallone.consuom AND
           (ttItemLookallone.item-type EQ "RM"                                      OR
            LOOKUP(ttItemLookallone.pruom,fg-uom-list)   EQ 0 OR
            LOOKUP(ttItemLookallone.consuom,fg-uom-list) EQ 0)     THEN
          RUN sys/ref/convcuom.p(ttItemLookallone.pruom,
                                 ttItemLookallone.consuom, v-basis-w,
                                 v-len, v-wid, v-dep,
                                 v-pb-cst, OUTPUT v-pb-cns).

        ttItemLookallone.poNxtPgBQty = IF ROUND(v-pb-qty,2) LE 0 THEN 0 ELSE ROUND(v-pb-qty,2).

      END.

      IF v-qty <> 0 THEN v-cost = (v-cost /*+ v-setup*/) / v-qty.  
      ELSE v-cost = (v-cost /*+ v-setup*/).

      IF ip-calc-cost NE ? THEN DO:
        IF ip-calc-cost THEN DO:            
          IF tt-ei.std-uom NE ttItemLookallone.pruom           AND
             (ttItemLookallone.item-type EQ "RM"                                   OR
              LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
              LOOKUP(ttItemLookallone.pruom,fg-uom-list) EQ 0)  THEN
            RUN sys/ref/convcuom.p(tt-ei.std-uom,
                                   ttItemLookallone.pruom, v-basis-w,
                                   (IF ttItemLookallone.prqtyuom EQ "ROLL" THEN 12 ELSE v-len),
                                   v-wid, v-dep,
                                   v-cost, OUTPUT v-cost).

          ASSIGN
            ip-calc-cost = YES
            ttItemLookallone.cost = v-cost
            ttItemLookallone.posetup = v-setup .
          

          IF ttItemLookallone.pruom NE ttItemLookallone.consuom AND
             (ttItemLookallone.item-type EQ "RM"                                     OR
              LOOKUP(ttItemLookallone.pruom,fg-uom-list)   EQ 0 OR
              LOOKUP(ttItemLookallone.consuom,fg-uom-list) EQ 0)     THEN
            RUN sys/ref/convcuom.p(ttItemLookallone.pruom,
                                   ttItemLookallone.consuom, v-basis-w,
                                   (IF ttItemLookallone.prqtyuom EQ "ROLL" THEN 12 ELSE v-len),
                                   v-wid, v-dep,
                                   v-cost, OUTPUT v-cost).

          ttItemLookallone.conscost = v-cost.     
          
        END.

        ELSE
        IF v-hold-op1 AND po-ord.stat NE "H" THEN DO:
          IF tt-ei.std-uom NE ttItemLookallone.pruom           AND
             (ttItemLookallone.item-type EQ "RM"                                   OR
              LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
              LOOKUP(ttItemLookallone.pruom,fg-uom-list) EQ 0)  THEN
            RUN sys/ref/convcuom.p(tt-ei.std-uom,
                                   ttItemLookallone.pruom, v-basis-w,
                                   v-len, v-wid, v-dep,
                                   v-cost, OUTPUT v-cost).              
          IF AVAIL job-mat THEN
            RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                           DEC(1),
                           v-cost,
                           ttItemLookallone.conscost,
                           OUTPUT v-cost,
                           OUTPUT lv-added-cons-cost,
                           OUTPUT lv-adder-setup).

          IF DEC(ttItemLookallone.cost) GT v-cost THEN DO:
            FIND CURRENT po-ord.
            po-ord.stat = "H".
            FIND CURRENT po-ord NO-LOCK.
          END.          
        END.
      END.
    END.

    IF AVAIL job-mat THEN DO:

      IF poqty-log THEN
        RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                       DEC( ttItemLookallone.poNxtPgBQty),
                       v-pb-cst,
                       v-pb-cns,
                       OUTPUT v-pb-cst,
                       OUTPUT v-pb-cns,
                       OUTPUT lv-adder-setup).

      RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                     DEC(1),
                     DEC(ttItemLookallone.cost),
                     ttItemLookallone.conscost,
                     OUTPUT lv-added-cost,
                     OUTPUT lv-added-cons-cost,
                     OUTPUT lv-adder-setup).
    
      IF ip-calc-cost THEN
        ASSIGN
         ttItemLookallone.cost = lv-added-cost
         ttItemLookallone.conscost = lv-added-cons-cost.
      
    END.

    IF poqty-log THEN DO:
      IF CAN-DO("L,LOT",ttItemLookallone.pruom) THEN
        lv-t-cost = (v-pb-cst + v-pb-stp) *
                    IF po-ordl.ord-qty LT 0 THEN -1 ELSE 1.

      ELSE DO:
        v-ord-qty = DEC(ttItemLookallone.poNxtPgBQty).

        IF ttItemLookallone.prqtyuom NE ttItemLookallone.pruom AND
           (po-ordl.item-type                                        OR
            LOOKUP(ttItemLookallone.prqtyuom,fg-uom-list) EQ 0 OR
            LOOKUP(ttItemLookallone.pruom,fg-uom-list)     EQ 0)     THEN
   
          RUN sys/ref/convquom.p(ttItemLookallone.prqtyuom,
                                 ttItemLookallone.pruom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-ord-qty, OUTPUT v-ord-qty).
     
        lv-t-cost = (v-ord-qty * v-pb-cst) + v-pb-stp.
      END.

      IF DEC(prmdiscount) NE 0 THEN
        lv-t-cost = lv-t-cost * (1 - (DEC(prmdiscount) / 100)).

      ttItemLookallone.pototcost = lv-t-cost.
      /*ttItemLookallone.pototcost = lv-t-cost.*/

      IF ttItemLookallone.pototcost LE 0 THEN ttItemLookallone.pototcost = 0.
    END.

    IF ip-calc-cost NE ? THEN DO:
      IF CAN-DO("L,LOT",ttItemLookallone.pruom) THEN
        lv-t-cost = (DEC(ttItemLookallone.cost) +
                     DEC(ttItemLookallone.posetup)) *
                    IF po-ordl.ord-qty LT 0 THEN -1 ELSE 1.

      ELSE DO:
        v-ord-qty = DEC(1).

        IF ttItemLookallone.prqtyuom NE ttItemLookallone.pruom AND
           (po-ordl.item-type                                        OR
            LOOKUP(ttItemLookallone.prqtyuom,fg-uom-list) EQ 0 OR
            LOOKUP(ttItemLookallone.pruom,fg-uom-list)     EQ 0)     THEN
   
          RUN sys/ref/convquom.p(ttItemLookallone.prqtyuom,
                                 ttItemLookallone.pruom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-ord-qty, OUTPUT v-ord-qty).
     
        lv-t-cost = (v-ord-qty * DEC(ttItemLookallone.cost)) +
                    DEC(ttItemLookallone.posetup).
      END.

      IF DEC(prmdiscount) NE 0 THEN
         lv-t-cost = lv-t-cost * (1 - (DEC(prmdiscount) / 100)).

     /* ttItemLookallone.pototcost = lv-t-cost.*/
    END.
  /*END.  */

END PROCEDURE.

PROCEDURE set-dims :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  
    ASSIGN
     v-len = DEC(ttItemLookallone.slen)
     v-wid = DEC(ttItemLookallone.swid).
     
      {po/calc10.i v-len}          .
     {po/calc10.i v-wid}.

    FIND FIRST ITEM
        WHERE item.company EQ cocode
          AND item.i-no    EQ prmtext
        NO-LOCK NO-ERROR.

    ASSIGN
      v-basis-w = IF AVAIL ITEM THEN item.basis-w ELSE 0
      v-dep     = IF AVAIL ITEM THEN item.s-dep ELSE 0.


END PROCEDURE.




