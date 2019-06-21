




/*------------------------------------------------------------------------
    File        : PopriceLook2.p
    Purpose     : 
    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttPoPriceItemLook2 NO-UNDO 
    FIELD pono         AS INTEGER
    FIELD slct         AS CHAR
    FIELD ino          AS CHAR
    FIELD iname        AS CHAR
    FIELD swid         AS DEC
    FIELD slen         AS DEC
    FIELD invqty       AS DEC
    FIELD recqty       AS DEC
    FIELD qtyinvuom    AS CHAR
    FIELD qtyrecuom    AS CHAR
    FIELD recdate      AS DATE
    FIELD LINE         AS INT
    FIELD job          AS CHARACTER
    FIELD job2         AS INT
    FIELD snum         AS INT
    FIELD podate       AS DATE
    FIELD pruom        AS CHAR
    FIELD cost         AS DEC
    FIELD tcost        AS DEC
    FIELD act          AS CHAR      
    FIELD actname      AS CHAR
    FIELD amt          AS DEC
    FIELD tamt         AS DEC
    FIELD rd_qty       AS INT
    FIELD tax          AS CHAR
    FIELD consuom      AS CHAR
    FIELD prqtyuom     AS CHAR
    FIELD price        AS DEC
    FIELD sqft         AS DEC
    FIELD ext          AS CHAR .

    
                                           
    
DEFINE DATASET dsPoPriceItemLook2 FOR ttPoPriceItemLook2 .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPono      AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmslect     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmrdqty     AS INT        NO-UNDO.
DEFINE OUTPUT PARAMETER cError      AS CHAR       NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPoPriceItemLook2.
       
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-ap-pur AS CHAR NO-UNDO.
DEF VAR v-wid AS DEC NO-UNDO.
DEF VAR v-len AS DEC NO-UNDO.
DEF VAR v-dep AS DEC NO-UNDO.
DEF VAR v-bwt AS DEC NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmPono      = ? THEN ASSIGN prmPono      = 0.
IF prmslect     = ? THEN ASSIGN prmslect     = "".
IF prmrdqty     = ? THEN ASSIGN prmrdqty     = 0.

{sys/inc/VAR.i "new shared"}
    {sys/inc/fgpostgl.i}
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN cocode = prmComp.

DEF VAR v-rmpostgl-char AS cha NO-UNDO.
DEF VAR lv-i AS INT NO-UNDO.
DEF VAR lv-po-glnum AS LOG NO-UNDO.
DEF VAR v-vend-actnum AS cha NO-UNDO.

DEF NEW SHARED TEMP-TABLE tt-pol FIELD selekt AS LOG LABEL "Selected"
                      FIELD rec-id AS RECID                      
                      FIELD qty-inv AS log
                      FIELD amt-inv AS LOG
                      FIELD qty-to-inv LIKE ap-invl.qty
                      FIELD qty-to-inv-uom AS CHAR.

DEF TEMP-TABLE tt-rec NO-UNDO
    FIELD selekt AS LOG FORMAT "yes/no" LABEL "Selected"
    FIELD rec-id AS RECID
    FIELD po-date LIKE fg-rcpth.trans-date LABEL "P.O. Date"
    FIELD rcpt-date LIKE fg-rcpth.trans-date LABEL "Receipt Date"
    FIELD r-no AS INT
    FIELD qty-rec AS DEC FORM "->>>,>>>,>>9.9<<"  LABEL "Qty Received"
    FIELD qty-rec-uom AS CHAR
    FIELD qty-inv AS DEC FORM "->>>,>>>,>>9.9<<"  LABEL "Qty To Invoice"
    FIELD qty-inv-uom AS CHAR
    FIELD s-len LIKE po-ordl.s-len
    FIELD row-id AS ROWID.

DEF TEMP-TABLE tt-ap-invl NO-UNDO LIKE ap-invl
    FIELD tt-rowid AS ROWID.


DEF VAR lv-num-rec AS INT NO-UNDO.

{sys/inc/appaper.i}

RUN build-table. 


if prmAction <> "search" then do:
    
    FOR EACH  tt-rec  NO-LOCK,
        EACH po-ordl WHERE rowid(po-ordl) EQ (tt-rec.row-id):
       

     /*  FIND FIRST po-ord NO-LOCK
          WHERE po-ord.company EQ po-ordl.company 
            AND po-ord.po-no   EQ po-ordl.po-no
          NO-ERROR.*/

       create ttPoPriceItemLook2.
                 assign                                     
                    ttPoPriceItemLook2.slct        = string(tt-rec.selekt)
                    ttPoPriceItemLook2.ino         = po-ordl.i-no
                    ttPoPriceItemLook2.iname       = po-ordl.i-name
                    ttPoPriceItemLook2.swid        = po-ordl.s-wid
                    ttPoPriceItemLook2.slen        = tt-rec.s-len
                    ttPoPriceItemLook2.invqty      = tt-rec.qty-inv
                    ttPoPriceItemLook2.recqty      = tt-rec.qty-rec
                    ttPoPriceItemLook2.qtyinvuom   = tt-rec.qty-inv-uom
                    ttPoPriceItemLook2.qtyrecuom   = tt-rec.qty-rec-uom
                    ttPoPriceItemLook2.recdate     = tt-rec.rcpt-date
                    ttPoPriceItemLook2.LINE        = po-ordl.line
                    ttPoPriceItemLook2.job         = po-ordl.job-no   
                    ttPoPriceItemLook2.job2        = po-ordl.job-no2  
                    ttPoPriceItemLook2.snum        = po-ordl.s-num
                    ttPoPriceItemLook2.podate      = tt-rec.po-date
                    ttPoPriceItemLook2.pruom       = po-ordl.pr-uom
                    ttPoPriceItemLook2.cost        = po-ordl.cost
                    ttPoPriceItemLook2.tcost       = po-ordl.t-cost .
         
                RUN create-ap-from-po .
  
 END.  /*FOR EACH tt-pol*/

  /*IF CAN-FIND(FIRST tt-pol
                WHERE tt-pol.selekt
                AND tt-pol.qty-to-inv NE 0) THEN
        RUN create-ap-from-po.
    ELSE do:
        cError = "Nothing selected for this PO".
        RETURN.
    END.*/
END.  /*ifif prmAction <> "search" */

IF prmAction = "Update"  THEN DO:
    

DEF VAR jk AS INT NO-UNDO.
FIND FIRST ap-inv WHERE  ap-inv.company = "001" 
      AND ap-inv.rec_key = prmText   NO-LOCK NO-ERROR.

 FOR EACH tt-rec,
      FIRST po-ordl WHERE ROWID(po-ordl) EQ tt-rec.row-id NO-LOCK:
          jk = 1 .
          

          IF STRING(entry(1,prmField)) = "Yes" THEN
            tt-rec.selekt = YES .
          ELSE
              tt-rec.selekt = NO.
          jk = jk + 1 .

          
    IF po-ordl.item-type THEN
    /*FOR EACH tt-rec WHERE tt-rec.row-id EQ ROWID(tt-pol):*/
      
      FOR EACH rm-rcpth WHERE RECID(rm-rcpth) EQ tt-rec.rec-id NO-LOCK,
          EACH rm-rdtlh
          WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
            AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code:
          
        IF tt-rec.selekt THEN
          ASSIGN
           rm-rdtlh.receiver-no = (STRING(ap-inv.i-no,"9999999999") +
                                   STRING(tt-rec.qty-inv,"-9999999999.99999"))
           /*tt-pol.qty-to-inv    = tt-pol.qty-to-inv + tt-rec.qty-inv
           tt-pol.qty-to-inv-uom = tt-rec.qty-inv-uom
           tt-pol.selekt        = YES */  .

        ELSE rm-rdtlh.receiver-no = "".
      END.
    

    ELSE
    /*FOR EACH tt-rec WHERE tt-rec.row-id EQ ROWID(tt-pol),*/
       FOR EACH fg-rcpth WHERE RECID(fg-rcpth) EQ tt-rec.rec-id NO-LOCK,
        EACH fg-rdtlh
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
          
      IF tt-rec.selekt THEN
        ASSIGN
         fg-rdtlh.receiver-no = (STRING(ap-inv.i-no,"9999999999") +
                                 STRING(tt-rec.qty-inv,"-9999999999.99999"))
        /* tt-pol.qty-to-inv    = tt-pol.qty-to-inv + tt-rec.qty-inv
         tt-pol.selekt        = YES*/  .

      ELSE fg-rdtlh.receiver-no = "".
    END.
END.

END.
    

PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ld AS DEC EXTENT 2 NO-UNDO.
DEF VAR v-wid AS DEC NO-UNDO.
DEF VAR v-len AS DEC NO-UNDO.
DEF VAR v-dep AS DEC NO-UNDO.
DEF VAR v-bwt AS DEC NO-UNDO.
DEF VAR v-qty AS DEC NO-UNDO.
DEF VAR v-rec-qty AS DEC NO-UNDO.
DEF VAR lv-uom LIKE po-ordl.pr-qty-uom NO-UNDO.
DEF VAR v-pur-qty AS DEC NO-UNDO.

DEF BUFFER b-ap-invl FOR ap-invl.

lv-num-rec = 0.

EMPTY TEMP-TABLE tt-rec.

FIND FIRST ap-inv WHERE  ap-inv.company = "001" 
      AND ap-inv.rec_key = prmText   NO-LOCK NO-ERROR.

/*FOR EACH tt-pol,*/ 
   FOR EACH po-ordl WHERE po-ordl.po-no EQ prmPono NO-LOCK,
    FIRST po-ord WHERE
          po-ord.company EQ po-ordl.company AND
          po-ord.po-no EQ po-ordl.po-no NO-LOCK:

 

  IF po-ordl.item-type THEN DO:
    ASSIGN
     v-bwt = 0
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     v-dep = 0.

    FIND FIRST item
        WHERE item.company EQ cocode
          AND item.i-no    EQ po-ordl.i-no
        NO-LOCK NO-ERROR.

    IF AVAIL item THEN DO:
      v-dep = item.s-dep.          
      {po/pol-dims.i}
    END.

    IF AVAIL item           AND
       item.i-code EQ "R"   AND
       INDEX("MOXY789",ITEM.mat-type) GT 0 AND
       item.stocked EQ NO   THEN DO:       

       v-qty = po-ordl.cons-qty.

       IF po-ordl.cons-uom NE po-ordl.pr-qty-uom THEN
          RUN sys/ref/convquom.p (po-ordl.cons-uom, po-ordl.pr-qty-uom,
                                  v-bwt, v-len, v-wid, v-dep,
                                  v-qty, OUTPUT v-qty).

       v-qty = v-qty - po-ordl.t-inv-qty.

       IF v-qty GT 0 THEN DO:
          CREATE tt-rec.
          ASSIGN
          tt-rec.rec-id     = ?
          tt-rec.selekt     = NO
          tt-rec.po-date = po-ord.po-date
          tt-rec.r-no       = 0
          tt-rec.qty-rec    = IF prmrdqty EQ 1 THEN v-qty
                              ELSE po-ordl.t-rec-qty
          tt-rec.qty-rec-uom = IF prmrdqty EQ 1 THEN po-ordl.pr-qty-uom
                               ELSE ITEM.cons-uom
          tt-rec.qty-inv    = IF prmrdqty EQ 1 THEN v-qty
                              ELSE po-ordl.t-rec-qty
          tt-rec.qty-inv-uom = po-ordl.pr-qty-uom
          tt-rec.row-id     = ROWID(po-ordl)  .

          FIND FIRST rm-rcpth WHERE
              rm-rcpth.company   EQ cocode AND
              rm-rcpth.i-no      EQ po-ordl.i-no AND
              rm-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>")) AND
              rm-rcpth.job-no    EQ po-ordl.job-no AND
              rm-rcpth.job-no2   EQ po-ordl.job-no2 AND
              rm-rcpth.rita-code EQ "R"
              NO-LOCK NO-ERROR.
      
          IF AVAIL rm-rcpth THEN
             tt-rec.rcpt-date = rm-rcpth.trans-date.
       END.
    END.  
    ELSE
    FOR EACH rm-rcpth
        WHERE rm-rcpth.company   EQ cocode
          AND rm-rcpth.i-no      EQ po-ordl.i-no
          AND rm-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
          AND rm-rcpth.job-no    EQ po-ordl.job-no
          AND rm-rcpth.job-no2   EQ po-ordl.job-no2
          AND rm-rcpth.rita-code EQ "R"
        USE-INDEX item-po NO-LOCK,

        EACH rm-rdtlh
        WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
          AND (rm-rdtlh.s-num    EQ po-ordl.s-num OR po-ordl.s-num EQ 0)
          AND NOT CAN-FIND(FIRST b-ap-invl WHERE b-ap-invl.i-no  EQ INT(SUBSTR(rm-rdtlh.receiver-no,1,10))
                                             AND b-ap-invl.line  EQ (po-ordl.po-no * 1000) + po-ordl.line)
        NO-LOCK:        

     lv-uom = po-ordl.pr-qty-uom.

     IF lv-uom EQ "ROLL" THEN
       ASSIGN
        v-len  = 12
        lv-uom = "LF".

     IF po-ordl.item-type AND appaper-chr NE "PO UOM" AND
        CAN-FIND(FIRST item
                 WHERE item.company EQ cocode
                   AND item.i-no    EQ po-ordl.i-no
                   AND item.mat-type EQ "P")          THEN
       lv-uom = appaper-chr.

     ASSIGN
        v-rec-qty = rm-rdtlh.qty
        v-qty = rm-rdtlh.qty.

      IF prmrdqty EQ 1 THEN
      DO:
         v-pur-qty = po-ordl.cons-qty.
      
         IF po-ordl.cons-uom NE lv-uom THEN
            RUN sys/ref/convquom.p (po-ordl.cons-uom, lv-uom,
                                    v-bwt, v-len, v-wid, v-dep,
                                    v-pur-qty, OUTPUT v-pur-qty).
      END.

      IF rm-rcpth.pur-uom NE lv-uom THEN
         RUN sys/ref/convquom.p (rm-rcpth.pur-uom, lv-uom,
                                 v-bwt, v-len, v-wid, v-dep,
                                 v-qty, OUTPUT v-qty).

      /* gdm - 05200908 end */      
      CREATE tt-rec.
      ASSIGN
       tt-rec.rec-id     = RECID(rm-rcpth)
       tt-rec.selekt     = SUBSTR(rm-rdtlh.receiver-no,1,10) EQ
                           STRING(ap-inv.i-no,"9999999999")
       tt-rec.po-date   = po-ord.po-date
       tt-rec.rcpt-date = rm-rcpth.trans-date
       tt-rec.r-no       = rm-rcpth.r-no
       /* gdm - 05200908  */ 
       tt-rec.qty-rec    = IF prmrdqty EQ 1 THEN v-pur-qty
                                          ELSE v-rec-qty
       tt-rec.qty-rec-uom = IF prmrdqty EQ 1 THEN lv-uom
                            ELSE rm-rcpth.pur-uom
       tt-rec.qty-inv    =  IF tt-rec.selekt THEN
                               DEC(SUBSTR(rm-rdtlh.receiver-no,11,17))
                             ELSE 
                                v-qty
       tt-rec.qty-inv-uom = lv-uom
       /* gdm - 05200908 end */ 
       tt-rec.s-len      = v-len
       tt-rec.row-id     = ROWID(po-ordl)    .
    END.
  END.

  ELSE
  FOR EACH fg-rcpth
      WHERE fg-rcpth.company   EQ cocode
        AND fg-rcpth.i-no      EQ po-ordl.i-no
        AND fg-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
        AND fg-rcpth.rita-code EQ "R"
      USE-INDEX item-po NO-LOCK,

      EACH fg-rdtlh
      WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        AND NOT CAN-FIND(FIRST b-ap-invl WHERE b-ap-invl.i-no  EQ INT(SUBSTR(fg-rdtlh.receiver-no,1,10))
                                           AND b-ap-invl.line  EQ (po-ordl.po-no * 1000) + po-ordl.line)
      NO-LOCK:
 
    CREATE tt-rec.
    ASSIGN
     tt-rec.rec-id     = RECID(fg-rcpth)
     tt-rec.selekt     = SUBSTR(fg-rdtlh.receiver-no,1,10) EQ
                         STRING(ap-inv.i-no,"9999999999")
     tt-rec.po-date   = po-ord.po-date
     tt-rec.rcpt-date = fg-rcpth.trans-date
     tt-rec.r-no       = fg-rcpth.r-no
     tt-rec.qty-rec    = fg-rdtlh.qty
     tt-rec.qty-inv    = IF tt-rec.selekt THEN DEC(SUBSTR(fg-rdtlh.receiver-no,11,17))
                                          ELSE fg-rdtlh.qty
     tt-rec.s-len      = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
     tt-rec.row-id     = ROWID(po-ordl).  

  END.

  FOR EACH tt-rec WHERE tt-rec.rec-id EQ ?:
    lv-num-rec = lv-num-rec + 1.
  END.
  
  FOR EACH tt-rec WHERE tt-rec.rec-id EQ ?:
    lv-num-rec = lv-num-rec + 1.
  END.

  FOR EACH tt-rec WHERE tt-rec.rec-id NE ? BREAK BY tt-rec.rec-id:
    ASSIGN
     ld[1] = ld[1] + tt-rec.qty-rec
     ld[2] = ld[2] + tt-rec.qty-inv.

    IF LAST-OF(tt-rec.rec-id) THEN
      ASSIGN
       lv-num-rec     = lv-num-rec + 1
       lv-i             = lv-i + 1
       tt-rec.qty-rec = ld[1]
       tt-rec.qty-inv = ld[2]
       ld             = 0.

    ELSE DELETE tt-rec.
  END.
END.



END PROCEDURE.

PROCEDURE create-ap-from-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-qty AS DEC NO-UNDO.
  DEF VAR v-vend-act AS CHAR NO-UNDO.
  DEF VAR v-dscr AS CHAR NO-UNDO.
  DEF VAR v-tmp-qty LIKE ap-invl.qty NO-UNDO.

  DEF VAR v-ext-cost AS DEC NO-UNDO.
  DEF VAR v-qty-uom AS CHAR NO-UNDO.
  DEF VAR v-out-qty AS DEC NO-UNDO.        
  DEF VAR v-out-cost AS DEC NO-UNDO.
  DEF VAR v-setup-per AS DEC NO-UNDO.

  {ce/msfcalc.i}


  /*FIND FIRST vend NO-LOCK
      WHERE vend.company EQ ap-inv.company
        AND vend.vend-no EQ ap-inv.vend-no
      NO-ERROR.
  ASSIGN
   v-vend-act = IF AVAIL vend THEN vend.actnum ELSE ""
   v-dscr     = "".

  IF v-vend-act EQ "" THEN
     v-vend-act = v-ap-pur.*/

  /*FOR EACH tt-pol
      WHERE tt-pol.selekt
        AND tt-pol.qty-to-inv NE 0,
      FIRST po-ordl WHERE RECID(po-ordl) EQ tt-pol.rec-id NO-LOCK
      BREAK BY po-ordl.po-no:

      FIND FIRST ttPoPriceItemLook2 WHERE ttPoPriceItemLook2.ino = po-ordl.i-no NO-LOCK.*/
FIND FIRST ap-inv WHERE  ap-inv.company = "001" 
      AND ap-inv.rec_key = prmText   NO-LOCK NO-ERROR.

      FIND FIRST po-ord NO-LOCK
          WHERE po-ord.company EQ po-ordl.company 
            AND po-ord.po-no   EQ po-ordl.po-no
          NO-ERROR.

      
     CREATE tt-ap-invl.
     ASSIGN tt-ap-invl.i-no =  ap-inv.i-no
            tt-ap-invl.actnum = v-vend-act
            tt-ap-invl.company = ap-inv.company
            tt-ap-invl.vend-no = ap-inv.vend-no
            tt-ap-invl.dscr = v-dscr
            tt-ap-invl.loc = ap-inv.loc
            tt-ap-invl.period = ap-inv.period
            tt-ap-invl.posted = ap-inv.posted
            tt-ap-invl.tax = ap-inv.tax-gr NE ""
            .
      /*IF aptax-chr = "ITEM" THEN DO:*/
        FIND ITEM WHERE ITEM.company = cocode
                 AND ITEM.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN tt-ap-invl.tax = ITEM.tax-rcpt .
     /*END.*/

     ASSIGN tt-ap-invl.po-no = (po-ord.po-no)
            tt-ap-invl.LINE = (po-ordl.LINE + (po-ord.po-no * 1000) ) /* ap/invline.i 1 */
            tt-ap-invl.dscr = po-ordl.i-name
            tt-ap-invl.unit-pr = (po-ordl.cost)
            tt-ap-invl.pr-qty-uom = po-ordl.pr-uom
            tt-ap-invl.cons-uom = po-ordl.pr-qty-uom
            v-wid = po-ordl.s-wid
            v-len = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
            v-dep = 0
            v-bwt = 0.
     
     IF tt-ap-invl.cons-uom EQ "ROLL" THEN tt-ap-invl.cons-uom = "LF".

     IF po-ordl.item-type AND appaper-chr NE "PO UOM" AND
        CAN-FIND(FIRST item
                 WHERE item.company EQ cocode
                   AND item.i-no    EQ po-ordl.i-no
                   AND item.mat-type EQ "P")          THEN
       tt-ap-invl.cons-uom = appaper-chr.

     RELEASE prod.
     RELEASE costtype.

     IF po-ordl.item-type EQ NO                          AND
        (fgpostgl EQ "AllItems" OR fgpostgl EQ "POOnly") THEN DO:
       FIND FIRST itemfg
           WHERE itemfg.company EQ po-ordl.company
             AND itemfg.i-no    EQ po-ordl.i-no
           NO-LOCK NO-ERROR.
              
       IF AVAIL itemfg THEN
       FIND FIRST prodl 
           WHERE prodl.company EQ itemfg.company
             AND prodl.procat  EQ itemfg.procat
             AND CAN-FIND(FIRST prod
                          WHERE prod.company EQ cocode
                            AND prod.prolin  EQ prodl.prolin)
           NO-LOCK NO-ERROR.

       IF AVAIL prodl THEN
       FIND FIRST prod
           WHERE prod.company EQ prodl.company
             AND prod.prolin  EQ prodl.prolin
           NO-LOCK NO-ERROR.
     END.

     ELSE
     IF po-ordl.item-type AND v-rmpostgl-char EQ "ALLITEMS" THEN DO:
       FIND FIRST item
          WHERE item.company EQ cocode
            AND item.i-no    EQ po-ordl.i-no
          NO-LOCK NO-ERROR.
      
       IF AVAIL item AND item.stocked THEN
       FIND FIRST costtype
           WHERE costtype.company   EQ cocode
             AND costtype.cost-type EQ item.cost-type
           NO-LOCK NO-ERROR.
     END.

     IF AVAIL prod AND prod.wip-mat NE "" THEN
        tt-ap-invl.actnum = prod.wip-mat.
     ELSE
     IF AVAIL costtype AND costtype.ap-accrued NE "" THEN
        tt-ap-invl.actnum = costtype.ap-accrued.
     ELSE
     IF lv-po-glnum THEN tt-ap-invl.actnum = po-ordl.actnum.
     ELSE DO:
        if v-vend-actnum eq "" then do:
            find first vend where vend.company eq cocode
                  and vend.vend-no eq po-ord.vend-no 
                no-lock no-error.
            if avail vend then v-vend-actnum = vend.actnum.
        end.
        tt-ap-invl.actnum = v-vend-actnum.
     end.  

     IF v-vend-actnum EQ "" THEN
        v-vend-actnum = v-ap-pur.

     if tt-ap-invl.actnum eq "" then tt-ap-invl.actnum = v-vend-actnum.

     find first ITEM where item.company eq cocode
                       and item.i-no    eq po-ordl.i-no
                       and po-ordl.item-type
                       no-lock no-error.            
     if avail item then do:
          v-dep = item.s-dep.          
          {po/pol-dims.i}
     end.

        IF NOT po-ordl.item-type AND tt-ap-invl.cons-uom NE "EA" THEN
          RUN sys/ref/convquom.p ("EA", tt-ap-invl.cons-uom,
                                  v-bwt, v-len, v-wid, v-dep,
                                  tt-rec.qty-inv, OUTPUT tt-rec.qty-inv).

        ASSIGN
         tt-ap-invl.qty     = tt-rec.qty-inv.

        IF tt-rec.qty-inv-uom NE "" AND
           tt-rec.qty-inv-uom NE po-ordl.pr-qty-uom THEN
           RUN sys/ref/convquom.p (tt-rec.qty-inv-uom, po-ordl.pr-qty-uom,
                                   v-bwt, v-len, v-wid, v-dep,
                                   tt-ap-invl.qty, OUTPUT v-tmp-qty).
        ELSE
           v-tmp-qty = tt-ap-invl.qty.
        IF po-ordl.pr-uom = "MSF" AND po-ordl.item-type = NO THEN DO:
        
          IF po-ordl.item-type = NO THEN DO:
        
            RUN calc-setup-cost(INPUT ROWID(po-ordl), 
                                INPUT v-tmp-qty,
                                OUTPUT v-ext-cost,
                                OUTPUT v-qty-uom,
                                OUTPUT v-out-qty,
                                OUTPUT v-out-cost,
                                OUTPUT v-setup-per).
     

            ASSIGN  tt-ap-invl.amt = (v-out-cost / 1000) * v-tmp-qty
                    tt-ap-invl.unit-pr = tt-ap-invl.amt / tt-ap-invl.qty.
          END.
        END.
        ELSE
          ASSIGN
           tt-ap-invl.amt = po-ordl.t-cost / po-ordl.ord-qty * v-tmp-qty
           tt-ap-invl.unit-pr = tt-ap-invl.amt / tt-ap-invl.qty.
        


        IF tt-ap-invl.cons-uom NE tt-ap-invl.pr-qty-uom THEN
          RUN sys/ref/convcuom.p (tt-ap-invl.cons-uom, tt-ap-invl.pr-qty-uom,
                                  v-bwt, v-len, v-wid, v-dep,
                                  tt-ap-invl.unit-pr, OUTPUT tt-ap-invl.unit-pr).
        tt-ap-invl.unit-pr = ROUND(tt-ap-invl.unit-pr, 2).

         if v-len eq 0 then v-len = 12.
        if v-wid eq 0 then v-wid = 12.
        
        tt-ap-invl.sf-sht = if v-corr then (v-len * v-wid * .007)
                                      else (v-len * v-wid / 144).

        if not avail item             and
           (v-len eq 0 or v-wid eq 0) then do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq po-ordl.i-no
                and NOT po-ordl.item-type
              no-lock no-error.
          if avail itemfg then tt-ap-invl.sf-sht = (itemfg.t-sqft).
        end.

        if tt-ap-invl.cons-uom eq "EA" THEN v-qty = tt-ap-invl.qty.          
        else
          run sys/ref/convquom.p(tt-ap-invl.cons-uom, "EA",
                                 v-bwt, v-len, v-wid, v-dep,
                                 tt-ap-invl.qty, output v-qty).

        tt-ap-invl.amt-msf = (tt-ap-invl.sf-sht * v-qty / 1000).


        ASSIGN  
                 ttPoPriceItemLook2.ino         = string(po-ordl.i-no)          
                 ttPoPriceItemLook2.iname       = tt-ap-invl.dscr        
                 /*ttPoPriceItemLook2.invqty      = tt-ap-invl.qty     */
                 ttPoPriceItemLook2.LINE        = tt-ap-invl.LINE          
                 ttPoPriceItemLook2.act         = tt-ap-invl.actnum      
                 ttPoPriceItemLook2.tamt        = tt-ap-invl.amt-msf  
                 ttPoPriceItemLook2.tax         = string(tt-ap-invl.tax)
                 ttPoPriceItemLook2.consuom     = tt-ap-invl.cons-uom
                 ttPoPriceItemLook2.prqtyuom    = tt-ap-invl.pr-qty-uom
                 ttPoPriceItemLook2.price       = tt-ap-invl.unit-pr
                 ttPoPriceItemLook2.sqft        = tt-ap-invl.sf-sht
                 ttPoPriceItemLook2.amt         = tt-ap-invl.amt . 

        FIND FIRST account WHERE account.company = cocode
                          AND account.actnum = tt-ap-invl.actnum  NO-LOCK NO-ERROR.
             IF AVAIL account THEN ASSIGN ttPoPriceItemLook2.actname = account.dscr.

  FIND CURRENT tt-ap-invl NO-LOCK NO-ERROR.
  DELETE tt-ap-invl .

 /* END.*/ 

END PROCEDURE.

