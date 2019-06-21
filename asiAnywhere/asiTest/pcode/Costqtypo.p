    

/*------------------------------------------------------------------------
    File        : Costqtypo.p
    Purpose     : 
    Syntax      :

    Description : 
    Author(s)   : 
    Created     : 20 July 2012 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttb_cost_qty_po NO-UNDO
        FIELD poscrconsum     AS CHAR
        FIELD potonnage       AS DEC  FORMAT ">>>>,>>9.999"
        FIELD poqty           AS DECIMAL
        FIELD pocost          AS DEC  FORMAT ">,>>>,>>9.99<<<<"
        FIELD posetup         AS DEC
        FIELD podiscount      AS DEC
        FIELD pototcost       AS DEC  FORMAT "->,>>>,>>9.99<<"
        FIELD poprqtyuom      AS CHAR
        FIELD popruom         AS CHAR
        FIELD poconsqty       AS DEC
        FIELD poconscost      AS DEC  FORMAT "->,>>>,>>9.99<<<<"
        FIELD poconsuom       AS CHAR
        FIELD pototmsf        AS DEC  FORMAT ">>>>,>>9.999"
        FIELD poRecKey        AS CHAR
        FIELD poNxtPgBQty     AS DEC
        .

DEFINE DATASET dsb_cost_qty_po FOR ttb_cost_qty_po .

DEFINE INPUT PARAMETER prmUser            AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmAction          AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpoLine          AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmpoNo            AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmpoitemtype      AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmqty             AS DECIMAL      NO-UNDO.
DEFINE INPUT PARAMETER prmcost            AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmsetup           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmdiscount        AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmprqtyuom        AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpruom           AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmslen            AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmswid            AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmino             AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmconsuom         AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey          AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmCust            AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmjob             AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmjob2            AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmsno             AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmbno             AS INT          NO-UNDO.

DEF OUTPUT PARAMETER cError AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsb_cost_qty_po.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.
DEF NEW SHARED VAR  g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR  g_loc AS CHAR NO-UNDO.

def NEW shared var v-po-qty as log initial true no-undo.
DEF VAR fg-uom-list AS CHAR NO-UNDO.
DEF VAR ld-roll-len AS DEC NO-UNDO.
DEFINE VARIABLE fi_pb-qty AS DECIMAL NO-UNDO.
def new shared var v-hold-op1 as log.
DEFINE VARIABLE addersText AS CHARACTER NO-UNDO.
DEF NEW shared var v-default-gl-log as log no-undo.
def NEW shared var v-default-gl-cha as cha no-undo.
def new shared var v-pocost1 as char.


def NEW shared var v-po-msf like sys-ctrl.int-fld no-undo.


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


IF prmUser            = ? THEN ASSIGN prmUser             = "".
IF prmAction          = ? THEN ASSIGN prmAction           = "View".
IF prmpoLine          = ? THEN ASSIGN prmpoLine           = 0.
IF prmpoNo            = ? THEN ASSIGN prmpoNo             = 0.
IF prmpoitemType      = ? THEN ASSIGN prmpoitemType       = "".
IF prmqty             = ? THEN ASSIGN prmqty              = 0.  
IF prmcost            = ? THEN ASSIGN prmcost             = 0.
IF prmsetup           = ? THEN ASSIGN prmsetup            = 0. 
IF prmdiscount        = ? THEN ASSIGN prmdiscount         = 0.  
IF prmprqtyuom        = ? THEN ASSIGN prmprqtyuom         = "". 
IF prmpruom           = ? THEN ASSIGN prmpruom            = "".
IF prmslen            = ? THEN ASSIGN prmslen             = 0. 
IF prmswid            = ? THEN ASSIGN prmswid             = 0.
IF prmino             = ? THEN ASSIGN prmino              = "".
IF prmconsuom         = ? THEN ASSIGN prmconsuom          = "".
IF prmRecKey          = ? THEN ASSIGN prmRecKey           = "".   
IF prmjob             = ? THEN ASSIGN prmjob              = "".
IF prmjob2            = ? THEN ASSIGN prmjob2             = 0.
IF prmsno             = ? THEN ASSIGN prmsno              = 0.
IF prmbno             = ? THEN ASSIGN prmbno              = 0.  
                                                         
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp 
    g_company = prmComp
    g_loc    = "Main" .

FIND FIRST rm-ctrl WHERE rm-ctrl.company EQ cocode NO-LOCK.
  FIND FIRST fg-ctrl WHERE fg-ctrl.company EQ cocode NO-LOCK.

  RUN po/po-sysct.p.
  {sys/ref/pocost.i}
  assign
   v-pocost1  = v-pocost
   v-hold-op1 = v-hold-op.

def var i  as int no-undo.

DO TRANSACTION:
  {sys/inc/pocostq.i}
  {sys/inc/poqty.i}
  {sys/inc/pouom.i}
  {sys/inc/aptax.i}
END.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

FIND FIRST uom NO-LOCK WHERE uom.uom EQ "ROLL" NO-ERROR.
IF AVAIL uom THEN ld-roll-len = uom.mult.


IF prmaction = "calqty" THEN DO:

    CREATE ttb_cost_qty_po.

       /* po/podisdet.i */
     FIND FIRST po-ordl WHERE 
       po-ordl.company eq cocode and 
           po-ordl.po-no EQ INT(prmpoNo)  and
           po-ordl.LINE EQ INT(prmpoLine) NO-LOCK NO-ERROR.
        
        FIND FIRST po-ord WHERE 
            po-ord.company eq cocode and 
            po-ord.po-no EQ INT(prmpoNo)  NO-LOCK NO-ERROR.
 
       
                

{po/cstpodisdet2.i}

ttb_cost_qty_po.poscrconsum = prmconsuom.
ttb_cost_qty_po.pocost = prmCost .
ttb_cost_qty_po.posetup = prmsetup.


IF prmpoitemType = "RM" AND
   prmino NE "" THEN
   DO:
      FIND FIRST buf-item WHERE
           buf-item.company EQ cocode AND
           buf-item.i-no EQ prmino
           NO-LOCK NO-ERROR.
     
      IF AVAIL buf-item AND buf-item.mat-type EQ "B" THEN
         ASSIGN
             ttb_cost_qty_po.potonnage = ROUND(v-tot-msf * buf-item.basis-w / 2000,4).
    END.

   IF prmpoitemType EQ "RM" THEN DO:
       FIND FIRST e-item NO-LOCK
           WHERE e-item.company EQ cocode
             AND e-item.i-no    EQ prmino
           NO-ERROR.
      
       IF AVAIL e-item THEN DO:
          CREATE tt-ei.
          ASSIGN tt-ei.std-uom = e-item.std-uom.
      
          FIND FIRST e-item-vend NO-LOCK
              WHERE e-item-vend.company EQ e-item.company
                AND e-item-vend.i-no    EQ e-item.i-no
                AND e-item-vend.vend-no EQ po-ord.vend-no
              NO-ERROR.
      
       END.
    END.
    ELSE DO:
    

      FIND FIRST e-itemfg NO-LOCK
          WHERE e-itemfg.company EQ cocode
            AND e-itemfg.i-no    EQ prmino
          NO-ERROR.

      IF AVAIL e-itemfg THEN DO:


        IF prmCust NE "" THEN
           FIND FIRST e-itemfg-vend NO-LOCK
               WHERE e-itemfg-vend.company EQ e-itemfg.company
                 AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                 AND e-itemfg-vend.vend-no EQ po-ord.vend-no
                 AND e-itemfg-vend.cust-no EQ prmCust
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


      END.
    
    END.

     /*IF ip-type NE "Update" THEN
        v-import = DEC(lv-save-ord-qty) NE DEC({&self-name}:SCREEN-VALUE).
     ELSE IF AVAIL e-itemfg-vend OR AVAIL e-item-vend THEN DO:

        /*MESSAGE 
           "Import Cost?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
           UPDATE v-import.
           v-was-imported = TRUE.*/
       RUN vend-cost (TRUE).

     END.*/
 
      
    
   IF prmRecKey = "True" THEN
       RUN vend-cost (TRUE).
 
     
END.



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
        WHERE job.company EQ po-ordl.company
          AND job.job-no  EQ prmjob
          AND job.job-no2 EQ INT(prmjob2)
        NO-ERROR.
    IF AVAIL job THEN
    FIND FIRST job-mat NO-LOCK
        WHERE job-mat.company  EQ job.company
          AND job-mat.job      EQ job.job
          AND job-mat.job-no   EQ job.job-no
          AND job-mat.job-no2  EQ job.job-no2
          AND job-mat.frm      EQ INT(prmsno)
          AND job-mat.blank-no EQ INT(prmbno) 
        USE-INDEX seq-idx NO-ERROR.
        
    IF AVAIL job-mat THEN lv-recid = RECID(job-mat).

    v-ord-qty = DEC(prmqty).

    IF prmpoitemtype EQ "RM" THEN DO:
       FIND FIRST e-item NO-LOCK
           WHERE e-item.company EQ cocode
             AND e-item.i-no    EQ prmino
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
            AND e-itemfg.i-no    EQ prmino
          NO-ERROR.

      IF AVAIL e-itemfg THEN DO:
        CREATE tt-ei.
        ASSIGN tt-ei.std-uom = e-itemfg.std-uom.

        IF prmcust NE "" THEN
           FIND FIRST e-itemfg-vend NO-LOCK
               WHERE e-itemfg-vend.company EQ e-itemfg.company
                 AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                 AND e-itemfg-vend.vend-no EQ po-ord.vend-no
                 AND e-itemfg-vend.cust-no EQ prmcust
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
       v-cost = DEC(prmcost)
       v-qty  = DEC(prmqty)
       ttb_cost_qty_po.pocost = prmcost.

      IF tt-ei.std-uom NE prmprqtyuom          AND
        (po-ordl.item-type                                        OR
         LOOKUP(tt-ei.std-uom,fg-uom-list)                  EQ 0 OR
         LOOKUP(prmprqtyuom,fg-uom-list) EQ 0)  THEN
        RUN sys/ref/convquom.p(prmprqtyuom,
                               tt-ei.std-uom, v-basis-w,
                               v-len, v-wid, v-dep,
                               v-qty, OUTPUT v-qty).

      v-save-qty = v-qty.
      IF prmjob NE "" THEN
        RUN po/groupcst.p (prmjob,
                           INT(prmjob2),
                           prmino,
                           INT(prmsno),
                           INT(prmbno),
                           INPUT-OUTPUT v-qty).

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

        IF tt-ei.std-uom NE prmprqtyuom           AND
           (po-ordl.item-type                                        OR
            LOOKUP(tt-ei.std-uom,fg-uom-list)                  EQ 0 OR
            LOOKUP(prmprqtyuom,fg-uom-list) EQ 0)  THEN
          RUN sys/ref/convquom.p(tt-ei.std-uom,
                                 prmprqtyuom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-pb-qty, OUTPUT v-pb-qty).

        IF tt-ei.std-uom NE prmpruom           AND
           (po-ordl.item-type                                    OR
            LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
            LOOKUP(prmpruom,fg-uom-list) EQ 0)  THEN
          RUN sys/ref/convcuom.p(tt-ei.std-uom,
                                 prmpruom, v-basis-w,
                                 v-len, v-wid, v-dep,
                                 v-pb-cst, OUTPUT v-pb-cst).

        IF prmpruom NE prmconsuom AND
           (po-ordl.item-type                                      OR
            LOOKUP(prmpruom,fg-uom-list)   EQ 0 OR
            LOOKUP(prmconsuom,fg-uom-list) EQ 0)     THEN
          RUN sys/ref/convcuom.p(prmpruom,
                                 prmconsuom, v-basis-w,
                                 v-len, v-wid, v-dep,
                                 v-pb-cst, OUTPUT v-pb-cns).

        ttb_cost_qty_po.poNxtPgBQty = IF v-pb-qty LE 0 THEN 0 ELSE v-pb-qty.
      END.

      IF v-qty <> 0 THEN v-cost = (v-cost /*+ v-setup*/) / v-qty.  
      ELSE v-cost = (v-cost /*+ v-setup*/).

      IF ip-calc-cost NE ? THEN DO:
        IF ip-calc-cost THEN DO:            
          IF tt-ei.std-uom NE prmpruom           AND
             (po-ordl.item-type                                    OR
              LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
              LOOKUP(prmpruom,fg-uom-list) EQ 0)  THEN
            RUN sys/ref/convcuom.p(tt-ei.std-uom,
                                   prmpruom, v-basis-w,
                                   (IF prmprqtyuom EQ "ROLL" THEN 12 ELSE v-len),
                                   v-wid, v-dep,
                                   v-cost, OUTPUT v-cost).
          ASSIGN
            ip-calc-cost = YES
            ttb_cost_qty_po.pocost = v-cost
            ttb_cost_qty_po.posetup = v-setup.
          
          IF prmpruom NE prmconsuom AND
             (po-ordl.item-type                                      OR
              LOOKUP(prmpruom,fg-uom-list)   EQ 0 OR
              LOOKUP(prmconsuom,fg-uom-list) EQ 0)     THEN
            RUN sys/ref/convcuom.p(prmpruom,
                                   prmconsuom, v-basis-w,
                                   (IF prmprqtyuom EQ "ROLL" THEN 12 ELSE v-len),
                                   v-wid, v-dep,
                                   v-cost, OUTPUT v-cost).

          ttb_cost_qty_po.poconscost = v-cost.     
          
        END.

        ELSE
        IF v-hold-op1 AND po-ord.stat NE "H" THEN DO:
          IF tt-ei.std-uom NE prmpruom           AND
             (po-ordl.item-type                                    OR
              LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
              LOOKUP(prmpruom,fg-uom-list) EQ 0)  THEN
            RUN sys/ref/convcuom.p(tt-ei.std-uom,
                                   prmpruom, v-basis-w,
                                   v-len, v-wid, v-dep,
                                   v-cost, OUTPUT v-cost).              
          IF AVAIL job-mat THEN
            RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                           DEC(prmqty),
                           v-cost,
                           ttb_cost_qty_po.poconscost,
                           OUTPUT v-cost,
                           OUTPUT lv-added-cons-cost,
                           OUTPUT lv-adder-setup).

          IF DEC(ttb_cost_qty_po.pocost) GT v-cost THEN DO:
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
                       DEC(ttb_cost_qty_po.poNxtPgBQty),
                       v-pb-cst,
                       v-pb-cns,
                       OUTPUT v-pb-cst,
                       OUTPUT v-pb-cns,
                       OUTPUT lv-adder-setup).

      RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                     DEC(prmqty),
                     DEC(ttb_cost_qty_po.pocost),
                     ttb_cost_qty_po.poconscost,
                     OUTPUT lv-added-cost,
                     OUTPUT lv-added-cons-cost,
                     OUTPUT lv-adder-setup).
    
      IF ip-calc-cost THEN
        ASSIGN
         ttb_cost_qty_po.pocost = lv-added-cost
         ttb_cost_qty_po.poconscost = lv-added-cons-cost.
      
    END.

    IF poqty-log THEN DO:
      IF CAN-DO("L,LOT",prmpruom) THEN
        lv-t-cost = (v-pb-cst + v-pb-stp) *
                    IF po-ordl.ord-qty LT 0 THEN -1 ELSE 1.

      ELSE DO:
        v-ord-qty = DEC(ttb_cost_qty_po.poNxtPgBQty).

        IF prmprqtyuom NE prmpruom AND
           (po-ordl.item-type                                        OR
            LOOKUP(prmprqtyuom,fg-uom-list) EQ 0 OR
            LOOKUP(prmpruom,fg-uom-list)     EQ 0)     THEN
   
          RUN sys/ref/convquom.p(prmprqtyuom,
                                 prmpruom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-ord-qty, OUTPUT v-ord-qty).
     
        lv-t-cost = (v-ord-qty * v-pb-cst) + v-pb-stp.
      END.

      IF DEC(prmdiscount) NE 0 THEN
        lv-t-cost = lv-t-cost * (1 - (DEC(prmdiscount) / 100)).

      ttb_cost_qty_po.poNxtPgBQty = lv-t-cost.

      IF ttb_cost_qty_po.poNxtPgBQty LE 0 THEN ttb_cost_qty_po.poNxtPgBQty = 0.
    END.

    IF ip-calc-cost NE ? THEN DO:
      IF CAN-DO("L,LOT",prmpruom) THEN
        lv-t-cost = (DEC(ttb_cost_qty_po.pocost) +
                     DEC(ttb_cost_qty_po.posetup)) *
                    IF po-ordl.ord-qty LT 0 THEN -1 ELSE 1.

      ELSE DO:
        v-ord-qty = DEC(prmqty).

        IF prmprqtyuom NE prmpruom AND
           (po-ordl.item-type                                        OR
            LOOKUP(prmprqtyuom,fg-uom-list) EQ 0 OR
            LOOKUP(prmpruom,fg-uom-list)     EQ 0)     THEN
   
          RUN sys/ref/convquom.p(prmprqtyuom,
                                 prmpruom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-ord-qty, OUTPUT v-ord-qty).
     
        lv-t-cost = (v-ord-qty * DEC(ttb_cost_qty_po.pocost)) +
                    DEC(ttb_cost_qty_po.posetup).
      END.

      IF DEC(prmdiscount) NE 0 THEN
         lv-t-cost = lv-t-cost * (1 - (DEC(prmdiscount) / 100)).

      ttb_cost_qty_po.pototcost = lv-t-cost.
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
     v-len = DEC(prmslen)
     v-wid = DEC(prmswid)
     {po/calc10.i v-len}
     {po/calc10.i v-wid}.

    FIND FIRST ITEM
        WHERE item.company EQ cocode
          AND item.i-no    EQ prmino
        NO-LOCK NO-ERROR.

    ASSIGN
      v-basis-w = IF AVAIL ITEM THEN item.basis-w ELSE 0
      v-dep     = IF AVAIL ITEM THEN item.s-dep ELSE 0.


END PROCEDURE.

PROCEDURE po-adder2 :
/*------------------------------------------------------------------------------
  Purpose:     
  PARAMs:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-recid  as recid.
DEF INPUT PARAM ip-recid1 as recid.
DEF INPUT PARAM ip-vend-no LIKE po-ord.vend-no NO-UNDO.
DEF INPUT PARAM ip-qty as DEC NO-UNDO.
DEF INPUT PARAM ip-cost as DEC NO-UNDO.
DEF INPUT PARAM ip-cons-cost as DEC NO-UNDO.

DEF OUTPUT PARAM op-cost AS DEC NO-UNDO.
DEF OUTPUT PARAM op-cons-cost AS DEC NO-UNDO.
DEF OUTPUT PARAM op-adder-setup AS DEC NO-UNDO.

def var v-tot-cost as dec no-undo.
def var v-cost     as dec no-undo.
def var v-add-cost as dec no-undo.
def var v-qty-comp as dec no-undo.
def var v-setup like e-item-vend.setup no-undo.
def var v-adder as dec extent 2 NO-UNDO.
DEF VAR v-index AS INT NO-UNDO.

def buffer xjob-mat for job-mat.

find xjob-mat where recid(xjob-mat) eq ip-recid1 no-lock.

assign
   addersText = ''
   op-cost = ip-cost
   op-cons-cost = ip-cons-cost.



   FIND first item where 
        item.company  eq job-mat.company AND
        item.i-no     eq prmino
        NO-LOCK NO-ERROR.

   IF AVAIL ITEM AND
      ITEM.mat-type NE "B" THEN
      LEAVE.

   ASSIGN
      v-adder[1] = ip-cost
      v-adder[2] = ip-cons-cost.

  IF prmpruom EQ "EA"                    OR
     (NOT po-ordl.item-type AND
      LOOKUP(prmpruom,fg-uom-list) EQ 0) THEN
     v-tot-cost = ip-cost.

  ELSE
    RUN sys/ref/convcuom.p(prmpruom, "EA",
                           v-basis-w, v-len, v-wid, v-dep,
                           ip-cost, OUTPUT v-tot-cost).
 
  for each job-mat no-lock
      where job-mat.company  eq xjob-mat.company
        and job-mat.job      eq xjob-mat.job
        and job-mat.frm      eq xjob-mat.frm
        and job-mat.job-no   eq xjob-mat.job-no
        and job-mat.job-no2  eq xjob-mat.job-no2
      use-index seq-idx,

      first item no-lock
      where item.company  eq job-mat.company
        and item.i-no     eq job-mat.i-no
        and item.mat-type eq "A":

    find first e-item no-lock
        where e-item.company eq po-ordl.company
          and e-item.i-no    eq prmino
        no-error.
    
    find first e-item-vend no-lock
        where e-item-vend.company eq item.company
          and e-item-vend.i-no    eq item.i-no
          and e-item-vend.vend-no eq ip-vend-no
        no-error.

    if avail e-item and avail e-item-vend AND ip-vend-no NE "" then do:
      if prmprqtyuom eq e-item.std-uom then
           v-qty-comp = ip-qty.
      else
        run sys/ref/convquom.p(prmprqtyuom, e-item.std-uom,
                               v-basis-w, v-len, v-wid, v-dep,
                               ip-qty, output v-qty-comp).
        

      v-setup = 0.

      EMPTY TEMP-TABLE tt-eiv-2.
      CREATE tt-eiv-2.

      DO v-index = 1 TO 10:
         ASSIGN
            tt-eiv-2.run-qty[v-index] = e-item-vend.run-qty[v-index]
            tt-eiv-2.run-cost[v-index] = e-item-vend.run-cost[v-index]
            tt-eiv-2.setups[v-index] = e-item-vend.setups[v-index].
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
               tt-eiv-2.run-qty[v-index + 10] = b-qty.val[v-index]
               tt-eiv-2.run-cost[v-index + 10] = b-cost.val[v-index]
               tt-eiv-2.setups[v-index + 10] = b-setup.val[v-index].
         END.
      END.

      do i = 1 to EXTENT(tt-eiv-2.run-qty):
         if v-qty-comp le tt-eiv-2.run-qty[i] then
            leave.
      end.
    /*  if i eq 1 then v-setup = e-item-vend.setup. */
      IF i GT EXTENT(tt-eiv-2.run-qty) THEN i = EXTENT(tt-eiv-2.run-qty).
      ASSIGN
        v-setup = tt-eiv-2.setups[i]
        op-adder-setup = op-adder-setup + v-setup
        v-cost = ((tt-eiv-2.run-cost[i] * v-qty-comp) + v-setup) / v-qty-comp.
      /* This adds the Adder cost in */
      IF e-item.std-uom NE prmpruom THEN
        RUN sys/ref/convcuom.p(e-item.std-uom, prmpruom, job-mat.basis-w,
                               job-mat.len, job-mat.wid, item.s-dep,
                               v-cost, OUTPUT v-cost).
    END.

    ELSE DO:
      v-cost = job-mat.std-cost.
      
      IF job-mat.sc-uom NE prmpruom THEN
        RUN sys/ref/convcuom.p(job-mat.sc-uom, prmpruom, job-mat.basis-w,
                               job-mat.len, job-mat.wid, item.s-dep,
                               job-mat.std-cost, OUTPUT v-cost).
    END.
    IF v-cost = ? THEN v-cost = 0.
    ASSIGN
     addersText = addersText + SUBSTR(item.i-name,1,18) +
                  FILL(' ',19 - LENGTH(SUBSTR(item.i-name,1,18))) +
                  STRING(v-cost,'-z,zz9.9999') + CHR(10)
     v-add-cost = v-add-cost + v-cost.

    /* gdm - */     
    IF v-cost NE 0                         
      THEN RUN po-adder3 (INPUT v-cost).   
    /* gdm - end */                                
  END.

  IF prmpruom NE "EA" THEN 
    RUN sys/ref/convcuom.p("EA", prmpruom,
                           v-basis-w, v-len, v-wid, v-dep,
                           v-tot-cost, OUTPUT v-tot-cost).
 
  op-cost = v-add-cost + v-tot-cost.

  IF prmpruom NE prmconsuom THEN
    RUN sys/ref/convcuom.p(prmpruom, prmconsuom,
                           v-basis-w, v-len, v-wid, v-dep,
                           ip-cost, OUTPUT op-cons-cost).

/*  display po-ordl.cost po-ordl.cons-cost.  */

assign
 v-adder[1] = op-cost      - v-adder[1]
 v-adder[2] = op-cons-cost - v-adder[2].


END PROCEDURE.

PROCEDURE po-adder3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-cost     as dec no-undo.
    FIND FIRST po-ordl-add 
      WHERE po-ordl-add.company    EQ po-ordl.company
        AND po-ordl-add.po-no      EQ po-ordl.po-no  
        AND po-ordl-add.line       EQ po-ordl.LINE   
        AND po-ordl-add.adder-i-no EQ job-mat.i-no NO-ERROR.
    
    IF NOT AVAIL po-ordl-add THEN DO:
    
        CREATE po-ordl-add.
        ASSIGN po-ordl-add.company     = po-ordl.company
               po-ordl-add.po-no       = po-ordl.po-no
               po-ordl-add.line        = po-ordl.LINE
               po-ordl-add.adder-i-no  = job-mat.i-no.
               
    /*****************************************************************
             THIS IN CASE WE NEED IT LATER
    *****************************************************************            
               po-ordl-add.cons-cost   =
               po-ordl-add.cons-qty    =
               po-ordl-add.cons-uom    =
               po-ordl-add.ord-qty     =
               po-ordl-add.pr-qty-uom  =
               po-ordl-add.pr-uom      = 
               po-ordl-add.t-cost      =
               po-ordl-add.setup       = .
    *****************************************************************/
        /*rec_key CREATION CALL */
        {custom/rec_key.i po-ordl-add}

    END.
    ELSE
    IF AVAIL po-ordl-add THEN DO:
     /* IF THERE THE SAME ADDER IN A JOB - RECORD THE BIGGER COST */        
     IF ip-cost GT po-ordl-add.cost 
      THEN ASSIGN po-ordl-add.cost = ip-cost.
    END.
    RELEASE po-ordl-add.

END PROCEDURE.
