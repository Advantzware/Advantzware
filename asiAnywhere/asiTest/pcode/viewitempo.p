    

/*------------------------------------------------------------------------
    File        : viewitempo.p
    Purpose     : 
    Syntax      :

    Description : Return a Dataset of Estimate Corrugated box
    Author(s)   : 
    Created     : 14 Jan 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttb_po_item_inq_view NO-UNDO
        FIELD poLine          AS INT  
        FIELD poNo            AS INT
        FIELD poDate          AS CHAR
        FIELD poLoc           AS CHAR
        FIELD poType          AS CHAR
        FIELD poTMsf          AS DEC
        FIELD poStat          AS CHAR
        FIELD poItemNo        AS CHAR
        FIELD poItemName      AS CHAR
        FIELD poJobNo         AS CHAR
        FIELD poJobNo2        AS INT
        FIELD poSNum          AS INT
        FIELD poOrdQty        AS DEC
        FIELD poCost          AS DEC
        FIELD poCustNo        AS CHAR
        FIELD poDueDate       AS CHAR
        FIELD poItemType      AS CHAR
        FIELD poBNum          AS int
        FIELD poPrQtyUom      AS CHAR
        FIELD poConsQty       AS DEC
        FIELD poDscr       AS CHAR EXTENT 2
        FIELD poPrUom         AS CHAR
        FIELD poConsCost      AS DEC
        FIELD poConsUom       AS CHAR
        /*FIELD poDscr[2]       AS CHAR*/
        FIELD poSetup         AS DEC
        FIELD poSwid          AS DEC
        FIELD poSlen          AS DEC
        FIELD poDisc          AS DEC
        FIELD poActNum        AS CHAR
        FIELD poVendINo       AS CHAR
        FIELD poTax           AS CHAR
        FIELD poOverPct       AS DEC
        FIELD poUnderPct      AS DEC
        FIELD poOrdNo         AS INT
        FIELD poTCost         AS DEC
        FIELD poFiUom         AS CHAR
        FIELD poScrConsUom    AS CHAR  
        FIELD poSDep          AS DEC
        FIELD poWidFrac       AS CHAR
        FIELD poLenFrac       AS CHAR
        FIELD poDepFrac       AS CHAR
        FIELD poGlDesc        AS CHAR
        FIELD poPbQty         AS DEC
        FIELD poPbCst         AS DEC
        FIELD poQOnh          AS DEC 
        FIELD poQOno          AS DEC
        FIELD poQComm         AS DEC
        FIELD poQBack         AS DEC
        FIELD poQAvail        AS DEC  
        FIELD poMOnh          AS DEC
        FIELD poMOno          AS DEC
        FIELD poMComm         AS DEC
        FIELD poMBack         AS DEC
        FIELD poMAvail        AS DEC
        FIELD poMsf           AS CHAR
        FIELD poTonnage       AS DEC
        FIELD poaddress       AS CHAR
        FIELD ficahdr         AS CHAR
        FIELD poRecKey        AS CHAR
        .

DEFINE DATASET dsb_po_item_inq_view FOR ttb_po_item_inq_view .

DEFINE INPUT PARAMETER prmUser            AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmAction          AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpoLine          AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmpoNo            AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmpoDate          AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpoLoc           AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpoType          AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpoTMsf          AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmpoStat          AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER prmpoItemNo        AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpoItemName      AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpoJobNo         AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpoJobNo2        AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmpoSNum          AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmpoOrdQty        AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmpoCost          AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmpoCustNo        AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpoDueDate       AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpoItemType      AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpoBNum          AS int          NO-UNDO.  
DEFINE INPUT PARAMETER prmpoPrQtyUom      AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER prmpoConsQty       AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoDscr         AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER prmpoDscr2         AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER prmpoPrUom         AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER prmpoConsCost      AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoConsUom       AS CHAR         NO-UNDO. 

DEFINE INPUT PARAMETER prmpoSetup         AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoSwid          AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoSlen          AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoDisc          AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoActNum        AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER prmpoVendINo       AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER prmpoTax           AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER prmpoOverPct       AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoUnderPct      AS DEC          NO-UNDO. 

DEFINE INPUT PARAMETER prmpoOrdNo         AS INT          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoTCost         AS DEC          NO-UNDO.  
DEFINE INPUT PARAMETER prmpoFiUom         AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER prmpoScrConsUom    AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER prmpoSDep          AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoWidFrac       AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER prmpoLenFrac       AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER prmpoDepFrac       AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER prmpoGlDesc        AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER prmpoPbQty         AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoPbCst         AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoQOnh          AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoQOno          AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoQComm         AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoQBack         AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoQAvail        AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoMOnh          AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoMOno          AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoMComm         AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoMBack         AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoMAvail        AS DEC          NO-UNDO.  
DEFINE INPUT PARAMETER prmpoMsf           AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER prmpoTonnage       AS DEC          NO-UNDO. 
DEFINE INPUT PARAMETER prmpoaddress       AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER prmpoRecKey        AS CHAR         NO-UNDO.
DEF OUTPUT PARAMETER cError AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsb_po_item_inq_view.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.
DEF NEW SHARED VAR  g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR  g_loc AS CHAR NO-UNDO.
DEFINE VARIABLE addersText AS CHARACTER NO-UNDO.

IF prmUser            = ? THEN ASSIGN prmUser             = "".
IF prmAction          = ? THEN ASSIGN prmAction           = "View".
IF prmpoLine          = ? THEN ASSIGN prmpoLine           = 0.
IF prmpoNo            = ? THEN ASSIGN prmpoNo             = 0.
IF prmpoDate          = ? THEN ASSIGN prmpoDate           = "".  
IF prmpoLoc           = ? THEN ASSIGN prmpoLoc            = "".
IF prmpoType          = ? THEN ASSIGN prmpoType           = "".
IF prmpoTMsf          = ? THEN ASSIGN prmpoTMsf           = 0.
IF prmpoStat          = ? THEN ASSIGN prmpoStat           = "".
IF prmpoItemNo        = ? THEN ASSIGN prmpoItemNo         = "".
IF prmpoItemName      = ? THEN ASSIGN prmpoItemName       = "".
IF prmpoJobNo         = ? THEN ASSIGN prmpoJobNo          = "".
IF prmpoJobNo2        = ? THEN ASSIGN prmpoJobNo2         = 0.  
IF prmpoSNum          = ? THEN ASSIGN prmpoSNum           = 0.
IF prmpoOrdQty        = ? THEN ASSIGN prmpoOrdQty         = 0.
IF prmpoCost          = ? THEN ASSIGN prmpoCost           = 0.
IF prmpoCustNo        = ? THEN ASSIGN prmpoCustNo         = "".
IF prmpoDueDate       = ? THEN ASSIGN prmpoDueDate        = "".
IF prmpoItemType      = ? THEN ASSIGN prmpoItemType       = "".
IF prmpoBNum          = ? THEN ASSIGN prmpoBNum           = 0.    
IF prmpoPrQtyUom      = ? THEN ASSIGN prmpoPrQtyUom       = "".   
IF prmpoConsQty       = ? THEN ASSIGN prmpoConsQty        = 0.    
IF prmpoDscr       = ? THEN ASSIGN prmpoDscr        = "".   
IF prmpoPrUom         = ? THEN ASSIGN prmpoPrUom          = "".   
IF prmpoConsCost      = ? THEN ASSIGN prmpoConsCost       = 0.    
IF prmpoConsUom       = ? THEN ASSIGN prmpoConsUom        = "".   
IF prmpoDscr2       = ? THEN ASSIGN prmpoDscr2        = "".   
IF prmpoSetup         = ? THEN ASSIGN prmpoSetup          = 0.    
IF prmpoSwid          = ? THEN ASSIGN prmpoSwid           = 0.    
IF prmpoSlen          = ? THEN ASSIGN prmpoSlen           = 0.    
IF prmpoDisc          = ? THEN ASSIGN prmpoDisc           = 0.    
IF prmpoActNum        = ? THEN ASSIGN prmpoActNum         = "".   
IF prmpoVendINo       = ? THEN ASSIGN prmpoVendINo        = "".   
IF prmpoTax           = ? THEN ASSIGN prmpoTax            = "".   
IF prmpoOverPct       = ? THEN ASSIGN prmpoOverPct        = 0.    
IF prmpoUnderPct      = ? THEN ASSIGN prmpoUnderPct       = 0.    
IF prmpoCustNo        = ? THEN ASSIGN prmpoCustNo         = "".   
IF prmpoOrdNo         = ? THEN ASSIGN prmpoOrdNo          = 0.    
IF prmpoTCost         = ? THEN ASSIGN prmpoTCost          = 0.    
IF prmpoFiUom         = ? THEN ASSIGN prmpoFiUom          = "".   
IF prmpoScrConsUom    = ? THEN ASSIGN prmpoScrConsUom     = "".   
IF prmpoSDep          = ? THEN ASSIGN prmpoSDep           = 0.    
IF prmpoWidFrac       = ? THEN ASSIGN prmpoWidFrac        = "".   
IF prmpoLenFrac       = ? THEN ASSIGN prmpoLenFrac        = "".   
IF prmpoDepFrac       = ? THEN ASSIGN prmpoDepFrac        = "".   
IF prmpoGlDesc        = ? THEN ASSIGN prmpoGlDesc         = "".   
IF prmpoPbQty         = ? THEN ASSIGN prmpoPbQty          = 0.    
IF prmpoPbCst         = ? THEN ASSIGN prmpoPbCst          = 0.    
IF prmpoQOnh          = ? THEN ASSIGN prmpoQOnh           = 0.    
IF prmpoQOno          = ? THEN ASSIGN prmpoQOno           = 0.    
IF prmpoQComm         = ? THEN ASSIGN prmpoQComm          = 0.    
IF prmpoQBack         = ? THEN ASSIGN prmpoQBack          = 0.    
IF prmpoQAvail        = ? THEN ASSIGN prmpoQAvail         = 0.    
IF prmpoMOnh          = ? THEN ASSIGN prmpoMOnh           = 0.    
IF prmpoMOno          = ? THEN ASSIGN prmpoMOno           = 0.    
IF prmpoMComm         = ? THEN ASSIGN prmpoMComm          = 0.    
IF prmpoMBack         = ? THEN ASSIGN prmpoMBack          = 0.    
IF prmpoMAvail        = ? THEN ASSIGN prmpoMAvail         = 0.    
IF prmpoMsf           = ? THEN ASSIGN prmpoMsf            = "".   
IF prmpoTonnage       = ? THEN ASSIGN prmpoTonnage        = 0.    
IF prmpoaddress       = ? THEN ASSIGN prmpoaddress        = "".   
IF prmpoRecKey        = ? THEN ASSIGN prmpoRecKey         = "".    

DEF NEW SHARED VAR v-basis-w AS DEC NO-UNDO. 
DEF NEW SHARED VAR v-len LIKE po-ordl.s-len NO-UNDO.
DEF NEW SHARED VAR v-wid LIKE po-ordl.s-wid NO-UNDO.
DEF NEW SHARED VAR v-dep LIKE po-ordl.s-len NO-UNDO.
def buffer xpo-ord for po-ord.
def buffer xpo-ordl for po-ordl.
DEF BUFFER bf-po-ordl FOR po-ordl .
DEF BUFFER buf-item FOR ITEM.
DEF BUFFER b-vend FOR vend.
DEF BUFFER b-po-ordl FOR po-ordl .
DEF BUFFER b-qty FOR reftable.
    DEF BUFFER b-cost FOR reftable.
    DEF BUFFER b-setup FOR reftable.


DEF VAR v-line AS INT NO-UNDO.
DEF VAR v-tot-msf AS DEC NO-UNDO.
def NEW shared var factor# as decimal no-undo.
def var fi_q-onh as DECIMAL no-undo.
def var fi_q-ono as decimal no-undo.
def var fi_q-comm as decimal no-undo.
def var fi_q-back as decimal no-undo.
def var fi_q-avail as decimal no-undo.
def var fi_m-onh as decimal no-undo.
def var fi_m-ono as decimal no-undo.
def var fi_m-comm as decimal no-undo.
def var fi_m-back as decimal no-undo.
def var fi_m-avail as decimal no-undo.
def var fi_msf  as CHAR no-undo.
def var fi_uom  as CHAR no-undo.
def var v-po-dep  as DECIMAL no-undo.
def var v-po-wid-frac  as CHAR no-undo.
def var v-po-len-frac  as CHAR no-undo.
def var v-po-dep-frac  as CHAR no-undo.
def var v-gl-desc  as CHAR no-undo.
def var fi_pb-qty  as DECIMAL no-undo.
def var fi_pb-cst  as DECIMAL no-undo.
DEF VAR v-ord-qty AS dec NO-UNDO.
DEFINE VARIABLE fi_c-a-hdr AS CHARACTER NO-UNDO.
DEF VAR v-wid-frac AS CHAR NO-UNDO.
DEF VAR v-len-frac AS CHAR NO-UNDO.
DEF VAR v-dep-frac AS CHAR NO-UNDO.

DEF NEW  shared var v-default-gl-log as log NO-UNDO INIT NO.
def NEW shared var v-default-gl-cha as cha no-undo.
def NEW shared var v-po-qty as log initial true no-undo.
def NEW shared var v-po-msf like sys-ctrl.int-fld no-undo.

def var lv-uom-list as cha init "C,CS,EA,L,LB,LF,LOT,M,MSF,SHT,TON,BF" no-undo.
DEF VAR pr-uom-list AS cha NO-UNDO INIT "EA,LB,M,MSF,TON,BF".
DEF VAR cons-uom-list AS CHA NO-UNDO INIT "M,LF,EA,LB,TON".
DEF VAR fg-uom-list AS CHAR NO-UNDO.
DEF VAR ld-roll-len AS DEC NO-UNDO.
DEF VAR lv-save-ord-no AS INT NO-UNDO.
DEF VAR ip-ord-no like po-ord.po-no no-undo.

DEF TEMP-TABLE tt-job-mat NO-UNDO LIKE job-mat
    FIELD orig-lot-cost-upd AS LOG
    FIELD orig-lot-cost AS DEC DECIMALS 6
    FIELD row-id AS ROWID.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD std-uom AS CHAR.

DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20
    FIELD rec_key AS CHAR.

DEF TEMP-TABLE tt-eiv-2 NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20
    FIELD rec_key AS CHAR.

DEF VAR ll-poord-warned AS LOG NO-UNDO.
DEF VAR ll-order-warned AS LOG NO-UNDO INIT NO.
def new shared var v-hold-op1 as log.
def new shared var v-pocost1 as char.

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

DEF BUFFER bf-itemfg        FOR itemfg.  
DEF BUFFER bf-e-itemfg      FOR e-itemfg.  
DEF BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.

{fg/fullset.i NEW}

DO TRANSACTION:
  {sys/inc/pocostq.i}
  {sys/inc/poqty.i}
  {sys/inc/pouom.i}
  {sys/inc/aptax.i}
END.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

FIND FIRST uom NO-LOCK WHERE uom.uom EQ "ROLL" NO-ERROR.
IF AVAIL uom THEN ld-roll-len = uom.mult.

FIND FIRST rm-ctrl WHERE rm-ctrl.company EQ cocode NO-LOCK.
FIND FIRST fg-ctrl WHERE fg-ctrl.company EQ cocode NO-LOCK.

  RUN po/po-sysct.p.
  {sys/ref/pocost.i}
  assign
   v-pocost1  = v-pocost
   v-hold-op1 = v-hold-op.

  DEF VAR sv-poord-warned LIKE ll-poord-warned NO-UNDO.
  DEF BUFFER b-po-ord FOR po-ord.



IF prmAction = "validateupdate" THEN DO:

    FIND FIRST po-ordl WHERE 
       po-ordl.company eq cocode and 
           po-ordl.po-no EQ INT(prmpoNo)  and
           po-ordl.LINE EQ INT(prmpoLine)  exclusive-LOCK NO-ERROR.
     FIND FIRST po-ord WHERE                
        po-ord.company EQ cocode AND 
        po-ord.po-no EQ prmpoNo
        NO-LOCK NO-ERROR. 
ASSIGN ip-ord-no = po-ord.po-no .

  
 
  
  IF prmpoMsf = "Yes" THEN DO:

      IF prmpoItemType EQ "RM" OR
       NOT CAN-FIND(FIRST itemfg
                    WHERE itemfg.company EQ g_company
                      AND itemfg.i-no    EQ prmpoItemNo) THEN
    FIND FIRST item
        WHERE item.company EQ g_company
          AND item.i-no    EQ prmpoItemNo
        NO-LOCK NO-ERROR.

    IF prmpoItemType EQ "FG" OR
       NOT CAN-FIND(FIRST item
                    WHERE item.company EQ g_company
                      AND item.i-no    EQ prmpoItemNo) THEN
    FIND FIRST itemfg
        WHERE itemfg.company EQ g_company
          AND itemfg.i-no    EQ prmpoItemNo
        NO-LOCK NO-ERROR.

    IF prmpoItemNo EQ ""       OR
       (NOT AVAIL item AND NOT AVAIL itemfg) THEN DO:
      cError =  "Invalid item#, try help..."  .
      RETURN .
    END.

    IF prmpoItemType EQ "FG" AND  prmpoItemNo NE "" THEN DO:
        FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"
                              AND reftable.company  EQ cocode
                              AND reftable.loc      EQ ""
                              AND reftable.code     EQ prmpoItemNo
                              AND reftable.code2 = "I"
                            NO-LOCK NO-ERROR.
        IF AVAIL reftable THEN DO:
           cError =  prmpoItemNo + " has InActive Status. Purchase Order cannot be placed for the Inactive Item." .
           RETURN .
        END.     
    END.


  END.  /* validate item*/ 


    IF TRIM(prmpoJobNo) NE ""   OR
       (prmpoItemType EQ "RM" AND
        CAN-FIND(FIRST item
                 WHERE item.company EQ g_company
                   AND item.i-no    EQ prmpoItemNo
                   AND item.i-code  EQ "E"))  THEN DO:

      prmpoJobNo = FILL(" ",6 - LENGTH(TRIM(prmpoJobNo))) + TRIM(prmpoJobNo).

      IF TRIM(prmpoJobNo) EQ ""   OR
         NOT CAN-FIND(FIRST job-hdr
                      WHERE job-hdr.company EQ g_company
                        AND job-hdr.job-no  EQ prmpoJobNo) THEN DO:
        cError =  "Invalid Job, try help..."   .
        RETURN .
      END.
   END.

   IF prmpoJobNo NE "" THEN DO:
      prmpoJobNo = FILL(" ",6 - LENGTH(TRIM(prmpoJobNo))) + TRIM(prmpoJobNo).

      IF NOT CAN-FIND(FIRST job-hdr
                      WHERE job-hdr.company EQ g_company
                        AND job-hdr.job-no  EQ prmpoJobNo
                        AND job-hdr.job-no2 EQ INT(prmpoJobNo2))
      THEN DO:
        cError =  "Invalid Job2, try help..." . 
        RETURN .
      END.
   END.

 IF prmpoJobNo NE "" THEN DO:
      prmpoJobNo = FILL(" ",6 - LENGTH(TRIM(prmpoJobNo))) + TRIM(prmpoJobNo).

      IF prmpoSNum NE 0 THEN
             FIND FIRST job-mat
                      WHERE job-mat.company EQ cocode
                        AND job-mat.job-no  EQ prmpoJobNo
                        AND job-mat.job-no2 EQ INT(prmpoJobNo2)
                        AND job-mat.frm     EQ INT(prmpoSNum)
                        AND job-mat.rm-i-no  EQ prmpoItemNo  NO-LOCK NO-ERROR.
      IF NOT AVAIL job-mat THEN DO:
        cError =  "Invalid sheet for job..." .
        RETURN .
      END.
        
      FIND FIRST job-mat WHERE job-mat.company  EQ g_company
          AND job-mat.job-no   EQ prmpoJobNo
          AND job-mat.job-no2  EQ INT(prmpoJobNo2)
          AND job-mat.frm      EQ INT(prmpoSNum)
          AND job-mat.blank-no EQ INT(prmpoBNum)
          AND job-mat.rm-i-no  EQ prmpoItemNo     NO-LOCK NO-ERROR.
                           
       IF NOT AVAIL job-mat THEN do:
        cError = "Invalid sheet/blank/item for job".
        RETURN.
      END.
 END.
     

  IF CAN-FIND(FIRST xpo-ordl
                  WHERE xpo-ordl.company EQ g_company
                    AND xpo-ordl.po-no   EQ prmpoNo
                    AND xpo-ordl.job-no  EQ prmpoJobNo
                    AND xpo-ordl.job-no2 EQ INT(prmpoJobNo2)
                    AND xpo-ordl.i-no    EQ prmpoItemNo
                    AND xpo-ordl.s-num   EQ INT(prmpoSNum)
                    AND xpo-ordl.b-num   EQ INT(prmpoBNum)
                    AND RECID(xpo-ordl)  NE RECID(po-ordl))
      THEN DO:
      cError = "PO line item alreadys exists for this PO/Item/Job/Sheet/Blank".
      RETURN.
      END.

    RUN valid-uom ("pr-qty-uom") NO-ERROR.
    RUN valid-uom ("pr-uom") NO-ERROR.
  

   IF (prmpoActNum EQ "" OR
        NOT CAN-FIND(FIRST account
                     WHERE account.company EQ g_company
                       AND account.actnum  EQ prmpoActNum
                       AND account.TYPE <> "T"))  THEN DO:
      IF prmpoActNum EQ "" THEN
        cError = "Account Number may not be spaces, try help..." .
      ELSE
        cError =  "Invalid GL#, try help..."  .
        RETURN .
    END.
  
/*RUN valid-ord-no NO-ERROR.*/

    

   /* FINISHED GOODS */
   IF INT(prmpoOrdNo) NE 0 AND
      prmpoItemType EQ "FG" THEN DO:

     FIND FIRST oe-ordl
         WHERE oe-ordl.company EQ cocode
           AND oe-ordl.ord-no  EQ INT(prmpoOrdNo)
           AND oe-ordl.i-no    EQ prmpoItemNo
         NO-LOCK NO-ERROR.

     IF NOT AVAIL oe-ordl THEN
     FOR EACH oe-ordl NO-LOCK
         WHERE oe-ordl.company EQ cocode
           AND oe-ordl.ord-no  EQ INT(prmpoOrdNo),
         FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ oe-ordl.company
           AND itemfg.i-no    EQ oe-ordl.i-no
           AND itemfg.isaset:
       RUN fg/fullset.p (ROWID(itemfg)).
       IF CAN-FIND(FIRST tt-fg-set
                   WHERE tt-fg-set.part-no EQ prmpoItemNo)
       THEN LEAVE.
     END.
     FOR EACH tt-fg-set:
       DELETE tt-fg-set.
     END.

     IF NOT AVAIL oe-ordl THEN DO:
       cError =  "FG Item# not on order, please try again..." .
       RETURN .
     END.

     IF NOT ll-order-warned THEN DO:
       FIND FIRST xpo-ordl
           WHERE xpo-ordl.company EQ cocode
             AND xpo-ordl.i-no    EQ prmpoItemNo
             AND xpo-ordl.ord-no  EQ INT(prmpoOrdNo)
             AND CAN-FIND(FIRST b-po-ord
                          WHERE b-po-ord.company EQ cocode
                            AND b-po-ord.po-no   EQ xpo-ordl.po-no)
             AND RECID(xpo-ordl)  NE RECID(po-ordl)               
           USE-INDEX item NO-LOCK NO-ERROR.

      /* ll-ans = NOT AVAIL xpo-ordl.*/

      /* IF NOT ll-ans THEN
       cError =  "Purchase order " +
               TRIM(STRING(xpo-ordl.po-no,">>>>>>>>")) +
               " already exists for order/item, continue?"
               VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans.

       IF NOT ll-ans THEN DO:
         APPLY "entry" TO po-ordl.ord-no.
         RETURN ERROR.
       END.

       ELSE*/
         ASSIGN
          ll-order-warned              = YES
          po-ordl.cust-no = oe-ordl.cust-no
          po-ordl.ord-no  = (oe-ordl.ord-no).
     END.
   END. /* FG END */
   ELSE DO: /* RAW MATERIALS */

       FIND FIRST oe-ord NO-LOCK
         WHERE oe-ord.company EQ cocode
           AND oe-ord.ord-no  EQ INT(prmpoOrdNo) NO-ERROR.
       IF AVAIL oe-ord 
         THEN
           ASSIGN
             ll-order-warned              = YES
             po-ordl.cust-no = oe-ord.cust-no
             po-ordl.ord-no  = (oe-ord.ord-no).

   END.





RUN valid-sheet-board-proc NO-ERROR.


END. /*validate update */






IF prmAction = "Update" THEN DO:

    FIND FIRST po-ordl WHERE 
       po-ordl.company eq cocode and 
           po-ordl.po-no EQ INT(prmpoNo)  and
           po-ordl.LINE EQ INT(prmpoLine)  exclusive-LOCK NO-ERROR.
     FIND FIRST po-ord WHERE                
        po-ord.company EQ cocode AND 
        po-ord.po-no EQ prmpoNo
        NO-LOCK NO-ERROR. 
ASSIGN ip-ord-no = po-ord.po-no .

 
  IF prmpoMsf = "Yes" THEN DO:

      IF prmpoItemType EQ "RM" OR
       NOT CAN-FIND(FIRST itemfg
                    WHERE itemfg.company EQ g_company
                      AND itemfg.i-no    EQ prmpoItemNo) THEN
    FIND FIRST item
        WHERE item.company EQ g_company
          AND item.i-no    EQ prmpoItemNo
        NO-LOCK NO-ERROR.

    IF prmpoItemType EQ "FG" OR
       NOT CAN-FIND(FIRST item
                    WHERE item.company EQ g_company
                      AND item.i-no    EQ prmpoItemNo) THEN
    FIND FIRST itemfg
        WHERE itemfg.company EQ g_company
          AND itemfg.i-no    EQ prmpoItemNo
        NO-LOCK NO-ERROR.

    IF prmpoItemNo EQ ""       OR
       (NOT AVAIL item AND NOT AVAIL itemfg) THEN DO:
      cError =  "Invalid item#, try help..."  .
      RETURN .
    END.

    IF prmpoItemType EQ "FG" AND  prmpoItemNo NE "" THEN DO:
        FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"
                              AND reftable.company  EQ cocode
                              AND reftable.loc      EQ ""
                              AND reftable.code     EQ prmpoItemNo
                              AND reftable.code2 = "I"
                            NO-LOCK NO-ERROR.
        IF AVAIL reftable THEN DO:
           cError =  prmpoItemNo + " has InActive Status. Purchase Order cannot be placed for the Inactive Item." .
           RETURN .
        END.     
    END.


  END.  /* validate item*/ 


    IF TRIM(prmpoJobNo) NE ""   OR
       (prmpoItemType EQ "RM" AND
        CAN-FIND(FIRST item
                 WHERE item.company EQ g_company
                   AND item.i-no    EQ prmpoItemNo
                   AND item.i-code  EQ "E"))  THEN DO:

      prmpoJobNo = FILL(" ",6 - LENGTH(TRIM(prmpoJobNo))) + TRIM(prmpoJobNo).

      IF TRIM(prmpoJobNo) EQ ""   OR
         NOT CAN-FIND(FIRST job-hdr
                      WHERE job-hdr.company EQ g_company
                        AND job-hdr.job-no  EQ prmpoJobNo) THEN DO:
        cError =  "Invalid Job, try help..."   .
        RETURN .
      END.
   END.

   IF prmpoJobNo NE "" THEN DO:
      prmpoJobNo = FILL(" ",6 - LENGTH(TRIM(prmpoJobNo))) + TRIM(prmpoJobNo).

      IF NOT CAN-FIND(FIRST job-hdr
                      WHERE job-hdr.company EQ g_company
                        AND job-hdr.job-no  EQ prmpoJobNo
                        AND job-hdr.job-no2 EQ INT(prmpoJobNo2))
      THEN DO:
        cError =  "Invalid Job2, try help..." . 
        RETURN .
      END.
   END.

 IF prmpoJobNo NE "" THEN DO:
      prmpoJobNo = FILL(" ",6 - LENGTH(TRIM(prmpoJobNo))) + TRIM(prmpoJobNo).

      IF prmpoSNum NE 0 THEN
             FIND FIRST job-mat
                      WHERE job-mat.company EQ cocode
                        AND job-mat.job-no  EQ prmpoJobNo
                        AND job-mat.job-no2 EQ INT(prmpoJobNo2)
                        AND job-mat.frm     EQ INT(prmpoSNum)
                        AND job-mat.rm-i-no  EQ prmpoItemNo  NO-LOCK NO-ERROR.
      IF NOT AVAIL job-mat THEN DO:
        cError =  "Invalid sheet for job..." .
        RETURN .
      END.
        
      FIND FIRST job-mat WHERE job-mat.company  EQ g_company
          AND job-mat.job-no   EQ prmpoJobNo
          AND job-mat.job-no2  EQ INT(prmpoJobNo2)
          AND job-mat.frm      EQ INT(prmpoSNum)
          AND job-mat.blank-no EQ INT(prmpoBNum)
          AND job-mat.rm-i-no  EQ prmpoItemNo     NO-LOCK NO-ERROR.
                           
       IF NOT AVAIL job-mat THEN do:
        cError = "Invalid sheet/blank/item for job".
        RETURN.
      END.
 END.
     

  IF CAN-FIND(FIRST xpo-ordl
                  WHERE xpo-ordl.company EQ g_company
                    AND xpo-ordl.po-no   EQ prmpoNo
                    AND xpo-ordl.job-no  EQ prmpoJobNo
                    AND xpo-ordl.job-no2 EQ INT(prmpoJobNo2)
                    AND xpo-ordl.i-no    EQ prmpoItemNo
                    AND xpo-ordl.s-num   EQ INT(prmpoSNum)
                    AND xpo-ordl.b-num   EQ INT(prmpoBNum)
                    AND RECID(xpo-ordl)  NE RECID(po-ordl))
      THEN DO:
      cError = "PO line item alreadys exists for this PO/Item/Job/Sheet/Blank".
      RETURN.
      END.

    RUN valid-uom ("pr-qty-uom") NO-ERROR.
    RUN valid-uom ("pr-uom") NO-ERROR.
  

   IF (prmpoActNum EQ "" OR
        NOT CAN-FIND(FIRST account
                     WHERE account.company EQ g_company
                       AND account.actnum  EQ prmpoActNum
                       AND account.TYPE <> "T"))  THEN DO:
      IF prmpoActNum EQ "" THEN
        cError = "Account Number may not be spaces, try help..." .
      ELSE
        cError =  "Invalid GL#, try help..."  .
        RETURN .
    END.
  
/*RUN valid-ord-no NO-ERROR.*/

   

   /* FINISHED GOODS */
   IF INT(prmpoOrdNo) NE 0 AND
      prmpoItemType EQ "FG" THEN DO:

     FIND FIRST oe-ordl
         WHERE oe-ordl.company EQ cocode
           AND oe-ordl.ord-no  EQ INT(prmpoOrdNo)
           AND oe-ordl.i-no    EQ prmpoItemNo
         NO-LOCK NO-ERROR.

     IF NOT AVAIL oe-ordl THEN
     FOR EACH oe-ordl NO-LOCK
         WHERE oe-ordl.company EQ cocode
           AND oe-ordl.ord-no  EQ INT(prmpoOrdNo),
         FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ oe-ordl.company
           AND itemfg.i-no    EQ oe-ordl.i-no
           AND itemfg.isaset:
       RUN fg/fullset.p (ROWID(itemfg)).
       IF CAN-FIND(FIRST tt-fg-set
                   WHERE tt-fg-set.part-no EQ prmpoItemNo)
       THEN LEAVE.
     END.
     FOR EACH tt-fg-set:
       DELETE tt-fg-set.
     END.

     IF NOT AVAIL oe-ordl THEN DO:
       cError =  "FG Item# not on order, please try again..." .
       RETURN .
     END.

     IF NOT ll-order-warned THEN DO:
       FIND FIRST xpo-ordl
           WHERE xpo-ordl.company EQ cocode
             AND xpo-ordl.i-no    EQ prmpoItemNo
             AND xpo-ordl.ord-no  EQ INT(prmpoOrdNo)
             AND CAN-FIND(FIRST b-po-ord
                          WHERE b-po-ord.company EQ cocode
                            AND b-po-ord.po-no   EQ xpo-ordl.po-no)
             AND RECID(xpo-ordl)  NE RECID(po-ordl)               
           USE-INDEX item NO-LOCK NO-ERROR.

      /* ll-ans = NOT AVAIL xpo-ordl.*/

      /* IF NOT ll-ans THEN
       cError =  "Purchase order " +
               TRIM(STRING(xpo-ordl.po-no,">>>>>>>>")) +
               " already exists for order/item, continue?"
               VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans.

       IF NOT ll-ans THEN DO:
         APPLY "entry" TO po-ordl.ord-no.
         RETURN ERROR.
       END.

       ELSE*/
         ASSIGN
          ll-order-warned              = YES
          po-ordl.cust-no = oe-ordl.cust-no
          po-ordl.ord-no  = (oe-ordl.ord-no).
     END.
   END. /* FG END */
   ELSE DO: /* RAW MATERIALS */

       FIND FIRST oe-ord NO-LOCK
         WHERE oe-ord.company EQ cocode
           AND oe-ord.ord-no  EQ INT(prmpoOrdNo) NO-ERROR.
       IF AVAIL oe-ord 
         THEN
           ASSIGN
             ll-order-warned              = YES
             po-ordl.cust-no = oe-ord.cust-no
             po-ordl.ord-no  = (oe-ord.ord-no).

   END.





RUN valid-sheet-board-proc NO-ERROR.


END. /*validate update */   


IF prmAction = "update" THEN DO:
      IF prmpoJobNo = "" THEN
          ASSIGN
             prmpoJobNo2 = 0
             prmpoSNum   = 0
             prmpoBNum   = 0.

         FIND FIRST po-ordl WHERE 
       po-ordl.company eq cocode and 
           po-ordl.po-no EQ INT(prmpoNo)  and
           po-ordl.LINE EQ INT(prmpoLine)  EXCLUSIVE-LOCK NO-ERROR.
       FIND FIRST po-ord WHERE                
        po-ord.company EQ po-ordl.company AND 
        po-ord.po-no EQ po-ordl.po-no 
        exclusive-LOCK NO-ERROR. 

        IF prmpoMsf = "Yes" THEN DO:
             po-ordl.i-no             =   prmpoItemNo .
             ASSIGN po-ordl.item-type = IF prmpoItemType = "RM" THEN TRUE ELSE FALSE  .
        END.

       ASSIGN
           po-ordl.i-name           =   prmpoItemName     
           po-ordl.job-no           =   prmpoJobNo     
           po-ordl.job-no2          =   prmpoJobNo2    
           po-ordl.s-num            =   prmpoSNum  
           po-ordl.b-num            =   prmpoBNum     
           po-ordl.ord-qty          =   prmpoOrdQty    
           po-ordl.cost             =   prmpoCost      
           po-ordl.cust-no          =   prmpoCustNo    
           po-ordl.due-date         =   date(prmpoDate)
           po-ordl.cons-qty        =   prmpoConsQty
           po-ordl.pr-qty-uom       =   prmpoPrQtyUom   
           po-ordl.dscr[2]          =   prmpoDscr2  
           po-ordl.pr-uom           =   prmpoPrUom
           po-ordl.cons-cost        =   prmpoConsCost
           po-ordl.cons-uom         =   prmpoConsUom
           po-ordl.setup            =   prmpoSetup     
           po-ordl.s-wid            =   prmpoSwid  
           po-ordl.s-len            =   prmpoSlen   
           po-ordl.disc             =   prmpoDisc                
           po-ordl.actnum           =   prmpoActNum     
           po-ordl.vend-i-no        =   prmpoVendINo      
          /* po-ordl.tax              =   LOG(prmpoTax)     */
           po-ordl.over-pct         =   prmpoOverPct      
           po-ordl.under-pct        =   prmpoUnderPct    
           po-ordl.ord-no           =   prmpoOrdNo       
           po-ordl.t-cost           =   prmpoTCost  .
       
           ASSIGN
               po-ordl.tax = IF prmpoTax = "True" THEN TRUE ELSE FALSE . 

        RUN update-shipto.

        RUN po/poordlup.p (RECID(po-ordl), -1, YES).
        
         lv-save-ord-no = po-ordl.ord-no.
    
        ASSIGN po-ord.po-change-date = today.
        IF po-ordl.stat NE "C" THEN DO:
            /*po-ordl.stat:SCREEN-VALUE   = "U".*/
            IF INDEX("HN", po-ord.stat) EQ 0 THEN po-ord.stat = "O".
        END.

          /* {po/calc10.i po-ordl.s-wid}
           {po/calc10.i po-ordl.s-len}*/
          /* v-dep = DEC(v-po-dep:SCREEN-VALUE)
           {po/calc10.i v-dep}.*/

           /* wfk - to make sure cons-qty was being updated */
           /*{po/podisdet.i}*/
      IF TRIM(po-ordl.job-no) EQ "" THEN po-ordl.job-no2 = 0.

      FIND FIRST reftable WHERE
          reftable.reftable EQ "POORDLDEPTH" AND
          reftable.company  EQ cocode AND
          reftable.loc      EQ STRING(ip-ord-no) AND
          reftable.code     EQ STRING(po-ordl.LINE)
          EXCLUSIVE-LOCK NO-ERROR.

      IF NOT AVAIL reftable THEN DO:
          CREATE reftable.
          ASSIGN
              reftable.reftable = "POORDLDEPTH"
              reftable.company  = cocode 
              reftable.loc      = STRING(ip-ord-no)
              reftable.code     = STRING(po-ordl.LINE).
      END.

      reftable.code2 = STRING(v-dep).
      RELEASE reftable.
        
      RUN vend-cost (NO).

     FOR EACH b-po-ordl
      WHERE b-po-ordl.company EQ po-ordl.company
        AND b-po-ordl.po-no   EQ po-ordl.po-no
        AND (b-po-ordl.line   LT 1 OR
             b-po-ordl.line   GE 99999999):
     DELETE b-po-ordl.
   END.

   RUN po/updordpo.p (BUFFER po-ordl).

  RUN po/po-total.p (RECID(po-ord)).

  RUN po/poordlup.p (RECID(po-ordl), 1, YES).



ASSIGN prmAction = "View" .



END. /* end update   */

IF prmAction = "Addnewline" THEN DO:

    find FIRST po-ord
      where po-ord.company = g_company 
        and po-ord.po-no = prmpoNo
      no-lock no-error.

  if avail po-ord then DO :
    
    find last po-ordl WHERE
         po-ordl.company EQ po-ord.company AND
         po-ordl.po-no EQ po-ord.po-no
         NO-LOCK no-error.

    v-line = if avail po-ordl then po-ordl.line + 1 else 1.
   
    create po-ordl.
    /*ASSIGN lv-item-recid = recid(po-ordl)
           ll-new-record = yes.*/
    find first vend WHERE vend.company = po-ord.company and vend.vend-no = po-ord.vend-no
                    NO-LOCK NO-ERROR.

    assign
     po-ordl.company   = cocode
     po-ordl.po-no     = po-ord.po-no
     po-ordl.stat = "O"
     po-ordl.ord-qty = 1
     po-ordl.cons-qty = 1
     po-ordl.line      = v-line
     po-ordl.due-date  = po-ord.due-date
     po-ordl.over-pct = po-ord.over-pct
     po-ordl.under-pct = po-ord.under-pct
     po-ordl.vend-no   = po-ord.vend-no.

   /* IF AVAIL bf-itemfg 
      THEN
        ASSIGN
          po-ordl.pr-qty-uom = IF pouom-chr EQ "Purchase" 
                                 THEN bf-itemfg.pur-uom
                                 ELSE bf-itemfg.prod-uom
          po-ordl.pr-uom     = IF AVAIL bf-e-itemfg-vend
                                 THEN bf-e-itemfg-vend.std-uom
                                 ELSE bf-itemfg.prod-uom.*/

    if avail vend then do:
      assign
       po-ordl.disc = vend.disc-%
       po-ordl.tax  = vend.tax-gr NE "" AND aptax-chr EQ "Vendor".
     
     /*IF v-default-gl-log AND INDEX(v-default-gl-cha,"Vend") GT 0 THEN */
         ASSIGN po-ordl.actnum = vend.actnum.
       
    end.

    IF po-ord.printed OR po-ord.stat NE "N" THEN po-ordl.stat = "A".
  end. /* avail po-ord */
  ASSIGN prmAction = "View"
         prmpoLine = v-line .
  


END.  /* end of addnewline  */

IF prmAction =  "deletepoline" THEN DO:
    
    RELEASE po-ordl.

    FIND FIRST po-ord WHERE                
        po-ord.company EQ prmComp AND 
        po-ord.po-no EQ prmpoNo 
        NO-LOCK NO-ERROR. 

    FIND FIRST po-ordl WHERE 
       po-ordl.company eq cocode and 
           po-ordl.po-no EQ INT(prmpoNo)  and
           po-ordl.LINE EQ INT(prmpoRecKey)  EXCLUSIVE-LOCK NO-ERROR.
      
    if avail po-ordl then delete po-ordl.

      find last po-ordl WHERE
         po-ordl.company EQ po-ord.company AND
         po-ordl.po-no EQ po-ord.po-no
         NO-LOCK no-error.
     IF AVAIL po-ordl THEN
         ASSIGN
         prmpoLine = po-ordl.LINE .
     ASSIGN prmAction = "View" .

END.  /*delete po line */

 IF prmAction = "View" THEN DO:
   
     RELEASE po-ord.
    FIND FIRST bf-po-ordl WHERE 
       bf-po-ordl.company eq cocode and 
           bf-po-ordl.po-no EQ INT(prmpoNo)  and
           bf-po-ordl.LINE EQ INT(prmpoLine)  NO-LOCK NO-ERROR.
       FIND FIRST po-ord WHERE                
        po-ord.company EQ bf-po-ordl.company AND 
        po-ord.po-no EQ bf-po-ordl.po-no 
        NO-LOCK NO-ERROR. 
 
            create ttb_po_item_inq_view.
            assign
                ttb_po_item_inq_view.poLine         = bf-po-ordl.LINE
                ttb_po_item_inq_view.poNo           = po-ord.po-no
                ttb_po_item_inq_view.poDate         = STRING(bf-po-ordl.due-date)
                ttb_po_item_inq_view.poLoc          = po-ord.loc
                ttb_po_item_inq_view.poType         = po-ord.type
                ttb_po_item_inq_view.poStat         = po-ord.stat
                ttb_po_item_inq_view.poItemNo       = bf-po-ordl.i-no
                ttb_po_item_inq_view.poItemName     = bf-po-ordl.i-name
                ttb_po_item_inq_view.poJobNo        = bf-po-ordl.job-no
                ttb_po_item_inq_view.poJobNo2       = bf-po-ordl.job-no2
                ttb_po_item_inq_view.poSNum         = bf-po-ordl.s-num
                ttb_po_item_inq_view.poBNum         = bf-po-ordl.b-num
                ttb_po_item_inq_view.poOrdQty       = bf-po-ordl.ord-qty
                ttb_po_item_inq_view.poCost         = bf-po-ordl.cost
                ttb_po_item_inq_view.poCustNo       = bf-po-ordl.cust-no
                ttb_po_item_inq_view.poDueDate      = STRING(bf-po-ordl.due-date)
                ttb_po_item_inq_view.poItemType     = IF bf-po-ordl.item-type = TRUE THEN "RM" ELSE "FG"
                ttb_po_item_inq_view.poPrQtyUom     = bf-po-ordl.pr-qty-uom
                ttb_po_item_inq_view.poConsQty      = bf-po-ordl.cons-qty
                ttb_po_item_inq_view.poDscr[1]      = bf-po-ordl.dscr[1]
                ttb_po_item_inq_view.poPrUom        = bf-po-ordl.pr-uom
                ttb_po_item_inq_view.poConsCost     = bf-po-ordl.cons-cost
                ttb_po_item_inq_view.poConsUom      = bf-po-ordl.cons-uom
                ttb_po_item_inq_view.poDscr[2]      = bf-po-ordl.dscr[2] 
                ttb_po_item_inq_view.poSetup        = bf-po-ordl.setup
                ttb_po_item_inq_view.poSwid         = bf-po-ordl.s-wid
                ttb_po_item_inq_view.poSlen         = bf-po-ordl.s-len
                ttb_po_item_inq_view.poDisc         = bf-po-ordl.disc
                ttb_po_item_inq_view.poActNum       = bf-po-ordl.actnum
                ttb_po_item_inq_view.poVendINo      = bf-po-ordl.vend-i-no
                ttb_po_item_inq_view.poTax          = IF bf-po-ordl.tax = TRUE THEN "TRUE" ELSE "FALSE"
                ttb_po_item_inq_view.poOverPct      = bf-po-ordl.over-pct
                ttb_po_item_inq_view.poUnderPct     = bf-po-ordl.under-pct
                ttb_po_item_inq_view.poCustNo       = bf-po-ordl.cust-no
                ttb_po_item_inq_view.poOrdNo        = bf-po-ordl.ord-no
                ttb_po_item_inq_view.poTCost        = bf-po-ordl.t-cost
                ttb_po_item_inq_view.poRecKey       = bf-po-ordl.rec_key
                ttb_po_item_inq_view.poScrConsUom   = bf-po-ordl.cons-uom   .
                    
                /*ttb_po_item_inq_view.poTonnage      = 
                ttb_po_item_inq_view.poaddress      = */
                
                      

              IF AVAIL bf-po-ordl THEN
                   IF bf-po-ordl.item-type THEN DO:
                       
                       FIND FIRST item WHERE item.company = g_company AND
                           item.i-no = bf-po-ordl.i-no NO-LOCK NO-ERROR .
                     
                       IF AVAIL item THEN RUN rm-qtys (ROWID(item)).
               END. /* item-type eq yes (RM) */
               ELSE DO:
                   FIND FIRST itemfg WHERE itemfg.company = g_company AND
                       itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
                   IF AVAIL itemfg THEN RUN fg-qtys (ROWID(itemfg)).
               END.
               
               ASSIGN
               v-wid = bf-po-ordl.s-wid
               v-len = bf-po-ordl.s-len
               {po/calc16.i v-wid}
               {po/calc16.i v-len}
               ttb_po_item_inq_view.poSwid  = (v-wid)
               ttb_po_item_inq_view.poSlen  = (v-len) .
               ASSIGN
                ttb_po_item_inq_view.poFiUom        = fi_uom
                ttb_po_item_inq_view.poPbQty        = fi_pb-qty
                ttb_po_item_inq_view.poPbCst        = fi_pb-cst
                ttb_po_item_inq_view.poQOnh         = fi_q-onh  
                ttb_po_item_inq_view.poQOno         = fi_q-ono  
                ttb_po_item_inq_view.poQComm        = fi_q-comm 
                ttb_po_item_inq_view.poQBack        = fi_q-back 
                ttb_po_item_inq_view.poQAvail       = fi_q-avail
                ttb_po_item_inq_view.poMOnh         = fi_m-onh  
                ttb_po_item_inq_view.poMOno         = fi_m-ono  
                ttb_po_item_inq_view.poMComm        = fi_m-comm 
                ttb_po_item_inq_view.poMBack        = fi_m-back 
                ttb_po_item_inq_view.poMAvail       = fi_m-avail
                ttb_po_item_inq_view.poMsf          = fi_msf 
                ttb_po_item_inq_view.ficahdr        = fi_c-a-hdr .


               FIND FIRST reftable WHERE
                   reftable.reftable EQ "POORDLDEPTH" AND
                   reftable.company  EQ cocode AND
                   reftable.loc      EQ STRING(po-ord.po-no) AND
                   reftable.code     EQ STRING(bf-po-ordl.LINE)
                   NO-LOCK NO-ERROR.
               IF AVAILABLE reftable THEN DO:
                   ASSIGN
                       v-dep = DEC(reftable.code2)
                       {po/calc16.i v-dep}
                       ttb_po_item_inq_view.poSDep     = (v-dep).
                   RELEASE reftable.
               END.

                RUN sys\inc\decfrac2.p(INPUT DEC(bf-po-ordl.s-wid), INPUT 32, OUTPUT v-wid-frac).
                RUN sys\inc\decfrac2.p(INPUT DEC(bf-po-ordl.s-len), INPUT 32, OUTPUT v-len-frac).
                RUN sys\inc\decfrac2.p(INPUT DEC(v-po-dep), INPUT 32, OUTPUT v-dep-frac).
                ASSIGN
                    ttb_po_item_inq_view.poWidFrac = v-wid-frac
                    ttb_po_item_inq_view.poLenFrac = v-len-frac
                    ttb_po_item_inq_view.poDepFrac = v-dep-frac.
        find first account
        where account.company eq cocode
          and account.actnum  eq bf-po-ordl.actnum no-lock no-error.
         ttb_po_item_inq_view.poGlDesc  = if avail account then account.dscr else ''. 

            
            RUN display-msf.
                
             ASSIGN ttb_po_item_inq_view.poTMsf    = v-tot-msf.

              FIND FIRST buf-item WHERE
                  buf-item.company EQ cocode AND
                  buf-item.i-no EQ bf-po-ordl.i-no
                  NO-LOCK NO-ERROR.
              IF AVAIL buf-item THEN ASSIGN
              ttb_po_item_inq_view.poTonnage = ROUND((v-tot-msf * buf-item.basis-w / 2000),4).
         
              RELEASE po-ordl .
        
         FIND FIRST po-ordl WHERE 
       po-ordl.company eq cocode and 
           po-ordl.po-no EQ INT(bf-po-ordl.po-no)  and
           po-ordl.LINE EQ INT(bf-po-ordl.LINE)  NO-LOCK NO-ERROR.
              
         RUN vend-cost (?).
                IF addersText NE "" THEN
                ttb_po_item_inq_view.poaddress =  "Adder Charges        "
                         /*+ FILL(" ",12 - LENGTH("Cost/" + po-ordl.pr-uom:SCREEN-VALUE))*/
                         + "Cost/" + po-ordl.pr-uom
                         + CHR(10) + "=============================="
                         + CHR(10) + addersText  .
                RELEASE po-ordl .
   
 END. /* end search */


 PROCEDURE display-msf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* po/podisdet.i */

  DEF VAR v-out-qty AS DEC NO-UNDO.
  DEF VAR lv-cons-qty AS DEC NO-UNDO.
  DEF VAR lv-cons-cost AS DEC NO-UNDO.
  DEF VAR lv-tot-msf LIKE v-tot-msf NO-UNDO.
 
    {ce/msfcalc.i}
  v-tot-msf = 0.

  FOR EACH po-ordl WHERE
      po-ordl.company EQ po-ord.company AND
      po-ordl.po-no   EQ po-ord.po-no
      NO-LOCK:

  FIND FIRST ITEM WHERE ITEM.company = cocode AND ITEM.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
  ASSIGN
    v-basis-w = IF AVAIL ITEM THEN ITEM.basis-w ELSE v-basis-w
    v-dep = IF AVAIL ITEM THEN ITEM.s-dep ELSE v-dep
   
    v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     v-ord-qty = po-ordl.ord-qty
    {po/calc10.i v-len}
    {po/calc10.i v-wid}.

  IF po-ordl.pr-qty-uom = "EA" THEN
     lv-tot-msf = IF v-corr THEN ((v-len * v-wid * .007 * po-ordl.ord-qty) / 1000)
                           ELSE ((((v-len * v-wid) / 144) * po-ordl.ord-qty) / 1000).
  else do:
                  /*convert whatever the UOM is into "EACH" first*/
      
                    lv-tot-msf = 0.
                  if po-ordl.pr-qty-uom ne "EA" then do:
                        lv-tot-msf = 0.
                        run sys/ref/convquom.p(po-ordl.pr-qty-uom,
                                               "EA",
                                               v-basis-w,
                                               v-len,
                                               v-wid,
                                               v-dep,
                                               v-ord-qty,
                                               output v-out-qty).

                        /*now convert from "EACH" into MSF*/   
                        lv-tot-msf = if v-corr
                          then
                            ((v-len * v-wid * .007 * v-out-qty) / 1000)
                          else
                            ((((v-len * v-wid) / 144) * v-out-qty) / 1000).
                       IF po-ordl.pr-qty-uom EQ "ROLL" THEN
                         lv-tot-msf = lv-tot-msf * (12 / v-len).
                  end. 
  end.
      
  v-tot-msf = v-tot-msf + round(lv-tot-msf,3).
  END. /* each po-ordl */
 

END PROCEDURE.

PROCEDURE update-shipto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR ll-choice AS LOG NO-UNDO.
   DEF VAR lv-stat AS CHAR NO-UNDO.


   
     IF prmpoItemType = "FG" THEN DO:
     
        FIND FIRST xpo-ord WHERE
             xpo-ord.company EQ g_company AND
             xpo-ord.po-no   EQ ip-ord-no
             NO-LOCK NO-ERROR.
     
        IF AVAIL xpo-ord AND
           xpo-ord.TYPE = "D" THEN DO:
          
          FOR EACH oe-rel FIELDS(company ord-no i-no ship-id) WHERE
            oe-rel.company EQ g_company AND
            oe-rel.ord-no = INT(prmpoOrdNo) AND
            oe-rel.i-no = prmpoItemNo
            NO-LOCK:
     
            RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
     
            IF LOOKUP(lv-stat,"S,I,L") = 0 THEN NEXT.
     
            /*IF oe-rel.ship-id NE xpo-ord.ship-id THEN
              MESSAGE "PO Shipto does not match Shipto on Order Release." SKIP
                      "Update Shipto?"
                  VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-choice.*/
            ASSIGN ll-choice = YES.
            
            IF ll-choice THEN DO:
     
              find first shipto where shipto.company eq g_company AND
                   shipto.cust-no eq xpo-ord.cust-no AND
                   shipto.ship-id eq oe-rel.ship-id
                   no-lock no-error.
           
              IF AVAILABLE shipto THEN DO:
              
                FIND FIRST xpo-ord WHERE
                  xpo-ord.company EQ g_company AND
                  xpo-ord.po-no   EQ ip-ord-no
                  EXCLUSIVE-LOCK NO-ERROR.
     
                IF AVAILABLE xpo-ord THEN DO:
                
                  ASSIGN xpo-ord.ship-id = oe-rel.ship-id
                         xpo-ord.ship-name = shipto.ship-name
                         xpo-ord.ship-addr[1] = shipto.ship-addr[1]
                         xpo-ord.ship-addr[2] = shipto.ship-addr[2]
                         xpo-ord.ship-city = shipto.ship-city
                         xpo-ord.ship-state = shipto.ship-state
                         xpo-ord.ship-zip = shipto.ship-zip
                         xpo-ord.ship-no = shipto.ship-no.
     
                  FIND FIRST xpo-ord WHERE
                    xpo-ord.company EQ g_company AND
                    xpo-ord.po-no   EQ ip-ord-no
                    NO-LOCK NO-ERROR.
                END.
                
                RELEASE shipto.
              END.
            END.
     
            LEAVE.
          END.
     
          RELEASE xpo-ord.
        END.
     END.
   
END PROCEDURE.

PROCEDURE fg-qtys :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


  FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL itemfg THEN DO:
    ASSIGN
     fi_c-a-hdr = "Allocated"
     fi_uom     = "EA"
     fi_q-onh   = itemfg.q-onh
     fi_q-ono   = itemfg.q-ono
     fi_q-comm  = itemfg.q-alloc
     fi_q-back  = itemfg.q-back
     fi_q-avail = itemfg.q-avail.
   
  END.

END PROCEDURE.


PROCEDURE rm-qtys :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO.


  FIND item WHERE ROWID(item) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL item THEN DO :
    ASSIGN
     fi_c-a-hdr = "Committed"
     fi_uom     = item.cons-uom
     fi_q-onh   = item.q-onh
     fi_q-ono   = item.q-ono
     fi_q-comm  = item.q-comm
     fi_q-back  = item.q-back
     fi_q-avail = item.q-avail.

    IF AVAIL ITEM AND item.i-code EQ "R"   AND
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
          WHEN 1 THEN fi_m-onh   = ld.
          WHEN 2 THEN fi_m-ono   = ld.
          WHEN 3 THEN fi_m-comm  = ld.
          WHEN 4 THEN fi_m-back  = ld.
          WHEN 5 THEN fi_m-avail = ld.
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
        WHERE job.company EQ po-ordl.company
          AND job.job-no  EQ po-ordl.job-no
          AND job.job-no2 EQ INT(po-ordl.job-no2)
        NO-ERROR.
    IF AVAIL job THEN
    FIND FIRST job-mat NO-LOCK
        WHERE job-mat.company  EQ job.company
          AND job-mat.job      EQ job.job
          AND job-mat.job-no   EQ job.job-no
          AND job-mat.job-no2  EQ job.job-no2
          AND job-mat.frm      EQ INT(po-ordl.s-num)
          AND job-mat.blank-no EQ INT(po-ordl.b-num) 
        USE-INDEX seq-idx NO-ERROR.
        
    IF AVAIL job-mat THEN lv-recid = RECID(job-mat).

    v-ord-qty = DEC(po-ordl.ord-qty).

    IF prmpoItemType EQ "RM" THEN DO:
       FIND FIRST e-item NO-LOCK
           WHERE e-item.company EQ cocode
             AND e-item.i-no    EQ po-ordl.i-no
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
            AND e-itemfg.i-no    EQ po-ordl.i-no
          NO-ERROR.

      IF AVAIL e-itemfg THEN DO:
        CREATE tt-ei.
        ASSIGN tt-ei.std-uom = e-itemfg.std-uom.

        IF po-ordl.cust-no NE "" THEN
           FIND FIRST e-itemfg-vend NO-LOCK
               WHERE e-itemfg-vend.company EQ e-itemfg.company
                 AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                 AND e-itemfg-vend.vend-no EQ po-ord.vend-no
                 AND e-itemfg-vend.cust-no EQ po-ordl.cust-no
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
       v-cost = DEC(po-ordl.cost)
       v-qty  = DEC(po-ordl.ord-qty).

      IF tt-ei.std-uom NE po-ordl.pr-qty-uom          AND
        (po-ordl.item-type                                        OR
         LOOKUP(tt-ei.std-uom,fg-uom-list)                  EQ 0 OR
         LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0)  THEN
        RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
                               tt-ei.std-uom, v-basis-w,
                               v-len, v-wid, v-dep,
                               v-qty, OUTPUT v-qty).

      v-save-qty = v-qty.
      IF po-ordl.job-no NE "" THEN
        RUN po/groupcst.p (po-ordl.job-no,
                           INT(po-ordl.job-no2),
                           po-ordl.i-no,
                           INT(po-ordl.s-num),
                           INT(po-ordl.b-num),
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

        IF tt-ei.std-uom NE po-ordl.pr-qty-uom           AND
           (po-ordl.item-type                                        OR
            LOOKUP(tt-ei.std-uom,fg-uom-list)                  EQ 0 OR
            LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0)  THEN
          RUN sys/ref/convquom.p(tt-ei.std-uom,
                                 po-ordl.pr-qty-uom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-pb-qty, OUTPUT v-pb-qty).

        IF tt-ei.std-uom NE po-ordl.pr-uom           AND
           (po-ordl.item-type                                    OR
            LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
            LOOKUP(po-ordl.pr-uom,fg-uom-list) EQ 0)  THEN
          RUN sys/ref/convcuom.p(tt-ei.std-uom,
                                 po-ordl.pr-uom, v-basis-w,
                                 v-len, v-wid, v-dep,
                                 v-pb-cst, OUTPUT v-pb-cst).

        IF po-ordl.pr-uom NE po-ordl.cons-uom AND
           (po-ordl.item-type                                      OR
            LOOKUP(po-ordl.pr-uom,fg-uom-list)   EQ 0 OR
            LOOKUP(po-ordl.cons-uom,fg-uom-list) EQ 0)     THEN
          RUN sys/ref/convcuom.p(po-ordl.pr-uom,
                                 po-ordl.cons-uom, v-basis-w,
                                 v-len, v-wid, v-dep,
                                 v-pb-cst, OUTPUT v-pb-cns).

        /*fi_pb-qty:SCREEN-VALUE = IF v-pb-qty LE 0 THEN "" ELSE STRING(v-pb-qty).*/
      END.

      IF v-qty <> 0 THEN v-cost = (v-cost /*+ v-setup*/) / v-qty.  
      ELSE v-cost = (v-cost /*+ v-setup*/).

      IF ip-calc-cost NE ? THEN DO:
        IF ip-calc-cost THEN DO:            
          IF tt-ei.std-uom NE po-ordl.pr-uom           AND
             (po-ordl.item-type                                    OR
              LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
              LOOKUP(po-ordl.pr-uom,fg-uom-list) EQ 0)  THEN
            RUN sys/ref/convcuom.p(tt-ei.std-uom,
                                   po-ordl.pr-uom, v-basis-w,
                                   (IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE v-len),
                                   v-wid, v-dep,
                                   v-cost, OUTPUT v-cost).
          ASSIGN
            ip-calc-cost = YES
            po-ordl.cost = (v-cost)
            po-ordl.setup = (v-setup).

          IF po-ordl.pr-uom NE po-ordl.cons-uom AND
             (po-ordl.item-type                                      OR
              LOOKUP(po-ordl.pr-uom,fg-uom-list)   EQ 0 OR
              LOOKUP(po-ordl.cons-uom,fg-uom-list) EQ 0)     THEN
            RUN sys/ref/convcuom.p(po-ordl.pr-uom,
                                   po-ordl.cons-uom, v-basis-w,
                                   (IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE v-len),
                                   v-wid, v-dep,
                                   v-cost, OUTPUT v-cost).

          po-ordl.cons-cost = (v-cost).     
          
        END.

        ELSE
        IF v-hold-op1 AND po-ord.stat NE "H" THEN DO:
          IF tt-ei.std-uom NE po-ordl.pr-uom           AND
             (po-ordl.item-type                                    OR
              LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
              LOOKUP(po-ordl.pr-uom,fg-uom-list) EQ 0)  THEN
            RUN sys/ref/convcuom.p(tt-ei.std-uom,
                                   po-ordl.pr-uom, v-basis-w,
                                   v-len, v-wid, v-dep,
                                   v-cost, OUTPUT v-cost).              
          IF AVAIL job-mat THEN
            RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                           DEC(po-ordl.ord-qty),
                           v-cost,
                           DEC(po-ordl.cons-cost),
                           OUTPUT v-cost,
                           OUTPUT lv-added-cons-cost,
                           OUTPUT lv-adder-setup).

          IF DEC(po-ordl.cost) GT v-cost THEN DO:
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
                       DEC(0),
                       v-pb-cst,
                       v-pb-cns,
                       OUTPUT v-pb-cst,
                       OUTPUT v-pb-cns,
                       OUTPUT lv-adder-setup).

      RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                     DEC(po-ordl.ord-qty),
                     DEC(po-ordl.cost),
                     DEC(po-ordl.cons-cost),
                     OUTPUT lv-added-cost,
                     OUTPUT lv-added-cons-cost,
                     OUTPUT lv-adder-setup).

      /*IF ip-calc-cost THEN
        ASSIGN
         po-ordl.cost:SCREEN-VALUE = STRING(lv-added-cost)
         po-ordl.cons-cost:SCREEN-VALUE = STRING(lv-added-cons-cost).*/
    END.

    IF poqty-log THEN DO:
      IF CAN-DO("L,LOT",po-ordl.pr-uom) THEN
        lv-t-cost = (v-pb-cst + v-pb-stp) *
                    IF po-ordl.ord-qty LT 0 THEN -1 ELSE 1.

      ELSE DO:
        v-ord-qty = DEC(fi_pb-qty).

        IF po-ordl.pr-qty-uom NE po-ordl.pr-uom AND
           (po-ordl.item-type                                        OR
            LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0 OR
            LOOKUP(po-ordl.pr-uom,fg-uom-list)     EQ 0)     THEN
   
          RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
                                 po-ordl.pr-uom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-ord-qty, OUTPUT v-ord-qty).
     
        lv-t-cost = (v-ord-qty * v-pb-cst) + v-pb-stp.
      END.

      IF DEC(po-ordl.disc) NE 0 THEN
        lv-t-cost = lv-t-cost * (1 - (DEC(po-ordl.disc) / 100)).

    /*  fi_pb-cst:SCREEN-VALUE = STRING(lv-t-cost).

      IF DEC(fi_pb-cst:SCREEN-VALUE) LE 0 THEN fi_pb-cst:SCREEN-VALUE = "".*/
    END.

    IF ip-calc-cost NE ? THEN DO:
      IF CAN-DO("L,LOT",po-ordl.pr-uom) THEN
        lv-t-cost = (DEC(po-ordl.cost) +
                     DEC(po-ordl.setup)) *
                    IF po-ordl.ord-qty LT 0 THEN -1 ELSE 1.

      ELSE DO:
        v-ord-qty = DEC(po-ordl.ord-qty).

        IF po-ordl.pr-qty-uom NE po-ordl.pr-uom AND
           (po-ordl.item-type                                        OR
            LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0 OR
            LOOKUP(po-ordl.pr-uom,fg-uom-list)     EQ 0)     THEN
   
          RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
                                 po-ordl.pr-uom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-ord-qty, OUTPUT v-ord-qty).
     
        lv-t-cost = (v-ord-qty * DEC(po-ordl.cost)) +
                    DEC(po-ordl.setup).
      END.

      IF DEC(po-ordl.disc) NE 0 THEN
         lv-t-cost = lv-t-cost * (1 - (DEC(po-ordl.disc) / 100)).

      po-ordl.t-cost = (lv-t-cost).
    END.
  /*END.  */

END PROCEDURE.



PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  PARAMs:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-field AS CHAR NO-UNDO.

  DEF VAR lv-uom LIKE uom.uom NO-UNDO.
  DEF VAR uom-list AS CHAR INIT "" NO-UNDO.


  
    RELEASE item.

    lv-uom = IF ip-field EQ "pr-uom" THEN prmpoPrUom
                                     ELSE prmpoPrQtyUom. 

    IF prmpoItemType EQ "RM" THEN
    FIND FIRST item
        WHERE item.company EQ g_company
          AND item.i-no    EQ prmpoItemNo
        NO-LOCK NO-ERROR.

    IF AVAIL item THEN RUN sys/ref/uom-rm.p (item.mat-type, OUTPUT uom-list).
    ELSE RUN sys/ref/uom-fg.p (NO, OUTPUT uom-list). /* for fgitem */

    IF uom-list EQ "" THEN
      uom-list = IF ip-field EQ "pr-uom" THEN pr-uom-list
                                         ELSE lv-uom-list.

    IF AVAIL item AND INDEX("MOXY789",ITEM.mat-type) GT 0 AND ip-field EQ "pr-uom" THEN
      uom-list = uom-list + ",L".

    IF LOOKUP(lv-uom,uom-list) LE 0 THEN DO:
      cError =  "UOM must be " + TRIM(uom-list) .
      RETURN .
    END.
  

END PROCEDURE.


PROCEDURE valid-sheet-board-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER b-po-ordl FOR po-ordl.
   DEF BUFFER b-item FOR ITEM.

   DEF VAR v-board-count AS INT NO-UNDO.

  
   
      IF po-ord.TYPE EQ "S" AND prmpoItemType = "RM" THEN
      DO:
         FIND FIRST b-item WHERE
              b-item.company EQ g_company AND
              b-item.i-no EQ prmpoItemNo AND
              b-item.mat-type = "B"
              NO-LOCK NO-ERROR.
     
         IF AVAIL b-item THEN
         DO:
            FOR EACH b-po-ordl FIELDS(i-no) WHERE
                b-po-ordl.company EQ g_company AND
                b-po-ordl.po-no EQ po-ord.po-no AND
                b-po-ordl.item-type EQ YES AND
                ROWID(b-po-ordl) NE ROWID(po-ordl)
                NO-LOCK,
                FIRST ITEM WHERE
                      ITEM.company EQ g_company AND
                      ITEM.i-no EQ b-po-ordl.i-no AND
                      ITEM.mat-type EQ "B"
                      NO-LOCK:
           
                v-board-count = v-board-count + 1.
            END.
           
            IF v-board-count GE 2 THEN
            DO:
               cError = "Maximum of 2 Board Items Allowed on Sheeting PO." .
               RETURN .
            END.
         END.
      END.
   
END PROCEDURE.



PROCEDURE set-dims :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  
    ASSIGN
     v-len = DEC(po-ordl.s-len)
     v-wid = DEC(po-ordl.s-wid)
     {po/calc10.i v-len}
     {po/calc10.i v-wid}.

    FIND FIRST ITEM
        WHERE item.company EQ cocode
          AND item.i-no    EQ po-ordl.i-no
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
        item.i-no     eq po-ordl.i-no
        NO-LOCK NO-ERROR.

   IF AVAIL ITEM AND
      ITEM.mat-type NE "B" THEN
      LEAVE.

   ASSIGN
      v-adder[1] = ip-cost
      v-adder[2] = ip-cons-cost.

  IF po-ordl.pr-uom EQ "EA"                    OR
     (NOT po-ordl.item-type AND
      LOOKUP(po-ordl.pr-uom,fg-uom-list) EQ 0) THEN
     v-tot-cost = ip-cost.

  ELSE
    RUN sys/ref/convcuom.p(po-ordl.pr-uom, "EA",
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
          and e-item.i-no    eq po-ordl.i-no
        no-error.
    
    find first e-item-vend no-lock
        where e-item-vend.company eq item.company
          and e-item-vend.i-no    eq item.i-no
          and e-item-vend.vend-no eq ip-vend-no
        no-error.

    if avail e-item and avail e-item-vend AND ip-vend-no NE "" then do:
      if po-ordl.pr-qty-uom eq e-item.std-uom then
           v-qty-comp = ip-qty.
      else
        run sys/ref/convquom.p(po-ordl.pr-qty-uom, e-item.std-uom,
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
DEF VAR i AS INT NO-UNDO.
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
      IF e-item.std-uom NE po-ordl.pr-uom THEN
        RUN sys/ref/convcuom.p(e-item.std-uom, po-ordl.pr-uom, job-mat.basis-w,
                               job-mat.len, job-mat.wid, item.s-dep,
                               v-cost, OUTPUT v-cost).
    END.

    ELSE DO:
      v-cost = job-mat.std-cost.
      
      IF job-mat.sc-uom NE po-ordl.pr-uom THEN
        RUN sys/ref/convcuom.p(job-mat.sc-uom, po-ordl.pr-uom, job-mat.basis-w,
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
    /*IF v-cost NE 0                         
      THEN RUN po-adder3 (INPUT v-cost).  */ 
    /* gdm - end */                                
  END.

  IF po-ordl.pr-uom NE "EA" THEN 
    RUN sys/ref/convcuom.p("EA", po-ordl.pr-uom,
                           v-basis-w, v-len, v-wid, v-dep,
                           v-tot-cost, OUTPUT v-tot-cost).
 
  op-cost = v-add-cost + v-tot-cost.

  IF po-ordl.pr-uom NE po-ordl.cons-uom THEN
    RUN sys/ref/convcuom.p(po-ordl.pr-uom, po-ordl.cons-uom,
                           v-basis-w, v-len, v-wid, v-dep,
                           ip-cost, OUTPUT op-cons-cost).

/*  display po-ordl.cost po-ordl.cons-cost.  */

assign
 v-adder[1] = op-cost      - v-adder[1]
 v-adder[2] = op-cons-cost - v-adder[2].


END PROCEDURE.
