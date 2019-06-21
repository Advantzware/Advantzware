

/*------------------------------------------------------------------------
    File        : listitempo.p
    Purpose     : 
    Syntax      :

    Description : Return a Dataset of Estimate Corrugated box
    Author(s)   : 
    Created     : 14 Jan 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttb_po_item_inqlist NO-UNDO
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
        FIELD poline          AS INT
        FIELD poRecKey        AS CHAR
        .

DEFINE DATASET dsb_po_item_inqlist FOR ttb_po_item_inqlist .

DEFINE INPUT PARAMETER prmUser         AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmAction       AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpoNo         AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmpoItemNo     AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmpoRecKey     AS CHAR         NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsb_po_item_inqlist.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.

IF prmUser          = ? THEN ASSIGN prmUser        = "".
IF prmAction        = ? THEN ASSIGN prmAction      = "Select".
IF prmpoNo          = ? THEN ASSIGN prmpoNo        = 0.
IF prmpoItemNo    = ? THEN ASSIGN prmpoItemNo  = "".
IF prmpoRecKey      = ? THEN ASSIGN prmpoRecKey    = "".

DEF NEW SHARED VAR v-basis-w AS DEC NO-UNDO. 
DEF NEW SHARED VAR v-len LIKE po-ordl.s-len NO-UNDO.
DEF NEW SHARED VAR v-wid LIKE po-ordl.s-wid NO-UNDO.
DEF NEW SHARED VAR v-dep LIKE po-ordl.s-len NO-UNDO.
DEF VAR v-tot-msf AS DEC NO-UNDO.
def NEW shared var factor# as decimal no-undo.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp .


 MESSAGE "jhgkjh  " prmpoNo prmpoItemNo .
 IF prmAction = "Search" THEN DO:

 
    FOR EACH po-ordl WHERE 
       po-ordl.company eq cocode and 
           (po-ordl.po-no EQ INT(prmpoNo) ) and
           (po-ordl.i-no EQ STRING(prmpoItemNo) OR prmpoItemNo = "") NO-LOCK, 
        FIRST po-ord WHERE                
        po-ord.company EQ po-ordl.company AND 
        po-ord.po-no EQ po-ordl.po-no 
        NO-LOCK: 

            create ttb_po_item_inqlist.
            assign
                ttb_po_item_inqlist.poNo           = po-ord.po-no
                ttb_po_item_inqlist.poDate         = string(po-ord.po-date)
                ttb_po_item_inqlist.poLoc          = po-ord.loc
                ttb_po_item_inqlist.poType         = po-ord.type
                ttb_po_item_inqlist.poStat         = po-ord.stat
                ttb_po_item_inqlist.poItemNo       = po-ordl.i-no
                ttb_po_item_inqlist.poItemName     = po-ordl.i-name
                ttb_po_item_inqlist.poJobNo        = po-ordl.job-no
                ttb_po_item_inqlist.poJobNo2       = po-ordl.job-no2
                ttb_po_item_inqlist.poSNum         = po-ordl.s-num
                ttb_po_item_inqlist.poOrdQty       = po-ordl.ord-qty
                ttb_po_item_inqlist.poCost         = po-ordl.cost
                ttb_po_item_inqlist.poCustNo       = po-ordl.cust-no
                ttb_po_item_inqlist.poDueDate      = STRING(po-ordl.due-date)
                ttb_po_item_inqlist.poItemType     = STRING(po-ordl.item-type) 
                ttb_po_item_inqlist.poRecKey       = po-ordl.rec_key  
                ttb_po_item_inqlist.poline       = po-ordl.LINE
                 .
           
            RUN display-msf.
                
             ASSIGN ttb_po_item_inqlist.poTMsf    = v-tot-msf.

    END.   /* end of for loop*/
            
            
 END. /* end search */

 IF prmAction = "Select" THEN DO:
     MESSAGE "prmpoNo  " prmpoNo .
     FIND FIRST po-ord WHERE                
        po-ord.company EQ cocode AND 
        po-ord.po-no EQ INT(prmpoNo) NO-LOCK NO-ERROR.
         
     create ttb_po_item_inqlist.
            assign
                ttb_po_item_inqlist.poNo           = po-ord.po-no
                ttb_po_item_inqlist.poDate         = string(po-ord.po-date)
                ttb_po_item_inqlist.poLoc          = po-ord.loc
                ttb_po_item_inqlist.poType         = po-ord.type
                ttb_po_item_inqlist.poStat         = po-ord.stat .
     
 END.


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
  DEF VAR v-ord-qty AS dec NO-UNDO.
  def var lv-uom-list as cha init "C,CS,EA,L,LB,LF,LOT,M,MSF,SHT,TON,BF" no-undo.
  DEF VAR pr-uom-list AS cha NO-UNDO INIT "EA,LB,M,MSF,TON,BF".
  DEF VAR cons-uom-list AS CHA NO-UNDO INIT "M,LF,EA,LB,TON".
 

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

