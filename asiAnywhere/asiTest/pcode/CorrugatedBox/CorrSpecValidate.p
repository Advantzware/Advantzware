

                                 
/*------------------------------------------------------------------------
    File        : FoldEstValidate.p
    Purpose     : Validation for add order

    Syntax      :

    Description : Validation for add order

    Author(s)   : Sewa
    Created     : sep 22, 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCustNum     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmShipTo      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmFgItem      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmCustPart    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmItemName    AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmPartDscr    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmDieNum      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmCadNum      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSpcNum      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmPlateNum    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmImage       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmUpcNum      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSman        AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSmanDscr    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmComm        AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmFgCat       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmFgCatDscr   AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmStyle       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmStyDscr     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmBoard       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmBrdDscr     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmLength      AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmWidth       AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmDepth       AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmFlute       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmTest        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTab         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmMetric      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmJointMat    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmDustFlap    AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmBotFlap     AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmLockTab     AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmTabWid      AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmScWid       AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmScLen       AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmTuck        AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmJointLen    AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmBlankWid    AS DEC NO-UNDO.                   
DEFINE INPUT PARAMETER prmBlankLen    AS DEC NO-UNDO. 
DEFINE INPUT PARAMETER prmBlankSqFt   AS DEC NO-UNDO. 
DEFINE INPUT PARAMETER prmEstNum      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFromDate     AS DATE NO-UNDO. 
DEFINE INPUT PARAMETER prmEstDate     AS DATE NO-UNDO. 
DEFINE INPUT PARAMETER prmModDate     AS DATE NO-UNDO. 
DEFINE INPUT PARAMETER prmOrderNum    AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmOrdDate     AS DATE NO-UNDO. 
DEFINE INPUT PARAMETER prmQty         AS DEC NO-UNDO.
DEFINE INPUT PARAMETER prmQtySet      AS DEC NO-UNDO.
DEFINE INPUT PARAMETER prmMsf         AS DEC NO-UNDO.
DEFINE INPUT PARAMETER prmShipName    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAddr        AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmAddr2        AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmCity        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmState       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmZip         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFormno      AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmEstFrom     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmBlankno      AS INT NO-UNDO.

DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 

IF prmUser        = ?  THEN ASSIGN    prmUser        = "".
IF prmAction      = ?  THEN ASSIGN    prmAction      = "".
IF prmAction      = "" THEN ASSIGN    prmAction      = "Select".
IF prmCustNum     = ?  THEN ASSIGN    prmCustNum     = "".
IF prmOrderNum    = ?  THEN ASSIGN    prmOrderNum    = 0.
IF prmShipTo      = "" THEN ASSIGN    prmShipTo      = "".
IF prmFgItem      = ?  THEN ASSIGN    prmFgItem      = "".      
IF prmCustPart    = ?  THEN ASSIGN    prmCustPart    = "".
IF prmPartDscr    = ?  THEN ASSIGN    prmPartDscr    = "".  
IF prmDieNum      = ?  THEN ASSIGN    prmDieNum      = "".  
IF prmCadNum      = ?  THEN ASSIGN    prmCadNum      = "".  
IF prmSpcNum      = ?  THEN ASSIGN    prmSpcNum      = "".             
IF prmPlateNum    = ?  THEN ASSIGN    prmPlateNum    = "".     
IF prmImage       = ?  THEN ASSIGN    prmImage       = "". 
IF prmUpcNum      = ?  THEN ASSIGN    prmUpcNum      = "".              
IF prmSman        = ?  THEN ASSIGN    prmSman        = "".     
IF prmComm        = ?  THEN ASSIGN    prmComm        = 0. 
IF prmFgCat       = ?  THEN ASSIGN    prmFgCat       = "". 
IF prmStyle       = ?  THEN ASSIGN    prmStyle       = "".    
IF prmBoard       = ?  THEN ASSIGN    prmBoard       = "".  
IF prmBrdDscr     = ?  THEN ASSIGN    prmBrdDscr     = "".  
IF prmLength      = ?  THEN ASSIGN    prmLength      = 0.  
IF prmWidth       = ?  THEN ASSIGN    prmWidth       = 0.  
IF prmDepth       = ?  THEN ASSIGN    prmDepth       = 0.  
IF prmFlute       = ?  THEN ASSIGN    prmFlute       = "".  
IF prmTest        = ?  THEN ASSIGN    prmTest        = "".  
IF prmJointMat    = ?  THEN ASSIGN    prmJointMat    = "".  
IF prmDustFlap    = ?  THEN ASSIGN    prmDustFlap    = 0.  
IF prmBotFlap     = ?  THEN ASSIGN    prmBotFlap     = 0.  
IF prmLockTab     = ?  THEN ASSIGN    prmLockTab     = 0.  
IF prmTabWid      = ?  THEN ASSIGN    prmTabWid      = 0.  
IF prmScWid       = ?  THEN ASSIGN    prmScWid       = 0.  
IF prmScLen       = ?  THEN ASSIGN    prmScLen       = 0.  
IF prmTuck        = ?  THEN ASSIGN    prmTuck        = 0.  
IF prmJointLen    = ?  THEN ASSIGN    prmJointLen    = 0.  
IF prmBlankWid    = ?  THEN ASSIGN    prmBlankWid    = 0.
IF prmBlankLen    = ?  THEN ASSIGN    prmBlankLen    = 0. 
IF prmBlankSqFt   = ?  THEN ASSIGN    prmBlankSqFt   = 0. 
IF prmEstNum      = ?  THEN ASSIGN    prmEstNum      = "". 
IF prmQty         = ?  THEN ASSIGN    prmQty         = 0.
IF prmShipName    = ?  THEN ASSIGN    prmShipName    = "".
IF prmAddr        = ?  THEN ASSIGN    prmAddr        = "".
IF prmAddr2        = ?  THEN ASSIGN    prmAddr2        = "".
IF prmCity        = ?  THEN ASSIGN    prmCity        = "".
IF prmState       = ?  THEN ASSIGN    prmState       = "".
IF prmZip         = ?  THEN ASSIGN    prmZip         = "".
IF prmEstFrom     = ?  THEN ASSIGN    prmEstFrom     = "".
IF prmBlankno     = ?  THEN ASSIGN    prmBlankno     = 1.
IF prmItemName    = ?  THEN ASSIGN    prmItemName  = "".

{cec/descalc.i new}
DEF VAR lv-cad-path AS cha NO-UNDO.  /* cad file - boximage path for Fibre */
DEF VAR lv-cad-ext AS cha NO-UNDO.
DEF VAR dieFile AS CHARACTER NO-UNDO.
DEF VAR cadFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE vEstimate AS CHAR.
DEFINE VARIABLE fi_from-est-no AS CHARACTER FORMAT "X(8)". 
DEFINE VARIABLE from-xest-no AS CHARACTER FORMAT "X(8)" .
DEFINE VARIABLE fi_msf AS DECIMAL .
DEFINE VARIABLE fi_per-set AS DECIMAL FORMAT "->>>9.9<<<" INITIAL 0 .
DEFINE VARIABLE tab-inout AS CHARACTER FORMAT "X(3)".
DEFINE VARIABLE procat_desc AS CHARACTER FORMAT "X(256)".
DEFINE VARIABLE sman_sname AS CHARACTER FORMAT "X(256)".
DEFINE VARIABLE style_dscr AS CHARACTER FORMAT "X(256)".
DEF VAR lv-foam as log no-undo.
DEF VAR tmpstore as cha no-undo.
DEF VAR i as int no-undo.
DEF VAR rec_key_value as cha no-undo.
DEF VAR header_value as cha no-undo.
DEF VAR lv-sqin as dec no-undo.
DEFINE VARIABLE vBlankSq AS DEC.
DEF VAR ll-blank-size-changed AS LOG NO-UNDO.
DEF TEMP-TABLE tt-array NO-UNDO FIELD tt-dec AS DEC FIELD tt-type AS CHAR.
DEF VAR v-l-array AS DEC EXTENT 30 NO-UNDO.
DEF VAR lv-industry like item.industry init "2".
DEF VAR char-hdl AS cha NO-UNDO.
DEF VAR lv-hld-cust like eb.cust-no no-undo.
DEF VAR lv-hld-ship like eb.ship-id no-undo.
DEF VAR lv-hld-stock-no like eb.stock-no no-undo.
DEF VAR lv-hld-part-no like eb.part-no no-undo.
DEF VAR lv-prev-cad# AS cha NO-UNDO.
DEF VAR lv-prev-style AS cha NO-UNDO. 
DEF VAR ll-set AS LOG NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR v-cad-no LIKE eb.cad-no NO-UNDO.
DEF VAR lv-box-des AS CHAR INIT "S" NO-UNDO.
DEF VAR v-dec AS DEC NO-UNDO.
DEF VAR v-dec2 AS DEC NO-UNDO.
DEF VAR v-w-array AS DEC EXTENT 30 NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR ll-auto-calc-selected as log no-undo.
DEF VAR lv-master-est-no LIKE eb.master-est-no NO-UNDO.

DEF VAR lv-hold-flute LIKE eb.flute NO-UNDO.
DEF VAR lv-hold-test LIKE eb.test NO-UNDO.
DEF VAR lv-len as dec no-undo.
DEF VAR lv-wid as dec no-undo.
DEF VAR k_frac as dec init 6.25 no-undo.


DEF VAR lv-comm LIKE eb.comm NO-UNDO.
DEF VAR lv-sman LIKE sman.sman NO-UNDO.
DEF VAR ld-markup AS DEC NO-UNDO.
DEF BUFFER bf-xeb FOR eb.
def  buffer xest    for est.
def  buffer xef     for ef.
def  buffer xeb     for eb.
DEF BUFFER xbox-design-hdr FOR box-design-hdr.
DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF VAR ll-valid AS LOG INIT NO NO-UNDO.
  DEF VAR li-pnt AS INT NO-UNDO.


DEFINE VAR prmComp AS CHAR .
    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_DEFault = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

vEstimate =  FILL(" ",8 - LENGTH(TRIM(prmEstNum))) + TRIM(prmEstNum).
ASSIGN
    cocode = prmComp .


/*************************************prmAction***************************************************/  


IF prmAction = "Override" THEN DO:

if prmFgItem <> "" then do:
       find first itemfg where itemfg.company = prmComp AND itemfg.i-no = prmFgItem no-lock no-error.
       if not avail itemfg then do:
           ASSIGN
                  cError = "This item does not exist, would you like to add it?" .
           RETURN.
       END.
END.

if prmBoard <> "" and
       not can-find(item where item.company = prmComp
                    and item.i-no = prmBoard )
       then do:
         ASSIGN
             cError =  "Invalid Board. Try Help. " .
         return .
         end.


IF prmFgCat <> "" THEN DO:
IF NOT CAN-FIND(FIRST fgcat
                    WHERE fgcat.company EQ prmComp
                      AND fgcat.procat  EQ prmFgCat) OR prmFgCat EQ ""     THEN DO:
      ASSIGN
          cError =  "Invalid Category, try help..." .
      RETURN.
    END.
  END.


IF prmShipTo <> "" THEN DO:
    IF NOT CAN-FIND(FIRST shipto
                    WHERE shipto.company EQ prmComp AND shipto.cust-no EQ prmCustNum AND shipto.ship-id EQ prmShipTo)  THEN DO:
      ASSIGN 
          cError =  " Invalid ShipTo, try help... OR Do you wish to add this Shipto ID to this Customer?" .
           RETURN .
      END.
    END.


 IF prmSman <> "" THEN DO:
     FIND FIRST sman WHERE sman.company EQ prmComp AND sman.sman    EQ prmSman NO-LOCK NO-ERROR.
     IF NOT AVAIL sman THEN DO:
       ASSIGN 
           cError = "Invalid Salesman. Try help." .
       RETURN.
    END.
   /* prmSman = sman.sNAME.*/
 END.


IF prmStyle <> "" THEN DO:
IF NOT CAN-FIND(FIRST style WHERE style.company  EQ prmComp AND style.style EQ prmStyle AND style.industry EQ lv-industry) OR
                                 prmStyle EQ ""  THEN DO:
      ASSIGN
          cError =  "Invalid Style, try help..." .
      RETURN.
    END.
  END.

IF prmFlute <> "" THEN DO:
     IF NOT CAN-FIND(FIRST flute WHERE flute.company EQ prmComp
                                  AND flute.code    EQ prmFlute)
    THEN DO:
      ASSIGN
          cError = "Invalid Flute, try help...".
      RETURN.
    END.
  END.

  IF prmTest <> ""  THEN DO:
      FOR EACH stack-flute WHERE stack-flute.company EQ prmComp
          AND stack-flute.loc     EQ "Main"
          AND stack-flute.code    EQ prmFlute  NO-LOCK:

          li-pnt = 1.                            
          DO WHILE (NOT ll-valid) AND li-pnt LE 16:  
              ll-valid = stack-flute.row-value[li-pnt] EQ prmTest.
              li-pnt = li-pnt + 1.
              END.
              
              IF ll-valid THEN LEAVE.
              END.
              IF NOT ll-valid THEN DO:
                  cError = "Invalid entry, try help..." .
                  RETURN .
               END.  
     END.  

    IF prmJointMat <>  ""  AND 
        NOT CAN-FIND(FIRST item
                    WHERE item.company  EQ prmComp
                      AND item.i-no     EQ prmJointMat
                      AND CAN-DO("G,S,T",item.mat-type))   THEN DO:
        ASSIGN
            cError =  "Invalid Joint Material, try help..." .
            RETURN .
    END.
  

END.
