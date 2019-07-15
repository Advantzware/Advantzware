/*------------------------------------------------------------------------
    File        : CorrSpecs.p
    Purpose     : Corrugated Box

    Syntax      :

    Description : Return a Dataset of all Corrugated Box

    Author(s)   : 
    Created     : 21 jan 2009 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  DEFinitions  ************************** */
DEFINE TEMP-TABLE ttCorrSpecs NO-UNDO
    FIELD  vCustNum     AS CHAR
    FIELD  vShipTo      AS CHAR     
    FIELD  vFgItem      AS CHAR     
    FIELD  vCustPart    AS CHAR     
    FIELD  vItemName    AS CHAR      
    FIELD  vDescr       AS CHAR     
    FIELD  vDieNum      AS CHAR     
    FIELD  vCadNum      AS CHAR     
    FIELD  vSpcNum      AS CHAR     
    FIELD  vPlateNum    AS CHAR     
    FIELD  vImage       AS CHAR     
    FIELD  vUpcNum      AS CHAR     
    FIELD  vSalesman    AS CHAR     
    FIELD  vSmanDscr    AS CHAR     
    FIELD  vComm        AS DEC     
    FIELD  vFgCategory  AS CHAR     
    FIELD  vFgCatDscr   AS CHAR     
    FIELD  vStyle       AS CHAR     
    FIELD  vStyleDscr   AS CHAR     
    FIELD  vBoard       AS CHAR  
    FIELD  vBrdDscr     AS CHAR     
    FIELD  vLength      AS DEC       
    FIELD  vWidth       AS DEC     
    FIELD  vDepth       AS DEC     
    FIELD  vFlute       AS CHAR     
    FIELD  vTest        AS CHAR     
    FIELD  vTab         AS CHAR     
    FIELD  vMetric      AS CHAR     
    FIELD  vJointMat    AS CHAR     
    FIELD  vDustFlap    AS DEC     
    FIELD  vBotFlap     AS DEC
    FIELD  vLockTab     AS DEC
    FIELD  vTabWid      AS DEC
    FIELD  vScoreWid    AS DEC 
    FIELD  vScoreLen    AS DEC   
    FIELD  vTuck        AS DEC   
    FIELD  vJointLen    AS DEC   
    FIELD  vBlankWid    AS DEC   
    FIELD  vBlankLen    AS DEC   
    FIELD  vBlankSqFt   AS DEC   
    FIELD  vEstNum      AS CHAR   
    FIELD  vFromDt      AS CHAR   
    FIELD  vEstDate     AS DATE   
    FIELD  vModDate     AS DATE   
    FIELD  vLastOrd     AS INT    
    FIELD  vOrdDate     AS DATE    
    FIELD  vQty         AS DEC
    FIELD  vQtySet      AS DEC
    FIELD  vMsf         AS DEC
    FIELD  vShipName    AS CHAR  
    FIELD  vAddr       AS CHAR 
    FIELD  vAddr2       AS CHAR 
    FIELD  vCity        AS CHAR 
    FIELD  vState       AS CHAR 
    FIELD  vZip         AS CHAR 
    FIELD  vImagePath   AS CHAR 
    .
DEFINE DATASET dsCorrSpecs FOR ttCorrSpecs.



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
DEFINE INPUT PARAMETER prmAutocalcSelected AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrSpecs.
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
IF prmAutocalcSelected    = ?  THEN ASSIGN  prmAutocalcSelected  = "".


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
DEF VAR ld-k-wid-array LIKE eb.k-wid-array2 NO-UNDO.
DEF VAR ld-k-len-array LIKE eb.k-len-array2 NO-UNDO.
DEF VAR ll-style-is-valid AS LOG NO-UNDO.
def new shared temp-table formule field formule as dec extent 12.
DEF VAR lv-panels as log no-undo.


DEF VAR lv-comm LIKE eb.comm NO-UNDO.
DEF VAR lv-sman LIKE sman.sman NO-UNDO.
DEF VAR ld-markup AS DEC NO-UNDO.
DEF BUFFER bf-xeb FOR eb.
def  buffer xest    for est.
def  buffer xef     for ef.
DEF NEW SHARED  buffer xeb     for eb.
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

{sys/inc/f16to32.i}
  {cec/msfcalc.i}

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


 /* IF prmJointMat <> "" THEN DO:
  prmJointMat = CAPS(prmJointMat).
     FIND FIRST ITEM WHERE item.company  EQ prmComp AND item.i-no  EQ prmJointMat AND CAN-DO("G,S,T",item.mat-type) NO-LOCK NO-ERROR.
     IF AVAIL ITEM THEN DO:
      ASSIGN 
          cError =  "Invalid Joint Material.".
      RETURN.
    END.
  END.*/


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


END.

  

IF prmAction  = "Override"  THEN DO:
    
FIND FIRST ef where ef.company = prmComp AND ef.est-no = vEstimate AND ef.form-no = prmFormno EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ef THEN do:
        ASSIGN 
            ef.cad-image  = prmImage
            ef.board      = prmBoard                                             
            ef.brd-dscr   = prmBrdDscr .
    END.
     find FIRST eb where eb.company = prmComp AND eb.est-no = ef.est-no AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno EXCLUSIVE-LOCK NO-ERROR.
       find FIRST est where est.company = prmComp AND est.est-no = ef.est-no EXCLUSIVE-LOCK NO-ERROR.

 IF  AVAIL eb AND AVAIL est THEN DO:
       ASSIGN 
              eb.cust-no    = prmCustNum   
              eb.ship-id    = prmShipTo    
              eb.stock-no   = prmFgItem    
              eb.part-no    = prmCustPart 
              eb.part-dscr1 = prmItemName 
              eb.part-dscr2 = prmPartDscr                                           
              eb.die-no     = prmDieNum                                              
              eb.cad-no     = prmCadNum                                             
              eb.spc-no     = prmSpcNum                                                
              eb.plate-no   = prmPlateNum                                            
                                                            
              eb.upc-no     = prmUpcNum                                              
              eb.sman       = prmSman                                               
              eb.comm       = prmComm                                              
              eb.procat     = prmFgCat                                              
              eb.style      = prmStyle 
              eb.flute      = prmFlute                                                
              eb.test       = prmTest.
       
       ASSIGN
              eb.len        =   trunc(prmLength,0) + ((prmLength - trunc(prmLength,0)) * K_FRAC)                                                     
              eb.wid        =   trunc(prmWidth,0) + ((prmWidth - trunc(prmWidth,0)) * K_FRAC)                                                  
              eb.dep        =   trunc(prmDepth,0) + ((prmDepth - trunc(prmDepth,0)) * K_FRAC)                                                
                 
              est.metric    = IF prmMetric = "Yes" THEN TRUE ELSE FALSE   
              eb.adhesive   = prmJointMat   
              eb.dust       = trunc(prmDustFlap,0) + ((prmDustFlap - trunc(prmDustFlap,0)) * K_FRAC)    
              eb.fpanel     = trunc(prmBotFlap,0) + ((prmBotFlap - trunc(prmBotFlap,0)) * K_FRAC)    
              eb.lock       = trunc(prmLockTab,0) + ((prmLockTab - trunc(prmLockTab,0)) * K_FRAC)    
              eb.gluelap    = trunc(prmTabWid,0) + ((prmTabWid - trunc(prmTabWid,0)) * K_FRAC)    
              eb.k-wid      = trunc(prmScWid,0) + ((prmScWid - trunc(prmScWid,0)) * K_FRAC)    
              eb.k-len      = trunc(prmScLen,0) + ((prmScLen - trunc(prmScLen,0)) * K_FRAC)    
              eb.tuck       = trunc(prmTuck,0) + ((prmTuck - trunc(prmTuck,0)) * K_FRAC)    
              eb.lin-in     = trunc(prmJointLen,0) + ((prmJointLen - trunc(prmJointLen,0)) * K_FRAC)  
              eb.t-wid      = trunc(prmBlankWid,0) + ((prmBlankWid - trunc(prmBlankWid,0)) * K_FRAC)
              eb.t-len      = trunc(prmBlankLen,0) + ((prmBlankLen - trunc(prmBlankLen,0)) * K_FRAC)
              eb.tab-in     = IF prmTab = "In" THEN TRUE ELSE FALSE  .
              lv-sqin       = ( prmBlankWid * prmBlankLen ).            
              eb.t-sqin     =  eb.t-len * eb.t-wid  .

              IF prmAutocalcSelected EQ "yes" THEN DO:
                ASSIGN
                    ll-style-is-valid = YES.

                RUN calc-blank-size.         
                FIND CURRENT eb.
              END.        

              ASSIGN
                   v-cad-no = eb.cad-no.

              find first sys-ctrl where sys-ctrl.company eq prmComp
                        and sys-ctrl.name    eq "CADFILE"
                        no-lock no-error.
              IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
                  create sys-ctrl.
                  assign sys-ctrl.company = prmComp
                      sys-ctrl.name    = "CADFILE"
                      sys-ctrl.descrip = "Dictate the location of the cad image to search."
                      sys-ctrl.char-fld = "R:\rcode\cadimage\".      
                  END.
                  lv-cad-path = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".


               find xeb where recid(xeb) = recid(eb) no-lock.
               find xest where recid(xest) = recid(est) no-lock.  
               RUN box-design NO-ERROR.
              
              ASSIGN prmAction = "Select".
 END.
 END.
            


 IF prmAction = "Select" THEN DO:
    
     FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstNum))) + TRIM(prmEstNum) AND est.company = prmComp NO-LOCK NO-ERROR.
     FOR EACH est-qty WHERE est-qty.est-no = est.est-no  NO-LOCK ,
         EACH ef WHERE ef.company = est-qty.company AND ef.est-no = est-qty.est-no 
         AND ef.eqty = est-qty.eqty AND ef.form-no = prmFormno NO-LOCK,
         EACH eb WHERE eb.company = ef.company AND eb.est-no = ef.est-no  AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno  NO-LOCK:
         
         FIND FIRST sman WHERE sman.company EQ prmComp AND sman.sman = eb.sman NO-LOCK NO-ERROR.
         IF AVAIL sman THEN DO:
             ASSIGN
                 sman_sname = sman.sname .
             END.  
             
             FIND fgcat WHERE fgcat.company EQ prmComp AND fgcat.procat  EQ eb.procat NO-LOCK NO-ERROR.
             IF AVAIL fgcat THEN DO:
                 procat_desc = fgcat.dscr.
             END.             

             IF NOT AVAIL est OR NOT AVAIL eb THEN RETURN.
             IF AVAIL est-qty THEN 
                 ASSIGN
                 tab-inout = IF eb.tab-in EQ YES THEN "In"  ELSE
                     IF eb.tab-in EQ NO  THEN "Out" ELSE ""
                         fi_per-set = IF eb.est-type GE 7 THEN 1
                             ELSE
                                 IF eb.yld-qty LT 0 THEN -1 / eb.yld-qty ELSE eb.yld-qty
                                     fi_msf     = (IF eb.est-type GE 7 THEN eb.bl-qty
                                         ELSE (est-qty.eqty * fi_per-set)) * (eb.t-sqin / 144)/ 1000.
                                              
                                  fi_from-est-no = IF eb.master-est-no NE "" AND
                                      eb.est-type EQ 8       THEN eb.master-est-no
                                      ELSE STRING(est.e-num,">>>>>>>>").

                                      IF AVAIL eb THEN
                                          ASSIGN
                                          lv-len = decimal(eb.t-len).
                                      lv-wid = decimal(eb.t-wid).

                                      /*IF v-cecscrn-char NE "Decimal" THEN
                                         assign lv-len = trunc(lv-len,3)
                                        lv-wid = trunc(lv-wid,3)
                                        lv-sqin = ( lv-len * lv-wid )
                                        vBlankSq = ( if v-corr then round(lv-sqin * 0.007,4) else round(lv-sqin / 144,4)).
                                    ELSE
                                      assign lv-len = trunc(lv-len,6)
                                             lv-wid = trunc(lv-wid,6)
                                             lv-sqin = ( lv-len * lv-wid )
                                             vBlankSq = ( if v-corr then round(lv-sqin * 0.007,6) else round(lv-sqin / 144,6)).
                                   /*   assign lv-len = trunc(lv-len,3)
                                          lv-wid = trunc(lv-wid,3).
                                      lv-sqin = ( lv-len * lv-wid )  .
                                      vBlankSq = round(lv-sqin / 144,4). */   */

              FIND FIRST style WHERE style.company  = prmComp AND style.style =  eb.style  NO-LOCK NO-ERROR.
              IF AVAIL style THEN
                  ASSIGN
                  lv-foam    = style.type EQ "F"
                  style_dscr = style.dscr.
              ELSE
                  ASSIGN
                      lv-foam         = NO
                      style_dscr      = "".                  
                  
                  CREATE ttCorrSpecs.
                  ASSIGN 
                      ttCorrSpecs.vCustNum        = eb.cust-no
                      ttCorrSpecs.vShipTo         = eb.ship-id
                      ttCorrSpecs.vFgItem         = eb.stock-no
                      ttCorrSpecs.vCustPart       = eb.part-no
                      ttCorrSpecs.vItemName       = eb.part-dscr1
                      ttCorrSpecs.vDescr          = eb.part-dscr2
                      ttCorrSpecs.vDieNum         = eb.die-no
                      ttCorrSpecs.vCadNum         = eb.cad-no
                      ttCorrSpecs.vSpcNum         = eb.spc-no
                      ttCorrSpecs.vPlateNum       = eb.plate-no
                      ttCorrSpecs.vImage          = ef.cad-image
                      ttCorrSpecs.vUpcNum         = eb.upc-no
                      ttCorrSpecs.vSalesman       = eb.sman
                      ttCorrSpecs.vSmanDscr       = sman_sname
                      ttCorrSpecs.vComm           = eb.comm
                      ttCorrSpecs.vFgCategory     = eb.procat
                      ttCorrSpecs.vFgCatDscr      = procat_desc
                      ttCorrSpecs.vStyle          = eb.style 
                      ttCorrSpecs.vStyleDscr      = style_dscr
                      ttCorrSpecs.vBoard          = ef.board
                      ttCorrSpecs.vBrdDscr        = ef.brd-dscr 
                      ttCorrSpecs.vLength         = round(trunc((eb.len),0) + (((eb.len) - trunc((eb.len),0)) / K_FRAC),2)
                      ttCorrSpecs.vWidth          =  round(trunc((eb.wid),0) + (((eb.wid) - trunc((eb.wid),0)) / K_FRAC),2) 
                      ttCorrSpecs.vDepth          = round(trunc((eb.dep),0) + (((eb.dep) - trunc((eb.dep),0)) / K_FRAC),2) 
                      ttCorrSpecs.vFlute          = eb.flute
                      ttCorrSpecs.vTest           = eb.test
                      ttCorrSpecs.vTab            = IF eb.tab-in  = TRUE THEN "In" ELSE "Out"
                      ttCorrSpecs.vMetric         = IF est.metric = TRUE THEN "Yes" ELSE "No"
                      ttCorrSpecs.vJointMat       = eb.adhesive 
                      ttCorrSpecs.vDustFlap       =   round(trunc((eb.dust),0) + (((eb.dust) - trunc((eb.dust),0)) / K_FRAC),2)
                      ttCorrSpecs.vBotFlap        =   round(trunc((eb.fpanel),0) + (((eb.fpanel) - trunc((eb.fpanel),0)) / K_FRAC),2)
                      ttCorrSpecs.vLockTab        =   round(trunc((eb.lock),0) + (((eb.lock) - trunc((eb.lock),0)) / K_FRAC),2)
                      ttCorrSpecs.vTabWid         =   ROUND(trunc((eb.gluelap),0) + (((eb.gluelap) - trunc((eb.gluelap),0)) / K_FRAC),2)
                      ttCorrSpecs.vScoreWid       =   round(trunc((eb.k-wid),0) + (((eb.k-wid) - trunc((eb.k-wid),0)) / K_FRAC),2)
                      ttCorrSpecs.vScoreLen       =   round(trunc((eb.k-len),0) + (((eb.k-len) - trunc((eb.k-len),0)) / K_FRAC),2)
                      ttCorrSpecs.vTuck           =   round(trunc((eb.tuck),0) + (((eb.tuck) - trunc((eb.tuck),0)) / K_FRAC),2)
                      ttCorrSpecs.vJointLen       =   round(trunc((eb.lin-in),0) + (((eb.lin-in) - trunc((eb.lin-in),0)) / K_FRAC),2)
                      ttCorrSpecs.vEstNum         = eb.est-no
                      ttCorrSpecs.vFromDt         = fi_from-est-no
                      ttCorrSpecs.vEstDate        = est.est-date 
                      ttCorrSpecs.vModDate        = est.mod-date
                      ttCorrSpecs.vLastOrd        = est.ord-no  
                      ttCorrSpecs.vOrdDate        = est.ord-date
                      ttCorrSpecs.vQty            = est-qty.eqty
                      ttCorrSpecs.vQtySet         = fi_per-set
                      ttCorrSpecs.vMsf            = fi_msf 
                      ttCorrSpecs.vShipName       = eb.ship-name
                      ttCorrSpecs.vAddr           = eb.ship-addr[1]
                      ttCorrSpecs.vAddr2          = eb.ship-addr[2]
                      ttCorrSpecs.vCity           = eb.ship-city
                      ttCorrSpecs.vState          = eb.ship-state
                      ttCorrSpecs.vZip            = eb.ship-zip
                      ttCorrSpecs.vBlankWid       = round(trunc((eb.t-wid),0) + (((eb.t-wid) - trunc((eb.t-wid),0)) / K_FRAC),2)
                      ttCorrSpecs.vBlankLen       = round(trunc((eb.t-len),0) + (((eb.t-len) - trunc((eb.t-len),0)) / K_FRAC),2)
                      ttCorrSpecs.vBlankSqFt      = if v-corr then (eb.t-sqin * .007)
                                                                    else (eb.t-sqin / 144).
   
                              
                      FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp
                           AND sys-ctrl.NAME = "DIEFILE" NO-LOCK NO-ERROR.
                            ASSIGN
                                ttCorrSpecs.vImagePath = (IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "") .
                                                                                                   
 END.
 END.
                         
/*************************procuder**************************************/

PROCEDURE box-design:
 
/* IF eb.cad-no <> "" AND lv-cad-path <> "" THEN DO:
    MESSAGE "cadimage"  lv-cad-path v-cad-no. 
     IF SEARCH(lv-cad-path + eb.cad-no + lv-cad-ext) <> ? OR
        (cadfile NE '' AND SEARCH(cadfile) <> ?) THEN DO:
        FIND first box-design-hdr where box-design-hdr.design-no = 0 and
                                     box-design-hdr.company = eb.company 
                                 and box-design-hdr.est-no = eb.est-no     
                                 and box-design-hdr.form-no   eq eb.form-no
                                 and box-design-hdr.blank-no  eq eb.blank-no NO-ERROR.
        IF AVAIL box-design-hdr AND
            ( ((cadfile NE '') AND SEARCH(cadfile) <> ?) OR
              SEARCH(lv-cad-path + eb.cad-no + lv-cad-ext) <> ? ) THEN 
        DO:
         MESSAGE "cadimage2"  lv-cad-path v-cad-no.
           ASSIGN box-design-hdr.box-image = IF cadfile NE '' THEN cadfile
                  ELSE lv-cad-path + eb.cad-no + lv-cad-ext. /*".jpg"*/.
        END.
     END.
     ELSE DO: /* reset from style */
         run cec/descalc.p (recid(xest), recid(xeb)).
        find first style where style.company EQ eb.company and style.style  eq eb.style
                 no-lock no-error.
        if avail style then
           find first xbox-design-hdr where xbox-design-hdr.design-no eq style.design-no
                                        and xbox-design-hdr.est-no    eq ""
                                        no-lock no-error.
           FIND first box-design-hdr where box-design-hdr.design-no = 0 AND
                                     box-design-hdr.company = eb.company 
                                 and box-design-hdr.est-no = eb.est-no     
                                 and box-design-hdr.form-no   eq eb.form-no
                                 and box-design-hdr.blank-no  eq eb.blank-no NO-ERROR.
        IF AVAIL box-design-hdr AND (SEARCH(box-design-hdr.box-image) EQ ? OR cadfile NE '') THEN 
           ASSIGN 
            box-design-hdr.box-image = IF cadfile NE '' THEN cadfile  ELSE xbox-design-hdr.box-image
             .
        
     END.
     ASSIGN
       cadfile = ''
       lv-cad-ext = "".
  END.
  IF eb.cad-no = ""  THEN DO:
        run cec/descalc.p (recid(xest), recid(xeb)).
     find first style where style.company EQ eb.company and style.style  eq eb.style
                 no-lock no-error.
     if avail style then
        find first xbox-design-hdr where xbox-design-hdr.design-no eq style.design-no
                                        and xbox-design-hdr.est-no    eq ""
                                        no-lock no-error.
     FIND first box-design-hdr where box-design-hdr.design-no = 0 AND
                                     box-design-hdr.company = eb.company 
                                 and box-design-hdr.est-no = eb.est-no     
                                 and box-design-hdr.form-no   eq eb.form-no
                                 and box-design-hdr.blank-no  eq eb.blank-no NO-ERROR.

     IF AVAIL box-design-hdr THEN 
         ASSIGN
          box-design-hdr.box-image = xbox-design-hdr.box-image
         .
         
  END.*/

 END PROCEDURE.




/*-------------------------------calc-blank-size----------------------------------*/
PROCEDURE calc-blank-size :
   def buffer bf-eb for eb .
   DEF VAR i as int no-undo.
   DEF VAR j as int no-undo.
   DEF VAR v-score-char like v-lscore-c extent 100.
   def buffer xest for est.   

   find xest where /*recid(xest) = recid(est) no-lock. */
                   xest.company = eb.company and
                   xest.est-no = eb.est-no
                   no-lock no-error.
   find xef where recid(xef) = recid(ef) no-lock.
   find xeb where recid(xeb) = recid(eb) no-lock.

   DO i = 1 TO EXTENT(eb.k-wid-array2):

     ASSIGN
      ld-k-wid-array[i]    = eb.k-wid-array2[i]
      lv-k-wid-scr-type[i] = eb.k-wid-scr-type2[i].
   END.
   DO i = 1 TO EXTENT(eb.k-len-array2):
     ASSIGN
      ld-k-len-array[i]    = eb.k-len-array2[i]
      lv-k-len-scr-type[i] = eb.k-len-scr-type2[i].
   END.   

   find style where style.company = eb.company and
                    style.style = eb.style
                    no-lock no-error.
   if avail style then do:
       if style.type <> "F"
          AND NOT ll-style-is-valid then run calc-blank-size2.        

      {cec/msfcalc.i}
      run est/u2kinc1c.p (recid(eb)).
      run est/u2kinc2c.p (recid(eb)).
      find first formule no-lock no-error.
      find bf-eb of eb exclusive-lock.      

      assign bf-eb.t-wid = (formule.formule[1])
          bf-eb.t-len = (formule.formule[2])
          bf-eb.t-sqin = (formule.formule[7] * formule.formule[8])
          .

       ASSIGN bf-eb.k-wid-array2 = 0
              bf-eb.k-len-array2 = 0.       

      if not lv-panels or style.type = "F" then 
         assign bf-eb.k-wid-array2[1] = bf-eb.t-wid
                bf-eb.k-len-array2[1] = bf-eb.t-len
                .
      else do:
         run cec/descalc.p (recid(xest),recid(xeb)).         

         DO i = 1 TO EXTENT(xeb.k-wid-scr-type2):
           ASSIGN
            xeb.k-wid-scr-type2[i] = lv-k-wid-scr-type[i]
            xeb.k-len-scr-type2[i] = lv-k-len-scr-type[i].
         END.

         if v-lscore-c begins "No" then
            assign  xeb.k-wid-array2[1] = xeb.t-wid
                    xeb.k-len-array2[1] = xeb.t-len.
         else do:
           i = 0.
           for each w-box-design-line:
              i = i + 1.
              xeb.k-wid-array2[i] = w-box-design-line.wscore-d.
                 {sys/inc/k16bb.i xeb.k-wid-array2[i]} 
           end.
           assign  v-score-char    = ""
                   j               = 1.

           do i = 1 to 80:
             if substr(v-lscore-c,i,1) ne "" then do:
                v-score-char[j] = v-score-char[j] + substr(v-lscore-c,i,1).
                if substr(v-lscore-c,i + 1,1) eq "" then
                   assign  v-score-char[j] = trim(v-score-char[j])
                           j = j + 1.
             end.
             if j gt EXTENT(xeb.k-len-array2) then leave.
           end.
           DO i = 1 TO EXTENT(xeb.k-len-array2):
              xeb.k-len-array2[i] = dec(v-score-char[i]).
              {sys/inc/k16bb.i xeb.k-len-array2[i]}. 
           end.
         end.  /* else v-lscore */
       end. /* panels or not foam */
   end.

   IF NOT ll-blank-size-changed THEN
   DO i = 1 TO EXTENT(ld-k-wid-array):
     IF xeb.k-wid-array2[i] NE ld-k-wid-array[i] THEN DO:
       ll-blank-size-changed = YES.
       LEAVE.
     END.
   END.

   IF NOT ll-blank-size-changed THEN
   DO i = 1 TO EXTENT(ld-k-len-array):
     IF xeb.k-len-array2[i] NE ld-k-len-array[i] THEN DO:
       ll-blank-size-changed = YES.
       LEAVE.
     END.
   END.

   /*RUN display-matrix.*/

END PROCEDURE.


/*-------------------------------------------calc-blank-size2---------------------------------*/
PROCEDURE calc-blank-size2 :
   FIND CURRENT eb .
   /*ASSIGN FRAME {&frame-name} {&list-5}.*/
           
   {sys/inc/k16bb.i eb.wid  } 
   {sys/inc/k16bb.i eb.len  } 
   {sys/inc/k16bb.i eb.dep  } 
   {sys/inc/k16bb.i eb.dust  } 
   {sys/inc/k16bb.i eb.fpanel  } 
   {sys/inc/k16bb.i eb.lock  }
   {sys/inc/k16bb.i eb.gluelap  } 
   {sys/inc/k16bb.i eb.k-wid  } 
   {sys/inc/k16bb.i eb.k-len  } 
   {sys/inc/k16bb.i eb.tuck  }  
   {sys/inc/k16bb.i eb.lin-in}

   find xeb where recid(xeb) = recid(eb) no-lock.

   find style where style.company = eb.company and
                    style.style = eb.style
                    no-lock no-error.
   if AVAIL style AND style.material[7] ne "" then do:
     eb.adhesive = style.material[7].
     if eb.gluelap ne 0 then eb.lin-in = eb.dep.
   end.

   {est/u2estc.i eb.gluelap 1}
   {est/u2estc.i eb.k-wid 2}

   find first item where item.company = est.company
                    and item.i-no eq eb.adhesive
                  no-lock no-error.
   if avail item and eb.adhesive ne "NO JOINT" then do:
            if item.mat-type eq "G" then do:
                    if eb.tab-in then do:
                       {est/u2estc.i eb.k-len 3}
                    end.
                    else do:
                       eb.tab-in = no.
                       {est/u2estc.i eb.k-len 4}
                    end.
            end.
            else if item.mat-type eq "S" then do:
                    if eb.tab-in then do:
                       {est/u2estc.i eb.k-len 5}
                    end.
                    else do:
                       eb.tab-in = no.
                       {est/u2estc.i eb.k-len 6}
                    end.
            end.
            else if item.mat-type eq "T" then do:
                    eb.tab-in = ?.
                    {est/u2estc.i eb.k-len 7}
            end.
    end.
    else do:
                 eb.tab-in = ?.
                 {est/u2estc.i eb.k-len 7}
    end.

    if eb.len eq eb.wid
    then do:
                 {est/u2estc.i eb.k-wid 2 dim-fit}
    end.
    else do:
                 {est/u2estc.i eb.k-wid 2}
    end.
    
    /*RUN display-matrix.*/

END PROCEDURE.
