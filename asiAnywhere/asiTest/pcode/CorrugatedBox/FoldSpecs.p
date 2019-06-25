/*------------------------------------------------------------------------
    File        : FoldSpecs.p
    Purpose     : Folding Box

    Syntax      :

    Description : Return a Dataset of all folding Box

    Author(s)   : 
    Created     :  
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  DEFinitions  ************************** */
DEFINE TEMP-TABLE ttFoldSpecs NO-UNDO
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
         
    FIELD  vMetric      AS CHAR     
    FIELD  vAdhesive    AS CHAR     
    FIELD  vDustFlap    AS DEC     
    FIELD  vPanel     AS DEC
    FIELD  vLockTab     AS DEC
    FIELD  vGlue      AS DEC
    FIELD  vDkWid    AS DEC 
    FIELD  vDkLen    AS DEC   
    FIELD  vTuck        AS DEC   
    FIELD  vLieInc    AS DEC   
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
    FIELD  vShipName    AS CHAR  
    FIELD  vAddr       AS CHAR 
    FIELD  vAddr2       AS CHAR 
    FIELD  vCity        AS CHAR 
    FIELD  vState       AS CHAR 
    FIELD  vZip         AS CHAR 
    FIELD mkl           AS CHAR
    FIELD vLinInc        AS DEC
    .
DEFINE DATASET dsFoldSpecs FOR ttFoldSpecs.



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
DEFINE INPUT PARAMETER prmMetric      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmAdhe       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmDustFlap    AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmPanel       AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmLockTab     AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmGlue      AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmScWid       AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmScLen       AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmTuck        AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmLinInc    AS DEC NO-UNDO.  
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
DEFINE INPUT PARAMETER prmShipName    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAddr        AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmAddr2        AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmCity        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmState       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmZip         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFormno      AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmEstFrom     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmBlankno     AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmAutocalcSelected AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFoldSpecs.
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
 
IF prmAdhe        = ?  THEN ASSIGN    prmAdhe    = "".  
IF prmDustFlap    = ?  THEN ASSIGN    prmDustFlap    = 0.  
IF prmPanel       = ?  THEN ASSIGN    prmPanel     = 0.  
IF prmLockTab     = ?  THEN ASSIGN    prmLockTab     = 0.  
IF prmGlue        = ?  THEN ASSIGN    prmGlue        = 0.  
IF prmScWid       = ?  THEN ASSIGN    prmScWid       = 0.  
IF prmScLen       = ?  THEN ASSIGN    prmScLen       = 0.  
IF prmTuck        = ?  THEN ASSIGN    prmTuck        = 0.  
IF prmLinInc      = ?  THEN ASSIGN    prmLinInc    = 0.  
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
IF prmBlankno     = ?  THEN ASSIGN    prmBlankno     = 0.
IF prmAutocalcSelected    = ?  THEN ASSIGN  prmAutocalcSelected  = "".

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
def var lv-industry like item.industry init "1".
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

def new shared var cocode as cha no-undo.
def new shared var locode as cha no-undo.
DEF NEW SHARED TEMP-TABLE formule FIELD formule AS DEC EXTENT 12.

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


DEFINE VAR prmComp AS CHAR .
    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_DEFault = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN
    cocode = prmComp.

vEstimate =  FILL(" ",8 - LENGTH(TRIM(prmEstNum))) + TRIM(prmEstNum).


/*************************************prmAction***************************************************/  





IF prmAction = "Override" THEN DO:

if prmFgItem <> "" then do:
       find first itemfg where itemfg.company = prmComp AND itemfg.i-no = prmFgItem no-lock no-error.
       if not avail itemfg then do:
           ASSIGN
                  cError = "This item does not exist,Try Help" .
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

 IF prmBoard <> "" THEN DO:
   FIND FIRST  item where item.company = prmComp
                    and item.i-no = prmBoard NO-LOCK NO-ERROR.
   IF AVAIL ITEM THEN
       prmBrdDscr = ITEM.i-name .
 END.


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
          cError =  " Invalid ShipTo, try help... " .
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

 /*RUN valid-fi_from-est-no.*/


 /* IF prmJointMat <> "" THEN DO:
  prmJointMat = CAPS(prmJointMat).
     FIND FIRST ITEM WHERE item.company  EQ prmComp AND item.i-no  EQ prmJointMat AND CAN-DO("G,S,T",item.mat-type) NO-LOCK NO-ERROR.
     IF AVAIL ITEM THEN DO:
      ASSIGN 
          cError =  "Invalid Joint Material.".
      RETURN.
    END.
  END.*/
IF prmAdhe <> "" THEN do:
  FIND FIRST ITEM  WHERE item.company  EQ prmComp
                      AND item.i-no     EQ prmAdhe
                      AND CAN-DO("G,T",item.mat-type) NO-LOCK NO-ERROR.
      IF NOT AVAIL ITEM THEN DO:
          ASSIGN
              cError =  "Invalid Adhesive, try help...".
         
      RETURN .
    END.
  END.

 END.

  

IF prmAction  = "Override"  THEN DO:

    
FIND FIRST ef where ef.company = prmComp AND ef.est-no = vEstimate AND ef.form-no = prmFormno EXCLUSIVE-LOCK NO-ERROR.
     find FIRST eb where eb.company = prmComp AND eb.est-no = ef.est-no AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno EXCLUSIVE-LOCK NO-ERROR.
       find FIRST est where est.company = prmComp AND est.est-no = ef.est-no EXCLUSIVE-LOCK NO-ERROR.
 IF AVAIL ef  AND AVAIL eb AND AVAIL est THEN DO:
       ASSIGN 
              eb.cust-no    = prmCustNum   
              eb.ship-id    = prmShipTo 
              eb.bl-qty     = prmQty
              eb.stock-no   = prmFgItem    
              eb.part-no    = prmCustPart 
              eb.part-dscr1 = prmItemName 
              eb.part-dscr2 = prmPartDscr                                           
              eb.die-no     = prmDieNum                                              
              eb.cad-no     = prmCadNum                                             
              eb.spc-no     = prmSpcNum                                                
              eb.plate-no   = prmPlateNum                                            
              ef.cad-image  = prmImage                                              
              eb.upc-no     = prmUpcNum                                              
              eb.sman       = prmSman                                               
              eb.comm       = prmComm                                              
              eb.procat     = prmFgCat                                              
              eb.style      = prmStyle                                            
              ef.board      = prmBoard                                             
              ef.brd-dscr   = prmBrdDscr                                              
              eb.len        = prmLength 
              eb.wid        = prmWidth 
              eb.dep        = prmDepth 
                
              est.metric    = IF prmMetric = "Yes" THEN TRUE ELSE FALSE   
              eb.adhesive   = prmAdhe   
              eb.dust       = prmDustFlap   
              eb.fpanel     = prmPanel    
              eb.lock       = prmLockTab    
              eb.gluelap    = prmGlue     
              eb.k-wid      = prmScWid      
              eb.k-len      = prmScLen      
              eb.tuck       = prmTuck       
              eb.lin-in     = prmLinInc    
              eb.t-wid      = prmBlankWid 
              eb.t-len      = prmBlankLen 
             
              .

              lv-sqin = ( prmBlankWid * prmBlankLen )  .
              ASSIGN
                  eb.t-sqin      = lv-sqin .

              IF prmAutocalcSelected EQ "yes" THEN DO:
                FIND CURRENT eb.
                RUN calc-blank-size.         
              END.

             /*  IF TRIM(prmEstFrom) NE TRIM(lv-master-est-no) THEN DO:
                       eb.master-est-no = FILL(" ",8 - LENGTH(TRIM(prmEstFrom))) +
                           TRIM(prmEstFrom).
                       
                       IF TRIM(prmEstFrom) EQ "" AND est.est-type EQ 4 THEN est.e-num = 0.
                END.*/

              
              ASSIGN prmAction = "Select".
 END.
 END.
            


 IF prmAction = "Select" THEN DO:
    
     FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstNum))) + TRIM(prmEstNum) AND est.company = prmComp  NO-LOCK NO-ERROR.
     FOR EACH est-qty WHERE est-qty.est-no = est.est-no  NO-LOCK ,
         EACH ef WHERE ef.company = est-qty.company AND ef.est-no = est-qty.est-no 
         AND ef.eqty = est-qty.eqty AND ef.form-no = prmFormno NO-LOCK,
         EACH eb WHERE eb.company = ef.company AND eb.est-no = ef.est-no  AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno NO-LOCK:
         
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
             fi_from-est-no = IF eb.master-est-no NE "" AND
             eb.est-type EQ 4       THEN eb.master-est-no
                 ELSE STRING(est.e-num,">>>>>>>>").
                 IF AVAIL eb THEN
                     ASSIGN
                     lv-len = decimal(eb.t-len).
                 lv-wid = decimal(eb.t-wid).
                 assign lv-len = trunc(lv-len,3)
                     lv-wid = trunc(lv-wid,3).

                 vBlankSq = ( lv-len * lv-wid )  .
                 
         FIND FIRST style WHERE style.company  = prmComp AND style.style =  eb.style  NO-LOCK NO-ERROR.
         IF AVAIL style THEN
             ASSIGN
             lv-foam    = style.type EQ "F"
             style_dscr = style.dscr.
         ELSE
             ASSIGN
                 lv-foam         = NO
                 style_dscr      = "".

             CREATE ttFoldSpecs.
             ASSIGN 
                 ttFoldSpecs.vCustNum        = eb.cust-no
                 ttFoldSpecs.vShipTo         = eb.ship-id
                 ttFoldSpecs.vFgItem         = eb.stock-no
                 ttFoldSpecs.vCustPart       = eb.part-no
                 ttFoldSpecs.vItemName       = eb.part-dscr1
                 ttFoldSpecs.vDescr          = eb.part-dscr2
                 ttFoldSpecs.vDieNum         = eb.die-no
                 ttFoldSpecs.vCadNum         = eb.cad-no
                 ttFoldSpecs.vSpcNum         = eb.spc-no
                 ttFoldSpecs.vPlateNum       = eb.plate-no
                 ttFoldSpecs.vImage          = ef.cad-image
                 ttFoldSpecs.vUpcNum         = eb.upc-no
                 ttFoldSpecs.vSalesman       = eb.sman
                 ttFoldSpecs.vSmanDscr       = sman_sname
                 ttFoldSpecs.vComm           = eb.comm
                 ttFoldSpecs.vFgCategory     = eb.procat
                 ttFoldSpecs.vFgCatDscr      = procat_desc
                 ttFoldSpecs.vStyle          = eb.style 
                 ttFoldSpecs.vStyleDscr      = style_dscr
                 ttFoldSpecs.vBoard          = ef.board
                 ttFoldSpecs.vBrdDscr        = ef.brd-dscr 
                 ttFoldSpecs.vLength         = eb.len 
                 ttFoldSpecs.vWidth          = eb.wid 
                 ttFoldSpecs.vDepth          = eb.dep 

                 ttFoldSpecs.vMetric         = IF est.metric = TRUE THEN "Yes" ELSE "No"
                     ttFoldSpecs.vAdhesive       = eb.adhesive 
                     ttFoldSpecs.vDustFlap       = eb.dust 
                     ttFoldSpecs.vPanel          = eb.fpanel 
                     ttFoldSpecs.vLockTab        = eb.lock 
                     ttFoldSpecs.vGlue           = eb.gluelap 
                     ttFoldSpecs.vDkWid          = eb.k-wid 
                     ttFoldSpecs.vDkLen          = eb.k-len 
                     ttFoldSpecs.vTuck           = eb.tuck  
                     ttFoldSpecs.vLinInc         = eb.lin-in 
                     ttFoldSpecs.vEstNum         = eb.est-no
                     ttFoldSpecs.vFromDt         = fi_from-est-no
                     ttFoldSpecs.vEstDate        = est.est-date 
                     ttFoldSpecs.vModDate        = est.mod-date
                     ttFoldSpecs.vLastOrd        = eb.ord-no  
                     ttFoldSpecs.vOrdDate        = est.ord-date
                     ttFoldSpecs.vQty            = eb.bl-qty

                     ttFoldSpecs.vShipName       = eb.ship-name
                     ttFoldSpecs.vAddr           = eb.ship-addr[1]
                     ttFoldSpecs.vAddr2          = eb.ship-addr[2]
                     ttFoldSpecs.vCity           = eb.ship-city
                     ttFoldSpecs.vState          = eb.ship-state
                     ttFoldSpecs.vZip            = eb.ship-zip
                     ttFoldSpecs.vBlankWid       = eb.t-wid 
                     ttFoldSpecs.vBlankLen       = eb.t-len 
                     /*ttFoldSpecs.vBlankSqFt      = vBlankSq */
                     ttFoldSpecs.vBlankSqFt      = eb.t-sqin        
                     .                    
                                                                                    
 END.
 END.  /*end of select*/


 PROCEDURE valid-fi_from-est-no :

     DEF VAR ll-master-est AS LOG NO-UNDO.
     DEF BUFFER b-est FOR est.
     DEF BUFFER b-eb FOR eb.
                            
  /*IF prmEstFrom  = STRING(INT(prmEstFrom),">>>>>>>>") THEN DO:
     cError = " Invalid Estimate ".
         RETURN .
    END.*/

    IF TRIM(prmEstFrom) NE TRIM(lv-master-est-no) AND
       INT(prmEstFrom) NE 0                       THEN DO:
      FIND FIRST b-est
          WHERE b-est.company  EQ prmComp
            AND b-est.est-no   EQ prmEstFrom
          NO-LOCK NO-ERROR.
      IF AVAIL b-est THEN DO:
        RUN ce/com/istandem.p (ROWID(b-est), OUTPUT ll-master-est).

        IF ll-master-est THEN
        FOR EACH b-eb OF b-est
            WHERE b-eb.master-est-no NE ""
            NO-LOCK:
          ll-master-est = NO.
          LEAVE.
        END.
      END.

      IF NOT ll-master-est           OR
         INT(prmEstFrom) GE INT(prmEstNum) THEN DO:
        cError =  " Est# is invalid..." .
        RETURN .
      END.
    END.
    

END PROCEDURE.
                         
                         
/*----------------------------------calc-blank-size------------------------------------*/
PROCEDURE calc-blank-size :
   def buffer bf-eb for eb .
   def var lv-panels as log no-undo.
   def var i as int no-undo.
   def var j as int no-undo.
   def var K_FRAC as dec init 6.25 no-undo.
   /*def var v-score-char like v-lscore-c extent 12.*/
   def buffer xest for est.
   
   find first sys-ctrl  where sys-ctrl.company eq cocode
                           and sys-ctrl.name    eq "PANELS"
        no-lock no-error.
   if not avail sys-ctrl then do transaction:
      create sys-ctrl.
      assign  sys-ctrl.company = cocode
              sys-ctrl.name    = "PANELS"
              sys-ctrl.descrip = "CE Lock=Yes Panel Size Popup when Overriding W&L?"
              sys-ctrl.log-fld = yes.
      MESSAGE sys-ctrl.descrip
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE sys-ctrl.log-fld.
   end.
   lv-panels = sys-ctrl.log-fld.
    
   find xest where /*recid(xest) = recid(est) no-lock. */
                   xest.company = eb.company and
                   xest.est-no = eb.est-no
                   no-lock no-error.
   find xef where recid(xef) = recid(ef) no-lock.
   find xeb where recid(xeb) = recid(eb) no-lock.
   
   find FIRST style where style.company = eb.company and
                    style.style = eb.style
                    no-lock no-error.
   if avail style then do:
      run est/u2kinc1.p (RECID(xeb)).
      run est/u2kinc2.p (RECID(xeb)).
      find bf-eb of eb exclusive-lock.    
      FIND FIRST formule NO-ERROR.

      assign bf-eb.t-wid = (formule[1])
          bf-eb.t-len = (formule[2])
          bf-eb.t-sqin = (formule[7] * formule[8])
          bf-eb.k-wid-array = 0
          bf-eb.k-len-array = 0.
   end.

END PROCEDURE.
