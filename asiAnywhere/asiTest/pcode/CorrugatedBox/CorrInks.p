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
DEFINE TEMP-TABLE ttCorrInks NO-UNDO
    FIELD  vEstNum     AS CHAR 
    FIELD  vCustPart   AS CHAR
    FIELD  vEstDate    AS DATETIME    
    FIELD  vFormNo     AS INT     
    FIELD  vFormQty    AS INT     
    FIELD  vBlankNo    AS INT      
    FIELD  vBlankQty   AS INT

    FIELD  vColor      AS INT     
    FIELD  vPasses     AS INT     
    FIELD  vCoat       AS INT     
    FIELD  vCoatPass   AS INT 
    FIELD  vDscr       AS CHAR
    FIELD  vPs1        AS INT 
    FIELD  vPs2        AS INT
    FIELD  vPs3        AS INT 
    FIELD  vPs4        AS INT
    FIELD  vPs5        AS INT 
    FIELD  vPs6        AS INT
    FIELD  vPs7        AS INT 
    FIELD  vPs8        AS INT
    FIELD  vPs9        AS INT 
    FIELD  vPs10        AS INT
    FIELD  vCode1       AS CHAR
    FIELD  vCode2       AS CHAR
    FIELD  vCode3       AS CHAR
    FIELD  vCode4       AS CHAR
    FIELD  vCode5       AS CHAR
    FIELD  vCode6       AS CHAR
    FIELD  vCode7       AS CHAR
    FIELD  vCode8       AS CHAR
    FIELD  vCode9       AS CHAR
    FIELD  vCode10      AS CHAR
    FIELD  vDscr1       AS CHARACTER
    FIELD  vDscr2       AS CHARACTER
    FIELD  vDscr3       AS CHARACTER
    FIELD  vDscr4       AS CHARACTER
    FIELD  vDscr5       AS CHARACTER
    FIELD  vDscr6       AS CHARACTER
    FIELD  vDscr7       AS CHARACTER
    FIELD  vDscr8       AS CHARACTER
    FIELD  vDscr9       AS CHARACTER
    FIELD  vDscr10      AS CHARACTER
    FIELD  vPer1        AS INT
    FIELD  vPer2        AS INT
    FIELD  vPer3        AS INT
    FIELD  vPer4        AS INT
    FIELD  vPer5        AS INT
    FIELD  vPer6        AS INT
    FIELD  vPer7        AS INT
    FIELD  vPer8        AS INT
    FIELD  vPer9        AS INT
    FIELD  vPer10       AS INT

    FIELD  vPackCode    AS CHAR     
    FIELD  vUnitLen     AS DECIMAL     
    FIELD  vCost        AS DECIMAL     
    FIELD  vUnitWid     AS DECIMAL     
    FIELD  vBoxCode     AS INTEGER     
    FIELD  vUnitDep     AS DECIMAL     
    FIELD  vBundl       AS INTEGER     
    FIELD  vWtPack      AS DECIMAL     
    FIELD  vUnit        AS CHAR  
    FIELD  vLength      AS DEC       
    FIELD  vWidth       AS DEC     
    FIELD  vHeight      AS DEC     
    FIELD  vCost2       AS DECIMAL     
    FIELD  vCount       AS INT     
    FIELD  vLayer       AS INT     
    FIELD  vStack       AS INT     
    FIELD  vStCode      AS CHAR     
    FIELD  vFrCharge    AS CHAR     
    FIELD  vWeiPer      AS DEC
    FIELD  vCarrier     AS CHARACTER
    FIELD  vCarrDscr    AS CHARACTER
    FIELD  vDelZon      AS CHARACTER 
    FIELD  vFreifgt       AS DEC   
    FIELD  vFreOut        AS DEC
    FIELD  vImages        LIKE reftable.dscr
    FIELD  order          AS CHAR 
        .
DEFINE DATASET dsCorrInks FOR ttCorrInks.

DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstNum      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFormNo      AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmColor       AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmPass        AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmCoat        AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmCoatPass    AS INT NO-UNDO. 
DEFINE INPUT PARAMETER prmDscr        AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmPs1         AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmPs2         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs3         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs4         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs5         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs6         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs7         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs8         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs9         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs10        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmCode1       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmCode2       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode3       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode4       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode5       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode6       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode7       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode8       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode9       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode10      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr1       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmDscr2       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr3       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr4       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr5       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr6       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr7       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr8       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr9       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr10      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmPer1        AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmPer2        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer3        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer4        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer5        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer6        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer7        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer8        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer9        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer10       AS INT NO-UNDO.

DEFINE INPUT PARAMETER prmPackCode    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmUnitLen     AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmCostEa      AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmUnitWid     AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmBoxCode     AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmUnitDep     AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmBundl       AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmWTPack      AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmUnit        AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmCost2       AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmCount       AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmLength      AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmWidth       AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmHeight      AS DEC NO-UNDO. 
DEFINE INPUT PARAMETER prmLayer       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmStack       AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmStCode      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFrCharge    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmWeightPer   AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmCarrier     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmCarrDscr    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmDelZon      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmFreight     AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmFreight2    AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmBlankno     AS INT NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrInks.
DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 

IF prmUser       = ?  THEN ASSIGN    prmUser        = "".
IF prmAction     = ?  THEN ASSIGN    prmAction      = "".
IF prmComp       = ?  THEN ASSIGN    prmComp        = "".
IF prmEstNum     = ?  THEN ASSIGN    prmEstNum      = "".
IF prmFormNo     = ?  THEN ASSIGN    prmFormNo      = 0.
IF prmColor      = ?  THEN ASSIGN    prmColor       = 0.
IF prmPass       = ?  THEN ASSIGN    prmPass        = 0.
IF prmCoat       = ?  THEN ASSIGN    prmCoat        = 0.
IF prmCoatPass   = ?  THEN ASSIGN    prmCoatPass    = 0.      
IF prmDscr       = ?  THEN ASSIGN    prmDscr        = "".
IF prmPs1        = ?  THEN ASSIGN    prmPs1         = 0.  
IF prmPs2        = ?  THEN ASSIGN    prmPs2         = 0.  
IF prmPs3        = ?  THEN ASSIGN    prmPs3         = 0.  
IF prmPs4        = ?  THEN ASSIGN    prmPs4         = 0.             
IF prmPs5        = ?  THEN ASSIGN    prmPs5         = 0.     
IF prmPs6        = ?  THEN ASSIGN    prmPs6         = 0. 
IF prmPs7        = ?  THEN ASSIGN    prmPs7         = 0.              
IF prmPs8        = ?  THEN ASSIGN    prmPs8         = 0.     
IF prmPs9        = ?  THEN ASSIGN    prmPs9         = 0. 
IF prmPs10       = ?  THEN ASSIGN    prmPs10        = 0. 
IF prmCode1      = ?  THEN ASSIGN    prmCode1       = "".    
IF prmCode2      = ?  THEN ASSIGN    prmCode2       = "".  
IF prmCode3      = ?  THEN ASSIGN    prmCode3       = "".  
IF prmCode4      = ?  THEN ASSIGN    prmCode4       = "".  
IF prmCode5      = ?  THEN ASSIGN    prmCode5       = "".  
IF prmCode6      = ?  THEN ASSIGN    prmCode6       = "".  
IF prmCode7      = ?  THEN ASSIGN    prmCode7       = "".  
IF prmCode8      = ?  THEN ASSIGN    prmCode8       = "".  
IF prmCode9      = ?  THEN ASSIGN    prmCode9       = "".  
IF prmCode10     = ?  THEN ASSIGN    prmCode10      = "".  
IF prmDscr1      = ?  THEN ASSIGN    prmDscr1       = "".  
IF prmDscr2      = ?  THEN ASSIGN    prmDscr2       = "".  
IF prmDscr3      = ?  THEN ASSIGN    prmDscr3       = "".  
IF prmDscr4      = ?  THEN ASSIGN    prmDscr4       = "".  
IF prmDscr5      = ?  THEN ASSIGN    prmDscr5       = "".  
IF prmDscr6      = ?  THEN ASSIGN    prmDscr6       = "".  
IF prmDscr7      = ?  THEN ASSIGN    prmDscr7       = "".  
IF prmDscr8      = ?  THEN ASSIGN    prmDscr8       = "".
IF prmDscr9      = ?  THEN ASSIGN    prmDscr9       = "". 
IF prmDscr10     = ?  THEN ASSIGN    prmDscr10      = "". 
IF prmPer1       = ?  THEN ASSIGN    prmPer1        = 0. 
IF prmPer2       = ?  THEN ASSIGN    prmPer2        = 0.
IF prmPer3       = ?  THEN ASSIGN    prmPer3        = 0.
IF prmPer4       = ?  THEN ASSIGN    prmPer4        = 0.
IF prmPer5       = ?  THEN ASSIGN    prmPer5        = 0.
IF prmPer6       = ?  THEN ASSIGN    prmPer6        = 0.
IF prmPer7       = ?  THEN ASSIGN    prmPer7        = 0.
IF prmPer8       = ?  THEN ASSIGN    prmPer8        = 0.
IF prmPer9       = ?  THEN ASSIGN    prmPer9        = 0.
IF prmPer10      = ?  THEN ASSIGN    prmPer10       = 0.
                                                 
IF prmPackCode   = ?  THEN ASSIGN    prmPackCode    = "".
IF prmUnitLen    = ?  THEN ASSIGN    prmUnitLen     = 0.
IF prmCostEa     = ?  THEN ASSIGN    prmCostEa      = 0.
IF prmUnitWid    = ?  THEN ASSIGN    prmUnitWid     = 0.
IF prmBoxCode    = ?  THEN ASSIGN    prmBoxCode     = 0.
IF prmUnitDep    = ?  THEN ASSIGN    prmUnitDep     =  0.
IF prmBundl      = ?  THEN ASSIGN    prmBundl       = 0.
IF prmWTPack     = ?  THEN ASSIGN    prmWTPack      = 0.
IF prmUnit       = ?  THEN ASSIGN    prmUnit        = "".
IF prmCost2      = ?  THEN ASSIGN    prmCost2       = 0.
IF prmCount      = ?  THEN ASSIGN    prmCount       = 0.
IF prmLength     = ?  THEN ASSIGN    prmLength      = 0.
IF prmWidth      = ?  THEN ASSIGN    prmWidth       = 0.
IF prmHeight     = ?  THEN ASSIGN    prmHeight      = 0.
IF prmStack      = ?  THEN ASSIGN    prmStack       = 0.
IF prmStCode     = ?  THEN ASSIGN    prmStCode      = "".
IF prmFrCharge   = ?  THEN ASSIGN    prmFrCharge    = "".
IF prmWeightPer  = ?  THEN ASSIGN    prmWeightPer   = 0.
IF prmCarrier    = ?  THEN ASSIGN    prmCarrier     = "".
IF prmCarrDscr   = ?  THEN ASSIGN    prmCarrDscr    = "".
IF prmDelZon     = ?  THEN ASSIGN    prmDelZon      = "".
IF prmFreight    = ?  THEN ASSIGN    prmFreight     = 0.
IF prmFreight2   = ?  THEN ASSIGN    prmFreight2    = 0.
IF prmBlankno    = ?  THEN ASSIGN    prmBlankno     = 1.
                
 DEF BUFFER b-ef FOR ef.
 DEF BUFFER b-eb FOR eb.
 def buffer bf-eb for eb .
 DEF VAR li AS INT NO-UNDO.
 DEFINE VAR bi AS INT NO-UNDO.
 DEFINE VAR vEstimate AS CHAR NO-UNDO.
 def var li-num-of-code as int no-undo.
 DEF VAR ll-foam AS LOG NO-UNDO.

 DEF VAR li1 AS INT NO-UNDO.
 DEF VAR li2 AS INT NO-UNDO.
 DEF VAR li3 AS INT NO-UNDO.
 DEF VAR li4 AS INT NO-UNDO.
 DEF VAR lv-rowid1 AS ROWID NO-UNDO.
 DEF VAR lv-rowid2 AS ROWID NO-UNDO.
 DEF VAR lv-i-ps LIKE eb.i-ps NO-UNDO.
 DEF VAR lv-i-code LIKE eb.i-code NO-UNDO.
 DEF VAR lv-i-dscr LIKE eb.i-dscr NO-UNDO.
 DEF VAR lv-i-% LIKE eb.i-% NO-UNDO.

 {est/inksvarn.i NEW}
 DEFINE NEW SHARED VARIABLE vError AS CHARACTER NO-UNDO.

 DEF NEW SHARED var k as int no-undo.
 def var counter as int no-undo.
 DEF NEW SHARED var i as int no-undo.
 DEF NEW SHARED var j as int no-undo.
 DEF NEW SHARED var x  as int no-undo.
 DEF NEW SHARED var  y  as int no-undo.
 DEF NEW SHARED var z  as   int no-undo.

 def var save_id as recid no-undo.
 def var save_id2 as recid no-undo.
 def buffer alt-item for item .
 def var choice as log no-undo.
 DEF NEW SHARED var cocode as  CHAR  no-undo.
 def NEW SHARED var locode as char  no-undo.
 def NEW SHARED var g_company as char  no-undo. 
 def NEW SHARED VAR g_loc as char  no-undo.


    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_DEFault = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

vEstimate =  FILL(" ",8 - LENGTH(TRIM(prmEstNum))) + TRIM(prmEstNum).

ASSIGN 
    cocode = prmComp
    locode = "MAIN"
    g_company = prmComp
    .

/*************************************prmAction***************************************************/  

IF prmAction = "InksUpdate" THEN DO:
   
      li-num-of-code = 0.
      if prmCode1 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode2 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode3 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode4 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode5 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode6 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode7 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode8 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode9 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode10 <> "" then li-num-of-code = li-num-of-code + 1.

      if li-num-of-code <> ((prmColor) + (prmCoat)) 
      then do:
          ASSIGN
          cError =  "Invalid Number of Color and Coating." .
          return .
      end.      

      if prmCarrier <> ""  THEN DO:
      FIND first carrier where carrier.carrier = prmCarrier AND carrier.company = prmComp NO-LOCK NO-ERROR.
      IF NOT AVAIL carrier then do:
          ASSIGN
         cError =  "Invalid Carrier. Try Help." .
         return .
      end.
      END.
      if prmPackCode <> "" THEN DO:
       FIND FIRST item where item.company = prmComp and item.i-no = prmPackCode NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM then do:
            ASSIGN
            cError = "Invalid Packing Code. Try Help." .
            return .
      end.
      END.
      if prmUnit <> "" THEN DO:
       FIND FIRST  item where item.company = prmComp and item.i-no = prmUnit NO-LOCK NO-ERROR.
       IF NOT AVAIL ITEM then do:
           ASSIGN
            cError =  "Invalid Unit#. Try Help." .
            return .
      end.
      END.
      IF prmDelZon <> "" THEN DO:
      FIND FIRST carr-mtx  WHERE carr-mtx.company  EQ prmComp 
                      /*AND carr-mtx.loc      EQ locode*/
                      AND carr-mtx.carrier  EQ prmCarrier
                      AND carr-mtx.del-zone EQ prmDelZon NO-LOCK NO-ERROR.
     IF NOT AVAIL carr-mtx THEN DO:
         ASSIGN
      cError =  "Invalid Delivery Zone, try help..." .
      RETURN .
    END.
    END.

      
      
      RUN stackImage (prmStCode).

    IF prmCode1 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode1  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code1, try help..." .
        RETURN .
      END.
      END.
    IF prmCode2 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode2  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code2, try help..." .
        RETURN .
      END.
      END.
    IF prmCode3 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode3  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code3, try help..." .
        RETURN .
      END.
      END.
    IF prmCode4 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode4  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code4, try help..." .
        RETURN .
      END.
      END.
    IF prmCode5 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode5  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code5, try help..." .
        RETURN .
      END.
      END.
    IF prmCode6 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode6  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code6, try help..." .
        RETURN .
      END.
      END.
    IF prmCode7 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode7  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code7, try help..." .
        RETURN .
      END.
      END.
    IF prmCode8 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode8  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code8, try help..." .
        RETURN .
      END.
      END.
    IF prmCode9 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode9  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code9, try help..." .
        RETURN .
      END.
      END.
    IF prmCode10 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode10  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code10, try help..." .
        RETURN .
      END.
      END.

       IF DEC(prmCount) NE 0 AND DEC(prmWTPack)  NE 0 THEN DO:
       ASSIGN
       cError =  "You may enter EITHER  Count OR  WTPack " .
       RETURN.
       END.


       IF  prmStCode NE "" THEN DO:
           FIND FIRST reftable   where (reftable.reftable eq "STACK" and
                                        reftable.company  eq "" /* KLUDGE */  and
                                        reftable.loc      eq "" /* KLUDGE */ )
               AND reftable.CODE EQ prmStCode NO-LOCK NO-ERROR.
           IF NOT AVAIL reftable  THEN DO:
               ASSIGN
                   cError =  "Invalid Stacking Code..."  .
               RETURN .
               END.
               RUN stackImage (prmStCode).
       END.

END. /*end of validation*/



IF prmAction = "InksUpdate" THEN DO:

FIND FIRST est where est.company = prmComp AND est.est-no = vEstimate EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL est THEN DO:
    ASSIGN
        est.updated-id  = prmUser.
    END.

FIND FIRST ef where ef.company = prmComp AND ef.est-no = vEstimate AND ef.form-no = prmFormno EXCLUSIVE-LOCK NO-ERROR.
     find FIRST eb where eb.company = prmComp AND eb.est-no = ef.est-no AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno  EXCLUSIVE-LOCK NO-ERROR.
      
 IF AVAIL ef  AND AVAIL eb  THEN DO:
       ASSIGN 
              eb.i-col     = prmColor    
              eb.i-pass    = prmPass     
              eb.i-coat    = prmCoat     
              eb.i-coat-p  = prmCoatPass 
              eb.i-coldscr = prmDscr     
              eb.i-ps[1]   = prmPs1      
              eb.i-ps[2]   = prmPs2      
              eb.i-ps[3]   = prmPs3      
              eb.i-ps[4]   = prmPs4      
              eb.i-ps[5]   = prmPs5      
              eb.i-ps[6]   = prmPs6      
              eb.i-ps[7]    = prmPs7      
              eb.i-ps[8]    = prmPs8      
              eb.i-ps[9]    = prmPs9      
              eb.i-ps[10]   = prmPs10     
              eb.i-code[1]  = prmCode1    
              eb.i-code[2]  = prmCode2    
              eb.i-code[3]  = prmCode3    
              eb.i-code[4]  = prmCode4                                                 
              eb.i-code[5]  = prmCode5                                          
              eb.i-code[6]  = prmCode6                                         
              eb.i-code[7]  = prmCode7    
              eb.i-code[8]  = prmCode8    
              eb.i-code[9]  = prmCode9    
              eb.i-code[10] = prmCode10   
              eb.i-dscr[1]  = prmDscr1    
              eb.i-dscr[2]  = prmDscr2    
              eb.i-dscr[3]  = prmDscr3    
              eb.i-dscr[4]  = prmDscr4    
              eb.i-dscr[5]  = prmDscr5    
              eb.i-dscr[6]  = prmDscr6    
              eb.i-dscr[7]  = prmDscr7    
              eb.i-dscr[8]  = prmDscr8    
              eb.i-dscr[9]  = prmDscr9   
              eb.i-dscr[10] = prmDscr10   
              eb.i-%[1]     = prmPer1 
              eb.i-%[2]     = prmPer2     
              eb.i-%[3]     = prmPer3     
              eb.i-%[4]     = prmPer4     
              eb.i-%[5]     = prmPer5     
              eb.i-%[6]     =  prmPer6     
              eb.i-%[7]     =  prmPer7     
              eb.i-%[8]     =  prmPer8     
              eb.i-%[9]     =  prmPer9     
              eb.i-%[10]    =  prmPer10    
                                          
              eb.cas-no       =  prmPackCode 
              eb.cas-len      =  prmUnitLen  
              eb.cas-cost     =  prmCostEa   
              eb.cas-wid      =  prmUnitWid  
              eb.cas-cnt      =  prmBoxCode  
              eb.cas-dep      =  prmUnitDep  
              eb.cas-pal      =  prmBundl    
              eb.cas-wt       =  prmWTPack   
              eb.tr-no        =  prmUnit     
              eb.tr-cost      =  prmCost2    
              eb.tr-cnt       =  prmCount    
              eb.tr-len       =  prmLength   
              eb.tr-wid       =  prmWidth    
              eb.tr-dep       =  prmHeight   
              eb.tr-cas       =  prmLayer    
              eb.stacks       =  prmStack    
              eb.stack-code   =  prmStCode   
              eb.chg-method   =  prmFrCharge 
              eb.weight-m     =  prmWeightPer
              eb.carrier      =  prmCarrier  
              eb.dest-code    =  prmDelZon   
              eb.fr-out-c     =  prmFreight  
              eb.fr-out-m     =  prmFreight2 .

       FIND first carrier where carrier.carrier = prmCarrier AND carrier.company = prmComp NO-LOCK NO-ERROR.
       IF AVAIL  carrier THEN DO:
           ASSIGN
               eb.carr-dscr    =  carrier.dscr  .
       END.


              ASSIGN prmAction = "Select".
 END.
 END.
          

  IF prmAction = "jobstds" THEN DO:

      
    FIND FIRST est where est.company = prmComp AND est.est-no = vEstimate EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL est THEN
        FOR EACH job-hdr
            WHERE job-hdr.company EQ est.company
            AND job-hdr.est-no  EQ est.est-no
            NO-LOCK,
            FIRST job
                where job.company EQ job-hdr.company
                and job.job     EQ job-hdr.job
                and job.job-no  EQ job-hdr.job-no
                and job.job-no2 EQ job-hdr.job-no2
                and job.est-no  EQ job-hdr.est-no
                AND job.opened  EQ YES
            NO-LOCK
            BREAK BY job.job:
      
            IF LAST(job.job) OR job-hdr.ord-no EQ est.ord-no THEN DO:

                
                RUN jc/jobstds.p (ROWID(job)).
                LEAVE.
            END.
        END.

        IF vError <> "" THEN DO:
            ASSIGN cError = vError .
            RETURN.
        END.           

        ASSIGN prmAction = "Select".
  END.


  IF prmAction = "ColorChange" THEN DO:
      
    FIND FIRST ef where ef.company = prmComp AND ef.est-no = vEstimate AND ef.form-no = prmFormno EXCLUSIVE-LOCK NO-ERROR.
    find FIRST eb where eb.company = prmComp AND eb.est-no = ef.est-no AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno  EXCLUSIVE-LOCK NO-ERROR.

    CREATE ttCorrInks.

    ASSIGN ttCorrInks.vPasses = DEC(prmPass).

    IF DEC(prmColor) EQ 0 THEN 
        ASSIGN ttCorrInks.vPasses = 0.
    ELSE
        IF DEC(prmPass) EQ 0 THEN ASSIGN  ttCorrInks.vPasses = 1.

    IF DEC(prmColor) GE 1 AND prmPass EQ 0 THEN ASSIGN prmPass = 1.

    {est/defInks.i}
  END.

  IF prmAction = "CoatChange" THEN DO:
    FIND FIRST ef where ef.company = prmComp AND ef.est-no = vEstimate AND ef.form-no = prmFormno EXCLUSIVE-LOCK NO-ERROR.
    find FIRST eb where eb.company = prmComp AND eb.est-no = ef.est-no AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno  EXCLUSIVE-LOCK NO-ERROR.

    CREATE ttCorrInks.

    ASSIGN ttCorrInks.vCoatPass = DEC(prmCoatPass).

    IF DEC(prmCoat) EQ 0 THEN 
        ASSIGN ttCorrInks.vCoatPass = 0.
    ELSE
        IF DEC(prmCoatPass) EQ 0 THEN ASSIGN  ttCorrInks.vCoatPass = 1.

    IF DEC(prmCoat) GE 1 AND prmCoatPass EQ 0 THEN ASSIGN prmCoatPass = 1.

    {est/defInks.i}
  END.  

  IF prmAction = "CoatPassChange" THEN DO:
    FIND FIRST ef where ef.company = prmComp AND ef.est-no = vEstimate AND ef.form-no = prmFormno EXCLUSIVE-LOCK NO-ERROR.
    find FIRST eb where eb.company = prmComp AND eb.est-no = ef.est-no AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno  EXCLUSIVE-LOCK NO-ERROR.

    CREATE ttCorrInks.    

    {est/defInks.i}
 END.

 IF prmAction = "PassesChange" THEN DO:
    FIND FIRST ef where ef.company = prmComp AND ef.est-no = vEstimate AND ef.form-no = prmFormno EXCLUSIVE-LOCK NO-ERROR.
    find FIRST eb where eb.company = prmComp AND eb.est-no = ef.est-no AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno  EXCLUSIVE-LOCK NO-ERROR.

    CREATE ttCorrInks.    

    {est/defInks.i}
 END.



 IF prmAction = "ResetInk" THEN DO:

     FIND FIRST ef where ef.company = prmComp AND ef.est-no = vEstimate AND ef.form-no = prmFormno EXCLUSIVE-LOCK NO-ERROR.
     find FIRST eb where eb.company = prmComp AND eb.est-no = ef.est-no AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno  EXCLUSIVE-LOCK NO-ERROR.

    find first style where style.company = eb.company and
                 style.style = eb.style no-lock no-error.

     if avail style then do:
         if k = 0 then k = integer(style.material[3]).
         find first item where item.company = eb.company and
                    item.i-no = style.material[2] no-lock no-error.
         if avail item then k = integer(style.material[3]).
         find first alt-item where alt-item.company  = eb.company  and
                                   alt-item.mat-type = "V"     and
                                   alt-item.i-no     = style.material[6]
                                   no-lock no-error.
      end.

      if not avail item or not avail alt-item or (k = 0) then do:
         find first ce-ctrl where ce-ctrl.company = eb.company and
                                  ce-ctrl.loc = eb.loc
                                   no-lock no-error.
         if k = 0 then k = ce-ctrl.def-inkcov.
         if not avail item then do:
            find first item where item.company = eb.company and
                       item.i-no = ce-ctrl.def-ink no-lock no-error.
         end.
         if not avail alt-item then
            find first alt-item where alt-item.company  = eb.company  and
                                      alt-item.mat-type = "V"     and
                                      alt-item.i-no     = ce-ctrl.def-coat
                                      no-lock no-error.
      end.

      save_id = recid(item). save_id2 = recid(alt-item).
      j = (integer(prmColor)
          + integer(prmCoat)) 
          / integer(prmPass).
          
      {sys/inc/roundup.i j}
      counter = 1.
      choice = true.      


      if choice then do i = 1 to 10:
         if i le integer(prmColor) then do :
              find item where recid(item) = save_id no-lock no-error.
                           

             case string(i) :
                when "1" then assign eb.i-ps[1]     = counter
                                     eb.i-code[1]   = item.i-no
                                     eb.i-dscr[1]   = item.est-dscr
                                     eb.i-%[1]      = k.
                when "2" then assign eb.i-ps[2]     = counter
                                     eb.i-code[2]   = item.i-no
                                     eb.i-dscr[2]   = item.est-dscr
                                     eb.i-%[2]      = k.
                when "3" then assign eb.i-ps[3]     = counter
                                     eb.i-code[3]   = item.i-no
                                     eb.i-dscr[3]   = item.est-dscr
                                     eb.i-%[3]      = k.
                when "4" then assign eb.i-ps[4]     = counter
                                     eb.i-code[4]   = item.i-no
                                     eb.i-dscr[4]   = item.est-dscr
                                     eb.i-%[4]      = k.
                when "5" then assign eb.i-ps[5]     = counter
                                     eb.i-code[5]   = item.i-no
                                     eb.i-dscr[5]   = item.est-dscr
                                     eb.i-%[5]      = k.
                when "6" then assign eb.i-ps[6]     = counter
                                     eb.i-code[6]   = item.i-no
                                     eb.i-dscr[6]   = item.est-dscr
                                     eb.i-%[6]      = k.
                
                when "7" then assign eb.i-ps[7]     = counter
                                     eb.i-code[7]    = item.i-no
                                     eb.i-dscr[7]   = item.est-dscr
                                     eb.i-%[7]      = k.
                                     
                when "8" then assign eb.i-ps[8]     = counter
                                     eb.i-code[8]   = item.i-no
                                     eb.i-dscr[8]   = item.est-dscr
                                     eb.i-%[8]      = k.
                when "9" then assign eb.i-ps[9]     = counter
                                     eb.i-code[9]   = item.i-no
                                     eb.i-dscr[9]   = item.est-dscr
                                     eb.i-%[9]      = k.
                when "10" then assign eb.i-ps[10]   = counter
                                     eb.i-code[10]  = item.i-no
                                     eb.i-dscr[10]  = item.est-dscr
                                     eb.i-%[10]     = k.             
                                                                     
             end case. 

         end. 

         else if (i > integer(prmColor)) and
                 (i <= (integer(prmColor) + 
                       integer(prmCoat)))
         then do:
              find alt-item where recid(alt-item) = save_id2 no-lock no-error.
        
              case string(i) :
                when "1" then assign eb.i-ps[1]     = counter
                                     eb.i-code[1]   = alt-item.i-no
                                     eb.i-dscr[1]   = alt-item.est-dscr
                                     eb.i-%[1]      = 100 .
                when "2" then assign eb.i-ps[2]     = counter
                                     eb.i-code[2]   = alt-item.i-no
                                     eb.i-dscr[2]   = alt-item.est-dscr
                                     eb.i-%[2]      = 100.
                when "3" then assign eb.i-ps[3]     = counter
                                     eb.i-code[3]   = alt-item.i-no
                                     eb.i-dscr[3]   = alt-item.est-dscr
                                     eb.i-%[3]      = 100.
                when "4" then assign eb.i-ps[4]     = counter
                                     eb.i-code[4]   = alt-item.i-no
                                     eb.i-dscr[4]   = alt-item.est-dscr
                                     eb.i-%[4]      = 100.
                when "5" then assign eb.i-ps[5]     = counter
                                     eb.i-code[5]   = alt-item.i-no
                                     eb.i-dscr[5]   = alt-item.est-dscr
                                     eb.i-%[5]      = 100.
                when "6" then assign eb.i-ps[6]     = counter
                                     eb.i-code[6]   = alt-item.i-no
                                     eb.i-dscr[6]   = alt-item.est-dscr
                                     eb.i-%[6]      = 100.                                     
                when "7" then assign eb.i-ps[7]     = counter
                                     eb.i-code[7]   = alt-item.i-no
                                     eb.i-dscr[7]   = alt-item.est-dscr
                                     eb.i-%[7]      = 100.
                when "8" then assign eb.i-ps[8]     = counter
                                     eb.i-code[8]   = alt-item.i-no
                                     eb.i-dscr[8]   = alt-item.est-dscr
                                     eb.i-%[8]      = 100.
                when "9" then assign eb.i-ps[9]     = counter
                                     eb.i-code[9]   = alt-item.i-no
                                     eb.i-dscr[9]   = alt-item.est-dscr
                                     eb.i-%[9]      = 100.
                when "10" then assign eb.i-ps[10]   = counter
                                     eb.i-code[10]  = alt-item.i-no
                                     eb.i-dscr[10]  = alt-item.est-dscr
                                     eb.i-%[10]     = 100.
                                               
             end.                   
             
         end.
         
         else if (i >  integer(prmColor) + 
                       integer(prmCoat) )
         then do:
        
              case string(i) :
                   when "1" then assign eb.i-ps[1]   = 0
                                        eb.i-code[1] = ""
                                        eb.i-dscr[1]= ""
                                        eb.i-%[1]   = 0.
                   when "2" then assign eb.i-ps[2]   = 0
                                        eb.i-code[2] = ""
                                        eb.i-dscr[2]= ""
                                        eb.i-%[2]   = 0.
                   when "3" then assign eb.i-ps[3]   = 0
                                        eb.i-code[3] = ""
                                        eb.i-dscr[3] = ""
                                        eb.i-%[3]    = 0.
                   when "4" then assign eb.i-ps[4]   = 0
                                        eb.i-code[4] = ""
                                        eb.i-dscr[4] = ""
                                        eb.i-%[4]   = 0.
                   when "5" then assign eb.i-ps[5]   = 0
                                        eb.i-code[5] = ""
                                        eb.i-dscr[5] = ""
                                        eb.i-%[5]    = 0.
                   when "6" then assign eb.i-ps[6]   = 0
                                        eb.i-code[6] = ""
                                        eb.i-dscr[6] = ""
                                        eb.i-%[6]    = 0.
                   when "7" then assign eb.i-ps[7]   = 0
                                        eb.i-code[7] = ""
                                        eb.i-dscr[7] = ""
                                        eb.i-%[7]   = 0.
                   when "8" then assign eb.i-ps[8]   = 0
                                        eb.i-code[8] = ""
                                        eb.i-dscr[8] = ""
                                        eb.i-%[8]    = 0.
                   when "9" then assign eb.i-ps[9]   = 0
                                        eb.i-code[9] = ""
                                        eb.i-dscr[9] = ""
                                        eb.i-%[9]    = 0.
                   when "10" then assign eb.i-ps[10]   = 0
                                        eb.i-code[10] = ""
                                        eb.i-dscr[10] = ""
                                        eb.i-%[10]    = 0.                                        

              end case.       
                     
         end.
         
         if i modulo j = 0 then counter = counter + 1.
         if counter > integer(prmPass) then counter = integer(prmPass).
      
     /* end.*/
        
   find bf-eb where recid(bf-eb) = recid(eb).
   assign bf-eb.i-ps[1] = int(eb.i-ps[1])
          bf-eb.i-ps[2] = int(eb.i-ps[2])
          bf-eb.i-ps[3] = int(eb.i-ps[3])
          bf-eb.i-ps[4] = int(eb.i-ps[4])
          bf-eb.i-ps[5] = int(eb.i-ps[5])
          bf-eb.i-ps[6] = int(eb.i-ps[6])
          bf-eb.i-ps[7] = int(eb.i-ps[7])
          bf-eb.i-ps[8] = int(eb.i-ps[8])
          bf-eb.i-ps[9] = int(eb.i-ps[9])
          bf-eb.i-ps[10] = int(eb.i-ps[10])
          bf-eb.i-code[1] = eb.i-code[1] 
          bf-eb.i-code[2] = eb.i-code[2] 
          bf-eb.i-code[3] = eb.i-code[3] 
          bf-eb.i-code[4] = eb.i-code[4] 
          bf-eb.i-code[5] = eb.i-code[5] 
          bf-eb.i-code[6] = eb.i-code[6] 
          bf-eb.i-code[7] = eb.i-code[7] 
          bf-eb.i-code[8] = eb.i-code[8] 
          bf-eb.i-code[9] = eb.i-code[9] 
          bf-eb.i-code[10] = eb.i-code[10] 
          bf-eb.i-dscr[1] = eb.i-dscr[1]
          bf-eb.i-dscr[2] = eb.i-dscr[2]
          bf-eb.i-dscr[3] = eb.i-dscr[3]
          bf-eb.i-dscr[4] = eb.i-dscr[4]
          bf-eb.i-dscr[5] = eb.i-dscr[5]
          bf-eb.i-dscr[6] = eb.i-dscr[6]
          bf-eb.i-dscr[7] = eb.i-dscr[7]
          bf-eb.i-dscr[8] = eb.i-dscr[8]
          bf-eb.i-dscr[9] = eb.i-dscr[9]
          bf-eb.i-dscr[10] = eb.i-dscr[10] 
          bf-eb.i-%[1] = int(eb.i-%[1] )
          bf-eb.i-%[2] = int(eb.i-%[2] )
          bf-eb.i-%[3] = int(eb.i-%[3] )
          bf-eb.i-%[4] = int(eb.i-%[4] )
          bf-eb.i-%[5] = int(eb.i-%[5] )
          bf-eb.i-%[6] = int(eb.i-%[6] )
          bf-eb.i-%[7] = int(eb.i-%[7] )
          bf-eb.i-%[8] = int(eb.i-%[8] )
          bf-eb.i-%[9] = int(eb.i-%[9] )
          bf-eb.i-%[10] = int(eb.i-%[10] )
          .
          
END.

    ASSIGN prmAction = "Select".

 END.


 IF prmAction = "Select" THEN DO:
     
   FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH b-ef
        WHERE b-ef.company EQ est.company
          AND b-ef.est-no  EQ vEstimate
        BREAK BY b-ef.eqty:
      IF FIRST-OF(b-ef.eqty) THEN li = 0.
      li = li + 1.
    END.
    IF li NE est.form-qty THEN DO:
      FIND CURRENT est EXCLUSIVE NO-WAIT NO-ERROR.
      IF AVAIL est THEN est.form-qty = li.
      FIND CURRENT est NO-LOCK.
    END.
   
    /*DISABLE TRIGGERS FOR LOAD OF ef.*/
    FIND FIRST ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ prmFormNo
        NO-LOCK NO-ERROR.
    IF AVAIL ef THEN DO:
      FOR EACH b-eb
          WHERE b-eb.company EQ ef.company
            AND b-eb.est-no  EQ ef.est-no
            AND b-eb.form-no EQ ef.form-no
          NO-LOCK:
        bi = bi + 1.
      END.
      IF li NE ef.blank-qty THEN DO:
        FIND CURRENT ef EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL ef THEN ef.blank-qty = bi.
        FIND CURRENT ef NO-LOCK.
      END.
    END.
  

  FOR EACH eb WHERE eb.company = prmComp AND eb.est-no = est.est-no  AND eb.form-no = prmFormNo AND eb.blank-no = prmBlankno NO-LOCK:
      IF AVAIL eb THEN DO:
          CREATE ttCorrInks.
          ASSIGN
              ttCorrInks.vEstNum        = est.est-no
              ttCorrInks.vCustPart      = eb.part-no
              ttCorrInks.vEstDate       = est.est-date
              ttCorrInks.vFormNo        = eb.form-no
              ttCorrInks.vFormQty       = est.form-qty
              ttCorrInks.vBlankNo       = eb.blank-no
              ttCorrInks.vBlankQty      = bi
              ttCorrInks.vColor         = eb.i-col
              ttCorrInks.vPasses        = eb.i-pass
              ttCorrInks.vCoat          = eb.i-coat
              ttCorrInks.vCoatPass      = eb.i-coat-p
              ttCorrInks.vDscr          = eb.i-coldscr
              ttCorrInks.vPs1           = eb.i-ps[1]
              ttCorrInks.vPs2           = eb.i-ps[2]
              ttCorrInks.vPs3           = eb.i-ps[3]
              ttCorrInks.vPs4           = eb.i-ps[4]
              ttCorrInks.vPs5           = eb.i-ps[5]
              ttCorrInks.vPs6           = eb.i-ps[6]
              ttCorrInks.vPs7           = eb.i-ps[7]
              ttCorrInks.vPs8           = eb.i-ps[8] 
              ttCorrInks.vPs9           = eb.i-ps[9]
              ttCorrInks.vPs10          = eb.i-ps[10]
              ttCorrInks.vCode1         = eb.i-code[1]
              ttCorrInks.vCode2         = eb.i-code[2]
              ttCorrInks.vCode3         = eb.i-code[3]
              ttCorrInks.vCode4         = eb.i-code[4]
              ttCorrInks.vCode5         = eb.i-code[5]
              ttCorrInks.vCode6         = eb.i-code[6]
              ttCorrInks.vCode7         = eb.i-code[7]
              ttCorrInks.vCode8         = eb.i-code[8]
              ttCorrInks.vCode9         = eb.i-code[9]
              ttCorrInks.vCode10        = eb.i-code[10]
              ttCorrInks.vDscr1         = eb.i-dscr[1]
              ttCorrInks.vDscr2         = eb.i-dscr[2]
              ttCorrInks.vDscr3         = eb.i-dscr[3]
              ttCorrInks.vDscr4         = eb.i-dscr[4]
              ttCorrInks.vDscr5         = eb.i-dscr[5]
              ttCorrInks.vDscr6         = eb.i-dscr[6]
              ttCorrInks.vDscr7         = eb.i-dscr[7]
              ttCorrInks.vDscr8         = eb.i-dscr[8]
              ttCorrInks.vDscr9         = eb.i-dscr[9]  
              ttCorrInks.vDscr10        = eb.i-dscr[10]
              ttCorrInks.vPer1          = eb.i-%[1]
              ttCorrInks.vPer2          = eb.i-%[2]
              ttCorrInks.vPer3          = eb.i-%[3] 
              ttCorrInks.vPer4          = eb.i-%[4]
              ttCorrInks.vPer5          = eb.i-%[5]
              ttCorrInks.vPer6          = eb.i-%[6]
              ttCorrInks.vPer7          = eb.i-%[7]
              ttCorrInks.vPer8          = eb.i-%[8]
              ttCorrInks.vPer9          = eb.i-%[9]
              ttCorrInks.vPer10         = eb.i-%[10]
              
              ttCorrInks.vPackCode      = eb.cas-no        
              ttCorrInks.vUnitLen       = eb.cas-len
              ttCorrInks.vCost          = eb.cas-cost
              ttCorrInks.vUnitWid       = eb.cas-wid
              ttCorrInks.vBoxCode       = eb.cas-cnt
              ttCorrInks.vUnitDep       = eb.cas-dep
              ttCorrInks.vBundl         = eb.cas-pal
              ttCorrInks.vWtPack        = eb.cas-wt
              ttCorrInks.vUnit          = eb.tr-no
              ttCorrInks.vLength        = eb.tr-len
              ttCorrInks.vWidth         = eb.tr-wid
              ttCorrInks.vHeight        = eb.tr-dep
              ttCorrInks.vCost2         = eb.tr-cost
              ttCorrInks.vCount         = eb.tr-cnt
              ttCorrInks.vLayer         = eb.tr-cas
              ttCorrInks.vStack         = eb.stacks
              ttCorrInks.vStCode        = eb.stack-code
              ttCorrInks.vFrCharge      = eb.chg-method
              ttCorrInks.vWeiPer        = eb.weight-m
              ttCorrInks.vCarrier       = eb.carrier
              ttCorrInks.vCarrDscr      = eb.carr-dscr
              ttCorrInks.vDelZon        = eb.dest-code
              ttCorrInks.vFreifgt       = eb.fr-out-c
              ttCorrInks.vFreOut        = eb.fr-out-m 
              ttCorrInks.order          = STRING(eb.ord-no) .
          IF ttCorrInks.vFrCharge = "" THEN
              ASSIGN ttCorrInks.vFrCharge = "P" .
         
          FIND FIRST reftable NO-LOCK  WHERE reftable.reftable EQ 'stackpat'
              AND reftable.company EQ ''  AND reftable.loc EQ ''
              AND reftable.code EQ eb.stack-code  NO-ERROR.
          IF AVAILABLE reftable  THEN
              ASSIGN
                  ttCorrInks.vImages    = reftable.dscr.
              .
              
END. /*ettCorrInksnd of  eb*/
  END. /* end ttCorrInksof eb*/
 END. /* end of select*/

/**********************procedure*******************************/




PROCEDURE stackImage :
DEFINE INPUT PARAMETER ipStackCode AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
  
  IF ipStackCode NE '' THEN
  DO:
    FIND FIRST reftable NO-LOCK
         WHERE reftable.reftable EQ 'stackpat'
           AND reftable.company EQ ''
           AND reftable.loc EQ ''
           AND reftable.code EQ ipStackCode NO-ERROR.
    IF AVAILABLE reftable AND SEARCH(reftable.dscr) NE ? THEN DO:
        
    /*ASSIGN
      vImages  = reftable.dscr.*/
        END.
  END.

END PROCEDURE.



