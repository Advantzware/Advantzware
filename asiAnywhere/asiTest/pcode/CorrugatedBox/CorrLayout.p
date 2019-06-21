

/*------------------------------------------------------------------------
    File        : CorrLayout.p
    Purpose     : CorrLayout
    Syntax      :

    Description : Return a Dataset of Corrugated Layout 
    Author(s)   : 
    Created     : 4 Feb 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttCorrLayout NO-UNDO
        FIELD vEstimate            AS CHAR FORMAT "x(8)" 
        FIELD vEstDate             AS DATE  
        FIELD vForm                AS INTEGER
        FIELD vFormQty             AS INTEGER
        FIELD vCustPart            AS CHAR    
        FIELD vMachine             AS CHAR FORMAT "x(6)" 
        FIELD vMachDscr            AS CHAR 
        FIELD vFrontBack           AS DECIMAL FORMAT ">>9.99"    
        FIELD vSideSide            AS DECIMAL FORMAT ">>9.99" 
        FIELD vRevCorr             AS CHAR FORMAT "X"
        FIELD vBoard               AS CHAR   
        FIELD vBoardName           AS CHAR  FORMAT "X(50)"  
        FIELD vReal                AS CHAR 
        FIELD vFlute               AS CHAR 
        FIELD vTest                AS CHAR    
        FIELD vCostMsf             AS DECIMAL    
        FIELD vCostUom             AS CHAR    
        FIELD vWeight              AS DECIMAL    
        FIELD vFreightCwt          AS DECIMAL 
        FIELD vFreightUom          AS CHAR
        FIELD vNc                  AS CHAR 
        FIELD vGrosShetWid         AS DECIMAL FORMAT ">>>9.99"
        FIELD vGrosShetLen         AS DECIMAL FORMAT ">>>9.99" 
        FIELD vGrosShetDep         AS DECIMAL FORMAT ">>>9.99"
        FIELD vOutWid              AS DECIMAL    
        FIELD vOutLen              AS DECIMAL   
        FIELD vOutDep              AS DECIMAL
        FIELD vOutCut              AS INTEGER
        FIELD vOutTotalUp          AS CHAR
        FIELD vOutSqFeet           AS CHAR   
        FIELD vDieInches           AS DECIMAL    
        FIELD vNetShetWid          AS DECIMAL   
        FIELD vNetShetLen          AS DECIMAL
        FIELD vNetShetDep          AS DECIMAL
        FIELD vDieSizeLen          AS DECIMAL
        FIELD vDieSizeWid          AS DECIMAL
        FIELD vDieSizeDep          AS DECIMAL
        FIELD vOnWid               AS DECIMAL
        FIELD vOnLen               AS DECIMAL
        FIELD vOnDep               AS DECIMAL
        FIELD vOnTotalUp           AS INTEGER
        FIELD vOnSqFeet            AS DECIMAL 
        FIELD vBlankWid            AS DECIMAL
        FIELD vBlankLen            AS DECIMAL 
        FIELD vBlankDep            AS DECIMAL
        FIELD vAdders1             AS CHAR    
        FIELD vAdders2             AS CHAR    
        FIELD vAdders3             AS CHAR    
        FIELD vAdders4             AS CHAR    
        FIELD vAdders5             AS CHAR    
        FIELD vAdders6             AS CHAR    
        FIELD vAdders7             AS CHAR    
        FIELD vAdders8             AS CHAR    
        FIELD vAdders9             AS CHAR    
        FIELD vAdders10            AS CHAR    
        FIELD vAdders11            AS CHAR    
        FIELD vAdders12            AS CHAR    
        FIELD vWaxLabel1           AS CHAR
        FIELD vWaxDesc1            AS CHAR
        FIELD vLeafS1              AS INTEGER 
        FIELD vLeafB1              AS INTEGER
        FIELD vLeafWid1            AS DECIMAL FORMAT ">>9.99"
        FIELD vLeafLen1            AS DECIMAL FORMAT ">>9.99"
        FIELD vWaxLabel2           AS CHAR
        FIELD vWaxDesc2            AS CHAR
        FIELD vLeafS2              AS INTEGER 
        FIELD vLeafB2              AS INTEGER
        FIELD vLeafWid2            AS DECIMAL FORMAT ">>9.99"
        FIELD vLeafLen2            AS DECIMAL FORMAT ">>9.99"
        FIELD vWaxLabel3           AS CHAR
        FIELD vWaxDesc3            AS CHAR
        FIELD vLeafS3              AS INTEGER 
        FIELD vLeafB3              AS INTEGER
        FIELD vLeafWid3            AS DECIMAL FORMAT ">>9.99"
        FIELD vLeafLen3            AS DECIMAL FORMAT ">>9.99"
        FIELD vWaxLabel4           AS CHAR
        FIELD vWaxDesc4            AS CHAR
        FIELD vLeafS4              AS INTEGER 
        FIELD vLeafB4              AS INTEGER
        FIELD vLeafWid4            AS DECIMAL FORMAT ">>9.99"
        FIELD vLeafLen4            AS DECIMAL FORMAT ">>9.99"
        FIELD vType                AS INT 
        FIELD style                AS CHAR
        FIELD order                AS CHAR .
        .
DEFINE DATASET dsCorrLayout FOR ttCorrLayout .

DEFINE INPUT PARAMETER prmUser                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmType                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmComp                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmEstDate                   AS DATE        NO-UNDO.
DEFINE INPUT PARAMETER prmForm                      AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmFormQty                   AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmCustPart                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmMachine                   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmMachDscr                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFrontBack                 AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSideSide                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmRevCorr                   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBoard                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBoardName                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmReal                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFlute                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTest                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCostMsf                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmCostUom                   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmWeightt                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmFreightMsf                AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmFreightUom                AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmNc                        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmGrosShetWid               AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmGrosShetLen               AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmGrosShetDep               AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmOutWid                    AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmOutLen                    AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmOutDep                    AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmOutCut                    AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmOutTotalUp                AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmOutSqFeet                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmDieInches                 AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmNetShetWid                AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmNetShetLen                AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmNetShetDep                AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmDieSizeWid                AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmDieSizeLen                AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmDieSizeDep                AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmOnWid                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmOnLen                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmOnDep                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmOnTotalUp                 AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmOnSqFeet                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmBlankWid                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmBlankLen                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmBlankDep                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmAdder1                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAdder2                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAdder3                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAdder4                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAdder5                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAdder6                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAdder7                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAdder8                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAdder9                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAdder10                   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAdder11                   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAdder12                   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmWaxLabel1                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmWaxDesc1                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmS1                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmB1                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafWid1                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafLen1                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmWaxLabel2                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmWaxDesc2                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmS2                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmB2                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafWid2                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafLen2                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmWaxLabel3                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmWaxDesc3                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmS3                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmB3                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafWid3                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafLen3                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmWaxLabel4                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmWaxDesc4                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmS4                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmB4                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafWid4                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafLen4                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmBlankno                   AS INT NO-UNDO.
DEFINE OUTPUT PARAMETER cError                      AS CHAR        NO-UNDO.



DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrLayout.

IF prmUser           = ?  THEN ASSIGN prmUser           = "".
IF prmAction         = ?  THEN ASSIGN prmAction          = "".
IF prmType           = ?  THEN ASSIGN prmType            = "".
IF prmComp           = ?  THEN ASSIGN prmComp            = "".
IF prmEstimate       = ?  THEN ASSIGN prmEstimate        = "".
IF prmForm           = ?  THEN ASSIGN prmForm            = 0.
IF prmFormQty        = ?  THEN ASSIGN prmFormQty         = 0.
IF prmCustPart       = ?  THEN ASSIGN prmCustPart        = "".
IF prmMachine        = ?  THEN ASSIGN prmMachine         = "".
IF prmMachDscr       = ?  THEN ASSIGN prmMachDscr        = "".
IF prmFrontBack      = ?  THEN ASSIGN prmFrontBack       = 0.
IF prmSideSide       = ?  THEN ASSIGN prmSideSide        = 0.
IF prmRevCorr        = ?  THEN ASSIGN prmRevCorr         = "".
IF prmBoard          = ?  THEN ASSIGN prmBoard           = "".
IF prmBoardName      = ?  THEN ASSIGN prmBoardName       = "".
IF prmReal           = ?  THEN ASSIGN prmReal            = "".
IF prmFlute          = ?  THEN ASSIGN prmFlute           = "".
IF prmTest           = ?  THEN ASSIGN prmTest            = "".
IF prmCostMsf        = ?  THEN ASSIGN prmCostMsf         = 0.
IF prmCostUom        = ?  THEN ASSIGN prmCostUom         = "".
IF prmWeightt        = ?  THEN ASSIGN prmWeightt         = 0.
IF prmFreightMsf     = ?  THEN ASSIGN prmFreightMsf      = 0.
IF prmFreightUom     = ?  THEN ASSIGN prmFreightUom      = "".
IF prmNc             = ?  THEN ASSIGN prmNc              = "".
IF prmGrosShetWid    = ?  THEN ASSIGN prmGrosShetWid     = 0.
IF prmGrosShetLen    = ?  THEN ASSIGN prmGrosShetLen     = 0.
IF prmGrosShetDep    = ?  THEN ASSIGN prmGrosShetDep     = 0.
IF prmOutWid         = ?  THEN ASSIGN prmOutWid          = 0.
IF prmOutLen         = ?  THEN ASSIGN prmOutLen          = 0.
IF prmOutDep         = ?  THEN ASSIGN prmOutDep          = 0.
IF prmOutCut         = ?  THEN ASSIGN prmOutCut          = 0.
IF prmOutTotalUp     = ?  THEN ASSIGN prmOutTotalUp      = "".
IF prmOutSqFeet      = ?  THEN ASSIGN prmOutSqFeet       = "".
IF prmDieInches      = ?  THEN ASSIGN prmDieInches       = 0.
IF prmNetShetWid     = ?  THEN ASSIGN prmNetShetWid      = 0.
IF prmNetShetLen     = ?  THEN ASSIGN prmNetShetLen      = 0.
IF prmNetShetDep     = ?  THEN ASSIGN prmNetShetDep      = 0.
IF prmDieSizeWid     = ?  THEN ASSIGN prmDieSizeWid      = 0.
IF prmDieSizeLen     = ?  THEN ASSIGN prmDieSizeLen      = 0.
IF prmDieSizeDep     = ?  THEN ASSIGN prmDieSizeDep      = 0.
IF prmOnWid          = ?  THEN ASSIGN prmOnWid           = 0.
IF prmOnLen          = ?  THEN ASSIGN prmOnLen           = 0.
IF prmOnDep          = ?  THEN ASSIGN prmOnDep           = 0.
IF prmOnTotalUp      = ?  THEN ASSIGN prmOnTotalUp       = 0.
IF prmOnSqFeet       = ?  THEN ASSIGN prmOnSqFeet        = 0.
IF prmBlankWid       = ?  THEN ASSIGN prmBlankWid        = 0.
IF prmBlankLen       = ?  THEN ASSIGN prmBlankLen        = 0.
IF prmBlankDep       = ?  THEN ASSIGN prmBlankDep        = 0.
IF prmAdder1         = ?  THEN ASSIGN prmAdder1          = "".
IF prmAdder2         = ?  THEN ASSIGN prmAdder2          = "".
IF prmAdder3         = ?  THEN ASSIGN prmAdder3          = "".
IF prmAdder4         = ?  THEN ASSIGN prmAdder4          = "".
IF prmAdder5         = ?  THEN ASSIGN prmAdder5          = "".
IF prmAdder6         = ?  THEN ASSIGN prmAdder6          = "".
IF prmAdder7         = ?  THEN ASSIGN prmAdder7          = "".
IF prmAdder8         = ?  THEN ASSIGN prmAdder8          = "".
IF prmAdder9         = ?  THEN ASSIGN prmAdder9          = "".
IF prmAdder10        = ?  THEN ASSIGN prmAdder10         = "".
IF prmAdder11        = ?  THEN ASSIGN prmAdder11         = "".
IF prmAdder12        = ?  THEN ASSIGN prmAdder12         = "".      
IF prmWaxLabel1      = ?  THEN ASSIGN prmWaxLabel1       = "".     
IF prmWaxDesc1       = ?  THEN ASSIGN prmWaxDesc1        = "".     
IF   prmS1           = ?  THEN ASSIGN prmS1              = 0.       
IF   prmB1           = ?  THEN ASSIGN prmB1              = 0.     
IF   prmLeafWid1     = ?  THEN ASSIGN prmLeafWid1        = 0.     
IF   prmLeafLen1     = ?  THEN ASSIGN prmLeafLen1        = 0.     
IF   prmWaxLabel2    = ?  THEN ASSIGN prmWaxLabel2       = "".     
IF   prmWaxDesc2     = ?  THEN ASSIGN prmWaxDesc2        = "".     
IF   prmS2           = ?  THEN ASSIGN prmS2              = 0.     
IF   prmB2           = ?  THEN ASSIGN prmB2              = 0.     
IF   prmLeafWid2     = ?  THEN ASSIGN prmLeafWid2        = 0.     
IF   prmLeafLen2     = ?  THEN ASSIGN prmLeafLen2        = 0.     
IF   prmWaxLabel3    = ?  THEN ASSIGN prmWaxLabel3       = "".     
IF   prmWaxDesc3     = ?  THEN ASSIGN prmWaxDesc3        = "".     
IF   prmS3           = ?  THEN ASSIGN prmS3              = 0.     
IF   prmB3           = ?  THEN ASSIGN prmB3              = 0.    
IF   prmLeafWid3     = ?  THEN ASSIGN prmLeafWid3        = 0.    
IF   prmLeafLen3     = ?  THEN ASSIGN prmLeafLen3        = 0.    
IF   prmWaxLabel4    = ?  THEN ASSIGN prmWaxLabel4       = "".    
IF   prmWaxDesc4     = ?  THEN ASSIGN prmWaxDesc4        = "".    
IF   prmS4           = ?  THEN ASSIGN prmS4              = 0.    
IF   prmB4           = ?  THEN ASSIGN prmB4              = 0.    
IF   prmLeafWid4     = ?  THEN ASSIGN prmLeafWid4        = 0.    
IF   prmLeafLen4     = ?  THEN ASSIGN prmLeafLen4        = 0.    

{sys/inc/var.i new shared}


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
 ASSIGN
     cocode = prmComp .
 {cec/msfcalc.i}

def var ll-auto-calc-selected as log no-undo.
def var k_frac as dec init 6.25 no-undo.
DEF VAR lv-tot-len AS INT NO-UNDO.
DEF VAR lv-tot-wid AS INT NO-UNDO.
DEF VAR lv-tot-up AS INT NO-UNDO.
def var li-n-cuts as int no-undo.
def var lv-is-roll as log no-undo.
def var ls-lam-dscr as cha no-undo.
def var lv-is-foam as log no-undo.
def var lv-industry as cha no-undo.
def var ld-roll-wid as dec no-undo.
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
def var uom-list as cha no-undo.
def var ll-is-sheet-calc as log no-undo.
def var ll-is-canceled as log no-undo.
def var ll-num-lw-changed as log no-undo.  /* if num-len, eb.num-wid changed */
def var ll-num-out-changed as log no-undo.  /* if n-out, eb.n-out-l changed */
DEF VAR ll-one-eb-on-ef AS LOG NO-UNDO.  
DEF VAR ll-one-ef-on-est AS LOG NO-UNDO.
DEF VAR ll-part-style AS LOG NO-UNDO.
DEF VAR lv-label AS CHAR EXTENT 10 NO-UNDO.


DEF BUFFER bf-est FOR est.

  DEF BUFFER bf-eb FOR eb.
      DEF BUFFER b-ef FOR ef.

  DEF VAR li AS INT NO-UNDO.

{sys/inc/f16to32.i}

/**********************************OverRide**************************************/
IF prmAction = "ValidateOverRide" THEN DO:
    
    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno EXCLUSIVE-LOCK NO-ERROR.
    RUN one-eb-on-ef (ROWID(ef), OUTPUT ll-one-eb-on-ef).
    RUN one-ef-on-est (ROWID(est), OUTPUT ll-one-ef-on-est).
    RUN part-style (ROWID(ef), OUTPUT ll-part-style).


    if not avail ef then return.
    assign lv-is-foam = NO
           lv-industry = "".
    find first style where style.company = prmComp and
                style.style = eb.style no-lock no-error.
    IF AVAIL style THEN
        DO:
        IF style.type = "F" then lv-is-foam = yes.
        lv-industry = style.industry.
        END.

        if prmMachine = "" then do:
            assign
                prmMachDscr = "" .
            end.

            if prmMachine <> "" and
                not can-find (first mach where mach.company = prmComp and
                              mach.loc = eb.loc and
                              mach.m-code = prmMachine)
                then do:
                cError = "Invalid Machine Code. Try Help." .
                return .
                end.
                FIND FIRST mach
                    WHERE mach.company EQ prmComp AND mach.m-code  EQ prmMachine NO-LOCK NO-ERROR.
                IF AVAIL mach AND TRIM(prmMachine) NE "" THEN DO:
                    prmMachDscr = mach.m-dscr.
                 END.
               
                if prmBoard <> "" then do:
                    find first item where item.company = prmComp and
                        ((index("BPR",item.mat-type) > 0 and not lv-is-foam) or
                         (index("1234",item.mat-type) > 0 and lv-is-foam) ) and
                        item.industry = lv-industry  and
                        item.i-no =  prmBoard no-lock no-error.
                    if not avail item then do:
                        cError = "Invalid Board. Try Help." .
                        return .
                        end.
                        if item.i-code = "R"  then do:
                            if item.r-wid = 0 then do:
                                if item.s-wid < prmBlankWid then do:
                                    cError = "Sheet Width less than Blank Width. " .
                                    return .
                                    end.
                                    if item.s-len < prmBlankLen then do:
                                        cError = "Sheet Length less than Blank Length. " .
                                        return .
                                        end.
                        end.  /* r-wid = 0 */
                        else if item.r-wid < prmBlankWid then do:
                            cError = "Roll Width less than Blank Width. " .
                            return .
                            end.
              end.  
      
       end.  /* lastkey <> -1 */

if  prmCostUom <> "" then do:
             find first item where item.company = prmComp and
                 item.i-no = prmBoard no-lock no-error.
             if avail item then find first e-item of item no-lock no-error.
             run sys/ref/uom-rm.p  (item.mat-type, output uom-list).
             if not can-find(first uom where lookup(uom.uom,uom-list) > 0 and
                             uom.uom = prmCostUom)
                 then do:
                 cError = "Invalid Cost UOM. Try Help."   .
                 return .
                 end.                 
         end.  

    
         if prmFreightUom <> "" then do:
             uom-list = "CWT,MSF,MSH,TON".
             if not can-find(first uom where lookup(uom.uom,uom-list) > 0 and
                             uom.uom = prmFreightUom)
                 then do:
                 cError = "Invalid Freight UOM. Try Help." .
                 return .
                 end.                 
         end.  


   
IF prmAdder1 NE "" THEN DO:
   FIND FIRST item  WHERE item.company  EQ prmComp
                    AND item.mat-type EQ "A"
                    AND item.i-no     EQ prmAdder1 NO-LOCK NO-ERROR.
   IF NOT AVAIL ITEM THEN DO:
        cError = "Invalid Adder1, try help".
        RETURN.
    END.
END.

 
IF prmAdder2 NE "" THEN DO:
    FIND FIRST ITEM WHERE item.company  EQ prmComp
            AND item.mat-type EQ "A"
            AND item.i-no     EQ prmAdder2 NO-LOCK NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
        ASSIGN
            cError = "Invalid Adder2, try help".
            RETURN.
    END.
END.

IF prmAdder3 NE "" THEN DO:
    FIND FIRST ITEM WHERE item.company  EQ prmComp
            AND item.mat-type EQ "A"
            AND item.i-no     EQ prmAdder3 NO-LOCK NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
        ASSIGN
            cError = "Invalid Adder3, try help".
            RETURN.
    END.
END.

IF prmAdder4 NE "" THEN DO:
    FIND FIRST ITEM WHERE item.company  EQ prmComp
            AND item.mat-type EQ "A"
            AND item.i-no     EQ prmAdder4 NO-LOCK NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
        ASSIGN
            cError = "Invalid Adder4, try help".
            RETURN.
    END.
END.

IF prmAdder5 NE "" THEN DO:
    FIND FIRST ITEM WHERE item.company  EQ prmComp
            AND item.mat-type EQ "A"
            AND item.i-no     EQ prmAdder5 NO-LOCK NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
        ASSIGN
            cError = "Invalid Adder5, try help".
            RETURN.
    END.
END.

IF prmAdder6 NE "" THEN DO:
    FIND FIRST ITEM WHERE item.company  EQ prmComp
            AND item.mat-type EQ "A"
            AND item.i-no     EQ prmAdder6 NO-LOCK NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
        ASSIGN
            cError = "Invalid Adder6, try help".
            RETURN.
    END.
END.

        RUN new-board.
        RUN n-out-changed.

    IF prmOnLen < 0 THEN DO:
        ASSIGN
            cError = "Length Cannot be negative".
            RETURN.
    END.

    /* RUN num-wid-len-dep-changed.*/
           
           IF prmOnWid < 0 THEN DO:
               ASSIGN 
                   cError = "Width cannot be negative" .
               RETURN.
               END.
               
             IF prmWaxLabel1 NE ""  AND
                 NOT CAN-FIND(FIRST item
                              WHERE item.company EQ prmComp
                              AND item.i-no    EQ prmWaxLabel1
                              AND INDEX("WLF",item.mat-type) GT 0) THEN DO:
                 cError = "Invalid Leaf/Film, try help..." .
                 RETURN .
                 END.
                 IF prmWaxLabel2 NE ""  AND
                     NOT CAN-FIND(FIRST item
                                  WHERE item.company EQ prmComp
                                  AND item.i-no    EQ prmWaxLabel2
                                  AND INDEX("WLF",item.mat-type) GT 0) THEN DO:
                     cError = "Invalid Leaf/Film, try help..." .
                     RETURN .
                     END.
                     IF prmWaxLabel3 NE ""  AND
                         NOT CAN-FIND(FIRST item
                                      WHERE item.company EQ prmComp
                                      AND item.i-no    EQ prmWaxLabel3
                                      AND INDEX("WLF",item.mat-type) GT 0) THEN DO:
                         cError = "Invalid Leaf/Film, try help..." .
                         RETURN .
                         END.
                         IF prmWaxLabel4 NE ""  AND
                             NOT CAN-FIND(FIRST item
                                          WHERE item.company EQ prmComp
                                          AND item.i-no    EQ prmWaxLabel4
                                          AND INDEX("WLF",item.mat-type) GT 0) THEN DO:
                             cError = "Invalid Leaf/Film, try help..." .
                             RETURN .
                             END.


                             DO li = 1 TO EXTENT(ef.leaf):
                                 RUN valid-leaf-snum (li) NO-ERROR.
                                 RUN valid-leaf-bnum (li) NO-ERROR.
                             END.
                             
                             IF prmWaxLabel1 <> "" and
                                 INT(prmLeafWid1) = 0 THEN DO:
                                 cError = "Width must be entered." .
                                 RETURN .
                                 END.
                                 IF prmWaxLabel2 <> "" and
                                     INT(prmLeafWid2) = 0 THEN DO:
                                     cError = "Width must be entered." .
                                     RETURN .
                                     END.
                                     IF prmWaxLabel3 <> "" and
                                         INT(prmLeafWid3) = 0 THEN DO:
                                         cError = "Width must be entered." .
                                         RETURN .
                                         END.
                                         IF prmWaxLabel4 <> "" and
                                             INT(prmLeafWid4) = 0 THEN DO:
                                             cError = "Width must be entered." .
                                             RETURN .
                                             END.

                                             IF prmWaxLabel1 <> "" and
                                                 INT(prmLeafLen1) = 0 THEN DO:
                                                 cError = "Length must be entered." .
                                                 RETURN .
                                                 END.
                                                 IF prmWaxLabel2 <> "" and
                                                     INT(prmLeafLen2) = 0 THEN DO:
                                                     cError = "Length must be entered." .
                                                     RETURN .
                                                     END.
                                                     IF prmWaxLabel3 <> "" and
                                                         INT(prmLeafLen3) = 0 THEN DO:
                                                         cError = "Length must be entered." .
                                                         RETURN .
                                                         END.
                                                         IF prmWaxLabel4 <> "" and
                                                             INT(prmLeafLen4) = 0 THEN DO:
                                                             cError = "Length must be entered." .
                                                             RETURN .
                                                             END.

                                                             if prmCostUom <> "" then do:
                                                                 find first item where item.company = prmComp and
                                                                     item.i-no = prmBoard   /* ef.board */ no-lock no-error.
                                                                 if avail item then find first e-item of item no-lock no-error.
                                                                 run sys/ref/uom-rm.p  (item.mat-type, output uom-list).
                                                                 if not can-find(first uom where lookup(uom.uom,uom-list) > 0 and
                                                                                 uom.uom = prmCostUom) /* ef.cost-uom)*/
                                                                     then do:
                                                                     cError =  "Invalid Cost UOM. Try Help." .
                                                                     return .
                                                                     end.                 
                                                             end.  
                                                             if prmFreightUom <> "" then do:
                                                                 uom-list = "CWT,MSF,MSH,TON".
                                                                 if not can-find(first uom where lookup(uom.uom,uom-list) > 0 and
                                                                                 uom.uom = prmFreightUom) /* ef.fr-uom */
                                                                     then do:
                                                                     cError = "Invalid Freight UOM. Try Help." .
                                                                     return .
                                                                     end.                 
                                                             end. 

                                                             
           if not ll-auto-calc-selected then do:
                /* RUN valid-gsh-wid NO-ERROR.*/
                IF DEC(prmGrosShetWid) LT (IF prmRevCorr EQ "S" THEN
                    DEC(prmNetShetLen)
                    ELSE
                        DEC(prmNetShetWid)) THEN DO:
                            ASSIGN
                                cError = "Gross Sheet Size can not be less than Net Sheet Size...".
                            RETURN.
                            END.
                            
                            FIND FIRST item
                                WHERE item.company EQ prmComp
                                AND item.i-no    EQ prmBoard  /* ef.board */
                                NO-LOCK NO-ERROR.
                            
                            IF AVAIL item THEN DO:
                              
                              /*  IF item.i-code EQ "R"    AND
                                    ((ef.roll EQ TRUE AND DEC(prmGrosShetWid) NE item.r-wid)  OR
                                     (ef.roll NE TRUE AND DEC(prmGrosShetWid) NE item.s-wid)) THEN DO:
                                    cError = "Gross Sheet Size may not be changed for a Real Material..." .
                                    RETURN .
                            END.*/
                            
                            IF ef.roll EQ TRUE  THEN DO:
                                IF item.i-code EQ "E" THEN DO:
                                    FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.
                                    IF AVAIL e-item THEN
                                        DO li = 1 TO 26:
                                        IF e-item.roll-w[li] GE DEC(prmGrosShetWid) THEN DO:
                                            prmGrosShetWid = e-item.roll-w[li].
                                            LEAVE.
                                            END.
                                  END.
                           END.
                  END.
                  /* RUN roll-display.*/
        END. 

        /*RUN valid-gsh-len NO-ERROR.*/
        IF DEC(prmGrosShetLen) LT (IF prmRevCorr EQ "S" THEN
            DEC(prmNetShetWid)
            ELSE
                DEC(prmNetShetLen)) THEN DO:
                    ASSIGN                                    
                        cError = "Gross Sheet Size can not be less than Net Sheet Size...".
                    RETURN  . 
                    END.
                    
                    RUN valid-gsh-dep NO-ERROR.
                    
                    if dec(prmNetShetWid) < dec(prmDieSizeWid) 
                        then do:
                        cError = "Net Sheet Size can not be less than Die Size." .
                        return .     
                        end.
                        if dec(prmNetShetLen) < dec(prmDieSizeLen) 
                            then do:
                            cError = "Net Sheet Size can not be less than Die Size." .
                            return .     
                            end.

                     IF DEC(prmDieSizeWid) < (IF prmRevCorr EQ "B" THEN DEC(prmBlankLen) ELSE DEC(prmBlankWid))
                         then do:
                         cError = "Die Size can not be less than Blank Size." .
                         RETURN .
                      END.

                     IF DEC(prmDieSizeLen) < (IF prmRevCorr EQ "B" THEN DEC(prmBlankWid) ELSE DEC(prmBlankLen))
                         THEN DO:
                         cError = "Die Size can not be less than Blank Size." .
                         RETURN .
                         END.

                      IF DEC(prmDieSizeDep) < DEC(prmBlankDep)
                          THEN DO:
                          cError = "Die Size can not be less than Blank Size." .
                          RETURN .
                        END.
                           
              end.  /* not auto-calc */  
        /*    END.
   END. /* not  ll-is-sheet-calc */ */

              if ll-is-sheet-calc then do:
                  ll-is-sheet-calc = no.
                 /* run sheet-calc2.*/
                  end.
                  
                  RUN release-shared-buffers. 
                   
 END.

/*********************************End OverRide**********************************/

/**********************************OverRide**************************************/
IF prmAction = "OverRide" THEN DO:
    
    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN
        ef.m-code         =   prmMachine                                                                           
        ef.m-dscr         =   prmMachDscr                                                                          
        ef.lsh-wid        =   trunc(prmFrontBack,0) + ((prmFrontBack - trunc(prmFrontBack,0)) * K_FRAC)
        ef.lsh-len        =   trunc(prmSideSide,0) + ((prmSideSide - trunc(prmSideSide,0)) * K_FRAC)
        ef.xgrain         =   prmRevCorr                                                                           
        ef.board          =   prmBoard                                                                             
        ef.brd-dscr       =   prmBoardName                                                                         
        ef.i-code         =   prmReal                                                                              
        ef.flute          =   prmFlute                                                                             
        ef.test           =   prmTest                                                                              
        ef.cost-uom       =   prmCostUom                                                                           
        ef.cost-msh       =   prmCostMsf                                                                           
        ef.weight         =   prmWeightt                                                                           
        ef.fr-msh         =   prmFreightMsf                                                                        
        ef.fr-uom         =   prmFreightUom                                                                        
        ef.nc             =   IF prmNc = "C" THEN TRUE ELSE FALSE                                                         /* = TRUE THEN "C" ELSE "N" */
        ef.gsh-wid        =   trunc(prmGrosShetWid,0) + ((prmGrosShetWid - trunc(prmGrosShetWid,0)) * K_FRAC)
        ef.gsh-len        =   trunc(prmGrosShetLen,0) + ((prmGrosShetLen - trunc(prmGrosShetLen,0)) * K_FRAC)
        ef.gsh-dep        =   trunc(prmGrosShetDep,0) + ((prmGrosShetDep - trunc(prmGrosShetDep,0)) * K_FRAC)
        ef.n-out          =   prmOutWid                                                      
        ef.n-out-l        =   prmOutLen                                                                            
        ef.n-out-d        =   prmOutDep                                                                            
        ef.n-cuts         =   prmOutCut                                                                        
        ef.die-in         =   prmDieInches                                 
        ef.nsh-wid        =   trunc(prmNetShetWid,0) + ((prmNetShetWid - trunc(prmNetShetWid,0)) * K_FRAC)
        ef.nsh-len        =   trunc(prmNetShetLen,0) + ((prmNetShetLen - trunc(prmNetShetLen,0)) * K_FRAC)
        ef.nsh-dep        =   trunc(prmNetShetDep,0) + ((prmNetShetDep - trunc(prmNetShetDep,0)) * K_FRAC)
        ef.trim-l         =   trunc(prmDieSizeLen,0) + ((prmDieSizeLen - trunc(prmDieSizeLen,0)) * K_FRAC)
        ef.trim-w         =   trunc(prmDieSizeWid,0) + ((prmDieSizeWid - trunc(prmDieSizeWid,0)) * K_FRAC)
        ef.trim-d         =   trunc(prmDieSizeDep,0) + ((prmDieSizeDep - trunc(prmDieSizeDep,0)) * K_FRAC) 
        eb.num-wid        =   prmOnWid                                                                         
        eb.num-len        =   prmOnLen                                                                         
        eb.num-dep        =   prmOnDep                                                                             
        eb.num-up         =   prmOnTotalUp                                                                            
        eb.t-wid          =   trunc(prmBlankWid,0) + ((prmBlankWid - trunc(prmBlankWid,0)) * K_FRAC)
        eb.t-len          =   trunc(prmBlankLen,0) + ((prmBlankLen - trunc(prmBlankLen,0)) * K_FRAC)    
        eb.t-dep          =   trunc(prmBlankDep,0) + ((prmBlankDep - trunc(prmBlankDep,0)) * K_FRAC)   
        ef.adder[1]       =   prmAdder1                                                                           
        ef.adder[2]       =   prmAdder2                                                                           
        ef.adder[3]       =   prmAdder3                                                                            
        ef.adder[4]       =   prmAdder4                                                                            
        ef.adder[5]       =   prmAdder5                                                                            
        ef.adder[6]       =   prmAdder6                                                                                
        ef.adder[7]       =   prmAdder7                                                                            
        ef.adder[8]       =   prmAdder8                                                                            
        ef.adder[9]       =   prmAdder9                                                                            
        ef.adder[10]      =   prmAdder10                                                                           
        ef.adder[11]      =   prmAdder11                                                                           /*prmOutSqFeet   prmOutTotalUp */  
        ef.adder[12]      =   prmAdder12                                                                           
        ef.leaf[1]        =   prmWaxLabel1                                                                          
        ef.leaf-dscr[1]   =   prmWaxDesc1                                                                           
        ef.leaf-snum[1]   =   prmS1                                                                                
        ef.leaf-bnum[1]   =   prmB1                                                                                
        ef.leaf-w[1]      =   trunc(prmLeafWid1,0) + ((prmLeafWid1 - trunc(prmLeafWid1,0)) * K_FRAC) 
        ef.leaf-l[1]      =   trunc(prmLeafLen1,0) + ((prmLeafLen1 - trunc(prmLeafLen1,0)) * K_FRAC)
        ef.leaf[2]        =   prmWaxLabel2                                                                         
        ef.leaf-dscr[2]   =   prmWaxDesc2                                                                          
        ef.leaf-snum[2]   =   prmS2                                                                                
        ef.leaf-bnum[2]   =   prmB2                                                                                
        ef.leaf-w[2]      =   trunc(prmLeafWid2,0) + ((prmLeafWid2 - trunc(prmLeafWid2,0)) * K_FRAC)  
        ef.leaf-l[2]      =   trunc(prmLeafLen2,0) + ((prmLeafLen2 - trunc(prmLeafLen2,0)) * K_FRAC)
        ef.leaf[3]        =   prmWaxLabel3                                                                         
        ef.leaf-dscr[3]   =   prmWaxDesc3                                                                          
        ef.leaf-snum[3]   =   prmS3                                                                                
        ef.leaf-bnum[3]   =   prmB3                                                                                
        ef.leaf-w[3]      =   trunc(prmLeafWid3,0) + ((prmLeafWid3 - trunc(prmLeafWid3,0)) * K_FRAC)
        ef.leaf-l[3]      =   trunc(prmLeafLen3,0) + ((prmLeafLen3 - trunc(prmLeafLen3,0)) * K_FRAC)
        ef.leaf[4]        =   prmWaxLabel4                                                                         
        ef.leaf-dscr[4]   =   prmWaxDesc4                                                                          
        ef.leaf-snum[4]   =   prmS4                                                                                
        ef.leaf-bnum[4]   =   prmB4                                                                                
        ef.leaf-w[4]      =   trunc(prmLeafWid4,0) + ((prmLeafWid4 - trunc(prmLeafWid4,0)) * K_FRAC)
        ef.leaf-l[4]      =   trunc(prmLeafLen4,0) + ((prmLeafLen4 - trunc(prmLeafLen4,0)) * K_FRAC)
            .   

        ASSIGN
            prmAction = "Select" .
  END.


/*********************************End OverRide**********************************/

/********************************Select****************************************/

IF prmAction = "Select" THEN DO:
    
    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH b-ef
        WHERE b-ef.company EQ est.company
          AND b-ef.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate)
        BREAK BY b-ef.eqty:
      IF FIRST-OF(b-ef.eqty) THEN li = 0.
      li = li + 1.
    END.
    IF li NE est.form-qty THEN DO:
      FIND est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp EXCLUSIVE NO-WAIT NO-ERROR.
      IF AVAIL est THEN est.form-qty = li.
      FIND CURRENT est NO-LOCK.
    END.

    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno NO-LOCK NO-ERROR.
    
    
    find first ce-ctrl where ce-ctrl.company = prmComp AND ce-ctrl.loc = eb.loc no-lock no-error.
    find first item where item.company = prmComp AND item.i-no = ef.board no-lock no-error.
    if avail item then DO:
        find first e-item of item no-lock no-error.
        ef.brd-dscr = ITEM.i-name.
        END.
        
        /*  if ef.lam-dscr = "R" OR (ef.lam-dscr <> "R" and ef.xgrain = "S" )
        then assign ef.lsh-len = ef.gsh-len
        ef.lsh-wid = ef.gsh-wid
        .
        else assign ef.lsh-len = ef.gsh-wid
        ef.lsh-wid = ef.gsh-len
        .
        */
        if ef.n-out = 0 then ef.n-out = 1.
        if ef.n-out-l = 0 then ef.n-out-l = 1.
        if ef.n-out-d = 0 then ef.n-out-d = 1.
        
        

        IF AVAIL est AND AVAIL ef AND AVAIL eb THEN DO:
            CREATE ttCorrLayout.
            ASSIGN
                ttCorrLayout.vEstimate      = est.est-no
                ttCorrLayout.vType          = est.est-type
                ttCorrLayout.vEstDate       = est.est-date
                ttCorrLayout.vForm          = ef.form-no
                ttCorrLayout.vFormQty       = est.form-qty
                ttCorrLayout.vCustPart      = eb.part-no
                ttCorrLayout.vMachine       = ef.m-code
                ttCorrLayout.vMachDscr      = ef.m-dscr
                ttCorrLayout.vFrontBack     = round(TRUNC((ef.lsh-wid),0) + (((ef.lsh-wid) - trunc((ef.lsh-wid),0)) / K_FRAC),2)   /*trunc((ef.lsh-wid),0) + (((ef.lsh-wid) - trunc((ef.lsh-wid),0)) * K_FRAC)*/ 
                ttCorrLayout.vSideSide      = round(TRUNC((ef.lsh-len),0) + (((ef.lsh-len) - trunc((ef.lsh-len),0)) / K_FRAC),2)  /*trunc((ef.lsh-len),0) + (((ef.lsh-len) - trunc((ef.lsh-len),0)) * K_FRAC)*/  
                ttCorrLayout.vRevCorr       = ef.xgrain
                ttCorrLayout.vBoard         = ef.board
                ttCorrLayout.vBoardName     = ef.brd-dscr
                ttCorrLayout.vReal          = ef.i-code
                ttCorrLayout.vFlute         = ef.flute
                ttCorrLayout.vTest          = ef.test
                ttCorrLayout.vCostUom       = ef.cost-uom
                ttCorrLayout.vCostMsf       = ef.cost-msh
                ttCorrLayout.vWeight        = ef.weight
                ttCorrLayout.vFreightCwt    = ef.fr-msh
                ttCorrLayout.vFreightUom    = ef.fr-uom
                ttCorrLayout.vNc            = IF ef.nc = TRUE THEN "C" ELSE "N"
                ttCorrLayout.vGrosShetWid   = round(TRUNC((ef.gsh-wid),0) + (((ef.gsh-wid) - trunc((ef.gsh-wid),0)) / K_FRAC),2)
                ttCorrLayout.vGrosShetLen   = round(TRUNC((ef.gsh-len),0) + (((ef.gsh-len) - trunc((ef.gsh-len),0)) / K_FRAC),2)
                ttCorrLayout.vGrosShetDep   = round(TRUNC((ef.gsh-dep),0) + (((ef.gsh-dep) - trunc((ef.gsh-dep),0)) / K_FRAC),2)
                ttCorrLayout.vOutWid        = ef.n-out
                ttCorrLayout.vOutLen        = ef.n-out-l
                ttCorrLayout.vOutDep        = ef.n-out-d
                ttCorrLayout.vOutCut        = ef.n-cuts
                /* ttCorrLayout.vOutTotalUp    = 
               ttCorrLayout.vOutSqFeet     = */
                ttCorrLayout.vDieInches     = INT(DEC( DEC(ef.die-in) / eb.num-up )  * int(eb.num-up))
                ttCorrLayout.vNetShetWid    = round(TRUNC((ef.nsh-wid),0) + (((ef.nsh-wid) - trunc((ef.nsh-wid),0)) / K_FRAC),2)
                ttCorrLayout.vNetShetLen    = round(TRUNC((ef.nsh-len),0) + (((ef.nsh-len) - trunc((ef.nsh-len),0)) / K_FRAC),2)
                ttCorrLayout.vNetShetDep    = round(TRUNC((ef.nsh-dep),0) + (((ef.nsh-dep) - trunc((ef.nsh-dep),0)) / K_FRAC),2)
                ttCorrLayout.vDieSizeLen    = round(TRUNC((ef.trim-l),0) + (((ef.trim-l) - trunc((ef.trim-l),0)) / K_FRAC),2)
                ttCorrLayout.vDieSizeWid    = round(TRUNC((ef.trim-w),0) + (((ef.trim-w) - trunc((ef.trim-w),0)) / K_FRAC),2)
                ttCorrLayout.vDieSizeDep    = round(TRUNC((ef.trim-d),0) + (((ef.trim-d) - trunc((ef.trim-d),0)) / K_FRAC),2)
                ttCorrLayout.vOnWid         = eb.num-wid
                ttCorrLayout.vOnLen         = eb.num-len
                ttCorrLayout.vOnDep         = eb.num-dep
                ttCorrLayout.vOnTotalUp     = eb.num-up
                ttCorrLayout.vOnSqFeet      = if v-corr then (eb.t-sqin * .007) else (eb.t-sqin / 144)
                ttCorrLayout.vBlankWid      = round(TRUNC((eb.t-wid),0) + (((eb.t-wid) - trunc((eb.t-wid),0)) / K_FRAC),2)
                ttCorrLayout.vBlankLen      = round(TRUNC((eb.t-len),0) + (((eb.t-len) - trunc((eb.t-len),0)) / K_FRAC),2)
                ttCorrLayout.vBlankDep      = round(TRUNC((eb.t-dep),0) + (((eb.t-dep) - trunc((eb.t-dep),0)) / K_FRAC),2)
                ttCorrLayout.vAdders1       = ef.adder[1]
                ttCorrLayout.vAdders2       = ef.adder[2]
                ttCorrLayout.vAdders3       = ef.adder[3]
                ttCorrLayout.vAdders4       = ef.adder[4]
                ttCorrLayout.vAdders5       = ef.adder[5]
                ttCorrLayout.vAdders6       = ef.adder[6]
                ttCorrLayout.vAdders7       = ef.adder[7]
                ttCorrLayout.vAdders8       = ef.adder[8]
                ttCorrLayout.vAdders9       = ef.adder[9]
                ttCorrLayout.vAdders10      = ef.adder[10]
                ttCorrLayout.vAdders11      = ef.adder[11]
                ttCorrLayout.vAdders12      = ef.adder[12]
                ttCorrLayout.vWaxLabel1     = ef.leaf[1]
                ttCorrLayout.vWaxDesc1      = ef.leaf-dscr[1]
                ttCorrLayout.vLeafS1        = ef.leaf-snum[1]
                ttCorrLayout.vLeafB1        = ef.leaf-bnum[1]
                ttCorrLayout.vLeafWid1      = round(TRUNC((ef.leaf-w[1]),0) + (((ef.leaf-w[1]) - trunc((ef.leaf-w[1]),0)) / K_FRAC),2)
                ttCorrLayout.vLeafLen1      = round(TRUNC((ef.leaf-l[1]),0) + (((ef.leaf-l[1]) - trunc((ef.leaf-l[1]),0)) / K_FRAC),2)
                ttCorrLayout.vWaxLabel2     = ef.leaf[2]
                ttCorrLayout.vWaxDesc2      = ef.leaf-dscr[2]
                ttCorrLayout.vLeafS2        = ef.leaf-snum[2]
                ttCorrLayout.vLeafB2        = ef.leaf-bnum[2]
                ttCorrLayout.vLeafWid2      = round(TRUNC((ef.leaf-w[2]),0) + (((ef.leaf-w[2]) - trunc((ef.leaf-w[2]),0)) / K_FRAC),2)
                ttCorrLayout.vLeafLen2      = round(TRUNC((ef.leaf-l[2]),0) + (((ef.leaf-l[2]) - trunc((ef.leaf-l[2]),0)) / K_FRAC),2)
                ttCorrLayout.vWaxLabel3     = ef.leaf[3]
                ttCorrLayout.vWaxDesc3      = ef.leaf-dscr[3] 
                ttCorrLayout.vLeafS3        = ef.leaf-snum[3]
                ttCorrLayout.vLeafB3        = ef.leaf-bnum[3]  
                ttCorrLayout.vLeafWid3      = round(TRUNC((ef.leaf-w[3]),0) + (((ef.leaf-w[3]) - trunc((ef.leaf-w[3]),0)) / K_FRAC),2)
                ttCorrLayout.vLeafLen3      = round(TRUNC((ef.leaf-l[3]),0) + (((ef.leaf-l[3]) - trunc((ef.leaf-l[3]),0)) / K_FRAC),2)
                ttCorrLayout.vWaxLabel4     = ef.leaf[4]
                ttCorrLayout.vWaxDesc4      = ef.leaf-dscr[4]
                ttCorrLayout.vLeafS4        = ef.leaf-snum[4]
                ttCorrLayout.vLeafB4        = ef.leaf-bnum[4]
                ttCorrLayout.vLeafWid4      = round(TRUNC((ef.leaf-w[4]),0) + (((ef.leaf-w[4]) - trunc((ef.leaf-w[4]),0)) / K_FRAC),2)
                ttCorrLayout.vLeafLen4      = round(TRUNC((ef.leaf-l[4]),0) + (((ef.leaf-l[4]) - trunc((ef.leaf-l[4]),0)) / K_FRAC),2) 
                ttCorrLayout.style          = eb.style  
                ttCorrLayout.order          = STRING(eb.ord-no)
                    .
          END.  
          
   END.

/*********************************End Select***********************************/



    PROCEDURE valid-leaf-snum :
        
        DEF INPUT PARAM ip-int AS INT NO-UNDO.
        DEF BUFFER bf-ef FOR ef.
            
            /* FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR. */
            FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
            FIND FIRST eb WHERE eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND eb.company = prmComp NO-LOCK NO-ERROR.
            
            DEF VAR lv-leaf AS CHAR NO-UNDO.
            DEF VAR lv-snum AS INTEGER NO-UNDO.
            
            CASE ip-int:
                WHEN 1 THEN
                    ASSIGN
                    lv-leaf = prmWaxLabel1
                    lv-snum = prmS1.
                    WHEN 2 THEN
                        ASSIGN
                        lv-leaf = prmWaxLabel2
                        lv-snum = prmS2.
                        WHEN 3 THEN
                            ASSIGN
                            lv-leaf = prmWaxLabel3
                            lv-snum = prmS3.
                            WHEN 4 THEN
                                ASSIGN
                                lv-leaf = prmWaxLabel4
                                lv-snum = prmS4.
                                END CASE.
                                
                                IF lv-leaf NE ""   THEN DO:
                                    CASE ip-int:
                                        WHEN 1 THEN prmS1 = ef.form-no.
                                            WHEN 2 THEN prmS2 = ef.form-no.
                                                WHEN 3 THEN prmS3 = ef.form-no.
                                                    WHEN 4 THEN prmS4 = ef.form-no.
                                     END CASE.
              END.
     END PROCEDURE.


     PROCEDURE valid-leaf-bnum :

         DEF INPUT PARAM ip-int AS INT NO-UNDO.
         DEF BUFFER bf-eb FOR eb.
             
             DEF VAR lv-leaf AS CHAR NO-UNDO.
             DEF VAR lv-snum AS INTEGER NO-UNDO.
             DEF VAR lv-bnum AS INTEGER NO-UNDO.
             
             CASE ip-int:
                 WHEN 1 THEN
                     ASSIGN
                     lv-leaf = prmWaxLabel1
                     lv-snum = prmS1
                     lv-bnum = prmB1.
                     WHEN 2 THEN
                         ASSIGN
                         lv-leaf = prmWaxLabel2
                         lv-snum = prmS2
                         lv-bnum = prmB2.
                         WHEN 3 THEN
                             ASSIGN
                             lv-leaf = prmWaxLabel3
                             lv-snum = prmS3
                             lv-bnum = prmB3.
                             WHEN 4 THEN
                                 ASSIGN
                                 lv-leaf = prmWaxLabel4
                                 lv-snum = prmS4
                                 lv-bnum = prmB4.
               END CASE.
               
               IF lv-leaf NE ""  AND
                   NOT CAN-FIND(FIRST bf-eb
                                WHERE bf-eb.company   EQ eb.company
                                AND bf-eb.est-no    EQ eb.est-no
                                AND bf-eb.form-no   EQ INT(lv-snum)
                                AND (bf-eb.blank-no EQ INT(lv-bnum) OR
                                     INT(lv-bnum) EQ 0))               THEN DO:
                   cError = "Blank does not exist on this form..." .
                   CASE ip-int:
                       WHEN 1 THEN prmB1 = ef.leaf-bnum[1].
                           WHEN 2 THEN prmB2 = ef.leaf-bnum[2].
                               WHEN 3 THEN prmB3 = ef.leaf-bnum[3].
                                   WHEN 4 THEN prmB4 = ef.leaf-bnum[4].
                    END CASE.
                  RETURN .
               END.
     END PROCEDURE.

     PROCEDURE auto-calc :
         
         /*{custom/checkuse.i}*/
         FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
         FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no NO-LOCK NO-ERROR.
         
         ll-auto-calc-selected = yes.
         
         find first item where item.company = prmComp and
             item.i-no = prmBoard  /* ef.board*/
             no-lock no-error.
         
         if not lv-is-foam or item.i-code = "E" then do:
             find first mach where mach.company = prmComp and
                 mach.loc = eb.loc and
                 mach.m-code = ef.m-code
                 use-index m-code no-lock no-error.
             if avail mach and mach.dept[1] eq "RC" then
                 assign 
                 ef.nsh-wid = ef.nsh-wid - (2 * mach.min-trimw)
                 ef.nsh-len = ef.nsh-len - (2 * mach.min-triml).   
             assign 
                 ef.n-out   = trunc(ef.lsh-len / ef.nsh-wid,0)
                 ef.n-out-l = trunc(ef.lsh-wid / ef.nsh-len,0)
                 ef.n-out-d = 1.
            end.

            assign  /* ef.roll    = STRING(ITEM.r-wid GT 0,"Y/N")*/
                ef.n-out   = 0
                ef.n-out-l = 0
                ef.n-out-d = 0
                ef.gsh-len = 0
                ef.gsh-wid = 0
                ef.gsh-dep = 0
                ef.nsh-len = 0
                ef.nsh-wid = 0
                ef.nsh-dep = 0
                ef.trim-w  = 0
                ef.trim-l  = 0
                ef.trim-d  = 0
                eb.num-len = 0
                eb.num-wid = 0
                eb.num-dep = 0.
    END PROCEDURE.

    /*PROCEDURE valid-gsh-wid :
    
    /* DEF VAR lv-msg AS CHAR NO-UNDO.    */
    
    /* IF DEC(prmGrosShetWid) - TRUNC(DEC(prmGrosShetWid),0) THEN  /* GE v-16-or-32 THEN*/
    lv-msg = "Can not have more than  as decimal, field is inches.16ths32nd's. ". /* " +  string(v-16-or-32 - 0.01) +  " */
    */
    IF DEC(prmGrosShetWid) LT (IF prmRevCorr EQ "S" THEN
    DEC(prmNetShetLen)
    ELSE
    DEC(prmNetShetWid)) THEN DO:
    ASSIGN
    cError = "Gross Sheet Size can not be less than Net Sheet Size...".
    RETURN.
    END.
    
    DEF VAR li AS INT NO-UNDO.
    FIND FIRST item
    WHERE item.company EQ prmComp
    AND item.i-no    EQ prmBoard  /* ef.board */
    NO-LOCK NO-ERROR.
    
    IF AVAIL item THEN DO:
    IF item.i-code EQ "R"                                                             AND
    /*((ef.roll EQ "Y" AND DEC(prmGrosShetWid) NE item.r-wid)  OR
    (ef.roll NE "Y" AND DEC(prmGrosShetWid) NE item.s-wid)) THEN DO:*/
    ((ef.roll EQ TRUE AND DEC(prmGrosShetWid) NE item.r-wid)  OR
    (ef.roll NE TRUE AND DEC(prmGrosShetWid) NE item.s-wid)) THEN DO:
    cError = "Gross Sheet Size may not be changed for a Real Material..." .
    RETURN .
    END.
    
    IF ef.roll EQ TRUE AND ll-auto-calc-selected THEN DO:
    IF item.i-code EQ "E" THEN DO:
    FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.
    IF AVAIL e-item THEN
    DO li = 1 TO 26:
    IF e-item.roll-w[li] GE DEC(prmGrosShetWid) THEN DO:
    prmGrosShetWid = e-item.roll-w[li].
    LEAVE.
    END.
    END.
    
    END.
    END.
    
    /* RUN roll-display.*/
    END.

END PROCEDURE.*/


/*PROCEDURE valid-gsh-len :

/* DEF VAR lv-msg AS CHAR NO-UNDO.*/

/* IF DEC(prmGrosShetLen) - TRUNC(DEC(prmGrosShetLen),0) GE v-16-or-32 THEN
lv-msg = "Can not have more than " + string(v-16-or-32 - 0.01) + " as decimal, field is (inches.16ths/32nd's). ".
*/

IF DEC(prmGrosShetLen) LT (IF prmRevCorr EQ "S" THEN
DEC(prmNetShetWid)
ELSE
DEC(prmNetShetLen)) THEN DO:
ASSIGN                                    
cError = "Gross Sheet Size can not be less than Net Sheet Size...".
RETURN  . 
END.
/* IF lv-msg NE "" THEN DO:
cError = lv-msg .
RETURN .     
END.*/


END PROCEDURE.*/
    

    PROCEDURE valid-gsh-dep :

        /*DEF VAR lv-msg AS CHAR NO-UNDO.*/
        
        /*  IF DEC(prmGrosShetDep) - TRUNC(DEC(prmGrosShetDep),0) GE v-16-or-32 THEN
        lv-msg = "Can not have more than " + string(v-16-or-32 - 0.01) + " as decimal, field is (inches.16ths/32nd's). ".
        */
        
        IF DEC(prmGrosShetDep) LT DEC(prmNetShetDep) THEN DO:
            
            ASSIGN
                cError = "Gross Sheet Size can not be less than Net Sheet Size...".
            RETURN.
            END.
    END PROCEDURE.

    PROCEDURE valid-trim-d :
        
        /*    IF DEC(prmDieSizeDep) - TRUNC(DEC(prmDieSizeDep),0) GT v-16-or-32
        THEN DO:
        message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) ".
        RETURN .
        END.
        */
        IF DEC(prmDieSizeDep) < DEC(prmBlankDep)

            THEN DO:
            cError = "Die Size can not be less than Blank Size." .
            RETURN .
            END.
            
        END PROCEDURE.

        PROCEDURE valid-trim-l :
            
            /* IF DEC(prmDieSizeLen) - TRUNC(DEC(prmDieSizeLen),0) GT v-16-or-32
            THEN DO:
            cError = "Can not have more than  v-16-or-32 - 0.01  as decimal, field is (inches.16ths/32nd's) ".
            RETURN .
            END.
            */
            

            IF DEC(prmDieSizeLen) < (IF prmRevCorr EQ "B" THEN DEC(prmBlankWid) ELSE DEC(prmBlankLen))
                THEN DO:
                cError = "Die Size can not be less than Blank Size." .
                RETURN .
                END.
                
       END PROCEDURE.

       PROCEDURE valid-trim-w :
           
           /*    IF DEC(prmDieSizeWid) - TRUNC(DEC(prmDieSizeWid),0) GT v-16-or-32
           THEN DO:
           cError = "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) ".
           RETURN .
           END.
           */
           IF DEC(prmDieSizeWid) < (IF prmRevCorr EQ "B" THEN DEC(prmBlankLen) ELSE DEC(prmBlankWid))
               then do:
               cError = "Die Size can not be less than Blank Size." .
               RETURN .
               END.
      END PROCEDURE.


      PROCEDURE sheet-calc2 :

          find xest where xest.company = ef.company and
              xest.est-no = ef.est-no
              no-lock no-error.
          find xef where recid(xef) = recid(ef) NO-LOCK NO-ERROR.
          find xeb where recid(xeb) = recid(eb) NO-LOCK NO-ERROR.
          
          run cec/bestfitc.p (prmMachine, 0, "").
          DEFINE BUFFER tt-ef FOR ef .
              DEFINE BUFFER tt-eb FOR eb .
                  FIND FIRST tt-ef NO-ERROR.
                  FIND FIRST tt-eb NO-ERROR.
                  
                  IF AVAIL tt-ef AND AVAIL tt-eb THEN DO:
                      assign
                          prmBoard = tt-ef.board
                          prmFlute = tt-ef.flute
                          prmTest = tt-ef.test
                          prmBoardName = tt-ef.brd-dscr
                          prmMachine = tt-ef.m-code
                          prmWeightt = tt-ef.weight
                          prmReal = tt-ef.i-code
                          prmSideSide = round(TRUNC((tt-ef.lsh-len),0) + (((tt-ef.lsh-len) - trunc((tt-ef.lsh-len),0)) / K_FRAC),2)  /* string({sys/inc/k16.i tt-ef.lsh-len} ) */
                          prmFrontBack = round(TRUNC((tt-ef.lsh-wid),0) + (((tt-ef.lsh-wid) - trunc((tt-ef.lsh-wid),0)) / K_FRAC),2) /* string({sys/inc/k16.i tt-ef.lsh-wid} ) */
                          /* ef.roll:screen-value = string(tt-ef.roll,"Y/N")*/
                          prmGrosShetLen = round(TRUNC((tt-ef.gsh-len),0) + (((tt-ef.gsh-len) - trunc((tt-ef.gsh-len),0)) / K_FRAC),2) /* string({sys/inc/k16.i tt-ef.gsh-len} ) */
                          prmGrosShetWid = round(TRUNC((tt-ef.gsh-wid),0) + (((tt-ef.gsh-wid) - trunc((tt-ef.gsh-wid),0)) / K_FRAC),2) /* string({sys/inc/k16.i tt-ef.gsh-wid} )*/
                          prmNetShetLen = round(TRUNC((tt-ef.nsh-len),0) + (((tt-ef.nsh-len) - trunc((tt-ef.nsh-len),0)) / K_FRAC),2) /* string({sys/inc/k16.i tt-ef.nsh-len} )*/
                          prmNetShetWid = round(TRUNC((tt-ef.nsh-wid),0) + (((tt-ef.nsh-wid) - trunc((tt-ef.nsh-wid),0)) / K_FRAC),2) /* string({sys/inc/k16.i tt-ef.nsh-wid} )*/
                          prmDieSizeLen = round(TRUNC((tt-ef.trim-l),0) + (((tt-ef.trim-l) - trunc((tt-ef.trim-l),0)) / K_FRAC),2) /* string({sys/inc/k16.i tt-ef.trim-l} ) */
                          prmDieSizeWid = round(TRUNC((tt-ef.trim-w),0) + (((tt-ef.trim-w) - trunc((tt-ef.trim-w),0)) / K_FRAC),2) /* string({sys/inc/k16.i tt-ef.trim-w} ) */
                          prmOutWid = tt-ef.n-out
                          prmOutLen = tt-ef.n-out-l
                          prmOutCut = tt-ef.n-cuts
                          prmOnWid = tt-eb.num-wid
                          prmOnLen = tt-eb.num-len
                          prmOnTotalUp = tt-eb.num-up
                          .
                      
                      /* RUN roll-display.*/
                      
                      if lv-is-foam then       
                          assign
                          prmGrosShetDep = round(TRUNC((tt-ef.gsh-dep),0) + (((tt-ef.gsh-dep) - trunc((tt-ef.gsh-dep),0)) / K_FRAC),2) /* string({sys/inc/k16.i tt-ef.gsh-dep} ) */
                          prmNetShetDep = round(TRUNC((tt-ef.nsh-dep),0) + (((tt-ef.nsh-dep) - trunc((tt-ef.nsh-dep),0)) / K_FRAC),2) /* string({sys/inc/k16.i tt-ef.nsh-dep} )*/
                          prmDieSizeDep = round(TRUNC((tt-ef.trim-d),0) + (((tt-ef.trim-d) - trunc((tt-ef.trim-d),0)) / K_FRAC),2) /* string({sys/inc/k16.i tt-ef.trim-d} )*/
                          prmOutDep = tt-ef.n-out-d
                          .
                      END.
           END PROCEDURE.

           PROCEDURE release-shared-buffers :
               
               RELEASE xest.
               RELEASE xef.
               RELEASE xeb.
            END PROCEDURE.

            PROCEDURE new-board :
                
                DEF VAR lv AS DECIMAL NO-UNDO.
                FIND FIRST item
                    WHERE item.company  EQ prmComp
                    AND ((INDEX("BPR",item.mat-type) GT 0 AND NOT lv-is-foam) OR
                         (INDEX("1234",item.mat-type) GT 0 AND lv-is-foam))
                    AND item.industry EQ lv-industry
                    AND item.i-no     EQ prmBoard
                    NO-LOCK NO-ERROR.
                IF AVAIL item AND TRIM(prmBoard) NE "" THEN DO:
                    ASSIGN
                        prmTest     = item.reg-no
                        prmBoardName = item.i-name
                        prmReal   = item.i-code
                        prmFlute    = item.flute
                        prmWeightt   = item.basis-w .
                    
                    RUN sys/ref/uom-rm.p (item.mat-type, OUTPUT uom-list).
                   /* IF uom-list NE "" AND LOOKUP(prmCostUom,uom-list) LE 0 THEN
                        prmCostUom = ENTRY(1,uom-list).
                    IF item.i-code EQ "R" THEN
                        IF item.r-wid GT 0 THEN
                            ASSIGN
                            prmGrosShetWid = round(TRUNC((item.r-wid),0) + (((item.r-wid) - trunc((item.r-wid),0)) / K_FRAC),2)   /* STRING({sys/inc/k16.i item.r-wid})*/
                            prmSideSide = round(TRUNC((item.r-wid),0) + (((item.r-wid) - trunc((item.r-wid),0)) / K_FRAC),2)      /*  STRING({sys/inc/k16.i item.r-wid})*/
                            /* ef.roll:SCREEN-VALUE    = "Y".*/  .
                        ELSE DO:
                            ASSIGN
                                /*prmGrosShetWid = round(TRUNC((item.s-wid),0) + (((item.s-wid) - trunc((item.s-wid),0)) / K_FRAC),2)   /* STRING({sys/inc/k16.i item.s-wid})*/
                                prmGrosShetLen = round(TRUNC((item.s-len),0) + (((item.s-len) - trunc((item.s-len),0)) / K_FRAC),2)   /* STRING({sys/inc/k16.i item.s-len})*/*/
                                prmSideSide = round(TRUNC((item.s-wid),0) + (((item.s-wid) - trunc((item.s-wid),0)) / K_FRAC),2)      /* STRING({sys/inc/k16.i item.s-wid})*/
                                prmFrontBack = round(TRUNC((item.s-len),0) + (((item.s-len) - trunc((item.s-len),0)) / K_FRAC),2)    /* STRING({sys/inc/k16.i item.s-len})*/
                                /* ef.roll:SCREEN-VALUE    = "N".*/  .
                            IF prmRevCorr EQ "S" THEN
                                ASSIGN
                                lv                      = prmGrosShetLen
                                prmGrosShetLen = prmGrosShetWid
                                prmGrosShetWid = lv.
                            END.
                            
                            FIND FIRST e-item OF item NO-LOCK NO-ERROR.
                            IF AVAIL e-item THEN prmCostUom = e-item.std-uom.*/
                            END.
                            
            END PROCEDURE.

            PROCEDURE n-out-changed :
                
                def var ld-gsh-wid as dec no-undo.
                def var ld-gsh-len as dec no-undo.
                def var ld-gsh-dep as dec no-undo.
                def var ld-nsh-wid as dec no-undo.
                def var ld-nsh-len as dec no-undo.
                def var ld-nsh-dep as dec no-undo.
                
                prmOutCut = (DEC(prmOutWid)   - 1) +
                    (DEC(prmOutLen) - 1) +
                    ((IF DEC(prmOutDep) NE 0 THEN DEC(prmOutDep) ELSE 1) - 1).
                                                                      
                                                                      IF INT(prmOutCut) LT 0 THEN prmOutCut = 0.
                                                                      if ll-auto-calc-selected then do:
                                                                          assign
                                                                              ld-gsh-wid = DEC(prmGrosShetWid)
                                                                              ld-gsh-len = DEC(prmGrosShetLen)
                                                                              ld-gsh-dep = DEC(prmGrosShetDep)
                                                                              ld-nsh-wid = DEC(prmNetShetWid)
                                                                              ld-nsh-len = DEC(prmNetShetLen)
                                                                              ld-nsh-dep = DEC(prmNetShetDep)
                                                                              
                                                                              ll-num-out-changed = yes.
                                                                              ld-gsh-wid = TRUNC((ld-gsh-wid),0) + (((ld-gsh-wid) - trunc((ld-gsh-wid),0)) * K_FRAC).
                                                                              ld-gsh-len = TRUNC((ld-gsh-len),0) + (((ld-gsh-len) - trunc((ld-gsh-len),0)) * K_FRAC).
                                                                              ld-gsh-dep = TRUNC((ld-gsh-dep),0) + (((ld-gsh-dep) - trunc((ld-gsh-dep),0)) * K_FRAC).
                                                                              ld-nsh-wid = TRUNC((ld-nsh-wid),0) + (((ld-nsh-wid) - trunc((ld-nsh-wid),0)) * K_FRAC).
                                                                              ld-nsh-len = TRUNC((ld-nsh-len),0) + (((ld-nsh-len) - trunc((ld-nsh-len),0)) * K_FRAC).
                                                                              ld-nsh-dep = TRUNC((ld-nsh-dep),0) + (((ld-nsh-dep) - trunc((ld-nsh-dep),0)) * K_FRAC).
                                                                              
            if prmMachine <> "" then 
                find first mach where mach.company = prmComp and
                mach.loc = ef.loc and
                mach.m-code = prmMachine 
                no-lock no-error.
            
            find first item where item.company = prmComp and
                item.i-no = prmBoard
                no-lock no-error.
            
            assign ld-gsh-wid = if not avail item or item.i-code eq "E" THEN ((INT(prmOutWid) *
                                                                               DEC(IF prmRevCorr EQ "S" THEN ld-nsh-len ELSE ld-nsh-wid)) +
                                                                              if avail mach and mach.dept[1] eq "RC" then
                                                                                  (2 * mach.min-trimw) else 0)
                else ld-gsh-wid
                    ld-gsh-len = if not avail item or item.i-code eq "E" THEN ((INT(prmOutLen) *
                                                                                DEC(IF prmRevCorr EQ "S" THEN ld-nsh-wid ELSE ld-nsh-len)) +
                                                                               if avail mach and mach.dept[1] eq "RC" then
                                                                                   (2 * mach.min-triml) else 0)
                        else ld-gsh-len
                            ld-gsh-dep = if not avail item or item.i-code eq "E" then
                                ( prmOutDep * prmNetShetDep)
                                else ld-gsh-dep.
                                     assign
                                         /*prmGrosShetLen = round(trunc((ld-gsh-len),0) + (((ld-gsh-len) - trunc((ld-gsh-len),0)) / K_FRAC),2)   /* string({sys/inc/k16.i ld-gsh-len})*/
                                         prmGrosShetWid = round(trunc((ld-gsh-len),0) + (((ld-gsh-len) - trunc((ld-gsh-len),0)) / K_FRAC),2)  /* string({sys/inc/k16.i ld-gsh-wid})*/
                                         prmGrosShetDep = round(trunc((ld-gsh-len),0) + (((ld-gsh-len) - trunc((ld-gsh-len),0)) / K_FRAC),2)  /* string({sys/inc/k16.i ld-gsh-dep}) */*/
                                         .
                                     END.
   END PROCEDURE.

   PROCEDURE num-wid-len-dep-changed :

       ASSIGN
           prmDieInches = DEC(prmDieInches) / INT(prmOnTotalUp)
           prmOnTotalUp = (IF INT(prmOnWid) EQ 0 THEN 1 ELSE INT(prmOnWid)) *
           (IF INT(prmOnLen) EQ 0 THEN 1 ELSE INT(prmOnLen)) *
           (IF INT(prmOnDep) EQ 0 THEN 1 ELSE INT(prmOnDep))
           prmDieInches = DEC(prmDieInches) *
           INT(prmOnTotalUp).
   END PROCEDURE.



PROCEDURE one-eb-on-ef :
  DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-one-eb AS LOG NO-UNDO.

  DEF BUFFER b-ac-eb FOR eb.
  DEF BUFFER b-ac-ef FOR ef.


  FIND b-ac-ef WHERE ROWID(b-ac-ef) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-ac-ef THEN
  FIND b-ac-eb
      WHERE b-ac-eb.company EQ b-ac-ef.company
        AND b-ac-eb.est-no  EQ b-ac-ef.est-no
        AND b-ac-eb.eqty    EQ b-ac-ef.eqty
        AND b-ac-eb.form-no EQ b-ac-ef.form-no
      NO-LOCK NO-ERROR.

  op-one-eb = AVAIL b-ac-eb.

END PROCEDURE.

PROCEDURE one-ef-on-est :

  DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-one-ef AS LOG NO-UNDO.

  DEF BUFFER b-ac-est FOR est.
  DEF BUFFER b-ac-ef FOR ef.


  FIND b-ac-est WHERE ROWID(b-ac-est) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-ac-est THEN
  FIND b-ac-ef WHERE b-ac-ef.company EQ b-ac-est.company
                 AND b-ac-ef.est-no  EQ b-ac-est.est-no        NO-LOCK NO-ERROR.

  op-one-ef = AVAIL b-ac-ef.

END PROCEDURE.

PROCEDURE part-style :

  DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-part-style AS LOG NO-UNDO.

  DEF BUFFER b-ac-eb FOR eb.
  DEF BUFFER b-ac-ef FOR ef.


  FIND b-ac-ef WHERE ROWID(b-ac-ef) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-ac-ef THEN
  FOR EACH b-ac-eb NO-LOCK
      WHERE b-ac-eb.company EQ b-ac-ef.company
        AND b-ac-eb.est-no  EQ b-ac-ef.est-no
        AND b-ac-eb.eqty    EQ b-ac-ef.eqty
        AND b-ac-eb.form-no EQ b-ac-ef.form-no,
      FIRST style NO-LOCK
      WHERE style.company EQ b-ac-eb.company
        AND style.style   EQ b-ac-eb.style:
    IF NOT CAN-DO("P,R",style.type)  OR
       b-ac-eb.t-wid NE eb.t-wid     OR
       b-ac-eb.num-len NE eb.num-len THEN LEAVE.
  END.

  op-part-style = NOT AVAIL b-ac-eb.

END PROCEDURE.
