

/*------------------------------------------------------------------------
    File        : FoldLayout.p
    Purpose     : FoldLayout
    Syntax      :

    Description : Return a Dataset of Folding Layout 
    Author(s)   : 
    Created     : 21 Feb 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttFoldLayout NO-UNDO
        FIELD vEstimate            AS CHAR FORMAT "x(8)" 
        FIELD vEstDate             AS DATE  
        FIELD vForm                AS INTEGER
        FIELD vFormQty             AS INTEGER
        FIELD vCustPart            AS CHAR    
        FIELD vMachine             AS CHAR FORMAT "x(6)" 
        FIELD vMachDscr            AS CHAR 
        FIELD vFrontBack           AS DECIMAL FORMAT ">>9.99"    
        FIELD vSideSide            AS DECIMAL FORMAT ">>9.99" 
        FIELD vXgrain              AS CHAR FORMAT "X"
        FIELD vBoard               AS CHAR   
        FIELD vBoardName           AS CHAR  FORMAT "X(50)"  
        FIELD vReal                AS CHAR 
        FIELD vCaliper             AS DECIMAL 
        FIELD vCostMsf             AS DECIMAL    
        FIELD vCostUom             AS CHAR    
        FIELD vWeight              AS DECIMAL    
        FIELD vFreightCwt          AS DECIMAL 
        FIELD vFreightUom          AS CHAR
        FIELD vNc                  AS CHAR 
        FIELD vRoll                AS CHAR 
        FIELD vRollWid             AS DECIMAL 
        FIELD vGrosShetWid         AS DECIMAL FORMAT ">>>9.99"
        FIELD vGrosShetLen         AS DECIMAL FORMAT ">>>9.99" 
        FIELD vOutWid              AS DECIMAL    
        FIELD vOutLen              AS DECIMAL    
        FIELD vOutCut              AS INTEGER
        
        FIELD vDieInches           AS DECIMAL    
        FIELD vMachFeedWid         AS DECIMAL   
        FIELD vMachFeedLen         AS DECIMAL
        FIELD vDieSizeLen          AS DECIMAL
        FIELD vDieSizeWid          AS DECIMAL
        FIELD vOnWid               AS DECIMAL
        FIELD vOnLen               AS DECIMAL
        FIELD vOnTotalUp           AS INTEGER
        FIELD vBlankWid            AS DECIMAL
        FIELD vBlankLen            AS DECIMAL 
        FIELD vBlankSqInch         AS DECIMAL 
        FIELD vLeaf1               AS CHAR    
        FIELD vLeaf2               AS CHAR    
        FIELD vLeaf3               AS CHAR    
        FIELD vLeaf4               AS CHAR    
        FIELD vLeafDesc1           AS CHAR    
        FIELD vLeafDesc2           AS CHAR    
        FIELD vLeafDesc3           AS CHAR    
        FIELD vLeafDesc4           AS CHAR    
        FIELD vLeafS1              AS INTEGER 
        FIELD vLeafB1              AS INTEGER
        FIELD vLeafWid1            AS DECIMAL FORMAT ">>9.99"
        FIELD vLeafLen1            AS DECIMAL FORMAT ">>9.99"
        FIELD vLeafS2              AS INTEGER 
        FIELD vLeafB2              AS INTEGER
        FIELD vLeafWid2            AS DECIMAL FORMAT ">>9.99"
        FIELD vLeafLen2            AS DECIMAL FORMAT ">>9.99"
        FIELD vLeafS3              AS INTEGER 
        FIELD vLeafB3              AS INTEGER
        FIELD vLeafWid3            AS DECIMAL FORMAT ">>9.99"
        FIELD vLeafLen3            AS DECIMAL FORMAT ">>9.99"
        FIELD vLeafS4              AS INTEGER 
        FIELD vLeafB4              AS INTEGER
        FIELD vLeafWid4            AS DECIMAL FORMAT ">>9.99"
        FIELD vLeafLen4            AS DECIMAL FORMAT ">>9.99"
        FIELD vType                AS INT
        FIELD vStyle               AS CHAR
        FIELD order                AS CHAR  .
       
DEFINE DATASET dsFoldLayout FOR ttFoldLayout .

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
DEFINE INPUT PARAMETER prmXgrain                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBoard                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBoardName                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmReal                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCaliper                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmCostMsf                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmCostUom                   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmWeightt                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmFreightMsf                AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmFreightUom                AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmNc                        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRoll                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRollWid                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmGrosShetWid               AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmGrosShetLen               AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmOutWid                    AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmOutLen                    AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmOutCut                    AS INT         NO-UNDO.

DEFINE INPUT PARAMETER prmDieInches                 AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmMachFeedWid               AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmMachFeedLen               AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmDieSizeWid                AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmDieSizeLen                AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmOnWid                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmOnLen                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmOnTotalUp                 AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmBlankWid                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmBlankLen                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmBlankSqInch               AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLeaf1                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLeaf2                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLeaf3                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLeaf4                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLeafDesc1                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLeafDesc2                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLeafDesc3                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLeafDesc4                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmS1                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmB1                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafWid1                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafLen1                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmS2                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmB2                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafWid2                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafLen2                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmS3                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmB3                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafWid3                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafLen3                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmS4                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmB4                        AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafWid4                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLeafLen4                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmBlankno                   AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmAuto                      AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER cError                      AS CHAR        NO-UNDO.



DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFoldLayout.

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
IF prmXgrain         = ?  THEN ASSIGN prmXgrain          = "".
IF prmBoard          = ?  THEN ASSIGN prmBoard           = "".
IF prmBoardName      = ?  THEN ASSIGN prmBoardName       = "".
IF prmReal           = ?  THEN ASSIGN prmReal            = "".
IF prmCaliper        = ?  THEN ASSIGN prmCaliper         = 0.
IF prmCostMsf        = ?  THEN ASSIGN prmCostMsf         = 0.
IF prmCostUom        = ?  THEN ASSIGN prmCostUom         = "".
IF prmWeightt        = ?  THEN ASSIGN prmWeightt         = 0.
IF prmFreightMsf     = ?  THEN ASSIGN prmFreightMsf      = 0.
IF prmFreightUom     = ?  THEN ASSIGN prmFreightUom      = "".
IF prmNc             = ?  THEN ASSIGN prmNc              = "".
IF prmRoll           = ?  THEN ASSIGN prmRoll            = "".
IF prmRollWid        = ?  THEN ASSIGN prmRollWid         = 0.
IF prmGrosShetWid    = ?  THEN ASSIGN prmGrosShetWid     = 0.
IF prmGrosShetLen    = ?  THEN ASSIGN prmGrosShetLen     = 0.
IF prmOutWid         = ?  THEN ASSIGN prmOutWid          = 0.
IF prmOutLen         = ?  THEN ASSIGN prmOutLen          = 0.
IF prmOutCut         = ?  THEN ASSIGN prmOutCut          = 0.

IF prmDieInches      = ?  THEN ASSIGN prmDieInches       = 0.
IF prmMachFeedWid    = ?  THEN ASSIGN prmMachFeedWid     = 0.
IF prmMachFeedLen    = ?  THEN ASSIGN prmMachFeedLen     = 0.
IF prmDieSizeWid     = ?  THEN ASSIGN prmDieSizeWid      = 0.
IF prmDieSizeLen     = ?  THEN ASSIGN prmDieSizeLen      = 0.
IF prmOnWid          = ?  THEN ASSIGN prmOnWid           = 0.
IF prmOnLen          = ?  THEN ASSIGN prmOnLen           = 0.
IF prmOnTotalUp      = ?  THEN ASSIGN prmOnTotalUp       = 0.
IF prmBlankWid       = ?  THEN ASSIGN prmBlankWid        = 0.
IF prmBlankLen       = ?  THEN ASSIGN prmBlankLen        = 0.
IF prmBlankSqInch    = ?  THEN ASSIGN prmBlankSqInch     = 0.
IF prmLeaf1          = ?  THEN ASSIGN prmLeaf1           = "".
IF prmLeaf2          = ?  THEN ASSIGN prmLeaf2           = "".
IF prmLeaf3          = ?  THEN ASSIGN prmLeaf3           = "".
IF prmLeaf4          = ?  THEN ASSIGN prmLeaf4           = "".
IF prmLeafDesc1      = ?  THEN ASSIGN prmLeafDesc1       = "".
IF prmLeafDesc2      = ?  THEN ASSIGN prmLeafDesc2       = "".
IF prmLeafDesc3      = ?  THEN ASSIGN prmLeafDesc3       = "".
IF prmLeafDesc4      = ?  THEN ASSIGN prmLeafDesc4       = "".

IF prmS1             = ?  THEN ASSIGN prmS1              = 0.
IF prmB1             = ?  THEN ASSIGN prmB1              = 0.
IF prmLeafWid1       = ?  THEN ASSIGN prmLeafWid1        = 0.
IF prmLeafLen1       = ?  THEN ASSIGN prmLeafLen1        = 0.
IF prmS2             = ?  THEN ASSIGN prmS2              = 0.
IF prmB2             = ?  THEN ASSIGN prmB2              = 0.
IF prmLeafWid2       = ?  THEN ASSIGN prmLeafWid2        = 0.      
IF prmLeafLen2       = ?  THEN ASSIGN prmLeafLen2        = 0.     
IF prmS3             = ?  THEN ASSIGN prmS3              = 0.     
IF prmB3             = ?  THEN ASSIGN prmB3              = 0.       
IF prmLeafWid3       = ?  THEN ASSIGN prmLeafWid3        = 0.     
IF prmLeafLen3       = ?  THEN ASSIGN prmLeafLen3        = 0.     
IF prmS4             = ?  THEN ASSIGN prmS4              = 0.     
IF prmB4             = ?  THEN ASSIGN prmB4              = 0.     
IF prmLeafWid4       = ?  THEN ASSIGN prmLeafWid4        = 0.     
IF prmLeafLen4       = ?  THEN ASSIGN prmLeafLen4        = 0.
       
{sys/inc/var.i new shared}

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp
    locode = "MAIN" .

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
DEF VAR ll-auto-calc AS LOG NO-UNDO.

DEF BUFFER bf-est FOR est.

  DEF BUFFER bf-eb FOR eb.
      DEF BUFFER b-ef FOR ef.

  DEF VAR li AS INT NO-UNDO.
  ASSIGN
      ll-auto-calc-selected =  IF prmAuto = "Yes" THEN  TRUE ELSE FALSE.

      MESSAGE "auto"  ll-auto-calc-selected .

/**********************************OverRide**************************************/
IF prmAction = "ValidateOverRide" THEN DO:
    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno EXCLUSIVE-LOCK NO-ERROR.
    
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

        if prmBoard <> "" then do:
                    find first item where item.company = prmComp and
                        ((index("BPR",item.mat-type) > 0 and not lv-is-foam) or
                         (index("1234",item.mat-type) > 0 and lv-is-foam) ) and
                        item.industry = lv-industry  and
                        item.i-no = prmBoard no-lock no-error.
                    if not avail item then do:
                        cError = "Invalid Board. Try Help." .
                        return .
                     end.

       if item.i-code = "R" and ll-auto-calc-selected then do:
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
                     
                     if item.i-code = "R" and ll-auto-calc-selected then do:
                         if item.r-wid = 0 then do: 
                             IF (item.s-wid LT prmBlankWid AND prmXgrain NE "B") OR
                                 (item.s-wid LT prmBlankLen AND prmXgrain EQ "B") THEN DO:
                                 cError = "Sheet Width less than Blank..." .
                                 return .
                             end.
                             IF (item.s-len LT prmBlankLen AND prmXgrain NE "B") OR
                                 (item.s-len LT prmBlankWid AND prmXgrain EQ "B") THEN DO:
                                 cError = "Sheet Length less than Blank..." .
                                 return .
                             end.
                             IF (prmXgrain NE "S" AND
                                 (ITEM.s-wid GT DEC(prmFrontBack) OR
                                  ITEM.s-len GT DEC(prmSideSide)))   OR
                                 (prmXgrain EQ "S" AND
                                  (ITEM.s-wid GT DEC(prmSideSide) OR
                                   ITEM.s-len GT DEC(prmFrontBack)))   THEN DO:
                                       cError = "Sheet does not fit on the machine..." .
                                       RETURN .
                               END.
                          end.  /* r-wid = 0 */

                          else
                              IF (item.r-wid LT prmBlankWid AND prmXgrain NE "B") OR
                                  (item.r-wid LT prmBlankLen AND prmXgrain EQ "B") THEN DO:
                                  cError = "Roll Width less than Blank Width. " .
                                  return .
                              end.
                    end.   
                     RUN new-board.
                     IF prmOutWid > 0 THEN DO:
                         if avail item and item.i-code = "R" then do:
                             if dec(prmOutWid) > ef.n-out then do:
                                 cError = "Cannot be greater than what was calculated." .
                                 return .
                             end.
                         end.
                    END.
     END.

        if prmMachine = "" then do:
            assign
                prmMachDscr = ""  .
        end.

        if prmMachine <> "" and
            not can-find (first mach where mach.company = prmComp and
                          mach.loc = eb.loc and
                          mach.m-code = prmMachine)
            then do:
            cError = "Invalid Machine Code. Try Help." .
            return .
            end.
               
        RUN new-m-code.
                
                        
         /*RUN n-out-changed.*/
         
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


         IF prmOnLen < 0 THEN DO:
             ASSIGN
                 cError = "Length Cannot be negative".
             RETURN.
         END.
           IF prmOnWid < 0 THEN DO:
               ASSIGN 
                   cError = "Width cannot be negative" .
               RETURN.
           END.
           
             
         
         DO li = 1 TO EXTENT(ef.leaf):
             RUN valid-leaf-snum (li) NO-ERROR.
             RUN valid-leaf-bnum (li) NO-ERROR.
         END.
         
         IF prmLeaf1 <> "" and
             INT(prmLeafWid1) = 0 THEN DO:
             cError = "Width must be entered." .
             RETURN .
             END.
             IF prmLeaf2 <> "" and
                 INT(prmLeafWid2) = 0 THEN DO:
                 cError = "Width must be entered." .
                 RETURN .
                 END.
                 IF prmLeaf3 <> "" and
                     INT(prmLeafWid3) = 0 THEN DO:
                     cError = "Width must be entered." .
                     RETURN .
                     END.
                     IF prmLeaf4 <> "" and
                         INT(prmLeafWid4) = 0 THEN DO:
                         cError = "Width must be entered." .
                         RETURN .
                         END.

                         IF prmLeaf1 <> "" and
                             INT(prmLeafLen1) = 0 THEN DO:
                             cError = "Length must be entered." .
                             RETURN .
                             END.
                             IF prmLeaf2 <> "" and
                                 INT(prmLeafLen2) = 0 THEN DO:
                                 cError = "Length must be entered." .
                                 RETURN .
                                 END.
                                 IF prmLeaf3 <> "" and
                                     INT(prmLeafLen3) = 0 THEN DO:
                                     cError = "Length must be entered." .
                                     RETURN .
                                     END.
                                     IF prmLeaf4 <> "" and
                                         INT(prmLeafLen4) = 0 THEN DO:
                                         cError = "Length must be entered." .
                                         RETURN .
                                         END.


                                         
            if not ll-auto-calc-selected then do:
                 RUN valid-gsh-wid NO-ERROR.
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

        /*RUN valid-gsh-len NO-ERROR.*/
        

IF ll-auto-calc-selected          AND
       ((DEC(prmGrosShetLen) LT DEC(prmMachFeedLen) AND
         prmXgrain NE "S")                 OR
        (DEC(prmGrosShetLen) LT DEC(prmMachFeedWid) AND
         prmXgrain EQ "S"))                THEN DO:
      cError = "Gross Sheet Size can not be less than Net Sheet Size..." .
      RETURN .     
    END.
                   /* RUN valid-gsh-dep NO-ERROR.*/
                    
                    IF ll-auto-calc-selected  AND dec(prmMachFeedWid) < dec(prmDieSizeWid) 
                        then do:
                        cError = "Net Sheet Size can not be less than Die Size." .
                        return .     
                        end.
                        IF ll-auto-calc-selected AND dec(prmMachFeedLen) < dec(prmDieSizeLen) 
                            then do:
                            cError = "Net Sheet Size can not be less than Die Size." .
                            return .     
                            end.
                            
                           /* RUN valid-trim-d NO-ERROR.*/
              end.  /* not auto-calc */

              
        /*    END.
   END. /* not  ll-is-sheet-calc */ */

              if ll-is-sheet-calc then do:
                  ll-is-sheet-calc = no.
                  run sheet-calc2.
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
                                                                                 
        ef.m-code       =   prmMachine                                                                           
        ef.m-dscr       =   prmMachDscr  
        ef.lsh-wid      =   prmFrontBack   
        ef.lsh-len      =   prmSideSide                                                                           
        ef.xgrain       =   prmXgrain                                                                            
        ef.board        =   prmBoard                                                                         
        ef.brd-dscr     =   prmBoardName                                                                             
        ef.i-code       =   prmReal                                                                             
        ef.cal          =   prmCaliper                                                                              
        ef.cost-uom     =   prmCostUom                                                                           
        ef.cost-msh     =   prmCostMsf                                                                           
        ef.weight       =   prmWeightt                                                                           
        ef.fr-msh       =   prmFreightMsf                                                                        
        ef.fr-uom       =   prmFreightUom                                                                        
        ef.nc           =   IF prmNc = "C" THEN TRUE ELSE FALSE                                                         /* = TRUE THEN "C" ELSE "N" */
        ef.roll         =   IF prmRoll = "Y" THEN TRUE ELSE FALSE
        ef.roll-wid     =   prmRollWid
        ef.gsh-wid      =   prmGrosShetWid
        ef.gsh-len      =   prmGrosShetLen                   
        ef.n-out        =   prmOutWid                                                                            
        ef.n-out-l      =   prmOutLen                                                                         
        ef.n-cuts       =   prmOutCut    
        ef.die-in       =   prmDieInches 
        ef.nsh-wid      =   prmMachFeedWid
        ef.nsh-len      =   prmMachFeedLen
        ef.trim-l       =   prmDieSizeLen                                                                          
        ef.trim-w       =   prmDieSizeWid                                                                          
        eb.num-wid      =   prmOnWid                                                                              
        eb.num-len      =   prmOnLen       
        eb.num-up       =   prmOnTotalUp        
        eb.t-wid        =   prmBlankWid                                                                             
        eb.t-len        =   prmBlankLen            
        ef.leaf[1]      =   prmLeaf1                                                                            
        ef.leaf[2]      =   prmLeaf2                                                                            
        ef.leaf[3]      =   prmLeaf3                                                                                
        ef.leaf[4]      =   prmLeaf4                                                                            
        ef.leaf-dscr[1] =   prmLeafDesc1                                                                            
        ef.leaf-dscr[2] =   prmLeafDesc2                                                                            
        ef.leaf-dscr[3] =   prmLeafDesc3                                                                           
        ef.leaf-dscr[4] =   prmLeafDesc4                                                                           /*prmOutSqFeet   prmOutTotalUp */  
        ef.leaf-snum[1] =   prmS1                                                                           
        ef.leaf-bnum[1] =   prmB1                                                                         
        ef.leaf-w[1]    =   prmLeafWid1                                                                           
        ef.leaf-l[1]    =   prmLeafLen1                                                                                
        ef.leaf-snum[2] =   prmS2                                                                                
        ef.leaf-bnum[2] =   prmB2   
        ef.leaf-w[2]    =   prmLeafWid2   
        ef.leaf-l[2]    =   prmLeafLen2                                                                         
        ef.leaf-snum[3] =   prmS3                                                                          
        ef.leaf-bnum[3] =   prmB3                                                                                
        ef.leaf-w[3]    =   prmLeafWid3                                                                                
        ef.leaf-l[3]    =   prmLeafLen3   
        ef.leaf-snum[4] =   prmS4   
        ef.leaf-bnum[4] =   prmB4                                                                         
        ef.leaf-w[4]    =   prmLeafWid4                                                                           
        ef.leaf-l[4]    =   prmLeafLen4                                                                                
              
            .   

        ASSIGN
            prmAction = "Select" .
  END.


/*********************************End OverRide**********************************/

/********************************Select****************************************/

IF prmAction = "Select" THEN DO:
    
    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH b-ef
        WHERE b-ef.company EQ est.company
          AND b-ef.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate)
        BREAK BY b-ef.eqty:
      IF FIRST-OF(b-ef.eqty) THEN li = 0.
      li = li + 1.
    END.
    IF li NE est.form-qty THEN DO:
      FIND CURRENT est EXCLUSIVE NO-WAIT NO-ERROR.
      IF AVAIL est THEN est.form-qty = li.
      FIND CURRENT est NO-LOCK.
    END.
    
    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno  NO-LOCK NO-ERROR.
    MESSAGE "select" prmAction  prmEstimate prmComp prmForm.


    find first ce-ctrl where ce-ctrl.company = prmComp AND ce-ctrl.loc = eb.loc no-lock no-error.
    find first item where item.company = prmComp AND item.i-no = ef.board no-lock no-error.
    if avail item then DO:
        find first e-item of item no-lock no-error.
        ef.brd-dscr = ITEM.i-name.
        END.
        
        /*
        if ef.n-out = 0 then ef.n-out = 1.
        if ef.n-out-l = 0 then ef.n-out-l = 1.
        if ef.n-out-d = 0 then ef.n-out-d = 1.*/
        
        IF ef.roll AND ef.roll-wid GT ef.gsh-wid THEN DO:
    ef.gsh-wid = ef.roll-wid.

    IF ef.i-code EQ "E" AND ll-auto-calc THEN
      IF ef.xgrain EQ "S" THEN
        ef.nsh-len = ef.gsh-wid / ef.n-out.
      ELSE
        ef.nsh-wid = ef.gsh-wid / ef.n-out.
  END.

        IF AVAIL est AND AVAIL ef AND AVAIL eb THEN DO:
            CREATE ttFoldLayout.
            ASSIGN
                ttFoldLayout.vEstimate      = est.est-no
                ttFoldLayout.vType          = est.est-type
                ttFoldLayout.vEstDate       = est.est-date
                ttFoldLayout.vForm          = ef.form-no
                ttFoldLayout.vFormQty       = est.form-qty
                ttFoldLayout.vCustPart      = eb.part-no
                ttFoldLayout.vMachine       = ef.m-code
                ttFoldLayout.vMachDscr      = ef.m-dscr
                ttFoldLayout.vFrontBack     = ef.lsh-wid
                ttFoldLayout.vSideSide      = ef.lsh-len
                ttFoldLayout.vXgrain        = ef.xgrain
                ttFoldLayout.vBoard         = ef.board
                ttFoldLayout.vBoardName     = ef.brd-dscr
                ttFoldLayout.vReal          = ef.i-code
                ttFoldLayout.vCaliper       = ef.cal
                ttFoldLayout.vCostUom       = ef.cost-uom
                ttFoldLayout.vCostMsf       = ef.cost-msh
                ttFoldLayout.vWeight        = ef.weight
                ttFoldLayout.vFreightCwt    = ef.fr-msh
                ttFoldLayout.vFreightUom    = ef.fr-uom
                ttFoldLayout.vNc            = IF ef.nc = TRUE THEN "C" ELSE "N"
                ttFoldLayout.vRoll          = IF ef.roll = TRUE THEN "Y" ELSE "N"
                ttFoldLayout.vRollWid       = ef.roll-wid
                ttFoldLayout.vGrosShetWid   = ef.gsh-wid
                ttFoldLayout.vGrosShetLen   = ef.gsh-len
                
                ttFoldLayout.vOutWid        = ef.n-out
                ttFoldLayout.vOutLen        = ef.n-out-l
                ttFoldLayout.vOutCut        = ef.n-cuts
              
                /* ttFoldLayout.vDieInches     = INT( INT(ef.die-in) / eb.num-up ) * int(eb.num-up) */
                ttFoldLayout.vDieInches     = INT(ef.die-in) 

                ttFoldLayout.vMachFeedWid   = ef.nsh-wid
                ttFoldLayout.vMachFeedLen   = ef.nsh-len
                
                ttFoldLayout.vDieSizeLen    = ef.trim-l
                ttFoldLayout.vDieSizeWid    = ef.trim-w
                
                ttFoldLayout.vOnWid         = eb.num-wid
                ttFoldLayout.vOnLen         = eb.num-len
                
                ttFoldLayout.vOnTotalUp     = eb.num-up
                
                ttFoldLayout.vBlankWid      = eb.t-wid
                ttFoldLayout.vBlankLen      = eb.t-len
                ttFoldLayout.vBlankSqInch   = eb.t-sqin
                ttFoldLayout.vLeaf1         = ef.leaf[1]
                ttFoldLayout.vLeaf2         = ef.leaf[2]
                ttFoldLayout.vLeaf3         = ef.leaf[3]
                ttFoldLayout.vLeaf4         = ef.leaf[4]
                ttFoldLayout.vLeafDesc1     = ef.leaf-dscr[1]
                ttFoldLayout.vLeafDesc2     = ef.leaf-dscr[2]
                ttFoldLayout.vLeafDesc3     = ef.leaf-dscr[3]
                ttFoldLayout.vLeafDesc4     = ef.leaf-dscr[4]
                ttFoldLayout.vLeafS1        = ef.leaf-snum[1]
                ttFoldLayout.vLeafB1        = ef.leaf-bnum[1]
                ttFoldLayout.vLeafWid1      = ef.leaf-w[1]
                ttFoldLayout.vLeafLen1      = ef.leaf-l[1]
                ttFoldLayout.vLeafS2        = ef.leaf-snum[2]
                ttFoldLayout.vLeafB2        = ef.leaf-bnum[2]
                ttFoldLayout.vLeafWid2      = ef.leaf-w[2]
                ttFoldLayout.vLeafLen2      = ef.leaf-l[2]
                ttFoldLayout.vLeafS3        = ef.leaf-snum[3]
                ttFoldLayout.vLeafB3        = ef.leaf-bnum[3]  
                ttFoldLayout.vLeafWid3      = ef.leaf-w[3]
                ttFoldLayout.vLeafLen3      = ef.leaf-l[3]
                ttFoldLayout.vLeafS4        = ef.leaf-snum[4]
                ttFoldLayout.vLeafB4        = ef.leaf-bnum[4]
                ttFoldLayout.vLeafWid4      = ef.leaf-w[4]
                ttFoldLayout.vLeafLen4      = ef.leaf-l[4]  
                ttFoldLayout.vStyle         = eb.style
                ttFoldLayout.order          = string(eb.ord-no)
                    .

                IF NOT AVAIL ef AND NOT AVAIL eb THEN DO:
                    ASSIGN 
                        ttFoldLayout.vDieInches     = INT( INT(ef.die-in) / eb.num-up ) * int(eb.num-up) .
                END.
          END.  
   END.

/*********************************End Select***********************************/

PROCEDURE new-m-code :
    FIND FIRST mach
        WHERE mach.company EQ prmComp AND mach.m-code  EQ prmMachine NO-LOCK NO-ERROR.
    IF AVAIL mach AND TRIM(prmMachine) NE "" THEN DO:
        prmMachDscr = mach.m-dscr.
         IF mach.p-type EQ "R" THEN DO:
        prmRoll = "Y" . 
        END.
     END.
END PROCEDURE.


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
                    lv-leaf = prmLeaf1
                    lv-snum = prmS1.
                    WHEN 2 THEN
                        ASSIGN
                        lv-leaf = prmLeaf2
                        lv-snum = prmS2.
                        WHEN 3 THEN
                            ASSIGN
                            lv-leaf = prmLeaf3
                            lv-snum = prmS3.
                            WHEN 4 THEN
                                ASSIGN
                                lv-leaf = prmLeaf4
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
                     lv-leaf = prmLeaf1
                     lv-snum = prmS1
                     lv-bnum = prmB1.
                     WHEN 2 THEN
                         ASSIGN
                         lv-leaf = prmLeaf2
                         lv-snum = prmS2
                         lv-bnum = prmB2.
                         WHEN 3 THEN
                             ASSIGN
                             lv-leaf = prmLeaf3
                             lv-snum = prmS3
                             lv-bnum = prmB3.
                             WHEN 4 THEN
                                 ASSIGN
                                 lv-leaf = prmLeaf4
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
                          prmBoardName = tt-ef.brd-dscr
                          prmMachine = tt-ef.m-code
                          prmWeightt = tt-ef.weight
                          prmReal = tt-ef.i-code
                          prmSideSide = tt-ef.lsh-len
                          prmFrontBack = tt-ef.lsh-wid
                          /* ef.roll:screen-value = string(tt-ef.roll,"Y/N")*/
                          /*prmGrosShetLen = round(TRUNC((tt-ef.gsh-len),0) + (((tt-ef.gsh-len) - trunc((tt-ef.gsh-len),0)) / K_FRAC),2) /* string({sys/inc/k16.i tt-ef.gsh-len} ) */
                          prmGrosShetWid = round(TRUNC((tt-ef.gsh-wid),0) + (((tt-ef.gsh-wid) - trunc((tt-ef.gsh-wid),0)) / K_FRAC),2) /* string({sys/inc/k16.i tt-ef.gsh-wid} )*/*/
                          prmMachFeedLen = tt-ef.nsh-len
                          prmMachFeedWid = tt-ef.nsh-wid
                          prmDieSizeLen = tt-ef.trim-l
                          prmDieSizeWid = tt-ef.trim-w
                          prmOutWid = tt-ef.n-out
                          prmOutLen = tt-ef.n-out-l
                          prmOutCut = tt-ef.n-cuts
                          prmOnWid = tt-eb.num-wid
                          prmOnLen = tt-eb.num-len
                          prmOnTotalUp = tt-eb.num-up
                          prmRollWid = tt-ef.roll-wid
                         /* prmRollWid = IF tt-ef.roll = TRUE THEN "Y" ELSE "N"*/
                          
                          .
                      
                      /* RUN roll-display.*/
                      
                      
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
            /* prmTest     = item.reg-no*/
            prmBoardName = item.i-name
            prmReal   = item.i-code
            prmCaliper    = item.cal
            prmWeightt   = item.basis-w .
        RUN sys/ref/uom-rm.p (item.mat-type, OUTPUT uom-list).
        IF uom-list NE "" AND LOOKUP(prmCostUom,uom-list) LE 0 THEN
            prmCostUom = ENTRY(1,uom-list).
    END.
                        
END PROCEDURE.
           

   PROCEDURE num-wid-len-dep-changed :

       ASSIGN
           prmDieInches = DEC(prmDieInches) / INT(prmOnTotalUp)
           prmOnTotalUp = (IF INT(prmOnWid) EQ 0 THEN 1 ELSE INT(prmOnWid)) *
           (IF INT(prmOnLen) EQ 0 THEN 1 ELSE INT(prmOnLen))

           prmDieInches = DEC(prmDieInches) *
           INT(prmOnTotalUp).
   END PROCEDURE.


   

PROCEDURE valid-gsh-wid :
 
    DEF VAR li AS INT NO-UNDO.
  DEF VAR ld-tons AS DEC INIT -1 NO-UNDO.

  DEF BUFFER bf-eb FOR eb.


  DO TRANSACTION:
    {sys/inc/celayout.i}
  END.

  IF celayout-dec GT 0 THEN RUN est/boardton.p (ROWID(ef), OUTPUT ld-tons).

  
    IF ll-auto-calc-selected   AND
       ((DEC(prmGrosShetWid) LT DEC(prmMachFeedWid) AND
         prmXgrain NE "S")              OR
        (DEC(prmGrosShetWid) LT DEC(prmMachFeedLen) AND
         prmXgrain EQ "S"))              THEN DO:
      cError = "Gross Sheet Size can not be less than Net Sheet Size..." .
      
      RETURN .     
    END.

    FIND FIRST item
        WHERE item.company EQ prmComp
          AND item.i-no    EQ prmBoard
        NO-LOCK NO-ERROR.

    IF AVAIL item THEN DO:
      IF item.i-code EQ "R"                                                             AND
         ((prmRoll EQ "Y" AND DEC(prmGrosShetWid) NE item.r-wid)  OR
          (prmRoll NE "Y" AND DEC(prmGrosShetWid) NE item.s-wid)) THEN DO:
        cError = "Gross Sheet Size may not be changed for a Real Material..." .
        RETURN .
      END.

      IF prmRoll EQ "Y" AND ll-auto-calc-selected THEN DO:
        IF item.i-code EQ "E" THEN DO:
          FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.
          IF AVAIL e-item AND ld-tons LT celayout-dec THEN
          DO li = 1 TO 26:
            IF e-item.roll-w[li] GE DEC(prmGrosShetWid) THEN DO:
              prmGrosShetWid = e-item.roll-w[li].
              LEAVE.
            END.
          END.

          /*APPLY "value-changed" TO ef.roll.*/
        END.
      END.

      /*RUN roll-display.*/
    END.
  

END PROCEDURE.
