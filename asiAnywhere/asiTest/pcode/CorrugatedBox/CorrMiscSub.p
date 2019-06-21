

/*------------------------------------------------------------------------
    File        : CorrMiscSub.p
    Purpose     : CorrMiscSub
    Syntax      :

    Description : Return a Dataset of Corrugated Misc/Sub 
    Author(s)   : 
    Created     : 9 Mar 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttCorrMiscSub NO-UNDO
        FIELD vEstimate            AS CHAR FORMAT "x(8)" 
        FIELD vEstDate             AS DATE  
        FIELD vForm                AS INTEGER
        FIELD vFormQty             AS INTEGER
        FIELD vBlk                 AS INTEGER
        FIELD vBlkQty              AS INTEGER
        FIELD vCustPart            AS CHAR    
        FIELD vS1                  AS INTEGER FORMAT ">9" 
        FIELD vB1                  AS INTEGER 
        FIELD vCost1               AS CHAR
        FIELD vMatf1               AS DECIMAL 
        FIELD vLabf1               AS DECIMAL 
        FIELD vMatm1               AS DECIMAL   
        FIELD vLabm1               AS DECIMAL  
        FIELD vSimon1              AS CHAR 
        FIELD vMarkUp1             AS DECIMAL 
        FIELD vS2                  AS INTEGER FORMAT ">9" 
        FIELD vB2                  AS INTEGER 
        FIELD vCost2               AS CHAR
        FIELD vMatf2               AS DECIMAL 
        FIELD vLabf2               AS DECIMAL 
        FIELD vMatm2               AS DECIMAL   
        FIELD vLabm2               AS DECIMAL  
        FIELD vSimon2              AS CHAR 
        FIELD vMarkUp2             AS DECIMAL 
        FIELD vS3                  AS INTEGER FORMAT ">9" 
        FIELD vB3                  AS INTEGER 
        FIELD vCost3               AS CHAR
        FIELD vMatf3               AS DECIMAL 
        FIELD vLabf3               AS DECIMAL 
        FIELD vMatm3               AS DECIMAL   
        FIELD vLabm3               AS DECIMAL  
        FIELD vSimon3              AS CHAR 
        FIELD vMarkUp3             AS DECIMAL 
        FIELD vS4                  AS INTEGER FORMAT ">9" 
        FIELD vB4                  AS INTEGER 
        FIELD vCost4               AS CHAR
        FIELD vMatf4               AS DECIMAL 
        FIELD vLabf4               AS DECIMAL 
        FIELD vMatm4               AS DECIMAL   
        FIELD vLabm4               AS DECIMAL  
        FIELD vSimon4              AS CHAR 
        FIELD vMarkUp4             AS DECIMAL 
        FIELD vS5                  AS INTEGER FORMAT ">9" 
        FIELD vB5                  AS INTEGER 
        FIELD vCost5               AS CHAR
        FIELD vMatf5               AS DECIMAL 
        FIELD vLabf5               AS DECIMAL 
        FIELD vMatm5               AS DECIMAL   
        FIELD vLabm5               AS DECIMAL  
        FIELD vSimon5              AS CHAR 
        FIELD vMarkUp5             AS DECIMAL 
        FIELD vS6                  AS INTEGER FORMAT ">9" 
        FIELD vB6                  AS INTEGER 
        FIELD vCost6               AS CHAR
        FIELD vMatf6               AS DECIMAL 
        FIELD vLabf6               AS DECIMAL 
        FIELD vMatm6               AS DECIMAL   
        FIELD vLabm6               AS DECIMAL  
        FIELD vSimon6              AS CHAR 
        FIELD vMarkUp6             AS DECIMAL 
        FIELD vItem1               AS CHAR
        FIELD vItemDscr1           AS CHAR
        FIELD vQty1                AS DECIMAL
        FIELD vUom1                AS CHAR
        FIELD vItem2               AS CHAR
        FIELD vItemDscr2           AS CHAR
        FIELD vQty2                AS DECIMAL
        FIELD vUom2                AS CHAR
        FIELD vItem3               AS CHAR
        FIELD vItemDscr3           AS CHAR
        FIELD vQty3                AS DECIMAL
        FIELD vUom3                AS CHAR
        FIELD vItem4               AS CHAR
        FIELD vItemDscr4           AS CHAR
        FIELD vQty4                AS DECIMAL
        FIELD vUom4                AS CHAR
        FIELD vItem5               AS CHAR
        FIELD vItemDscr5           AS CHAR
        FIELD vQty5                AS DECIMAL
        FIELD vUom5                AS CHAR
        FIELD vItem6               AS CHAR
        FIELD vItemDscr6           AS CHAR
        FIELD vQty6                AS DECIMAL
        FIELD vUom6                AS CHAR
        FIELD vItem7               AS CHAR
        FIELD vItemDscr7           AS CHAR
        FIELD vQty7                AS DECIMAL
        FIELD vUom7                AS CHAR
        FIELD vItem8               AS CHAR
        FIELD vItemDscr8           AS CHAR
        FIELD vQty8                AS DECIMAL
        FIELD vUom8                AS CHAR
        
        FIELD vType                AS INT .
        .
DEFINE DATASET dsCorrMiscSub FOR ttCorrMiscSub .

DEFINE INPUT PARAMETER prmUser                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmType                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmComp                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmEstDate                   AS DATE        NO-UNDO.
DEFINE INPUT PARAMETER prmForm                      AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmFormQty                   AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmBlk                       AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmBlkQty                    AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmCustPart                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmS1                        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmB1                        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmCost1                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmMatf1                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmMatm1                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLabf1                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLabm1                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSimon1                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmMarkup1                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmS2                        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmB2                        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmCost2                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmMatf2                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmMatm2                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLabf2                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLabm2                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSimon2                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmMarkup2                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmS3                        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmB3                        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmCost3                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmMatf3                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmMatm3                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLabf3                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLabm3                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSimon3                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmMarkup3                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmS4                        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmB4                        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmCost4                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmMatf4                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmMatm4                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLabf4                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLabm4                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSimon4                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmMarkup4                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmS5                        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmB5                        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmCost5                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmMatf5                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmMatm5                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLabf5                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLabm5                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSimon5                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmMarkup5                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmS6                        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmB6                        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmCost6                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmMatf6                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmMatm6                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLabf6                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmLabm6                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSimon6                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmMarkup6                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmItem1                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmItemDesc1                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmQty1                      AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmUom1                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmItem2                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmItemDesc2                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmQty2                      AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmUom2                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmItem3                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmItemDesc3                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmQty3                      AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmUom3                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmItem4                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmItemDesc4                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmQty4                      AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmUom4                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmItem5                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmItemDesc5                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmQty5                      AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmUom5                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmItem6                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmItemDesc6                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmQty6                      AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmUom6                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmItem7                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmItemDesc7                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmQty7                      AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmUom7                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmItem8                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmItemDesc8                 AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmQty8                      AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmUom8                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBlank                     AS INT NO-UNDO .
DEFINE OUTPUT PARAMETER cError                      AS CHAR        NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrMiscSub.

IF prmUser         = ?  THEN ASSIGN prmUser         = "".
IF prmAction       = ?  THEN ASSIGN prmAction        = "".
IF prmType         = ?  THEN ASSIGN prmType          = "".
IF prmComp         = ?  THEN ASSIGN prmComp          = "".
IF prmEstimate     = ?  THEN ASSIGN prmEstimate      = "".
IF prmForm         = ?  THEN ASSIGN prmForm          = 0.
IF prmFormQty      = ?  THEN ASSIGN prmFormQty       = 0.
IF prmBlk          = ?  THEN ASSIGN prmBlk           = 0.
IF prmBlkQty       = ?  THEN ASSIGN prmBlkQty        = 0.
IF prmCustPart     = ?  THEN ASSIGN prmCustPart      = "".
IF prmS1           = ?  THEN ASSIGN prmS1            = 0.
IF prmB1           = ?  THEN ASSIGN prmB1            = 0.
IF prmCost1        = ?  THEN ASSIGN prmCost1         = "".
IF prmMatf1        = ?  THEN ASSIGN prmMatf1         = 0.
IF prmMatm1        = ?  THEN ASSIGN prmMatm1         = 0.
IF prmLabf1        = ?  THEN ASSIGN prmLabf1         = 0.
IF prmLabm1        = ?  THEN ASSIGN prmLabm1         = 0.
IF prmSimon1       = ?  THEN ASSIGN prmSimon1        = "".
IF prmMarkup1      = ?  THEN ASSIGN prmMarkup1       = 0.
IF prmS2           = ?  THEN ASSIGN prmS2            = 0.
IF prmB2           = ?  THEN ASSIGN prmB2            = 0.
IF prmCost2        = ?  THEN ASSIGN prmCost2         = "".
IF prmMatf2        = ?  THEN ASSIGN prmMatf2         = 0.
IF prmMatm2        = ?  THEN ASSIGN prmMatm2         = 0.
IF prmLabf2        = ?  THEN ASSIGN prmLabf2         = 0.
IF prmLabm2        = ?  THEN ASSIGN prmLabm2         = 0.
IF prmSimon2       = ?  THEN ASSIGN prmSimon2        = "".
IF prmMarkup2      = ?  THEN ASSIGN prmMarkup2       = 0.
IF prmS3           = ?  THEN ASSIGN prmS3            = 0.
IF prmB3           = ?  THEN ASSIGN prmB3            = 0.
IF prmCost3        = ?  THEN ASSIGN prmCost3         = "".
IF prmMatf3        = ?  THEN ASSIGN prmMatf3         = 0.
IF prmMatm3        = ?  THEN ASSIGN prmMatm3         = 0.
IF prmLabf3        = ?  THEN ASSIGN prmLabf3         = 0.
IF prmLabm3        = ?  THEN ASSIGN prmLabm3         = 0.
IF prmSimon3       = ?  THEN ASSIGN prmSimon3        = "".
IF prmMarkup3      = ?  THEN ASSIGN prmMarkup3       = 0.
IF prmS4           = ?  THEN ASSIGN prmS4            = 0.
IF prmB4           = ?  THEN ASSIGN prmB4            = 0.
IF prmCost4        = ?  THEN ASSIGN prmCost4         = "".
IF prmMatf4        = ?  THEN ASSIGN prmMatf4         = 0.
IF prmMatm4        = ?  THEN ASSIGN prmMatm4         = 0.
IF prmLabf4        = ?  THEN ASSIGN prmLabf4         = 0.
IF prmLabm4        = ?  THEN ASSIGN prmLabm4         = 0.
IF prmSimon4       = ?  THEN ASSIGN prmSimon4        = "".
IF prmMarkup4      = ?  THEN ASSIGN prmMarkup4       = 0.
IF prmS5           = ?  THEN ASSIGN prmS5            = 0.
IF prmB5           = ?  THEN ASSIGN prmB5            = 0.
IF prmCost5        = ?  THEN ASSIGN prmCost5         = "".
IF prmMatf5        = ?  THEN ASSIGN prmMatf5         = 0.
IF prmMatm5        = ?  THEN ASSIGN prmMatm5         = 0.
IF prmLabf5        = ?  THEN ASSIGN prmLabf5         = 0.
IF prmLabm5        = ?  THEN ASSIGN prmLabm5         = 0.
IF prmSimon5       = ?  THEN ASSIGN prmSimon5        = "".
IF prmMarkup5      = ?  THEN ASSIGN prmMarkup5       = 0.
IF prmS6           = ?  THEN ASSIGN prmS6            = 0.
IF prmB6           = ?  THEN ASSIGN prmB6            = 0.
IF prmCost6        = ?  THEN ASSIGN prmCost6         = "".
IF prmMatf6        = ?  THEN ASSIGN prmMatf6         = 0.      
IF prmMatm6        = ?  THEN ASSIGN prmMatm6         = 0.     
IF prmLabf6        = ?  THEN ASSIGN prmLabf6         = 0.     
IF prmLabm6        = ?  THEN ASSIGN prmLabm6         = 0.       
IF prmSimon6       = ?  THEN ASSIGN prmSimon6        = "".     
IF prmMarkup6      = ?  THEN ASSIGN prmMarkup6       = 0.     
IF prmItem1        = ?  THEN ASSIGN prmItem1         = "".     
IF prmItemDesc1    = ?  THEN ASSIGN prmItemDesc1     = "".     
IF prmQty1         = ?  THEN ASSIGN prmQty1          = 0.     
IF prmUom1         = ?  THEN ASSIGN prmUom1          = "".     
IF prmItem2        = ?  THEN ASSIGN prmItem2         = "".     
IF prmItemDesc2    = ?  THEN ASSIGN prmItemDesc2     = "".     
IF prmQty2         = ?  THEN ASSIGN prmQty2          = 0.     
IF prmUom2         = ?  THEN ASSIGN prmUom2          = "".     
IF prmItem3        = ?  THEN ASSIGN prmItem3         = "".     
IF prmItemDesc3    = ?  THEN ASSIGN prmItemDesc3     = "".     
IF prmQty3         = ?  THEN ASSIGN prmQty3          = 0.    
IF prmUom3         = ?  THEN ASSIGN prmUom3          = "".    
IF prmItem4        = ?  THEN ASSIGN prmItem4         = "".    
IF prmItemDesc4    = ?  THEN ASSIGN prmItemDesc4     = "".    
IF prmQty4         = ?  THEN ASSIGN prmQty4          = 0.    
IF prmUom4         = ?  THEN ASSIGN prmUom4          = "".    
IF prmItem5        = ?  THEN ASSIGN prmItem5         = "".    
IF prmItemDesc5    = ?  THEN ASSIGN prmItemDesc5     = "".    
IF prmQty5         = ?  THEN ASSIGN prmQty5          = 0.    
IF prmUom5         = ?  THEN ASSIGN prmUom5          = "".
IF prmItem6        = ?  THEN ASSIGN prmItem6         = "".
IF prmItemDesc6    = ?  THEN ASSIGN prmItemDesc6     = "".
IF prmQty6         = ?  THEN ASSIGN prmQty6          = 0.
IF prmUom6         = ?  THEN ASSIGN prmUom6          = "".
IF prmItem7        = ?  THEN ASSIGN prmItem7         = "".
IF prmItemDesc7    = ?  THEN ASSIGN prmItemDesc7     = "".
IF prmQty7         = ?  THEN ASSIGN prmQty7          = 0.
IF prmUom7         = ?  THEN ASSIGN prmUom7          = "".
IF prmItem8        = ?  THEN ASSIGN prmItem8         = "".
IF prmItemDesc8    = ?  THEN ASSIGN prmItemDesc8     = "".
IF prmQty8         = ?  THEN ASSIGN prmQty8          = 0.
IF prmUom8         = ?  THEN ASSIGN prmUom8          = "".
                   


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/**********************************Update**************************************/
IF prmAction = "Update" THEN DO:

    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no NO-LOCK NO-ERROR.

  def var lv-ref-rec-qty as recid no-undo.
  def var lv-ref-rec-cst as recid no-undo.

   /* IF prmCost1 = "" THEN DO:
        {cec/refestg2.i "MAT" 1}
        {cec/refestg2.i "LAB" 1}
    END.
    IF prmCost2 = "" THEN DO:
        {cec/refestg2.i "MAT" 2}
        {cec/refestg2.i "LAB" 2}
    END.
    IF prmCost3 = "" THEN DO:
        {cec/refestg2.i "MAT" 3}
        {cec/refestg2.i "LAB" 3}
    END.
    IF prmCost4 = "" THEN DO:
        {cec/refestg2.i "MAT" 4}
        {cec/refestg2.i "LAB" 4}
    END.
    IF prmCost5 = "" THEN DO:
        {cec/refestg2.i "MAT" 5}
        {cec/refestg2.i "LAB" 5}
    END.
    IF prmCost6 = "" THEN DO:
        {cec/refestg2.i "MAT" 6}
        {cec/refestg2.i "LAB" 6}
    END.*/
    IF prmCost1 <> "" OR prmCost2 <> "" OR prmCost3 <> "" OR prmCost4 <> "" OR prmCost5 <> "" OR prmCost6 <> "" THEN DO:
        RUN new-mis-cost.
    END.

    IF prmItem1 <> "" THEN DO:
        find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789",ITEM.mat-type) GT 0 and
                          item.i-no = prmItem1
                          no-lock no-error.
            if not avail item then do:
                if prmItem1 = "" then do:
                    prmItemDesc1 = "".
                end.
                cError =  "Invalid Item1. Try Help." .
                return .
            end.                          
     prmItemDesc1 = item.i-name.
    END.

    IF prmItem2 <> "" THEN DO:
        find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789",ITEM.mat-type) GT 0 and
                          item.i-no = prmItem2
                          no-lock no-error.
    if not avail item then do:
       if prmItem2 = "" then do:
          prmItemDesc2 = "".
          end.
       cError =  "Invalid Item2. Try Help." .
       return .
    end.                          
    prmItemDesc2 = item.i-name.
    END.
    
    IF prmItem3 <> "" THEN DO:
        find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789",ITEM.mat-type) GT 0 and
                          item.i-no = prmItem3
                          no-lock no-error.
    if not avail item then do:
       if prmItem3 = "" then do:
          prmItemDesc3 = "".
          end.
       cError =  "Invalid Item3. Try Help." .
       return .
    end.                          
    prmItemDesc3 = item.i-name.
    END.
    IF prmItem4 <> "" THEN DO:
        find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789",ITEM.mat-type) GT 0 and
                          item.i-no = prmItem4
                          no-lock no-error.
    if not avail item then do:
       if prmItem4 = "" then do:
          prmItemDesc4 = "".
          
       end.
       cError =  "Invalid Item4. Try Help." .
       return .
    end.                          
    prmItemDesc4 = item.i-name.
    END.
    IF prmItem5 <> "" THEN DO:
        find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789",ITEM.mat-type) GT 0 and
                          item.i-no = prmItem5
                          no-lock no-error.
    if not avail item then do:
       if prmItem5 = "" then do:
          prmItemDesc5 = "".
          
       end.
       cError =  "Invalid Item5. Try Help." .
       return .
    end.                          
    prmItemDesc5 = item.i-name.
    END.

    IF prmItem6 <> "" THEN DO:
        find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789",ITEM.mat-type) GT 0 and
                          item.i-no = prmItem6
                          no-lock no-error.
    if not avail item then do:
       if prmItem6 = "" then do:
          prmItemDesc6 = "".
          end.
       cError =  "Invalid Item6. Try Help." .
       return .
    end.                          
    prmItemDesc6 = item.i-name.
    END.
    IF prmItem7 <> "" THEN DO:
        find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789",ITEM.mat-type) GT 0 and
                          item.i-no = prmItem7
                          no-lock no-error.
    if not avail item then do:
       if prmItem7 = "" then do:
          prmItemDesc7 = "".
          end.
       cError =  "Invalid Item7. Try Help." .
       return .
    end.                          
    prmItemDesc7 = item.i-name.
    END.
    IF prmItem8 <> "" THEN DO:
        find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789",ITEM.mat-type) GT 0 and
                          item.i-no = prmItem8
                          no-lock no-error.
    if not avail item then do:
       if prmItem8 = "" then do:
          prmItemDesc8 = "".
          end.
       cError =  "Invalid Item8. Try Help." .
       return .
    end.                          
    prmItemDesc8 = item.i-name.
    END.
END.
/**********************************End Update*********************************/

/**********************************Update**************************************/
IF prmAction = "Update" THEN DO:
    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN
       /* est.est-no        =   prmEstimate                                                                           
        est.est-date      =   prmEstDate                                                                          
        ef.form-no        =   prmForm                                                                             
        est.form-qty      =   prmFormQty 
        eb.blank-no       =   prmBlk      
        ef.blank-qty      =   prmBlkQty   
        eb.part-no        =   prmCustPart */                                                  
        ef.mis-snum[1]    =   prmS1       
        ef.mis-bnum[1]    =   prmB1       
        ef.mis-cost[1]    =   prmCost1                                                      
        ef.mis-matf[1]    =   prmMatf1                                                      
        ef.mis-labf[1]    =   prmLabf1 
        ef.mis-matm[1]    =   prmMatm1                                                      
        ef.mis-labm[1]    =   prmLabm1
        ef.mis-simon[1]   =   prmSimon1                                                     
        ef.mis-mkup[1]    =   prmMarkup1                                                    
        ef.mis-snum[2]    =   prmS2                                                         
        ef.mis-bnum[2]    =   prmB2                                                         
        ef.mis-cost[2]    =   prmCost2                                                      
        ef.mis-matf[2]    =   prmMatf2                                                      
        ef.mis-labf[2]    =   prmLabf2                                                            /* = TRUE THEN "C" ELSE "N" */
        ef.mis-matm[2]    =   prmMatm2    
        ef.mis-labm[2]    =   prmLabm2 
        ef.mis-simon[2]   =   prmSimon2                                                     
        ef.mis-mkup[2]    =   prmMarkup2                                                    
        ef.mis-snum[3]    =   prmS3       
        ef.mis-bnum[3]    =   prmB3       
        ef.mis-cost[3]    =   prmCost3                                                      
        ef.mis-matf[3]    =   prmMatf3                                                      
        ef.mis-labf[3]    =   prmLabf3                                                      
        ef.mis-matm[3]    =   prmMatm3                                                      
        ef.mis-labm[3]    =   prmLabm3 
        ef.mis-simon[3]   =   prmSimon3   
        ef.mis-mkup[3]    =   prmMarkup3                                                    
        ef.mis-snum[4]    =   prmS4                                                         
        ef.mis-bnum[4]    =   prmB4                                                         
        ef.mis-cost[4]    =   prmCost4                                                      
        ef.mis-matf[4]    =   prmMatf4    
        ef.mis-labf[4]    =   prmLabf4    
        ef.mis-matm[4]    =   prmMatm4    
        ef.mis-labm[4]    =   prmLabm4    
        ef.mis-simon[4]   =   prmSimon4   
        ef.mis-mkup[4]    =   prmMarkup4  
        ef.mis-snum[5]    =   prmS5       
        ef.mis-bnum[5]    =   prmB5       
        ef.mis-cost[5]    =   prmCost5    
        ef.mis-matf[5]    =   prmMatf5    
        ef.mis-labf[5]    =   prmLabf5    
        ef.mis-matm[5]    =   prmMatm5    
        ef.mis-labm[5]    =   prmLabm5    
        ef.mis-simon[5]   =   prmSimon5   
        ef.mis-mkup[5]    =   prmMarkup5  
        ef.mis-snum[6]    =   prmS6       
        ef.mis-bnum[6]    =   prmB6       
        ef.mis-cost[6]    =   prmCost6    
        ef.mis-matf[6]    =   prmMatf6    
        ef.mis-labf[6]    =   prmLabf6    
        ef.mis-matm[6]    =   prmMatm6    
        ef.mis-labm[6]    =   prmLabm6    
        ef.mis-simon[6]   =   prmSimon6   
        ef.mis-mkup[6]    =   prmMarkup6  
        ef.spec-no[1]     =   prmItem1    
        ef.spec-dscr[1]   =   prmItemDesc1
        ef.spec-qty[1]    =   prmQty1 * 10000    
        ef.spec-uom[1]    =   prmUom1     
        ef.spec-no[2]     =   prmItem2    
        ef.spec-dscr[2]   =   prmItemDesc2
        ef.spec-qty[2]    =   prmQty2 * 10000     
        ef.spec-uom[2]    =   prmUom2     
        ef.spec-no[3]     =   prmItem3    
        ef.spec-dscr[3]   =   prmItemDesc3
        ef.spec-qty[3]    =   prmQty3 * 10000    
        ef.spec-uom[3]    =   prmUom3     
        ef.spec-no[4]     =   prmItem4    
        ef.spec-dscr[4]   =   prmItemDesc4
        ef.spec-qty[4]    =   prmQty4 * 10000     
        ef.spec-uom[4]    =   prmUom4     
        ef.spec-no[5]     =   prmItem5    
        ef.spec-dscr[5]   =   prmItemDesc5
        ef.spec-qty[5]    =   prmQty5 * 10000     
        ef.spec-uom[5]    =   prmUom5     
        ef.spec-no[6]     =   prmItem6    
        ef.spec-dscr[6]   =   prmItemDesc6
        ef.spec-qty[6]    =   prmQty6 * 10000    
        ef.spec-uom[6]    =   prmUom6     
        ef.spec-no[7]     =   prmItem7    
        ef.spec-dscr[7]   =   prmItemDesc7
        ef.spec-qty[7]    =   prmQty7 * 10000    
        ef.spec-uom[7]    =   prmUom7     
        ef.spec-no[8]     =   prmItem8    
        ef.spec-dscr[8]   =   prmItemDesc8
        ef.spec-qty[8]    =   prmQty8 * 10000     
        ef.spec-uom[8]    =   prmUom8     
            .   

        ASSIGN
            prmAction = "Select" .
  END.


/*********************************End Update**********************************/

/********************************Select****************************************/

IF prmAction = "Select" THEN DO:
    
    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    
    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no AND eb.blank-no = prmBlank NO-LOCK NO-ERROR.
    MESSAGE "select" prmAction  prmEstimate prmComp prmForm.
        
        IF AVAIL est AND AVAIL ef AND AVAIL eb THEN DO:
            CREATE ttCorrMiscSub.
            ASSIGN
                ttCorrMiscSub.vEstimate         = est.est-no   
                ttCorrMiscSub.vEstDate          = est.est-date
                ttCorrMiscSub.vForm             = ef.form-no  
                ttCorrMiscSub.vFormQty          = est.form-qty    
                ttCorrMiscSub.vBlk              = eb.blank-no
                ttCorrMiscSub.vBlkQty           = ef.blank-qty
                ttCorrMiscSub.vCustPart         = eb.part-no
                ttCorrMiscSub.vS1               = ef.mis-snum[1]
                ttCorrMiscSub.vB1               = ef.mis-bnum[1]
                ttCorrMiscSub.vCost1            = ef.mis-cost[1]
                ttCorrMiscSub.vMatf1            = ef.mis-matf[1]
                ttCorrMiscSub.vLabf1            = ef.mis-labf[1]
                ttCorrMiscSub.vMatm1            = ef.mis-matm[1]
                ttCorrMiscSub.vLabm1            = ef.mis-labm[1]
                ttCorrMiscSub.vSimon1           = ef.mis-simon[1]
                ttCorrMiscSub.vMarkUp1          = ef.mis-mkup[1]
                ttCorrMiscSub.vS2               = ef.mis-snum[2]
                ttCorrMiscSub.vB2               = ef.mis-bnum[2]
                ttCorrMiscSub.vCost2            = ef.mis-cost[2]
                ttCorrMiscSub.vMatf2            = ef.mis-matf[2]
                ttCorrMiscSub.vLabf2            = ef.mis-labf[2]
                ttCorrMiscSub.vMatm2            = ef.mis-matm[2]
                ttCorrMiscSub.vLabm2            = ef.mis-labm[2]
                ttCorrMiscSub.vSimon2           = ef.mis-simon[2]
                ttCorrMiscSub.vMarkUp2          = ef.mis-mkup[2]
                ttCorrMiscSub.vS3               = ef.mis-snum[3]
                ttCorrMiscSub.vB3               = ef.mis-bnum[3]
                ttCorrMiscSub.vCost3            = ef.mis-cost[3]
                ttCorrMiscSub.vMatf3            = ef.mis-matf[3]
                ttCorrMiscSub.vLabf3            = ef.mis-labf[3]
                ttCorrMiscSub.vMatm3            = ef.mis-matm[3]
                ttCorrMiscSub.vLabm3            = ef.mis-labm[3]
                ttCorrMiscSub.vSimon3           = ef.mis-simon[3]
                ttCorrMiscSub.vMarkUp3          = ef.mis-mkup[3]
                ttCorrMiscSub.vS4               = ef.mis-snum[4]
                ttCorrMiscSub.vB4               = ef.mis-bnum[4]
                ttCorrMiscSub.vCost4            = ef.mis-cost[4]
                ttCorrMiscSub.vMatf4            = ef.mis-matf[4]
                ttCorrMiscSub.vLabf4            = ef.mis-labf[4]
                ttCorrMiscSub.vMatm4            = ef.mis-matm[4]
                ttCorrMiscSub.vLabm4            = ef.mis-labm[4]
                ttCorrMiscSub.vSimon4           = ef.mis-simon[4]
                ttCorrMiscSub.vMarkUp4          = ef.mis-mkup[4]
                ttCorrMiscSub.vS5               = ef.mis-snum[5]
                ttCorrMiscSub.vB5               = ef.mis-bnum[5]
                ttCorrMiscSub.vCost5            = ef.mis-cost[5]
                ttCorrMiscSub.vMatf5            = ef.mis-matf[5]
                ttCorrMiscSub.vLabf5            = ef.mis-labf[5]
                ttCorrMiscSub.vMatm5            = ef.mis-matm[5]
                ttCorrMiscSub.vLabm5            = ef.mis-labm[5]
                ttCorrMiscSub.vSimon5           = ef.mis-simon[5]
                ttCorrMiscSub.vMarkUp5          = ef.mis-mkup[5]
                ttCorrMiscSub.vS6               = ef.mis-snum[6]
                ttCorrMiscSub.vB6               = ef.mis-bnum[6]
                ttCorrMiscSub.vCost6            = ef.mis-cost[6]
                ttCorrMiscSub.vMatf6            = ef.mis-matf[6]
                ttCorrMiscSub.vLabf6            = ef.mis-labf[6]
                ttCorrMiscSub.vMatm6            = ef.mis-matm[6]
                ttCorrMiscSub.vLabm6            = ef.mis-labm[6]
                ttCorrMiscSub.vSimon6           = ef.mis-simon[6]
                ttCorrMiscSub.vMarkUp6          = ef.mis-mkup[6]
                ttCorrMiscSub.vItem1            = ef.spec-no[1]
                ttCorrMiscSub.vItemDscr1        = ef.spec-dscr[1]
                ttCorrMiscSub.vQty1             = ef.spec-qty[1] * .0001
                ttCorrMiscSub.vUom1             = ef.spec-uom[1]
                ttCorrMiscSub.vItem2            = ef.spec-no[2]
                ttCorrMiscSub.vItemDscr2        = ef.spec-dscr[2]
                ttCorrMiscSub.vQty2             = ef.spec-qty[2] * .0001
                ttCorrMiscSub.vUom2             = ef.spec-uom[2]
                ttCorrMiscSub.vItem3            = ef.spec-no[3]
                ttCorrMiscSub.vItemDscr3        = ef.spec-dscr[3]
                ttCorrMiscSub.vQty3             = ef.spec-qty[3] * .0001
                ttCorrMiscSub.vUom3             = ef.spec-uom[3]
                ttCorrMiscSub.vItem4            = ef.spec-no[4]
                ttCorrMiscSub.vItemDscr4        = ef.spec-dscr[4]
                ttCorrMiscSub.vQty4             = ef.spec-qty[4] * .0001
                ttCorrMiscSub.vUom4             = ef.spec-uom[4]
                ttCorrMiscSub.vItem5            = ef.spec-no[5]
                ttCorrMiscSub.vItemDscr5        = ef.spec-dscr[5]
                ttCorrMiscSub.vQty5             = ef.spec-qty[5] * .0001
                ttCorrMiscSub.vUom5             = ef.spec-uom[5]
                ttCorrMiscSub.vItem6            = ef.spec-no[6]
                ttCorrMiscSub.vItemDscr6        = ef.spec-dscr[6]
                ttCorrMiscSub.vQty6             = ef.spec-qty[6] * .0001
                ttCorrMiscSub.vUom6             = ef.spec-uom[6]
                ttCorrMiscSub.vItem7            = ef.spec-no[7]
                ttCorrMiscSub.vItemDscr7        = ef.spec-dscr[7]
                ttCorrMiscSub.vQty7             = ef.spec-qty[7] * .0001
                ttCorrMiscSub.vUom7             = ef.spec-uom[7]
                ttCorrMiscSub.vItem8            = ef.spec-no[8]
                ttCorrMiscSub.vItemDscr8        = ef.spec-dscr[8]
                ttCorrMiscSub.vQty8             = ef.spec-qty[8] * .0001
                ttCorrMiscSub.vUom8             = ef.spec-uom[8]
                  
                    .
          END.  
   END.

/*********************************End Select***********************************/

PROCEDURE new-mis-cost :
 
/*    ASSIGN
     prmS1 = IF prmCost1 NE "" THEN ef.form-no ELSE ""
     prmS2 = IF prmCost2 NE "" THEN ef.form-no ELSE ""
     prmS3 = IF prmCost3 NE "" THEN ef.form-no ELSE ""
     prmS4 = IF prmCost4 NE "" THEN ef.form-no ELSE ""
     prmS5 = IF prmCost5 NE "" THEN ef.form-no ELSE ""
     prmS6 = IF prmCost6 NE "" THEN ef.form-no ELSE "".*/

    IF prmCost1 NE "" AND prmB1 EQ 0 THEN prmB1 = 1.
    IF prmCost2 NE "" AND prmB2 EQ 0 THEN prmB2 = 1.
    IF prmCost3 NE "" AND prmB3 EQ 0 THEN prmB3 = 1.
    IF prmCost4 NE "" AND prmB4 EQ 0 THEN prmB4 = 1.
    IF prmCost5 NE "" AND prmB5 EQ 0 THEN prmB5 = 1.
    IF prmCost6 NE "" AND prmB6 EQ 0 THEN prmB6 = 1.

   /* IF prmCost1 NE ""     AND DEC(prmMarkup1) EQ 0 THEN
      prmMarkup1 = cemisc-dec.

    IF prmCost2 NE ""     AND DEC(prmMarkup2) EQ 0 THEN
      prmMarkup2 = cemisc-dec.

    IF prmCost3 NE ""     AND DEC(prmMarkup3) EQ 0 THEN
      prmMarkup3 = cemisc-dec.

    IF prmCost4 NE ""     AND DEC(prmMarkup4) EQ 0 THEN
      prmMarkup4 = cemisc-dec.

    IF prmCost5 NE ""     AND DEC(prmMarkup5) EQ 0 THEN
      prmMarkup5 = cemisc-dec.

    IF prmCost6 NE ""     AND DEC(prmMarkup6) EQ 0 THEN
      prmMarkup6 = cemisc-dec.*/
  
END PROCEDURE.

