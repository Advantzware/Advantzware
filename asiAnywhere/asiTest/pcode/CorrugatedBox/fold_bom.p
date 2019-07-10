

/*------------------------------------------------------------------------
    File        : fold_bom.p
    Purpose     : foldBom
    Syntax      :

    Description : Return a Dataset of Folding Layout Bom 
    Author(s)   : 
    Created     : 23 sep 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttFoldBom NO-UNDO
        FIELD vMedium         AS CHAR  
        FIELD vLiner          AS CHAR  
        FIELD vLamCode         AS CHAR    
        FIELD vAdhesive        AS CHAR 
        FIELD vFlute          AS DECIMAL
        FIELD vSqInch          AS DECIMAL FORMAT ">>,>>9.9<<<"
        
        .
DEFINE DATASET dsFoldBom FOR ttFoldBom .

DEFINE INPUT PARAMETER prmUser                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmComp                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmForm                      AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmMedium                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLiner                     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLamCode                   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAdhesive                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFlute                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSqInch                    AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER cError                      AS CHAR        NO-UNDO.



DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFoldBom.

IF prmUser        = ?  THEN ASSIGN prmUser             = "".
IF prmAction      = ?  THEN ASSIGN prmAction           = "".
IF prmComp        = ?  THEN ASSIGN prmComp             = "".
IF prmEstimate    = ?  THEN ASSIGN prmEstimate         = "".
IF prmForm        = ?  THEN ASSIGN prmForm             = 0.
IF prmMedium      = ?  THEN ASSIGN prmMedium           = "".
IF prmLiner       = ?  THEN ASSIGN prmLiner            = "".
IF prmLamCode     = ?  THEN ASSIGN prmLamCode          = "".
IF prmAdhesive    = ?  THEN ASSIGN prmAdhesive         = "".
IF prmFlute       = ?  THEN ASSIGN prmFlute            = 0.
IF prmSqInch      = ?  THEN ASSIGN prmSqInch           = 0.


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/**********************************OverRide**************************************/
IF prmAction = "Update" THEN DO:

    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm EXCLUSIVE-LOCK.
    
    IF prmAdhesive NE "" THEN DO:
      IF NOT CAN-FIND(FIRST ITEM
                      where item.company  = prmComp and
                       item.mat-type = "G"
                        AND item.industry EQ "1"
                        AND item.i-no EQ prmAdhesive) THEN DO:
            ASSIGN cError = "Invalid adhesive, try help..." .
            RETURN .
      END.
      prmSqInch = ef.gsh-len * ef.gsh-wid.
    END.

    IF prmLiner NE "" AND
       NOT CAN-FIND(FIRST ITEM
                   where item.company  eq prmComp 
                    and  item.mat-type eq "P"
                     AND item.industry EQ "1"
                     AND item.i-no EQ prmLiner) THEN DO:
      ASSIGN cError = "Invalid liner, try help..." .
      RETURN .
    END.

    IF prmLamCode NE ""   AND
       NOT CAN-FIND(FIRST ITEM
                   where item.company  = prmComp and
                     item.mat-type = "L"
                     AND item.industry EQ "1"
                     AND item.i-no EQ prmLamCode) THEN DO:
      ASSIGN cError = "Invalid lamcode, try help..." .
      RETURN .
    END.

    IF prmMedium NE ""  AND
       NOT CAN-FIND(FIRST ITEM
                   where item.company  eq prmComp 
                    and  item.mat-type eq "P"
                     AND item.industry EQ "1"
                     AND item.i-no EQ prmMedium) THEN DO:
      ASSIGN cError = "Invalid medium, try help..." .
      RETURN .
    END.

    IF AVAIL ef  THEN DO:
        ASSIGN
            ef.medium    = prmMedium
            ef.flute     = prmLiner
            ef.lam-code  = prmLamCode
            ef.adh-code  = prmAdhesive
            ef.trim-pen  = prmFlute
            ef.adh-sqin  = prmSqInch
                .
    END.

    ASSIGN
            prmAction = "Select" .
 END.  /* end update*/

/*********************************End OverRide**********************************/

/**********************************OverRide**************************************/
IF prmAction = "Select" THEN DO:
   
    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
    
    IF AVAIL ef THEN DO:
    
        CREATE ttFoldBom.
        ASSIGN
            ttFoldBom.vMedium     = ef.medium
            ttFoldBom.vLiner      = ef.flute
            ttFoldBom.vLamCode    = ef.lam-code
            ttFoldBom.vAdhesive   = ef.adh-code
            ttFoldBom.vFlute      = ef.trim-pen
            ttFoldBom.vSqInch     = ef.adh-sqin         /*ef.gsh-len * ef.gsh-wid*/
                 .
    END.
    
  END.    /* end override*/


/*********************************End OverRide**********************************/

  /*------------------------------------------------------------------------------*/
  
/*------------------------------------------------------------------------------*/
