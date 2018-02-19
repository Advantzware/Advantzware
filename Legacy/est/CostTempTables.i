
/*------------------------------------------------------------------------
    File        : CostTempTables.i
    Purpose     : 

    Syntax      :

    Description : Definitions for Cost Temp Tables

    Author(s)   : BV
    Created     : Wed Feb 07 08:12:55 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE {1} TEMP-TABLE ttCostMaster
    FIELD company AS CHARACTER 
    FIELD estimateNo LIKE est.est-no
    FIELD masterQuantity AS DECIMAL  
    .
    
DEFINE {1} TEMP-TABLE ttCostHeader
    LIKE costHeader.
/*    FIELD company                 AS CHARACTER        */
/*    FIELD rec_key                 LIKE probe.rec_key  */
/*    FIELD rec_keyParentForm       LIKE probe.rec_key  */
/*    FIELD rec_keyParentSet        LIKE probe.rec_key  */
/*    FIELD isItem                  AS LOGICAL          */
/*    FIELD estimateNo              LIKE probe.est-no   */
/*    FIELD industry                AS INTEGER          */
/*    FIELD estimateType            AS INTEGER          */
/*    FIELD quantityMaster          LIKE probe.est-qty  */
/*    FIELD headerDescription       AS CHARACTER        */
/*    FIELD formNo                  AS INTEGER          */
/*    FIELD blankNo                 AS INTEGER          */
/*    FIELD jobNo                   LIKE job.job-no     */
/*    FIELD jobNo2                  LIKE job.job-no2    */
/*    FIELD calculationTime         AS DATETIME         */
/*    FIELD calculatedBy            AS CHARACTER        */
/*    FIELD customerPartID          AS CHARACTER        */
/*    FIELD fgItemID                AS CHARACTER        */
/*    FIELD stdCostBoard            AS DECIMAL          */
/*    FIELD stdCostDirectMaterial   AS DECIMAL          */
/*    FIELD stdCostDirectLabor      AS DECIMAL          */
/*    FIELD stdCostVariableOverhead AS DECIMAL          */
/*    FIELD stdCostDirectFactory    AS DECIMAL          */
/*    FIELD stdCostFixedOverhead    AS DECIMAL          */
/*    FIELD stdCostTotalFactory     AS DECIMAL          */
/*    FIELD stdCostPrepLaborCOGS    AS DECIMAL          */
/*    FIELD stdCostPrepMaterialCOGS AS DECIMAL          */
/*    FIELD stdCostMiscLaborCOGS    AS DECIMAL          */
/*    FIELD stdCostMiscMaterialCOGS AS DECIMAL          */
/*    FIELD stdCostTotalOther       AS DECIMAL          */
/*    FIELD stdCostFreight          AS DECIMAL          */
/*    FIELD stdCostGSAMaterial      AS DECIMAL          */
/*    FIELD stdCostGSALabor         AS DECIMAL          */
/*    FIELD stdCostGSABoard         AS DECIMAL          */
/*    FIELD stdCostTotalGSA         AS DECIMAL          */
/*    FIELD stdCostRoyalty          AS DECIMAL          */
/*    FIELD stdCostWarehousing      AS DECIMAL          */
/*    FIELD stdCostBrokerComm       AS DECIMAL          */
/*    FIELD stdCostSpecial1         AS DECIMAL          */
/*    FIELD stdCostSpecial2         AS DECIMAL          */
/*    FIELD stdCostSpecial3         AS DECIMAL          */
/*    FIELD stdCostCommission       AS DECIMAL          */
/*    FIELD stdCostFull             AS DECIMAL          */
/*    FIELD stdProfitNet            AS DECIMAL          */
/*    FIELD stdProfitGross          AS DECIMAL          */
/*    FIELD stdSellPrice            AS DECIMAL          */
/*/*    FIELD quantityBlanksOnNetSheet AS DECIMAL     */*/
/*/*    FIELD quantityNetSheetsOnGrossSheet AS DECIMAL*/*/
/*    FIELD quantityPerSet          AS DECIMAL          */
/*    FIELD quantityRequest         AS DECIMAL          */
/*    FIELD quantityYield           AS DECIMAL          */
/*    FIELD lengthGross             AS DECIMAL          */
/*    FIELD widthGross              AS DECIMAL          */
/*    FIELD depthGross              AS DECIMAL          */
/*    FIELD lengthNet               AS DECIMAL          */
/*    FIELD widthNet                AS DECIMAL          */
/*    FIELD depthNet                AS DECIMAL          */
/*    FIELD lengthBlank             AS DECIMAL          */
/*    FIELD widthBlank              AS DECIMAL          */
/*    FIELD blankSquareFeet         AS DECIMAL          */
/*    FIELD factorForm              AS DECIMAL          */
/*    FIELD factorSet               AS DECIMAL          */
    .


DEFINE {1} TEMP-TABLE ttCostDetail
    FIELD company                  AS CHARACTER 
    FIELD rec_key                  LIKE probe.rec_key
    FIELD rec_keyHeader            LIKE probe.rec_key
    FIELD rec_KeyActivity          LIKE probe.rec_key
    FIELD rec_keyModel             LIKE probe.rec_key
    FIELD modelType                AS CHARACTER 
    FIELD stdCost             AS DECIMAL 
    FIELD stdCostPerUOM       AS DECIMAL 
    FIELD stdCostUOM          AS CHARACTER 
    FIELD stdQuantityRequired AS DECIMAL 
    FIELD stdQuantityUOM      AS CHARACTER 
    FIELD actCost               AS DECIMAL 
    FIELD actCostPerUOM         AS DECIMAL 
    FIELD actCostUOM            AS CHARACTER 
    FIELD actQuantityRequired   AS DECIMAL 
    FIELD actQuantityUOM        AS CHARACTER 
    FIELD costDetailDescription    AS CHARACTER 
    .

DEFINE {1} TEMP-TABLE ttCostDetailMarkup
    FIELD company              AS CHARACTER 
    FIELD rec_key              LIKE probe.rec_key
    FIELD rec_keyHeader        LIKE probe.rec_key
    FIELD stdCostDescription   AS CHARACTER /*Commission, Margin, GS&A Board, [Special 1 Custom Description], etc.*/
    FIELD stdCostVariable      AS DECIMAL 
    FIELD stdCostFixed         AS DECIMAL 
    FIELD markupType           AS CHARACTER /*COM,GSAB,GSAM,GSAL,S1,S2,S3,ROY,BC,FRT,MAR*/ 
    FIELD stdCost              AS DECIMAL 
    FIELD markupPercent        AS DECIMAL 
    FIELD markupPercentOf      AS CHARACTER 
    FIELD markupFlatAmount     AS DECIMAL
    FIELD markupIsMargin       AS LOGICAL /*% of Sell Price vs. Cost Markup*/ 
    FIELD lookupValue          AS DECIMAL /* eg. Factory cost for Margin lookup*/
    FIELD lookupValueType      AS CHARACTER   /*eg "Factory Cost or Board Cost" for Margin Lookup*/
    FIELD lookupResult         AS DECIMAL  /*to determine overall markup in case there is Alt*/
    FIELD lookupAltValue       AS DECIMAL /*eg. value for Board % of Total Cost for Margin Reduction*/
    FIELD lookupAltValueType   AS CHARACTER /*eg "Board % of Total Cost" for Margin Reduction %*/
    FIELD lookupAltResult      AS DECIMAL /*to determine overall markup %*/
    FIELD includeInFactoryCost AS LOGICAL 
    . 
DEFINE {1} TEMP-TABLE ttCostDetailMisc
    FIELD company              AS CHARACTER 
    FIELD rec_key              LIKE probe.rec_key
    FIELD rec_keyHeader        LIKE probe.rec_key
    FIELD stdCostDescription   AS CHARACTER
    FIELD stdCostVariable      AS DECIMAL
    FIELD stdCostFixed         AS DECIMAL 
    FIELD materialID           LIKE item.i-no
    FIELD materialType         LIKE item.mat-type
    FIELD prepID               LIKE prep.code
    FIELD vendorID             LIKE vend.vend-no
    FIELD quantity             AS DECIMAL 
    FIELD quantityUOM          AS CHARACTER 
    FIELD stdCostBasePerUOM    AS DECIMAL 
    FIELD stdCostBaseUOM       AS CHARACTER
    FIELD stdCostSetup         AS DECIMAL 
    FIELD simonCode            AS CHARACTER
    FIELD markupPercent        AS DECIMAL 
    FIELD markupIsMargin       AS LOGICAL
    FIELD sellPrice            AS DECIMAL 
    FIELD profit               AS DECIMAL  
    FIELD excludeSellPriceFromCommission   AS LOGICAL 
    FIELD excludeChargeFromMarginLookup AS LOGICAL   
    FIELD excludeProfitFromCost AS LOGICAL 
    .
    
DEFINE {1} TEMP-TABLE ttCostDetailOperations
    FIELD company AS character
    FIELD rec_key LIKE probe.rec_key
    FIELD rec_keyHeader LIKE probe.rec_key
    .
    
DEFINE {1} TEMP-TABLE ttCostActivity
    FIELD company                AS CHARACTER 
    FIELD rec_key                LIKE probe.rec_key
    FIELD activityID             AS INTEGER 
    FIELD activityDescription    AS CHARACTER 
    FIELD profitLossCategory     AS CHARACTER  //COGS - Material, COGS - Labor, Prep Labor (e.g. the estimate summary bins)
    FIELD produtionCategory      AS CHARACTER //Setup,Run, Cleanup, Downtime etc.
    FIELD costSource             AS CHARACTER //Vendor,Production,Admin,Carrier, etc.
    FIELD calculationModel       AS CHARACTER //Board,Misc,Ink,Glue,Production,etc.
    FIELD calculationDescription AS CHARACTER //Description of the calculation formula/process for this cost center
    .
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
