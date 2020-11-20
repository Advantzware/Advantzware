
/*------------------------------------------------------------------------
    File        : ttItemPurchased.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Jan 15 15:44:29 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
                  /* old w-job-mat */
DEFINE TEMP-TABLE ttItemPurchased NO-UNDO like job-mat
    FIELD w-rowid      AS ROWID
    FIELD w-recid      AS RECID
    FIELD this-is-a-rm AS LOG
    FIELD isaset       AS LOG
    FIELD isacomponent AS LOG
    FIELD fg-i-no      LIKE job-hdr.i-no
    FIELD est-no       LIKE eb.est-no
    FIELD eqty         LIKE eb.eqty
    FIELD prep         AS LOG
    field estPrepEQty  AS DEC
    field estPrepLine  as int
    field miscType     as int
    field miscInd      as char
    FIELD fg-part-no   AS CHARACTER
    FIELD dropShip     AS character
    FIELD PurchaseType AS CHAR  /* purchase/prompt */
    FIELD ReasonType AS CHAR 
    FIELD SELECTED AS LOGICAL 
    FIELD poDate AS date
    FIELD DueDate AS date
    FIELD CostPerUom AS decimal
    FIELD CostSetup AS decimal
    FIELD CostUOM AS char
    FIELD CostTotal AS decimal 
    FIELD Canceled AS logical         
    .


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
