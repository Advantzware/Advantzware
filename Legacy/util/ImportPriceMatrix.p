
/*------------------------------------------------------------------------
    File        : ImportPriceMatrix.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Price Matrix	

    Author(s)   : SEWA
    Created     : Wed May 23 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportPriceMatrix
    FIELD Company       AS CHARACTER 
    FIELD Location      AS CHARACTER 
    FIELD EffectiveDate AS DATE      FORMAT "99/99/9999" INITIAL "01/01/0001" COLUMN-LABEL "Eff. Date" HELP "Defaults to Today"
    FIELD CustomerID    AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Customer" HELP "Optional - Must be valid - size:8"
    FIELD CustomerType  AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Type" HELP "Optional - Must be valid - Size:8"
    FIELD Category      AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Category" HELP "Required - Must be valid - Size:5"
    FIELD FGItemID      AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Item Code" HELP "Optional - Must be valid - Size:15"
    FIELD PriceBasis    AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Price Basis" HELP "Required - must be Price or Discount"
    FIELD Quantity1     AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty1" HELP "Optional - Integer"
    FIELD Price1        AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price1" HELP "Optional - Decimal"
    FIELD Discount1     AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc1" HELP "Optional - Decimal"
    FIELD UOM1          AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM1" HELP "Optional - must be Valid"
    FIELD Quantity2     AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty2" HELP "Optional - Integer"
    FIELD Price2        AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price2" HELP "Optional - Decimal"
    FIELD Discount2     AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc2" HELP "Optional - Decimal"
    FIELD UOM2          AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM2" HELP "Optional - must be Valid"
    FIELD Quantity3     AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty3" HELP "Optional - Integer"
    FIELD Price3        AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price3" HELP "Optional - Decimal"
    FIELD Discount3     AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc3" HELP "Optional - Decimal"
    FIELD UOM3          AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM3" HELP "Optional - must be Valid"
    FIELD Quantity4     AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty4" HELP "Optional - Integer"
    FIELD Price4        AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price4" HELP "Optional - Decimal"
    FIELD Discount4     AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc4" HELP "Optional - Decimal"
    FIELD UOM4          AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM4" HELP "Optional - must be Valid"
    FIELD Quantity5     AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty5" HELP "Optional - Integer"
    FIELD Price5        AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price5" HELP "Optional - Decimal"
    FIELD Discount5     AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc5" HELP "Optional - Decimal"
    FIELD UOM5          AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM5" HELP "Optional - must be Valid"
    FIELD Quantity6     AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty6" HELP "Optional - Integer"
    FIELD Price6        AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price6" HELP "Optional - Decimal"
    FIELD Discount6     AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc6" HELP "Optional - Decimal"
    FIELD UOM6          AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM6" HELP "Optional - must be Valid"
    FIELD Quantity7     AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty7" HELP "Optional - Integer"
    FIELD Price7        AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price7" HELP "Optional - Decimal"
    FIELD Discount7     AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc7" HELP "Optional - Decimal"
    FIELD UOM7          AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM7" HELP "Optional - must be Valid"
    FIELD Quantity8     AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty8" HELP "Optional - Integer"
    FIELD Price8        AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price8" HELP "Optional - Decimal"
    FIELD Discount8     AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc8" HELP "Optional - Decimal"
    FIELD UOM8          AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM8" HELP "Optional - must be Valid"
    FIELD Quantity9     AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty9" HELP "Optional - Integer"
    FIELD Price9        AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price9" HELP "Optional - Decimal"
    FIELD Discount9     AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc9" HELP "Optional - Decimal"
    FIELD UOM9          AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM9" HELP "Optional - must be Valid"
    FIELD Quantity10    AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty10" HELP "Optional - Integer"
    FIELD Price10       AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price10" HELP "Optional - Decimal"
    FIELD Discount10    AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc10" HELP "Optional - Decimal"
    FIELD UOM10         AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM10" HELP "Optional - must be Valid"
    FIELD ExpireDate      AS DATE      FORMAT "99/99/9999" INITIAL "12/31/2099" COLUMN-LABEL "Exp Date" HELP "validated - must be greater than eff date"
    FIELD ShipTo       AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "ShipTo" HELP "Optional - Size:8"
    FIELD cOnline    AS CHARACTER FORMAT "X(3)" COLUMN-LABEL "Online" HELP "Optional - Yes or N0"  .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

                                    
/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportPriceMatrix"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportPriceMatrix FOR ttImportPriceMatrix.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportPriceMatrix FOR ttImportPriceMatrix.
    DEFINE VARIABLE cUOMList AS CHARACTER INITIAL "M,EA,L,CS,C,LB,DRM,ROL,PKG,SET,DOZ,BDL" NO-UNDO.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportPriceMatrix.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company.".
    END.

    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportPriceMatrix.CustomerID NE "" THEN 
            RUN pIsValidCustomerID IN hdValidator (ipbf-ttImportPriceMatrix.CustomerID, NO, ipbf-ttImportPriceMatrix.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportPriceMatrix.CustomerType NE "" THEN 
            RUN pIsValidCustomerType IN hdValidator (ipbf-ttImportPriceMatrix.CustomerType, NO, ipbf-ttImportPriceMatrix.Company, OUTPUT oplValid, OUTPUT cValidNote).
        
        IF oplValid AND ipbf-ttImportPriceMatrix.Category NE "" THEN
            RUN pIsValidFGCategory IN hdValidator (ipbf-ttImportPriceMatrix.Category, NO, ipbf-ttImportPriceMatrix.Company, OUTPUT oplValid, OUTPUT cValidNote). 
        
        IF oplValid AND ipbf-ttImportPriceMatrix.FGItemID NE "" THEN 
            RUN pIsValidFGItemID IN hdValidator (ipbf-ttImportPriceMatrix.FGItemID, NO, ipbf-ttImportPriceMatrix.Company, OUTPUT oplValid, OUTPUT cValidNote).
      
        IF oplValid AND ipbf-ttImportPriceMatrix.UOM1 NE "" THEN 
        DO:
            IF ipbf-ttImportPriceMatrix.Quantity1 NE 0 THEN 
            DO:
                FIND FIRST uom NO-LOCK
                    WHERE uom.uom EQ ipbf-ttImportPriceMatrix.UOM1
                    AND lookup(uom.uom,cUOMList) NE 0 NO-ERROR.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM1".
            END.
        END.

        IF oplValid AND ipbf-ttImportPriceMatrix.UOM2 NE "" THEN 
        DO:
            IF ipbf-ttImportPriceMatrix.Quantity2 NE 0 THEN 
            DO:
                FIND FIRST uom NO-LOCK
                    WHERE uom.uom EQ ipbf-ttImportPriceMatrix.UOM2
                    AND lookup(uom.uom,cUOMList) NE 0 NO-ERROR.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM2".
            END.
        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.UOM3 NE "" THEN 
        DO:
            IF ipbf-ttImportPriceMatrix.Quantity3 NE 0 THEN 
            DO:
                FIND FIRST uom NO-LOCK
                    WHERE uom.uom EQ ipbf-ttImportPriceMatrix.UOM3
                    AND lookup(uom.uom,cUOMList) NE 0 NO-ERROR.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM3".
            END.
        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.UOM4 NE "" THEN 
        DO:
            IF ipbf-ttImportPriceMatrix.Quantity4 NE 0 THEN 
            DO:
                FIND FIRST uom NO-LOCK
                    WHERE uom.uom EQ ipbf-ttImportPriceMatrix.UOM4
                    AND lookup(uom.uom,cUOMList) NE 0 NO-ERROR.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM4".
            END.
        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.UOM5 NE "" THEN 
        DO:
            IF ipbf-ttImportPriceMatrix.Quantity5 NE 0 THEN 
            DO:
                FIND FIRST uom NO-LOCK
                    WHERE uom.uom EQ ipbf-ttImportPriceMatrix.UOM5
                    AND lookup(uom.uom,cUOMList) NE 0 NO-ERROR.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM5".
            END.
        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.UOM6 NE "" THEN 
        DO:
            IF ipbf-ttImportPriceMatrix.Quantity6 NE 0 THEN 
            DO:
                FIND FIRST uom NO-LOCK
                    WHERE uom.uom EQ ipbf-ttImportPriceMatrix.UOM6
                    AND lookup(uom.uom,cUOMList) NE 0 NO-ERROR.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM6".
            END.
        END.
        
        IF oplValid AND ipbf-ttImportPriceMatrix.UOM7 NE "" THEN 
        DO:
            IF ipbf-ttImportPriceMatrix.Quantity7 NE 0 THEN 
            DO:
                FIND FIRST uom NO-LOCK
                    WHERE uom.uom EQ ipbf-ttImportPriceMatrix.UOM7
                    AND lookup(uom.uom,cUOMList) NE 0 NO-ERROR.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM7".
            END.
        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.UOM8 NE "" THEN 
        DO:
            IF ipbf-ttImportPriceMatrix.Quantity8 NE 0 THEN 
            DO:
                FIND FIRST uom NO-LOCK
                    WHERE uom.uom EQ ipbf-ttImportPriceMatrix.UOM8
                    AND lookup(uom.uom,cUOMList) NE 0 NO-ERROR.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM8".
            END.
        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.UOM9 NE "" THEN 
        DO:
            IF ipbf-ttImportPriceMatrix.Quantity9 NE 0 THEN 
            DO:
                FIND FIRST uom NO-LOCK
                    WHERE uom.uom EQ ipbf-ttImportPriceMatrix.UOM9
                    AND lookup(uom.uom,cUOMList) NE 0 NO-ERROR.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM9".
            END.
        END.

        IF oplValid AND ipbf-ttImportPriceMatrix.UOM10 NE "" THEN 
        DO:
            IF ipbf-ttImportPriceMatrix.Quantity10 NE 0 THEN 
            DO:
                FIND FIRST uom NO-LOCK
                    WHERE uom.uom EQ ipbf-ttImportPriceMatrix.UOM10
                    AND lookup(uom.uom,cUOMList) NE 0 NO-ERROR.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM10".
            END.
        END.

        IF oplValid AND ipbf-ttImportPriceMatrix.ExpireDate NE ? THEN 
        DO:
            IF ipbf-ttImportPriceMatrix.ExpireDate LT ipbf-ttImportPriceMatrix.EffectiveDate THEN
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Expiration date should be greater than Effective Date.".
        END.

        IF oplValid AND ipbf-ttImportPriceMatrix.PriceBasis NE "" THEN 
            RUN pIsValidFromList IN hdValidator ("Price Basis", ipbf-ttImportPriceMatrix.PriceBasis, "Price,Discount", OUTPUT oplValid, OUTPUT cValidNote).
       
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportPriceMatrix FOR ttImportPriceMatrix.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE dtEffDate AS DATE NO-UNDO.
    
    dtEffDate = IF ipbf-ttImportPriceMatrix.EffectiveDate NE ? THEN ipbf-ttImportPriceMatrix.EffectiveDate ELSE TODAY.
    
    FIND FIRST oe-prmtx EXCLUSIVE-LOCK
        WHERE oe-prmtx.company EQ ipbf-ttImportPriceMatrix.Company
        AND oe-prmtx.cust-no EQ  ipbf-ttImportPriceMatrix.CustomerID
        AND oe-prmtx.custype EQ  ipbf-ttImportPriceMatrix.CustomerType
        AND oe-prmtx.procat  EQ  ipbf-ttImportPriceMatrix.Category
        AND oe-prmtx.i-no  EQ  ipbf-ttImportPriceMatrix.FGItemID 
        AND oe-prmtx.eff-date EQ dtEffDate
        NO-ERROR.
    IF NOT AVAILABLE oe-prmtx THEN 
    DO: 
        iopiAdded = iopiAdded + 1.
                
        CREATE oe-prmtx.
        ASSIGN 
            oe-prmtx.company  = ipbf-ttImportPriceMatrix.Company
            oe-prmtx.eff-date = dtEffDate
            oe-prmtx.cust-no  = ipbf-ttImportPriceMatrix.CustomerID 
            oe-prmtx.custype  = ipbf-ttImportPriceMatrix.CustomerType         
            oe-prmtx.procat   = ipbf-ttImportPriceMatrix.Category     
            oe-prmtx.i-no     = ipbf-ttImportPriceMatrix.FGItemID
            oe-prmtx.meth     = YES         
            .
    END. 
    RUN pAssignValueDate (ipbf-ttImportPriceMatrix.ExpireDate, YES, INPUT-OUTPUT oe-prmtx.exp-date).
    RUN pAssignValueCToL (ipbf-ttImportPriceMatrix.PriceBasis, "Price", YES, INPUT-OUTPUT oe-prmtx.meth).
    RUN pAssignValueC (ipbf-ttImportPriceMatrix.ShipTo, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.custShipID).  
    RUN pAssignValueI (ipbf-ttImportPriceMatrix.Quantity1, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.qty[1]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Price1, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.price[1]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Discount1, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.discount[1]).
    RUN pAssignValueC (ipbf-ttImportPriceMatrix.UOM1, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.uom[1]).  
    RUN pAssignValueI (ipbf-ttImportPriceMatrix.Quantity2, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.qty[2]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Price2, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.price[2]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Discount2, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.discount[2]).
    RUN pAssignValueC (ipbf-ttImportPriceMatrix.UOM2, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.uom[2]).  
    RUN pAssignValueI (ipbf-ttImportPriceMatrix.Quantity3, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.qty[3]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Price3, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.price[3]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Discount3, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.discount[3]).
    RUN pAssignValueC (ipbf-ttImportPriceMatrix.UOM3, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.uom[3]).  
    RUN pAssignValueI (ipbf-ttImportPriceMatrix.Quantity4, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.qty[4]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Price4, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.price[4]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Discount4, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.discount[4]).
    RUN pAssignValueC (ipbf-ttImportPriceMatrix.UOM4, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.uom[4]).  
    RUN pAssignValueI (ipbf-ttImportPriceMatrix.Quantity5, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.qty[5]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Price5, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.price[5]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Discount5, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.discount[5]).
    RUN pAssignValueC (ipbf-ttImportPriceMatrix.UOM5, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.uom[5]).  
    RUN pAssignValueI (ipbf-ttImportPriceMatrix.Quantity6, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.qty[6]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Price6, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.price[6]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Discount6, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.discount[6]).
    RUN pAssignValueC (ipbf-ttImportPriceMatrix.UOM6, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.uom[6]).  
    RUN pAssignValueI (ipbf-ttImportPriceMatrix.Quantity7, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.qty[7]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Price7, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.price[7]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Discount7, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.discount[7]).
    RUN pAssignValueC (ipbf-ttImportPriceMatrix.UOM7, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.uom[7]).  
    RUN pAssignValueI (ipbf-ttImportPriceMatrix.Quantity8, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.qty[8]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Price8, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.price[8]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Discount8, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.discount[8]).
    RUN pAssignValueC (ipbf-ttImportPriceMatrix.UOM8, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.uom[8]).
    RUN pAssignValueI (ipbf-ttImportPriceMatrix.Quantity9, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.qty[9]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Price9, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.price[9]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Discount9, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.discount[9]).
    RUN pAssignValueC (ipbf-ttImportPriceMatrix.UOM9, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.uom[9]).
    RUN pAssignValueI (ipbf-ttImportPriceMatrix.Quantity10, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.qty[10]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Price10, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.price[10]).
    RUN pAssignValueD (ipbf-ttImportPriceMatrix.Discount10, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.discount[10]).
    RUN pAssignValueC (ipbf-ttImportPriceMatrix.UOM10, iplIgnoreBlanks, INPUT-OUTPUT oe-prmtx.uom[10]).
    RUN pAssignValueC (ipbf-ttImportPriceMatrix.cOnline, YES, INPUT-OUTPUT oe-prmtx.online).

    
END PROCEDURE.


/* ************************  Function Implementations ***************** */

