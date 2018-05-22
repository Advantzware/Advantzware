
/*------------------------------------------------------------------------
    File        : ImportShipTo.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Ship Tos	

    Author(s)   : BV
    Created     : Fri Nov 24 16:18:38 EST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportPriceMatrix
    FIELD Company     AS CHARACTER 
    FIELD Location    AS CHARACTER 
    FIELD dEffdate    AS DATE      FORMAT "99/99/9999" INITIAL "01/01/0001" COLUMN-LABEL "Eff. Date" HELP "Defaults to Today"
    FIELD cCustomer   AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Customer" HELP "Optional - Must be valid - size:8"
    FIELD cType       AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Type" HELP "Optional - Must be valid - Size:8"
    FIELD cCategory   AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Category" HELP "Required - Must be valid - Size:5"
    FIELD cItem       AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "Item Code" HELP "Optional - Must be valid - Size:15"
    FIELD cPriceBasis AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Price Basis" HELP "Required - must be Price/Discount"
    FIELD iQty1       AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty1" HELP "Optional - Integer"
    FIELD dPrice1     AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price1" HELP "Optional - Decimal"
    FIELD dDiscount1  AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc1" HELP "Optional - Decimal"
    FIELD cUOM1       AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM1" HELP "Optional - must be Valid"
    FIELD iQty2       AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty2" HELP "Optional - Integer"
    FIELD dPrice2     AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price2" HELP "Optional - Decimal"
    FIELD dDiscount2  AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc2" HELP "Optional - Decimal"
    FIELD cUOM2       AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM2" HELP "Optional - must be Valid"
    FIELD iQty3       AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty3" HELP "Optional - Integer"
    FIELD dPrice3     AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price3" HELP "Optional - Decimal"
    FIELD dDiscount3  AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc3" HELP "Optional - Decimal"
    FIELD cUOM3       AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM3" HELP "Optional - must be Valid"
    FIELD iQty4       AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty4" HELP "Optional - Integer"
    FIELD dPrice4     AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price4" HELP "Optional - Decimal"
    FIELD dDiscount4  AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc4" HELP "Optional - Decimal"
    FIELD cUOM4       AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM4" HELP "Optional - must be Valid"
    FIELD iQty5       AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty5" HELP "Optional - Integer"
    FIELD dPrice5     AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price5" HELP "Optional - Decimal"
    FIELD dDiscount5  AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc5" HELP "Optional - Decimal"
    FIELD cUOM5       AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM5" HELP "Optional - must be Valid"
    FIELD iQty6       AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty6" HELP "Optional - Integer"
    FIELD dPrice6     AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price6" HELP "Optional - Decimal"
    FIELD dDiscount6  AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc6" HELP "Optional - Decimal"
    FIELD cUOM6       AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM6" HELP "Optional - must be Valid"
    FIELD iQty7       AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty7" HELP "Optional - Integer"
    FIELD dPrice7     AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price7" HELP "Optional - Decimal"
    FIELD dDiscount7  AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc7" HELP "Optional - Decimal"
    FIELD cUOM7       AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM7" HELP "Optional - must be Valid"
    FIELD iQty8       AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty8" HELP "Optional - Integer"
    FIELD dPrice8     AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price8" HELP "Optional - Decimal"
    FIELD dDiscount8  AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc8" HELP "Optional - Decimal"
    FIELD cUOM8       AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM8" HELP "Optional - must be Valid"
    FIELD iQty9       AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty9" HELP "Optional - Integer"
    FIELD dPrice9     AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price9" HELP "Optional - Decimal"
    FIELD dDiscount9  AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc9" HELP "Optional - Decimal"
    FIELD cUOM9       AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM9" HELP "Optional - must be Valid"
    FIELD iQty10      AS INTEGER   FORMAT ">>,>>>,>>9" COLUMN-LABEL "Qty10" HELP "Optional - Integer"
    FIELD dPrice10    AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" COLUMN-LABEL "Price10" HELP "Optional - Decimal"
    FIELD dDiscount10 AS DECIMAL   FORMAT "->>>9.99<<<" COLUMN-LABEL "Dsc10" HELP "Optional - Decimal"
    FIELD cUOM10      AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "UOM10" HELP "Optional - must be Valid"
    FIELD dExpdate    AS DATE      FORMAT "99/99/9999" INITIAL "12/31/2099" COLUMN-LABEL "Exp Date" HELP "validated - must be greater than eff date"
    FIELD cShipTo     AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "ShipTo" HELP "Optional - Size:8".

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
    DEFINE VARIABLE cUomlist AS CHARACTER INITIAL "M,EA,L,CS,C,LB,DRM,ROL,PKG,SET,DOZ,BDL" NO-UNDO.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportPriceMatrix.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportPriceMatrix.cCategory EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Category".
    END.
    

    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportPriceMatrix.cCustomer NE "" THEN 
        DO:
            FIND FIRST cust  NO-LOCK
                WHERE cust.company EQ ipbf-ttImportPriceMatrix.Company
                AND cust.cust-no EQ ipbf-ttImportPriceMatrix.cCustomer  NO-ERROR.
            IF NOT AVAILABLE(cust) THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid Customer".

        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.cType NE "" THEN 
        DO:
           FIND FIRST cust NO-LOCK 
               WHERE cust.company EQ ipbf-ttImportPriceMatrix.Company
               AND cust.cust-no EQ ipbf-ttImportPriceMatrix.cCustomer
               AND cust.TYPE EQ ipbf-ttImportPriceMatrix.cType NO-ERROR.
            IF NOT AVAILABLE(cust) THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Type must be from custmer.".

        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.cCategory NE "" THEN 
        DO:
            FIND FIRST fgcat NO-LOCK 
                WHERE fgcat.company EQ ipbf-ttImportPriceMatrix.Company
                AND fgcat.procat EQ ipbf-ttImportPriceMatrix.cCategory NO-ERROR.
            IF NOT AVAILABLE(fgcat) THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid Category".

        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.cItem NE "" THEN 
        DO:
            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ ipbf-ttImportPriceMatrix.Company
                AND itemfg.i-no EQ ipbf-ttImportPriceMatrix.cItem NO-ERROR.
            IF NOT AVAILABLE(itemfg) THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid Item Code".

        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.cUOM1 NE "" THEN 
        DO:
            if ipbf-ttImportPriceMatrix.iQty1 ne 0 then 
            do:
                find first uom no-lock
                    where uom.uom eq ipbf-ttImportPriceMatrix.cUOM1
                    and lookup(uom.uom,cUomlist) ne 0 no-error.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM1".
            END.
        END.

        IF oplValid AND ipbf-ttImportPriceMatrix.cUOM2 NE "" THEN 
        DO:
            if ipbf-ttImportPriceMatrix.iQty2 ne 0 then 
            do:
                find first uom no-lock
                    where uom.uom eq ipbf-ttImportPriceMatrix.cUOM2
                    and lookup(uom.uom,cUomlist) ne 0 no-error.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM2".
            END.
        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.cUOM3 NE "" THEN 
        DO:
            if ipbf-ttImportPriceMatrix.iQty3 ne 0 then 
            do:
                find first uom no-lock
                    where uom.uom eq ipbf-ttImportPriceMatrix.cUOM3
                    and lookup(uom.uom,cUomlist) ne 0 no-error.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM3".
            END.
        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.cUOM4 NE "" THEN 
        DO:
            if ipbf-ttImportPriceMatrix.iQty4 ne 0 then 
            do:
                find first uom no-lock
                    where uom.uom eq ipbf-ttImportPriceMatrix.cUOM4
                    and lookup(uom.uom,cUomlist) ne 0 no-error.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM4".
            END.
        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.cUOM5 NE "" THEN 
        DO:
            if ipbf-ttImportPriceMatrix.iQty5 ne 0 then 
            do:
                find first uom no-lock
                    where uom.uom eq ipbf-ttImportPriceMatrix.cUOM5
                    and lookup(uom.uom,cUomlist) ne 0 no-error.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM5".
            END.
        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.cUOM6 NE "" THEN 
        DO:
            if ipbf-ttImportPriceMatrix.iQty6 ne 0 then 
            do:
                find first uom no-lock
                    where uom.uom eq ipbf-ttImportPriceMatrix.cUOM6
                    and lookup(uom.uom,cUomlist) ne 0 no-error.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM6".
            END.
        END.
        
        IF oplValid AND ipbf-ttImportPriceMatrix.cUOM7 NE "" THEN 
        DO:
            if ipbf-ttImportPriceMatrix.iQty7 ne 0 then 
            do:
                find first uom no-lock
                    where uom.uom eq ipbf-ttImportPriceMatrix.cUOM7
                    and lookup(uom.uom,cUomlist) ne 0 no-error.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM7".
            END.
        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.cUOM8 NE "" THEN 
        DO:
            if ipbf-ttImportPriceMatrix.iQty8 ne 0 then 
            do:
                find first uom no-lock
                    where uom.uom eq ipbf-ttImportPriceMatrix.cUOM8
                    and lookup(uom.uom,cUomlist) ne 0 no-error.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM8".
            END.
        END.
        IF oplValid AND ipbf-ttImportPriceMatrix.cUOM9 NE "" THEN 
        DO:
            if ipbf-ttImportPriceMatrix.iQty9 ne 0 then 
            do:
                find first uom no-lock
                    where uom.uom eq ipbf-ttImportPriceMatrix.cUOM9
                    and lookup(uom.uom,cUomlist) ne 0 no-error.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM9".
            END.
        END.

        IF oplValid AND ipbf-ttImportPriceMatrix.cUOM10 NE "" THEN 
        DO:
            if ipbf-ttImportPriceMatrix.iQty10 ne 0 then 
            do:
                find first uom no-lock
                    where uom.uom eq ipbf-ttImportPriceMatrix.cUOM10
                    and lookup(uom.uom,cUomlist) ne 0 no-error.
                IF NOT AVAILABLE uom THEN
                    ASSIGN 
                        oplValid = NO 
                        opcNote  = "Invalid UOM10".
            END.
        END.

        IF oplValid AND ipbf-ttImportPriceMatrix.dExpdate NE ? THEN 
        DO:
            IF ipbf-ttImportPriceMatrix.dExpdate LT ipbf-ttImportPriceMatrix.dEffdate THEN
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Expiration date should be greater than Effective Date.".
        END.

        IF oplValid AND ipbf-ttImportPriceMatrix.cPriceBasis NE "" THEN 
        DO:
            IF LOOKUP(ipbf-ttImportPriceMatrix.cPriceBasis,"Price,Discount") LE 0 THEN
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Invalid Price Basis".
        END. 
       
    END.
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportPriceMatrix FOR ttImportPriceMatrix.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE dtEffDate AS DATE NO-UNDO.
    
    dtEffDate = IF ipbf-ttImportPriceMatrix.dEffdate NE ? THEN ipbf-ttImportPriceMatrix.dEffdate ELSE TODAY.
    
    FIND FIRST oe-prmtx EXCLUSIVE-LOCK
        WHERE oe-prmtx.company EQ ipbf-ttImportPriceMatrix.Company
        AND oe-prmtx.cust-no EQ  ipbf-ttImportPriceMatrix.cCustomer
        AND oe-prmtx.custype EQ  ipbf-ttImportPriceMatrix.cType
        AND oe-prmtx.procat  EQ  ipbf-ttImportPriceMatrix.cCategory
        AND oe-prmtx.i-no  EQ  ipbf-ttImportPriceMatrix.cItem 
        AND oe-prmtx.eff-date EQ dtEffDate
        NO-ERROR.
    IF NOT AVAILABLE oe-prmtx THEN 
    DO: 
        iopiAdded = iopiAdded + 1.
                
        CREATE oe-prmtx.
        ASSIGN 
            oe-prmtx.company  = ipbf-ttImportPriceMatrix.Company
            oe-prmtx.eff-date = dtEffDate
            oe-prmtx.cust-no  = ipbf-ttImportPriceMatrix.cCustomer 
            oe-prmtx.custype  = ipbf-ttImportPriceMatrix.cType         
            oe-prmtx.procat   = ipbf-ttImportPriceMatrix.cCategory     
            oe-prmtx.i-no     = ipbf-ttImportPriceMatrix.cItem     
                
            .
    END. 
       
    ASSIGN
        oe-prmtx.eff-date     = dtEffDate
        oe-prmtx.exp-date     = ipbf-ttImportPriceMatrix.dExpdate
        oe-prmtx.cust-no      = ipbf-ttImportPriceMatrix.cCustomer     
        oe-prmtx.custype      = ipbf-ttImportPriceMatrix.cType         
        oe-prmtx.procat       = ipbf-ttImportPriceMatrix.cCategory     
        oe-prmtx.i-no         = SUBSTRING(ipbf-ttImportPriceMatrix.cItem,01,15)        
        oe-prmtx.meth         = IF ipbf-ttImportPriceMatrix.cPriceBasis EQ "price" THEN YES ELSE NO   
        oe-prmtx.custShipID   = ipbf-ttImportPriceMatrix.cShipTo  
        oe-prmtx.qty[1]       = ipbf-ttImportPriceMatrix.iQty1                  
        oe-prmtx.price[1]     = ipbf-ttImportPriceMatrix.dPrice1    
        oe-prmtx.discount[1]  = ipbf-ttImportPriceMatrix.dDiscount1 
        oe-prmtx.uom[1]       = ipbf-ttImportPriceMatrix.cUOM1      
        oe-prmtx.qty[2]       = ipbf-ttImportPriceMatrix.iQty2     
        oe-prmtx.price[2]     = ipbf-ttImportPriceMatrix.dPrice2   
        oe-prmtx.discount[2]  = ipbf-ttImportPriceMatrix.dDiscount2
        oe-prmtx.uom[2]       = ipbf-ttImportPriceMatrix.cUOM2     
        oe-prmtx.qty[3]       = ipbf-ttImportPriceMatrix.iQty3     
        oe-prmtx.price[3]     = ipbf-ttImportPriceMatrix.dPrice3  
        oe-prmtx.discount[3]  = ipbf-ttImportPriceMatrix.dDiscount3
        oe-prmtx.uom[3]       = ipbf-ttImportPriceMatrix.cUOM3     
        oe-prmtx.qty[4]       = ipbf-ttImportPriceMatrix.iQty4       
        oe-prmtx.price[4]     = ipbf-ttImportPriceMatrix.dPrice4     
        oe-prmtx.discount[4]  = ipbf-ttImportPriceMatrix.dDiscount4  
        oe-prmtx.uom[4]       = ipbf-ttImportPriceMatrix.cUOM4       
        oe-prmtx.qty[5]       = ipbf-ttImportPriceMatrix.iQty5     
        oe-prmtx.price[5]     = ipbf-ttImportPriceMatrix.dPrice5   
        oe-prmtx.discount[5]  = ipbf-ttImportPriceMatrix.dDiscount5
        oe-prmtx.uom[5]       = ipbf-ttImportPriceMatrix.cUOM5     
        oe-prmtx.qty[6]       = ipbf-ttImportPriceMatrix.iQty6      
        oe-prmtx.price[6]     = ipbf-ttImportPriceMatrix.dPrice6    
        oe-prmtx.discount[6]  = ipbf-ttImportPriceMatrix.dDiscount6 
        oe-prmtx.uom[6]       = ipbf-ttImportPriceMatrix.cUOM6      
        oe-prmtx.qty[7]       = ipbf-ttImportPriceMatrix.iQty7      
        oe-prmtx.price[7]     = ipbf-ttImportPriceMatrix.dPrice7    
        oe-prmtx.discount[7]  = ipbf-ttImportPriceMatrix.dDiscount7 
        oe-prmtx.uom[7]       = ipbf-ttImportPriceMatrix.cUOM7      
        oe-prmtx.qty[8]       = ipbf-ttImportPriceMatrix.iQty8     
        oe-prmtx.price[8]     = ipbf-ttImportPriceMatrix.dPrice8   
        oe-prmtx.discount[8]  = ipbf-ttImportPriceMatrix.dDiscount8
        oe-prmtx.uom[8]       = ipbf-ttImportPriceMatrix.cUOM8     
        oe-prmtx.qty[9]       = ipbf-ttImportPriceMatrix.iQty9     
        oe-prmtx.price[9]     = ipbf-ttImportPriceMatrix.dPrice9   
        oe-prmtx.discount[9]  = ipbf-ttImportPriceMatrix.dDiscount9
        oe-prmtx.uom[9]       = ipbf-ttImportPriceMatrix.cUOM9     
        oe-prmtx.qty[10]      = ipbf-ttImportPriceMatrix.iQty10      
        oe-prmtx.price[10]    = ipbf-ttImportPriceMatrix.dPrice10    
        oe-prmtx.discount[10] = ipbf-ttImportPriceMatrix.dDiscount10 
        oe-prmtx.uom[10]      = ipbf-ttImportPriceMatrix.cUOM10    
                
        .
    
END PROCEDURE.


/* ************************  Function Implementations ***************** */

