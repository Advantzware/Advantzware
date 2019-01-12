/*------------------------------------------------------------------------
    File        : FGItemIDProcs.p
    Purpose     : Persistent procedure for handling all functions related to 
                NK1 FGItem#

    Syntax      :

    Description : Returns the Next Automated FG Item ID (i-no) based on the setting for FGITEM# NK1
                  Replaces:
                      fg/autofg.p
                      fg/hughesfg.p
                      fg/fibre-fg.p                      
                        

    Author(s)   : BV
    Created     : Tue Jul 17 20:57:42 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
                                        

DEFINE TEMP-TABLE ttFGItemIDComponent
    FIELD Sequence        AS INTEGER 
    FIELD ValueOriginal   AS CHARACTER 
    FIELD ValueToUse      AS CHARACTER 
    FIELD ValueType       AS CHARACTER 
    FIELD ValueSize       AS INTEGER
    FIELD IsUniqueElement AS LOGICAL
    FIELD IsCounter       AS LOGICAL 
    FIELD IsSetCounter    AS LOGICAL 
    .

DEFINE VARIABLE gcCharInd AS CHARACTER NO-UNDO INIT "@".
DEFINE VARIABLE gcCharProdCat AS CHARACTER NO-UNDO INIT "%".
DEFINE VARIABLE gcCharCustID AS CHARACTER NO-UNDO INIT "$".
DEFINE VARIABLE gcCharSeq AS CHARACTER NO-UNDO INIT "#".
DEFINE VARIABLE gcCharSeqSet AS CHARACTER NO-UNDO INIT "&".
DEFINE VARIABLE gcCharSeqSetCond AS CHARACTER NO-UNDO INIT ">".

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fApplySize RETURNS CHARACTER PRIVATE
    (ipcValue AS CHARACTER,
    ipiSize AS INTEGER,
    ipcFillChar AS CHARACTER) FORWARD.

FUNCTION fGetFGItemFormat RETURNS CHARACTER 
    (ipcCompany AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildFGItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER lUniqueIDOnly AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcFGItemID AS CHARACTER NO-UNDO.
    
    IF CAN-FIND(FIRST ttFGItemIDComponent) THEN iopcFGItemID = "".
    FOR EACH ttFGItemIDComponent
        WHERE ttFGItemIDComponent.IsUniqueElement OR NOT lUniqueIDOnly
        BY ttFGItemIDComponent.Sequence:
        
        iopcFGItemID = iopcFGItemID + ttFGItemIDComponent.ValueToUse.
    END.

END PROCEDURE.

PROCEDURE pBuildIDComponents PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes the component temp-table to build
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
       
    DEFINE VARIABLE lIsUniqueElement AS LOGICAL.
    DEFINE VARIABLE cProductCategory AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerID      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIndustry        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iSetCounter      AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-setheader-eb FOR eb.
    DEFINE BUFFER bf-eb           FOR eb.
    DEFINE BUFFER bf-est          FOR est.
    
    FIND FIRST bf-est NO-LOCK 
        WHERE bf-est.company EQ ipbf-eb.company
        AND bf-est.est-no EQ ipbf-eb.est-no
        NO-ERROR.
    IF NOT AVAILABLE bf-est THEN LEAVE.
    
    IF bf-est.est-type EQ 2 OR bf-est.est-type EQ 6 THEN 
    DO: /*Get set header buffer*/
        /*Find the set header*/
        FIND FIRST bf-setheader-eb NO-LOCK 
            WHERE bf-setheader-eb.company EQ ipbf-eb.company
            AND bf-setheader-eb.est-no EQ ipbf-eb.est-no
            AND bf-setheader-eb.form-no EQ 0
            NO-ERROR.
        /*Get Set Counter for a set*/
        RUN pGetSetCounter (BUFFER ipbf-eb, BUFFER bf-est, OUTPUT iSetCounter).
    END.
    ELSE iSetCounter = 99.
    
    FIND FIRST bf-eb NO-LOCK OF bf-est
        WHERE bf-eb.form-no NE 0.
         
    /*Get Product Category*/        
    IF cProductCategory EQ "" AND AVAILABLE bf-setheader-eb THEN cProductCategory = bf-setheader-eb.procat.  /*If Set header available use it*/
    IF cProductCategory EQ "" THEN cProductCategory = ipbf-eb.procat. /*Fall back to blank*/
    
    /*Get Customer ID*/
    IF cCustomerID EQ "" THEN cCustomerID = bf-eb.cust-no. /*Use customer from first non-set header blank*/  
    
    /*Get Industry*/
    cIndustry =  IF bf-est.est-type LT 5 THEN "F" ELSE "C". /*Use Main estimate type to determine industry*/
    
    
    FOR EACH ttFGItemIDComponent
        BY ttFGItemIDComponent.Sequence:
        CASE ttFGItemIDComponent.ValueType:
            WHEN gcCharProdCat THEN 
                ASSIGN  
                    ttFGItemIDComponent.ValueOriginal = cProductCategory
                    ttFGItemIDComponent.ValueToUse    = fApplySize(ttFGItemIDComponent.ValueOriginal,ttFGItemIDComponent.ValueSize,"0")
                    .
            WHEN gcCharInd THEN 
                ASSIGN  
                    ttFGItemIDComponent.ValueOriginal = cIndustry
                    ttFGItemIDComponent.ValueToUse    = fApplySize(ttFGItemIDComponent.ValueOriginal,ttFGItemIDComponent.ValueSize,"0")
                    .
            WHEN gcCharCustID THEN 
                ASSIGN  
                    ttFGItemIDComponent.ValueOriginal = cCustomerID
                    ttFGItemIDComponent.ValueToUse    = fApplySize(ttFGItemIDComponent.ValueOriginal,ttFGItemIDComponent.ValueSize,"0")
                    .
            WHEN gcCharSeq THEN 
                ASSIGN 
                    ttFGItemIDComponent.IsCounter = YES
                    .
            WHEN gcCharSeqSet THEN 
                ASSIGN 
                    ttFGItemIDComponent.IsSetCounter = YES
                    ttFGItemIDComponent.ValueOriginal = STRING(iSetCounter)
                    ttFGItemIDComponent.ValueToUse    = fApplySize(ttFGItemIDComponent.ValueOriginal,ttFGItemIDComponent.ValueSize,"0")
                    .
            WHEN gcCharSeqSetCond THEN 
                ASSIGN
                    ttFGItemIDComponent.IsSetCounter = YES 
                    ttFGItemIDComponent.ValueOriginal = IF iSetCounter NE 99 THEN STRING(iSetCounter) ELSE ""
                    ttFGItemIDComponent.ValueToUse    = ttFGItemIDComponent.ValueOriginal
                    .
            OTHERWISE 
            ASSIGN 
                ttFGItemIDComponent.ValueOriginal = ttFGItemIDComponent.ValueType
                ttFGItemIDComponent.ValueToUse    = fApplySize(ttFGItemIDComponent.ValueOriginal,ttFGItemIDComponent.ValueSize,ttFGItemIDComponent.ValueType)
                .
        END CASE .
    END.
     
END PROCEDURE.

PROCEDURE pGetFGItemID:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipriEb AS   ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipcFGItemIDSetHeader AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcFGItemID  AS CHARACTER NO-UNDO.
DEFINE BUFFER bf-eb FOR eb.

DEFINE VARIABLE cFGItemFormat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUniqueID     AS CHARACTER NO-UNDO.

FIND FIRST bf-eb NO-LOCK 
    WHERE ROWID(bf-eb) EQ ipriEb
    NO-ERROR.
IF NOT AVAILABLE bf-eb THEN RETURN.  /*Can't process without valid subject record*/
/*Set default fall back to passed in partID*/

IF cFGItemFormat EQ "" THEN 
    cFGItemFormat = fGetFGItemFormat (bf-eb.company).

IF cFGItemFormat NE "Manual" THEN
    opcFGItemID = bf-eb.part-no.  /* Ticket - 39141 */

CASE cFGItemFormat:
    WHEN "Manual" OR 
    WHEN "" OR 
    WHEN "None" THEN 
        RETURN.
    WHEN "Hughes" THEN 
        cFGItemFormat = "%$$$$$$####A&&".
    WHEN "Fibre"  THEN 
        cFGItemFormat = "@$$$$$$$$####A>".
END CASE.

/*This builds the temp-table for building the FG Item ID based on the format mask*/ 
RUN pParseFormat (cFGItemFormat).  

/*Fill up the temp-table with formatted values - includes set counter calc but not main sequence*/
RUN pBuildIDComponents (BUFFER bf-eb).

/*Get only the unique ID portion of the FG Item*/
RUN pBuildFGItem(YES, INPUT-OUTPUT cUniqueID). 

/*Get the next sequence# based on portion of FGItem considered the Unique ID and assign to IsSequence record of tt*/
RUN pGetNextSequence(bf-eb.company, cUniqueID,ipcFGItemIDSetHeader).

/*Combine the FG Item elements to build final value*/
RUN pBuildFGItem(NO, INPUT-OUTPUT opcFGItemID).


END PROCEDURE.

PROCEDURE pGetNextSequence PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Determines the next available sequence for FG Item based on Unique ID passed in.
               Updates the "#" type (or sequence ID) in the temptable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipcUniqueIDToSearch AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcSetHeaderID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iNextSequence AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNextSequenceOrd AS INTEGER NO-UNDO.
    DEFINE VARIABLE iUniqueIDSize AS INTEGER NO-UNDO.
    DEFINE VARIABLE iSequenceSize AS INTEGER NO-UNDO.

    FIND FIRST ttFGItemIDComponent
        WHERE ttFGItemIDComponent.IsCounter
        NO-ERROR.
    IF AVAILABLE ttFGItemIDComponent THEN 
    DO:
        ASSIGN 
            iUniqueIDSize = LENGTH(ipcUniqueIDToSearch) + 1
            iSequenceSize = ttFGItemIDComponent.ValueSize
            .
        IF ipcSetHeaderID EQ "" THEN DO:
            /*Find the next available sequence based on the unique ID string*/
            FOR EACH itemfg NO-LOCK 
                WHERE itemfg.company EQ ipcCompany
                AND itemfg.i-no BEGINS ipcUniqueIDToSearch
                BY itemfg.i-no DESCENDING:
                iNextSequence = INT(SUBSTRING(itemfg.i-no,iUniqueIDSize,iSequenceSize)) NO-ERROR. 
                IF ERROR-STATUS:ERROR THEN 
                    iNextSequence = 0.
                ELSE 
                    LEAVE.
            END.
            FOR EACH oe-ordl NO-LOCK 
                WHERE oe-ordl.company EQ ipcCompany
                AND oe-ordl.i-no BEGINS ipcUniqueIDToSearch
                AND oe-ordl.opened
                BY oe-ordl.i-no DESCENDING:
                iNextSequenceOrd = INT(SUBSTRING(oe-ordl.i-no,iUniqueIDSize,iSequenceSize)) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                    iNextSequenceOrd = 0.
                ELSE 
                    LEAVE.   
            END. 
            ASSIGN 
                iNextSequence = MAX(iNextSequence,iNextSequenceOrd)
                iNextSequence = iNextSequence + 1 /* increment */
                ttFGItemIDComponent.ValueOriginal = STRING(iNextSequence).
        END.
        ELSE /*Set Header already determined the sequence so use it*/
            ttFGItemIDComponent.ValueOriginal = SUBSTRING(ipcSetHeaderID,iUniqueIDSize,iSequenceSize).

        ttFGItemIDComponent.ValueToUse    = fApplySize(ttFGItemIDComponent.ValueOriginal,iSequenceSize,"0").
        
    END.
END PROCEDURE.

PROCEDURE pGetSetCounter PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a blank that is part of a set, return the set component counter
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb  FOR eb.
    DEFINE PARAMETER BUFFER ipbf-est FOR est.
    DEFINE OUTPUT PARAMETER opiCounter AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-eb FOR eb.

    IF ipbf-eb.form-no EQ 0 THEN 
        opiCounter = 0.  /*Set header*/
    ELSE 
    DO:

        IF AVAILABLE ipbf-est THEN 
        DO:
            /*Query sorted by form, blank, so 1-1 = 1, 1-2 = 2, 2-1 = 3, etc*/
            FOR EACH bf-eb OF ipbf-est
                WHERE bf-eb.form-no NE 0
                AND bf-eb.blank-no NE 0
                NO-LOCK
                BY bf-eb.form-no BY bf-eb.blank-no:
                opiCounter = opiCounter + 1.
                IF ROWID(bf-eb) EQ ROWID(ipbf-eb) THEN LEAVE.
            END.
        END.
    END.
END PROCEDURE.

PROCEDURE pParseFormat PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a format mask, generate the temp-table to build the elements
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFormatMask AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iCharPos  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cChar     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCharLast AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iSequence AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lIsUniqueElement AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttFGItemIDComponent.

    ASSIGN 
        iSequence = 0
        lIsUniqueElement = YES.
    DO iCharPos = 1 TO LENGTH(ipcFormatMask):
        cChar = SUBSTRING(ipcFormatMask,iCharPos,1).
        IF cChar NE cCharLast THEN 
        DO:
            CREATE ttFGItemIDComponent.
            ASSIGN 
                iSequence                     = iSequence + 1
                ttFGItemIDComponent.Sequence  = iSequence
                ttFGitemIDComponent.ValueType = cChar
                cCharLast                     = cChar
                .
            IF ttFGitemIDComponent.ValueType EQ gcCharSeq THEN DO: 
                /*as soon as it hits first counter/sequence, set the rest of the fields to not uniqueIDElement*/
                ttFGItemIDComponent.IsCounter = YES.
                lIsUniqueElement = NO.
            END.
            ttFGItemIDComponent.IsUniqueElement = lIsUniqueElement.
        END.
        ttFGItemIDComponent.ValueSize = ttFGItemIDComponent.ValueSize + 1.
    END. 


END PROCEDURE.

PROCEDURE pValidateFormatMask:
/*------------------------------------------------------------------------------
 Purpose:  Accepts format mask string and returns yes/no for valid and 
 a message if the format is not valid
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcFormatMask AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

DEFINE VARIABLE iLengthLimit AS INTEGER NO-UNDO INIT 15.
oplValid = YES.
IF LOOKUP(ipcFormatMask, "Manual,None") GT 0 OR ipcFormatMask EQ "" THEN RETURN.
IF LENGTH(ipcFormatMask) GT iLengthLimit THEN DO:
    ASSIGN
        oplValid = NO
        opcMessage = "Format Mask must not be larger than " + STRING(iLengthLimit) + " characters"
        .
    RETURN.
END.
RUN pParseFormat(ipcFormatMask).
RUN pValidateParsedFormat(OUTPUT oplValid, OUTPUT opcMessage).

END PROCEDURE.

PROCEDURE pValidateParsedFormat PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Given temp-table, run validation rules and return validity logical with message if invalid
 Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

DEFINE VARIABLE iSequenceMin AS INTEGER NO-UNDO INIT 4.
DEFINE VARIABLE iSequenceMax AS INTEGER NO-UNDO INIT 7.
DEFINE VARIABLE iUniqueMin AS INTEGER NO-UNDO INIT 5.

DEFINE VARIABLE iUniqueCount AS INTEGER NO-UNDO.
DEFINE VARIABLE lLastElementFound AS LOGICAL NO-UNDO.

oplValid = YES.

FOR EACH ttFGItemIDComponent NO-LOCK
    BY ttFGItemIDComponent.Sequence DESCENDING:
    IF NOT lLastElementFound THEN DO:
        lLastElementFound = YES.
        IF NOT (ttFGItemIDComponent.ValueType EQ gcCharSeqSet OR ttFGItemIDComponent.ValueType EQ gcCharSeqSetCond) THEN DO:
            ASSIGN 
                oplValid = NO 
                opcMessage = "End of format must be set counter - either " + gcCharSeqSet + " or " + gcCharSeqSetCond.
                .
            RETURN.
        END.    
    END.    
    IF ttFGItemIDComponent.IsCounter THEN DO:
        IF ttFGItemIDComponent.ValueSize LT iSequenceMin THEN DO: 
            ASSIGN 
                oplValid = NO 
                opcMessage = "Sequence characters - " + gcCharSeq + " - must be greater than or equal to " + STRING(iSequenceMin)
                .
            RETURN.
        END.
        IF ttFGItemIDComponent.ValueSize GT iSequenceMax THEN DO: 
            ASSIGN 
                oplValid = NO 
                opcMessage = "Sequence characters - " + gcCharSeq + " - must be less than or equal to " + STRING(iSequenceMax)
                .
            RETURN.
        END.
    END.
    IF ttFGItemIDComponent.IsUniqueElement THEN 
        iUniqueCount = iUniqueCount + ttFGItemIDComponent.ValueSize.
END.
IF iUniqueCount LT iUniqueMin THEN DO:
    ASSIGN 
        oplValid = NO 
        opcMessage = "Unique ID component - everything to left of " + gcCharSeq + " - must be greater than or equal to " + STRING(iUniqueMin)
        .
    RETURN.
END.

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fApplySize RETURNS CHARACTER PRIVATE
    (ipcValue AS CHARACTER , ipiSize AS INTEGER, ipcFillChar AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Returns a character string of given size, fills with 0 if smaller
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iFill   AS INTEGER   NO-UNDO.

    ipcValue = TRIM(ipcValue).  /*Remove spaces*/
    IF LENGTH(ipcValue) LT ipiSize THEN 
    DO:  /*Add leading 0s to any string smaller than required size*/
        iFill = ipiSize - LENGTH(ipcValue).
        cReturn = FILL(ipcFillChar, iFill) + ipcValue.
    END.
    ELSE 
        cReturn = SUBSTRING(ipcValue, 1, ipiSize).

    cReturn = CAPS(cReturn).  /*Capitalize output*/
    
    RETURN cReturn.
		
END FUNCTION.


FUNCTION fGetFGItemFormat RETURNS CHARACTER 
    ( ipcCompany AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose: Returns character value of NK1 FGItem#
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.

    RUN sys\ref\nk1look.p (ipcCompany,
        "FGITEM#",
        "C",
        NO,
        NO,
        "",
        "", 
        OUTPUT cReturn,
        OUTPUT lFound).

    RETURN cReturn.
		
END FUNCTION.
