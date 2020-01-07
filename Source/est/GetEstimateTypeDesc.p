
/*------------------------------------------------------------------------
    File        : GetEstimateTypeDesc.p
    Purpose     : 

    Syntax      :

    Description : Will return estimate type description

    Author(s)   : Sewa Singh 
    Created     : Tue Jan 7 12:37:10 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiEstType AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipcEstNo AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcEstTypeDesc AS CHARACTER NO-UNDO.
 

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


/* ***************************  Main Block  *************************** */
IF ipiEstType GT 4 THEN
    ipiEstType = ipiEstType - 4 .

RUN pGetEstDesc(ipcCompany,
    ipiEstType,
    ipcEstNo,
    OUTPUT opcEstTypeDesc).

/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetEstDesc:
    /*------------------------------------------------------------------------------
     Purpose:  Returns the estimate type description
     Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompanyValue AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiTypeValue AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateValue AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcEstTypeDescValue AS CHARACTER NO-UNDO. 
    DEFINE BUFFER bf-eb FOR eb .

    IF ipiTypeValue EQ 1 THEN
        opcEstTypeDescValue = "Single" .
    ELSE IF ipiTypeValue EQ 2 THEN do:
        opcEstTypeDescValue = "Set " .
        FIND FIRST bf-eb NO-LOCK
            WHERE bf-eb.company EQ ipcCompanyValue
            AND bf-eb.est-no EQ ipcEstimateValue
            AND bf-eb.form-no EQ 0 NO-ERROR .
        IF AVAIL bf-eb  THEN do: 
            IF bf-eb.set-is-assembled EQ YES THEN
                opcEstTypeDescValue = opcEstTypeDescValue + " Assembled" .
            ELSE IF bf-eb.set-is-assembled EQ NO THEN
                opcEstTypeDescValue = opcEstTypeDescValue + " Unassembled" .
            ELSE IF bf-eb.set-is-assembled EQ ? THEN
                opcEstTypeDescValue = opcEstTypeDescValue + " Assembled w/Part Receipts" .
        END.
    END.
    ELSE IF ipiTypeValue EQ 4 THEN do:
          opcEstTypeDescValue = "Combo/Tandem" .
    END.

END PROCEDURE.


/* ************************  Function Implementations ***************** */


