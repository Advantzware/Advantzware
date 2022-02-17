/*------------------------------------------------------------------------
  File:         invalidDMIID.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 2.23.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttDMITrans
DEFINE TEMP-TABLE ttDMITrans NO-UNDO LIKE dmiTrans.

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 163
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */



/* ************************  Function Prototypes ********************** */

FUNCTION fGetLocation RETURNS CHARACTER PRIVATE
	(ipcCompany AS CHARACTER,
	 ipcMachineID AS CHARACTER) FORWARD.

FUNCTION fGetTag RETURNS CHARACTER PRIVATE
	(BUFFER dmiTrans FOR dmiTrans) FORWARD.

FUNCTION fGetXRef RETURNS CHARACTER PRIVATE
	(ipcCompany AS CHARACTER,
	 ipcLookup AS CHARACTER) FORWARD.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cEstimateID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTag        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lExportOnly AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cLocation   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bDMITrans FOR dmiTrans.

    EMPTY TEMP-TABLE ttDMITrans.
    FOR EACH dmiTrans NO-LOCK
        WHERE dmiTrans.posted EQ NO
           BY dmiTrans.dmiID
           BY dmiTrans.startDate
           BY dmiTrans.startTime
        :
        idx = idx + 1.
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, idx, ?).
        IF CAN-FIND(FIRST mach
                    WHERE mach.spare-int-2 EQ dmiTrans.dmiID) THEN
        NEXT.
        // only process dmitrans record with invalid dmiid values
            
        CREATE ttDMITrans.
        BUFFER-COPY dmiTrans TO ttDMITrans.
        CASE cPostDelete:
            WHEN "Delete" THEN DO TRANSACTION:                
                DELETE dmiTrans.
            END. /* delete */
            WHEN "Post" THEN DO:
                IF dmiTrans.transState EQ "RUN" THEN DO:
                    ASSIGN
                        cEstimateID = fGetXRef(cCompany, dmiTrans.jobID)
                        cTag        = fGetTag(BUFFER dmiTrans)
                        cLocation   = fGetLocation(cCompany, STRING(dmiTrans.dmiID))
                        lExportOnly = NO
                        .
                    RUN jc\ProcessFurnishBatch.p (
                        cCompany,
                        cEstimateID,
                        cTag,
                        TODAY,
                        cLocation, 
                        lExportOnly,
                        OUTPUT lError,
                        OUTPUT cMessage
                        ).
                END.
                DO TRANSACTION:
                    FIND FIRST bDMITrans EXCLUSIVE-LOCK
                         WHERE ROWID(bDMITrans) EQ ROWID(dmiTrans).
                    bDMITrans.posted = YES.
                END. /* do trans */
            END. /* post */
        END CASE.
    END. /* each dmitrans */

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fGetLocation RETURNS CHARACTER PRIVATE
	(ipcCompany AS CHARACTER, ipcMachineID AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:  Given a machine code, return the location of that machine
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cLocation AS CHARACTER.
    
    FIND FIRST mach NO-LOCK 
        WHERE mach.company EQ ipcCompany
        AND mach.m-code EQ ipcMachineID
        NO-ERROR.
	IF AVAILABLE mach THEN 
	   cLocation = mach.physicalLoc.
        
    RETURN cLocation.        
END FUNCTION.

FUNCTION fGetTag RETURNS CHARACTER PRIVATE
	(BUFFER dmiTrans FOR dmiTrans):
/*------------------------------------------------------------------------------
 Purpose: Build the tag from the dmiTrans record
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cTag AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDateTime AS CHARACTER NO-UNDO.
    
    ASSIGN 
        cDateTime = STRING(YEAR(dmiTrans.tranDate),"9999") + STRING(MONTH(dmiTrans.tranDate),"99") + STRING(DAY(dmiTrans.tranDate),"99") + STRING(dmiTrans.tranTime,"99999")
        cTag = STRING(dmiTrans.dmiID) + "-" + dmiTrans.jobID + cDateTime 
        .
	RETURN SUBSTRING(cTag,1,20).
	
END FUNCTION.

FUNCTION fGetXRef RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER, ipcLookup AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose: Lookup a xref value  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

    RUN sys/ref/getXref.p (INPUT ipcCompany,
        INPUT "Formula", 
        INPUT ipcLookup, 
        OUTPUT cReturn).
    
    RUN util/rjust.p (INPUT-OUTPUT cReturn, 8).
    
    RETURN cReturn.   /* Function return value. */
		
END FUNCTION.
