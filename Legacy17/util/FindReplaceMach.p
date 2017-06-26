
/*------------------------------------------------------------------------
    File        : FindReplaceMach.p
    Purpose     : One-time run.  Imports an Excel file of old machine codes and new machine codes. 
                  Checks the machine codes and presents the results to the user.
                  Makes the change to routing and est-op 
                  Logs changes made

    Syntax      : RUN FindReplaceMachine(INPUT "001",
                                         INPUT "C:\Temp\ImportFile.csv"  <-2 columns, no headers, old in first column, new in second
                                         INPUT "C:\Tetmp\ImportLog.log"
                                         Input YES).  <- Set LogOnly to Yes to validate that the import file is good and what changes will be made

    Description : 

    Author(s)   : BV
    Created     : Thu Oct 06 14:17:57 EDT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcImportFile AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcLogFile AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplLogOnly AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE ttImportedMachines
    FIELD cOldMachine     AS CHARACTER
    FIELD cOldMachineDept LIKE mach.dept
    FIELD cNewMachine     AS CHARACTER
    FIELD cNewMachineDept LIKE mach.dept
    FIELD lValid          AS LOGICAL
    FIELD cInvalidReason  AS CHARACTER
    INDEX OldMachine IS PRIMARY cOldMachine
    INDEX Valid                 lValid.

DEFINE STREAM sRow.
DEFINE STREAM sLog.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fValidateImport RETURNS LOGICAL 
    (  ) FORWARD.


/* ***************************  Main Block  *************************** */

RUN pImportFile.
IF fValidateImport() THEN
    RUN pProcessChanges.
ELSE
    RUN pDisplayErrors.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pDisplayErrors:
    /*------------------------------------------------------------------------------
     Purpose: Displays records with errors
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH ttImportedMachines NO-LOCK
        WHERE NOT ttImportedMachines.lValid:
        DISPLAY 
            ttImportedMachines.cInvalidReason FORMAT "x(60)"
            ttImportedMachines.cOldMachine
            ttImportedMachines.cNewMachine
            .   
    END.

END PROCEDURE.

PROCEDURE pImportFile:
    /*------------------------------------------------------------------------------
     Purpose: Reads the contents of an Excel File into a temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttImportedMachines.
    cFile = ipcImportFile.
    IF SEARCH(cFile) NE ? THEN 
    DO:
        INPUT STREAM sRow FROM VALUE(cFile).
        REPEAT:
            CREATE ttImportedMachines.
            IMPORT STREAM sRow DELIMITER ","
                ttImportedMachines.cOldMachine
                ttImportedMachines.cNewMachine
                . 
        END.
        OUTPUT STREAM sRow CLOSE.
    END.
    FOR EACH ttImportedMachines:
        IF TRIM(ttImportedMachines.cOldMachine) EQ "" THEN
            DELETE ttImportedMachines.
    END.

END PROCEDURE.

PROCEDURE pProcessChanges:
    /*------------------------------------------------------------------------------
     Purpose:  Processes completely valid temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    OUTPUT STREAM sLog TO VALUE(ipcLogFile).
    FOR EACH routing EXCLUSIVE-LOCK
        WHERE routing.company EQ ipcCompany
        :
        DO iIndex = 1 TO EXTENT(routing.m-code):
            FIND FIRST ttImportedMachines NO-LOCK
                WHERE ttImportedMachines.cOldMachine EQ routing.m-code[iIndex]
                NO-ERROR.
            IF AVAILABLE ttImportedMachines THEN 
            DO:
                EXPORT STREAM sLog DELIMITER ","
                    "Routing" 
                    routing.r-code 
                    routing.m-code[iIndex] 
                    ttImportedMachines.cNewMachine
                    .
                IF NOT iplLogOnly THEN   
                    routing.m-code[iIndex] = ttImportedMachines.cNewMachine.
            END.
        END.
    END.
    FOR EACH ttImportedMachines NO-LOCK
        WHERE ttImportedMachines.lValid
        AND ttImportedMachines.cOldMachine NE ttImportedMachines.cNewMachine
        :
    
        FOR EACH est-op EXCLUSIVE-LOCK
            WHERE est-op.company EQ ipcCompany
            AND est-op.m-code EQ ttImportedMachines.cOldMachine
            :
            EXPORT STREAM sLog DELIMITER "," 
                "Estimate Routing"
                est-op.est-no 
                est-op.m-code 
                ttImportedMachines.cNewMachine
                .
            IF NOT iplLogOnly THEN
                est-op.m-code = ttImportedMachines.cNewMachine.
        END.
    
    END.
    OUTPUT STREAM sLog CLOSE.

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fValidateImport RETURNS LOGICAL 
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Seaches each machine to make sure the machine codes are valid
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE lAllValid AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iX        AS INTEGER NO-UNDO.
    
    FOR EACH ttImportedMachines:
        
        FIND FIRST mach
            WHERE mach.company EQ ipcCompany
            AND mach.m-code EQ ttImportedMachines.cOldMachine
            NO-LOCK NO-ERROR.
        IF AVAILABLE mach THEN 
        DO:
            ASSIGN 
                ttImportedMachines.cOldMachineDept = mach.dept
                ttImportedMachines.lValid          = YES.
        END.
        ELSE 
        DO:
            ASSIGN 
                ttImportedMachines.lValid         = NO
                ttImportedMachines.cInvalidReason = "Old machine code: " + ttImportedMachines.cOldMachine + " not valid.".
        END.
        IF ttImportedMachines.lValid THEN 
        DO:
            FIND FIRST mach
                WHERE mach.company EQ ipcCompany
                AND mach.m-code EQ ttImportedMachines.cNewMachine
                NO-LOCK NO-ERROR.
            IF AVAILABLE mach THEN 
            DO:
                ASSIGN 
                    ttImportedMachines.cNewMachineDept = mach.dept
                    ttImportedMachines.lValid          = YES.
               
            END.
            ELSE 
            DO:
                ASSIGN 
                    ttImportedMachines.lValid         = NO
                    ttImportedMachines.cInvalidReason = "New machine code: " + ttImportedMachines.cNewMachine + " not valid.".
            END. 
        END.
        IF ttImportedMachines.lValid THEN 
        DO iX = 1 TO EXTENT(ttImportedMachines.cNewMachineDept):
        
            IF ttImportedMachines.lValid AND ttImportedMachines.cNewMachineDept[iX] NE ttImportedMachines.cOldMachineDept[iX] THEN 
            DO:
                ASSIGN 
                    ttImportedMachines.lValid         = NO
                    ttImportedMachines.cInvalidReason = ttImportedMAchines.cInvalidReason + "New machine dept: " + ttImportedMachines.cNewMachineDept[iX] + " is different from old machine dept: " + ttImportedMachines.cOldMachineDept[iX] + chr(13).
            END.
            ELSE 
            DO:
                ASSIGN 
                    ttImportedMachines.lValid = YES.
               
            END. 
        END.
    END.
    
    lAllValid = NOT CAN-FIND(FIRST ttImportedMAchines WHERE lValid = NO).

    RETURN lAllValid.


		
END FUNCTION.


