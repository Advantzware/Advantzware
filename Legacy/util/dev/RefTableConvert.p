/*------------------------------------------------------------------------
    File        : RefTableConvert.p
    Purpose     : 
    Syntax      :
    Description : Used to perform reftable conversions to native data files
    Author(s)   : MYT
    Created     : Mon Mar 11 14:06:04 EDT 2019
    Notes       : This is the "batch wrapper" for Reftable Conversions
                    The work is performed by the structured procedure spReftable.p which
                    is called as a persistent procedure here and is referenced as hConvert.
                    In general, there is no "maintenance" for this program, as the variable 
                    is maintained by reading internal procedures in the SP.  All other processing 
                    and error catch are either built-in, or handled by hConvert.
                    Logs are configured in hConvert, but are stored in c:\tmp by default.
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF VAR cRefTableEntry AS CHAR NO-UNDO.
DEF VAR cRefTableList AS CHAR NO-UNDO.
DEF VAR hConvert AS HANDLE NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR iCount AS INT NO-UNDO.
DEF VAR iProcessed AS INT NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN util/dev/spRefTable.p PERSISTENT SET hConvert.

DO iCtr = 1 TO NUM-ENTRIES(hConvert:INTERNAL-ENTRIES):
    IF NOT ENTRY(iCtr,hConvert:INTERNAL-ENTRIES) BEGINS "_" THEN ASSIGN 
        cRefTableList = cRefTableList + ENTRY(iCtr,hConvert:INTERNAL-ENTRIES) + ",".
END.
ASSIGN 
    cRefTableList = TRIM(cRefTableList,",").
    
RUN _initializeLog IN hConvert.

DO iCtr = 1 TO NUM-ENTRIES(cRefTableList):
    ASSIGN 
        cRefTableEntry = ENTRY(iCtr,cRefTableList). 
    IF CAN-DO(hConvert:INTERNAL-ENTRIES,cRefTableEntry) THEN DO:
        IF CAN-FIND(FIRST reftable WHERE reftable.reftable EQ cRefTableEntry) THEN 
            RUN _epConvert IN hConvert (INPUT cRefTableEntry,
                                        INPUT "Convert",
                                        OUTPUT iCount,
                                        OUTPUT iProcessed). 
        ELSE 
            RUN _writeLog IN hConvert (cRefTableEntry, 0, "Zero").    
    END.
    ELSE 
        RUN _writeLog IN hConvert (cRefTableEntry,1,"Conversion routine not defined in spRefTable.p.").
END. 

RUN _closeLog IN hConvert.
