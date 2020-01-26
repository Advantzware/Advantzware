
/*------------------------------------------------------------------------
    File        : PremAcctConversion.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : MYT
    Created     : Thu Jan 23 15:47:44 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF VAR cFileName1 AS CHAR NO-UNDO INITIAL "C:\tmp\AcctConv\AF6 - stax table.csv".
DEF VAR cFileName2 AS CHAR NO-UNDO INITIAL "C:\tmp\AcctConv\IF3 - fgcat table.csv".
DEF VAR cFileName3 AS CHAR NO-UNDO INITIAL "C:\tmp\AcctConv\JF3 - prod table.csv".
DEF VAR cLogFile AS CHAR NO-UNDO INITIAL "c:\tmp\AcctConv\processlog.txt".
 
DEF TEMP-TABLE ttAcctConv
    FIELD cTable AS CHAR 
    FIELD cCompany AS CHAR 
    FIELD cKey1 AS CHAR 
    FIELD cKey2 AS CHAR 
    FIELD cKey3 AS CHAR 
    FIELD cRecKey AS CHAR 
    FIELD cOldAcct1 AS CHAR 
    FIELD cNewAcct1 AS CHAR
    FIELD cOldAcct2 AS CHAR 
    FIELD cNewAcct2 AS CHAR
    .
DEF VAR iCountIn AS INT NO-UNDO.
DEF VAR iCountInTot AS INT NO-UNDO.    
DEF VAR iCountOut AS INT NO-UNDO.
DEF VAR iCountOutTot AS INT NO-UNDO.  
DEF VAR jCtr AS INT NO-UNDO.  
    
DEF VAR cRaw AS CHAR.    

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
    OUTPUT TO VALUE(cLogFile).    
    
    ASSIGN 
        iCountIn = 0
        iCountInTot = 0.
        
    PUT UNFORMATTED FILL("-",80) + CHR(10).
    PUT UNFORMATTED "Beginning PremAcctConversion process on " + STRING(TODAY,"99/99/9999") + " at " + STRING(TIME,"HH:MM:SS AM") +  CHR(10). 
    PUT UNFORMATTED "Using files: " + CHR(10). 
    PUT UNFORMATTED "   " + cFileName1 + CHR(10). 
    PUT UNFORMATTED "   " + cFileName2 + CHR(10). 
    PUT UNFORMATTED "   " + cFileName3 + CHR(10). 
    PUT UNFORMATTED FILL("-",80) + CHR(10).

    INPUT FROM VALUE(cFileName1).
    IMPORT UNFORMATTED cRaw.
    REPEAT:
        IMPORT UNFORMATTED cRaw.
        CREATE ttAcctConv.
        ASSIGN 
            ttAcctConv.cTable = "stax"
            ttAcctConv.cCompany = ENTRY(6,cRaw,",")
            ttAcctConv.cKey1 = ""
            ttAcctConv.cKey2 = ""
            ttAcctConv.cKey3 = ""
            ttAcctConv.cRecKey = ENTRY(14,cRaw,",")
            ttAcctConv.cOldAcct1 = ENTRY(12, cRaw, ",")
            ttAcctConv.cNewAcct1 = ENTRY(5, cRaw, ",")
            iCountIn = iCountIn + 1.  
    END.
    INPUT CLOSE.
    PUT UNFORMATTED "Read " + STRING(iCountIn) + " records from " + cFileName1 + CHR(10). 

    ASSIGN 
        iCountInTot = iCountInTot + iCountIn
        iCountIn = 0.
        
    INPUT FROM VALUE(cFileName2).
    IMPORT UNFORMATTED cRaw.
    REPEAT:
        IMPORT UNFORMATTED cRaw.
        CREATE ttAcctConv.
        ASSIGN 
            ttAcctConv.cTable = "fgcat"
            ttAcctConv.cCompany = ENTRY(2,cRaw,",")
            ttAcctConv.cKey1 = ENTRY(7, cRaw, ",")
            ttAcctConv.cKey2 = ""
            ttAcctConv.cKey3 = ""
            ttAcctConv.cRecKey = ""
            ttAcctConv.cOldAcct1 = ENTRY(5, cRaw, ",")
            ttAcctConv.cNewAcct1 = ENTRY(4, cRaw, ",")
            iCountIn = iCountIn + 1.
    END.  
    INPUT CLOSE.
    PUT UNFORMATTED "Read " + STRING(iCountIn) + " records from " + cFileName2 + CHR(10). 

    ASSIGN 
        iCountInTot = iCountInTot + iCountIn
        iCountIn = 0.

    INPUT FROM VALUE(cFileName3).
    IMPORT UNFORMATTED cRaw.
    REPEAT:
        IMPORT UNFORMATTED cRaw.
        CREATE ttAcctConv.
        ASSIGN 
            ttAcctConv.cTable = "prod"
            ttAcctConv.cCompany = ENTRY(23,cRaw,",")
            ttAcctConv.cKey1 = ENTRY(24,cRaw,",")
            ttAcctConv.cKey2 = ""
            ttAcctConv.cKey3 = ""
            ttAcctConv.cRecKey = ""
            ttAcctConv.cOldAcct1 = ENTRY(8, cRaw, ",")
            ttAcctConv.cNewAcct1 = ENTRY(7, cRaw, ",")
            ttAcctConv.cOldAcct2 = ENTRY(11, cRaw, ",")
            ttAcctConv.cNewAcct2 = ENTRY(10, cRaw, ",")
            iCountIn = iCountIn + 1.
    END.  
    INPUT CLOSE.
    PUT UNFORMATTED "Read " + STRING(iCountIn) + " records from " + cFileName3 + CHR(10). 

    ASSIGN 
        iCountInTot = iCountInTot + iCountIn
        iCountIn = 0.
    PUT UNFORMATTED "Read " + STRING(iCountInTot) + " total records" + CHR(10). 
    PUT UNFORMATTED CHR(10).
            
    ASSIGN 
        iCountIn = 0
        iCountInTot = 0
        iCountOut = 0
        iCountOutTot = 0.
        
    PUT UNFORMATTED "Beginning account conversion for stax records" + CHR(10). 
    FOR EACH ttAcctConv WHERE 
        ttAcctConv.cTable EQ "stax":
        ASSIGN 
            iCountIn = iCountIn + 1.
        FIND FIRST stax WHERE
            stax.company EQ ttAcctConv.cCompany AND  
            stax.rec_key EQ  ttAcctConv.cRecKey
            EXCLUSIVE NO-ERROR.
        IF NOT AVAIL stax THEN 
        DO:
            PUT UNFORMATTED "***Unable to locate an stax record with rec_key " + ttAcctConv.cRecKey + CHR(10).
            NEXT.
        END.
        DO jCtr = 1 TO NUM-ENTRIES(ttAcctConv.cOldAcct1,";"):
            IF stax.tax-acc[jCtr] NE entry(jCtr,ttAcctConv.cOldAcct1,";")
            OR stax.tax-acc1[jCtr] NE entry(jCtr,ttAcctConv.cOldAcct1,";") THEN 
            DO:
                PUT UNFORMATTED "***Mismatched tax accounts for record with rec_key " + ttAcctConv.cRecKey + CHR(10).
                PUT UNFORMATTED "tax-acc[" + string(jctr) + "]=" + stax.tax-acc[jctr] + " tax-acc1[" + string(jctr) + "]=" + stax.tax-acc1[jctr] + " cOldAcct1=" + entry(jctr,ttAcctConv.cOldAcct1,";") + CHR(10).
                NEXT.
            END.
            ELSE 
            DO:
                ASSIGN 
                    stax.tax-acc[jCtr] = ttAcctConv.cNewAcct1
                    stax.tax-acc1[jCtr] = ttAcctConv.cNewAcct1
                    iCountOut = IF jCtr EQ 1 THEN iCountOut + 1 ELSE iCountOut.
                PUT UNFORMATTED "   Processed record with rec_key " + ttAcctConv.cRecKey + CHR(10).
            END.
        END.
    END.
    PUT UNFORMATTED "Successfully converted " + STRING(iCountOut) + " of " + STRING(iCountIn) + " stax records" + CHR(10).
    ASSIGN 
        iCountInTot = iCountInTot + iCountIn
        iCountOutTot = iCountOutTot + iCountOut
        iCountIn = 0
        iCountOut = 0.       
    
    /***************************/
    
    PUT UNFORMATTED "Beginning account conversion for fgcat records" + CHR(10). 
    FOR EACH ttAcctConv WHERE 
        ttAcctConv.cTable EQ "fgcat".
        ASSIGN 
            iCountIn = iCountIn + 1.
        FIND FIRST fgcat WHERE 
            fgcat.company EQ ttAcctConv.cCompany AND  
            fgcat.procat EQ  ttAcctConv.cKey1
            EXCLUSIVE NO-ERROR.
        IF NOT AVAIL fgcat THEN 
        DO:
            PUT UNFORMATTED "***Unable to locate an fgcat record with procat " + ttAcctConv.cKey1 + CHR(10).
            NEXT.
        END.
        ELSE IF fgcat.glacc NE ttAcctConv.cOldAcct1 THEN 
        DO:
            PUT UNFORMATTED "***Mismatched tax accounts for record with procat " + ttAcctConv.cKey1 + CHR(10).
            NEXT.
        END.
        ELSE 
        DO:
            ASSIGN 
                fgcat.glacc = ttAcctConv.cNewAcct1
                iCountOut = iCountOut + 1.
            PUT UNFORMATTED "   Processed record with procat " + ttAcctConv.cKey1 + CHR(10).
        END.
    END.
    PUT UNFORMATTED "Successfully converted " + STRING(iCountOut) + " of " + STRING(iCountIn) + " fgcat records" + CHR(10).
    ASSIGN 
        iCountInTot = iCountInTot + iCountIn
        iCountOutTot = iCountOutTot + iCountOut
        iCountIn = 0
        iCountOut = 0.       

    /***************************/
        
    PUT UNFORMATTED "Beginning account conversion for prod records" + CHR(10). 
    FOR EACH ttAcctConv WHERE 
        ttAcctConv.cTable EQ "prod".
        ASSIGN 
            iCountIn = iCountIn + 1.
        FIND FIRST prod WHERE 
            prod.company EQ ttAcctConv.cCompany AND  
            prod.prolin EQ  ttAcctConv.cKey1
        EXCLUSIVE NO-ERROR.
        IF NOT AVAIL prod THEN 
        DO:
            PUT UNFORMATTED "***Unable to locate a prod record with prolin " + ttAcctConv.cKey1 + CHR(10).
            NEXT.
        END.
        ELSE IF prod.fg-mat NE ttAcctConv.cOldAcct1 
        OR prod.cgs-mat NE ttAcctConv.cOldAcct2 THEN 
        DO:
            PUT UNFORMATTED "***Mismatched tax accounts for record with prolin " + ttAcctConv.cKey1 + CHR(10).
            NEXT.
        END.
        ELSE 
        DO:
            ASSIGN 
                prod.fg-mat = ttAcctConv.cNewAcct1
                prod.cgs-mat = ttAcctConv.cNewAcct2
                iCountOut = iCountOut + 1.
            PUT UNFORMATTED "   Processed record with prolin " + ttAcctConv.cKey1 + CHR(10).
        END.
    END.
    PUT UNFORMATTED "Successfully converted " + STRING(iCountOut) + " of " + STRING(iCountIn) + " prod records" + CHR(10).
    PUT UNFORMATTED FILL("-",80) + CHR(10).
    PUT UNFORMATTED "Successfully converted " + STRING(iCountOutTot) + " of " + STRING(iCountInTot) + " total records" + CHR(10).
    PUT UNFORMATTED "Process complete" + CHR(10).
    PUT UNFORMATTED FILL("-",80) + CHR(10).
    OUTPUT CLOSE.
    
        
