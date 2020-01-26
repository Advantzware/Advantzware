
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
DEF VAR cFileName1 AS CHAR NO-UNDO INITIAL "C:\tmp\AcctConv\IF3 - fgcat table.csv".
DEF VAR cFileName2 AS CHAR NO-UNDO INITIAL "C:\tmp\AcctConv\JF3 - prod table.csv".
DEF VAR cFileName3 AS CHAR NO-UNDO INITIAL "C:\tmp\AcctConv\EB8 - prep table.csv".
DEF VAR cFileName4 AS CHAR NO-UNDO INITIAL "C:\tmp\AcctConv\AF6 - stax table.csv".
DEF VAR cFileName5 AS CHAR NO-UNDO INITIAL "C:\tmp\AcctConv\VF1 - vend table.csv".
DEF VAR cFileName6 AS CHAR NO-UNDO INITIAL "C:\tmp\AcctConv\GU3 - gl-jrnl table.csv".

DEF VAR cLogFile AS CHAR NO-UNDO INITIAL "c:\tmp\AcctConv\processlog.txt".
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
    iCountInTot = 0
    iCountOutTot = 0.
        
PUT UNFORMATTED FILL("-",80) + CHR(10).
PUT UNFORMATTED "Beginning PremAcctConversion process on " + STRING(TODAY,"99/99/9999") + " at " + STRING(TIME,"HH:MM:SS AM") +  CHR(10). 
PUT UNFORMATTED "Using files: " + CHR(10). 
PUT UNFORMATTED "   " + cFileName1 + CHR(10). 
PUT UNFORMATTED "   " + cFileName2 + CHR(10). 
PUT UNFORMATTED "   " + cFileName3 + CHR(10). 
PUT UNFORMATTED "   " + cFileName4 + CHR(10). 
PUT UNFORMATTED "   " + cFileName5 + CHR(10). 
PUT UNFORMATTED "   " + cFileName6 + CHR(10). 
PUT UNFORMATTED FILL("-",80) + CHR(10).

RUN pConvFgcat  (INPUT cFileName1, INPUT "fgcat").
RUN pConvProd   (INPUT cFileName2, INPUT "prod").
RUN pConvPrep   (INPUT cFileName3, INPUT "prep").
RUN pConvStax   (INPUT cFileName4, INPUT "stax").
RUN pConvVend   (INPUT cFileName5, INPUT "vend").
RUN pConvGljrnl (INPUT cFileName6, INPUT "gl-jrnl").

PUT UNFORMATTED FILL("-",80) + CHR(10).
PUT UNFORMATTED "Successfully converted " + STRING(iCountOutTot) + " of " + STRING(iCountInTot) + " total records" + CHR(10).
PUT UNFORMATTED "Process complete" + CHR(10).
PUT UNFORMATTED FILL("-",80) + CHR(10).
OUTPUT CLOSE.


/* **********************  Internal Procedures  *********************** */

PROCEDURE pConvFgCat:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcInputFile AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcConvFile AS CHAR NO-UNDO.
    
    ASSIGN 
        iCountIn = 0
        iCountOut = 0.

    PUT UNFORMATTED "Beginning account conversion for " + ipcConvFile + " records" + CHR(10). 
    INPUT FROM VALUE(ipcInputFile).
    IMPORT UNFORMATTED cRaw.
    REPEAT:
        IMPORT UNFORMATTED cRaw.
        ASSIGN 
            iCountIn = iCountIn + 1.
        IF ENTRY(2,cRaw) EQ "company" THEN NEXT.  /* this record contains labels */          
        FIND fgcat WHERE 
            fgcat.company EQ ENTRY(2,cRaw) AND 
            fgcat.procat EQ ENTRY(7,cRaw)
            EXCLUSIVE NO-ERROR.
        IF NOT AVAIL fgcat THEN 
        DO:
            PUT UNFORMATTED "***Unable to locate an " + ipcConvFile + " record with key " + ENTRY(7, cRaw) + CHR(10).
            NEXT.
        END.
        ELSE IF fgcat.glacc NE ENTRY(4,cRaw) THEN 
        DO:
            PUT UNFORMATTED "***Mismatched accounts for record with key " + ENTRY(7, cRaw) + CHR(10).
            NEXT.
        END.
        ELSE 
        DO:
            ASSIGN 
                fgcat.glacc = ENTRY(5,cRaw)
                iCountOut = iCountOut + 1.
            PUT UNFORMATTED "   Processed record with procat " + ENTRY(7,cRaw) + CHR(10).
        END.            
    END.        
    INPUT CLOSE.
    PUT UNFORMATTED "Read " + STRING(iCountIn) + " records from " + ipcInputFile + CHR(10). 
    PUT UNFORMATTED "Successfully converted " + STRING(iCountOut) + " of " + STRING(iCountIn) + " " + ipcConvFile + " records" + CHR(10).
    ASSIGN 
        iCountInTot = iCountInTot + iCountIn
        iCountOutTot = iCountOutTot + iCountOut.
END PROCEDURE.

PROCEDURE pConvProd:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcInputFile AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcConvFile AS CHAR NO-UNDO.
    
    ASSIGN 
        iCountIn = 0
        iCountOut = 0.

    PUT UNFORMATTED "Beginning account conversion for " + ipcConvFile + " records" + CHR(10). 
    INPUT FROM VALUE(ipcInputFile).
    IMPORT UNFORMATTED cRaw.
    REPEAT:
        IMPORT UNFORMATTED cRaw.
        ASSIGN 
            iCountIn = iCountIn + 1.
        IF ENTRY(23, cRaw) EQ "company" THEN NEXT.  /* this record contains labels */          
        FIND FIRST prod WHERE 
            prod.company EQ ENTRY(23, cRaw) AND  
            prod.prolin EQ  ENTRY(24, cRaw)
        EXCLUSIVE NO-ERROR.
        IF NOT AVAIL prod THEN 
        DO:
            PUT UNFORMATTED "***Unable to locate an " + ipcConvFile + " record with key " + ENTRY(24, cRaw) + CHR(10).
            NEXT.
        END.
        ELSE IF prod.fg-mat NE ENTRY(7, cRaw) 
        OR prod.cgs-mat NE ENTRY(10, cRaw) THEN 
        DO:
            PUT UNFORMATTED "***Mismatched accounts for record with key " + ENTRY(24, cRaw) + CHR(10).
            NEXT.
        END.
        ELSE 
        DO:
            ASSIGN 
                prod.fg-mat = ENTRY(8, cRaw)
                prod.cgs-mat = ENTRY(11, cRaw)
                iCountOut = iCountOut + 1.
            PUT UNFORMATTED "   Processed record with key " + ENTRY(24, cRaw) + CHR(10).
        END.
    END.        
    INPUT CLOSE.
    PUT UNFORMATTED "Read " + STRING(iCountIn) + " records from " + ipcInputFile + CHR(10). 
    PUT UNFORMATTED "Successfully converted " + STRING(iCountOut) + " of " + STRING(iCountIn) + " " + ipcConvFile + " records" + CHR(10).
    ASSIGN 
        iCountInTot = iCountInTot + iCountIn
        iCountOutTot = iCountOutTot + iCountOut.
END PROCEDURE.


PROCEDURE pConvStax:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcInputFile AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcConvFile AS CHAR NO-UNDO.
    
    ASSIGN 
        iCountIn = 0
        iCountOut = 0.

    PUT UNFORMATTED "Beginning account conversion for " + ipcConvFile + " records" + CHR(10). 
    INPUT FROM VALUE(ipcInputFile).
    IMPORT UNFORMATTED cRaw.
    REPEAT:
        IMPORT UNFORMATTED cRaw.
        ASSIGN 
            iCountIn = iCountIn + 1.
        IF ENTRY(23, cRaw) EQ "company" THEN NEXT.  /* this record contains labels */          
        FIND FIRST stax WHERE 
            stax.company EQ ENTRY(6, cRaw) AND  
            stax.rec_key EQ  ENTRY(14, cRaw)
        EXCLUSIVE NO-ERROR.
        IF NOT AVAIL stax THEN 
        DO:
            PUT UNFORMATTED "***Unable to locate an " + ipcConvFile + " record with rec_key " + ENTRY(14, cRaw) + CHR(10).
            NEXT.
        END.
        DO jCtr = 1 TO NUM-ENTRIES(ENTRY(5,cRaw),";"):
            IF stax.tax-acc[jCtr] NE entry(jCtr,ENTRY(5,cRaw),";")
                OR stax.tax-acc1[jCtr] NE entry(jCtr,ENTRY(5,cRaw),";") THEN 
            DO:
                PUT UNFORMATTED "***Mismatched tax accounts for record with rec_key " + ENTRY(14,cRaw) + CHR(10).
                PUT UNFORMATTED "tax-acc[" + string(jctr) + "]=" + stax.tax-acc[jctr] + " tax-acc1[" + string(jctr) + "]=" + stax.tax-acc1[jctr] + " cOldAcct1=" + entry(jctr,ENTRY(5,cRaw),";") + CHR(10).
                NEXT.
            END.
            ELSE 
            DO:
                ASSIGN 
                    stax.tax-acc[jCtr] = ENTRY(12,cRaw)
                    stax.tax-acc1[jCtr] = ENTRY(12,cRaw)
                    iCountOut = IF jCtr EQ 1 THEN iCountOut + 1 ELSE iCountOut.
                PUT UNFORMATTED "   Processed record with rec_key " + ENTRY(14,cRaw) + CHR(10).
            END.
        END.
    END.        
    INPUT CLOSE.
    PUT UNFORMATTED "Read " + STRING(iCountIn) + " records from " + ipcInputFile + CHR(10). 
    PUT UNFORMATTED "Successfully converted " + STRING(iCountOut) + " of " + STRING(iCountIn) + " " + ipcConvFile + " records" + CHR(10).
    ASSIGN 
        iCountInTot = iCountInTot + iCountIn
        iCountOutTot = iCountOutTot + iCountOut.
END PROCEDURE.

PROCEDURE pConvVend:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcInputFile AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcConvFile AS CHAR NO-UNDO.
    
    ASSIGN 
        iCountIn = 0
        iCountOut = 0.

    PUT UNFORMATTED "Beginning account conversion for " + ipcConvFile + " records" + CHR(10). 
    INPUT FROM VALUE(ipcInputFile).
    IMPORT UNFORMATTED cRaw.
    REPEAT:
        IMPORT UNFORMATTED cRaw.
        ASSIGN 
            iCountIn = iCountIn + 1.
        IF ENTRY(23, cRaw) EQ "company" THEN NEXT.  /* this record contains labels */          
        FIND FIRST vend WHERE 
            vend.company EQ "001" AND 
            vend.vend-no EQ ENTRY(1,cRaw)
        EXCLUSIVE NO-ERROR.
        IF NOT AVAIL vend THEN 
        DO:
            PUT UNFORMATTED "***Unable to locate an " + ipcConvFile + " record with rec_key " + ENTRY(1, cRaw) + CHR(10).
            NEXT.
        END.
        ELSE IF vend.actnum NE ENTRY(28, cRaw) THEN 
        DO:
            PUT UNFORMATTED "***Mismatched accounts for record with key " + ENTRY(1, cRaw) + CHR(10).
            NEXT.
        END.
        ELSE 
        DO:
            ASSIGN 
                vend.actnum = ENTRY(29, cRaw)
                iCountOut = iCountOut + 1.
            PUT UNFORMATTED "   Processed record with key " + ENTRY(1, cRaw) + CHR(10).
        END.
    END.
    INPUT CLOSE.
    PUT UNFORMATTED "Read " + STRING(iCountIn) + " records from " + ipcInputFile + CHR(10). 
    PUT UNFORMATTED "Successfully converted " + STRING(iCountOut) + " of " + STRING(iCountIn) + " " + ipcConvFile + " records" + CHR(10).
    ASSIGN 
        iCountInTot = iCountInTot + iCountIn
        iCountOutTot = iCountOutTot + iCountOut.
END PROCEDURE.


PROCEDURE pConvPrep:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcInputFile AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcConvFile AS CHAR NO-UNDO.
    
    ASSIGN 
        iCountIn = 0
        iCountOut = 0.

    PUT UNFORMATTED "Beginning account conversion for " + ipcConvFile + " records" + CHR(10). 
    INPUT FROM VALUE(ipcInputFile).
    IMPORT UNFORMATTED cRaw.
    REPEAT:
        IMPORT UNFORMATTED cRaw.
        ASSIGN 
            iCountIn = iCountIn + 1.
        IF ENTRY(10, cRaw) EQ "company" THEN NEXT.  /* this record contains labels */          
        FIND FIRST prep WHERE 
            prep.company EQ entry(10, cRaw) AND 
            prep.loc EQ ENTRY(11, cRaw) AND 
            prep.code EQ ENTRY(2, cRaw)
        EXCLUSIVE NO-ERROR.
        IF NOT AVAIL prep THEN 
        DO:
            PUT UNFORMATTED "***Unable to locate an " + ipcConvFile + " record with key " + ENTRY(10, cRaw) + "|" + ENTRY(11, cRaw) + "|" + ENTRY(2, cRaw) + CHR(10).
            NEXT.
        END.
        ELSE IF prep.actnum NE ENTRY(13, cRaw) THEN 
            DO:
                PUT UNFORMATTED "***Mismatched accounts for record with key " + ENTRY(10, cRaw) + "|" + ENTRY(11, cRaw) + "|" + ENTRY(2, cRaw) + CHR(10).
                NEXT.
            END.
        ELSE 
        DO:
            ASSIGN 
                prep.actnum = ENTRY(14, cRaw)
                iCountOut = iCountOut + 1.
            PUT UNFORMATTED "   Processed record with key " + ENTRY(10, cRaw) + "|" + ENTRY(11, cRaw) + "|" + ENTRY(2, cRaw) + CHR(10).
        END.
    END.
    INPUT CLOSE.
    PUT UNFORMATTED "Read " + STRING(iCountIn) + " records from " + ipcInputFile + CHR(10). 
    PUT UNFORMATTED "Successfully converted " + STRING(iCountOut) + " of " + STRING(iCountIn) + " " + ipcConvFile + " records" + CHR(10).
    ASSIGN 
        iCountInTot = iCountInTot + iCountIn
        iCountOutTot = iCountOutTot + iCountOut.
END PROCEDURE.

PROCEDURE pConvGljrnl:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcInputFile AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcConvFile AS CHAR NO-UNDO.
    
    ASSIGN 
        iCountIn = 0
        iCountOut = 0.

    PUT UNFORMATTED "Beginning account conversion for " + ipcConvFile + " records" + CHR(10). 
    INPUT FROM VALUE(ipcInputFile).
    IMPORT UNFORMATTED cRaw.
    REPEAT:
        IMPORT UNFORMATTED cRaw.
        ASSIGN 
            iCountIn = iCountIn + 1.
        IF ENTRY(23, cRaw) EQ "company" THEN NEXT.  /* this record contains labels */          
        FIND FIRST prep WHERE 
            prep.company EQ entry(10, cRaw) AND 
            prep.loc EQ ENTRY(11, cRaw) AND 
            prep.code EQ ENTRY(2, cRaw)
        EXCLUSIVE NO-ERROR.
        IF NOT AVAIL prep THEN 
        DO:
            PUT UNFORMATTED "***Unable to locate an " + ipcConvFile + " record with key " + ENTRY(10, cRaw) + "|" + ENTRY(11, cRaw) + "|" + ENTRY(2, cRaw) + CHR(10).
            NEXT.
        END.
        ELSE IF prep.actnum NE ENTRY(13, cRaw) THEN 
            DO:
                PUT UNFORMATTED "***Mismatched accounts for record with key " + ENTRY(10, cRaw) + "|" + ENTRY(11, cRaw) + "|" + ENTRY(2, cRaw) + CHR(10).
                NEXT.
            END.
            ELSE 
            DO:
                ASSIGN 
                    prep.actnum = ENTRY(14, cRaw)
                    iCountOut = iCountOut + 1.
                PUT UNFORMATTED "   Processed record with key " + ENTRY(10, cRaw) + "|" + ENTRY(11, cRaw) + "|" + ENTRY(2, cRaw) + CHR(10).
            END.
    END.
    INPUT CLOSE.
    PUT UNFORMATTED "Read " + STRING(iCountIn) + " records from " + ipcInputFile + CHR(10). 
    PUT UNFORMATTED "Successfully converted " + STRING(iCountOut) + " of " + STRING(iCountIn) + " " + ipcConvFile + " records" + CHR(10).
    ASSIGN 
        iCountInTot = iCountInTot + iCountIn
        iCountOutTot = iCountOutTot + iCountOut.
END PROCEDURE.

