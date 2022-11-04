/* According to Alyssa (in ticket 107258), do not update freight, and update every line for the listed BOL nos */

DEFINE BUFFER b-oe-bolh FOR oe-bolh.
DEFINE BUFFER b-oe-boll FOR oe-boll.
DEFINE BUFFER b2-oe-boll FOR oe-boll.

DEFINE STREAM sImport. 
DEFINE STREAM sOutput.  

DEFINE VARIABLE iImported AS INTEGER.
DEFINE VARIABLE iProcessed AS INTEGER.
DEFINE VARIABLE cLine AS CHAR.
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKPressed AS LOG INITIAL TRUE.

SYSTEM-DIALOG GET-FILE cFile
    TITLE "BOL Date Update File"
    FILTERS "CSV Files (*.CSV)" "*.CSV"
    MUST-EXIST 
    INITIAL-DIR "c:\tmp"
    USE-FILENAME 
    UPDATE OKPressed.
    
IF cFile EQ "" THEN RETURN.

OUTPUT STREAM sOutput TO c:\tmp\BolDateChangeFails.txt.
   
DEFINE TEMP-TABLE ttBOL NO-UNDO
   FIELD company AS CHAR  
   FIELD order# AS CHAR 
   FIELD customerPO AS CHAR 
   FIELD trailer# AS CHAR 
   FIELD freightCost AS CHAR 
   FIELD printed AS CHAR 
   FIELD BOL# AS char
   FIELD Whse AS char
   FIELD FGItem# AS CHAR 
   FIELD CustPart# AS CHAR 
   FIELD carrier AS CHAR 
   FIELD Posted AS CHAR 
   FIELD UpdDate AS CHAR 
   FIELD Tag AS CHAR 
   FIELD BOLDate AS CHAR 
   FIELD BolStatus AS char
   FIELD ShipTo AS CHAR 
   FIELD updBy AS CHAR 
   .

/* Import from file and create temp-table */
INPUT STREAM sImport FROM VALUE(cFile).
IMPORT STREAM sImport UNFORMATTED cLine.  /* This is the header line, no TT */
REPEAT:
    IMPORT STREAM sImport UNFORMATTED cLine.
    CREATE ttBOL.
    ASSIGN 
        ttBol.company = "001"
        ttBol.order# = ENTRY(1,cLine,",") 
        ttBol.customerPO = ENTRY(2,cLine,",") 
        ttBol.trailer# = ENTRY(3,cLine,",") 
        ttBol.freightCost = ENTRY(4,cLine,",") 
        ttBol.printed = ENTRY(5,cLine,",") 
        ttBol.BOL# = ENTRY(6,cLine,",")
        ttBol.Whse = ENTRY(7,cLine,",")
        ttBol.FGItem# = ENTRY(8,cLine,",") 
        ttBol.CustPart# = ENTRY(9,cLine,",") 
        ttBol.carrier = ENTRY(10,cLine,",") 
        ttBol.Posted = ENTRY(11,cLine,",") 
        ttBol.UpdDate = ENTRY(12,cLine,",") 
        ttBol.Tag = ENTRY(13,cLine,",") 
        ttBol.BOLDate = ENTRY(14,cLine,",") 
        ttBol.BolStatus = ENTRY(15,cLine,",")
        ttBol.ShipTo = ENTRY(16,cLine,",") 
        ttBol.updBy = ENTRY(17,cLine,",") 
        iImported = iImported + 1      
        . 
END.
INPUT STREAM sImport CLOSE.

FOR EACH ttBOL:

    FIND FIRST b-oe-bolh EXCLUSIVE WHERE 
        b-oe-bolh.company EQ ttBol.company AND 
        b-oe-bolh.bol-no EQ INTEGER(ttBol.BOL#)
        NO-ERROR.
        
    IF AVAIL b-oe-bolh THEN DO:
        FOR EACH b-oe-boll EXCLUSIVE WHERE 
            b-oe-boll.company EQ b-oe-bolh.company AND 
            b-oe-boll.bol-no EQ b-oe-bolh.bol-no:
            ASSIGN 
                b-oe-boll.bol-date = DATE(ttBol.BolDate)
                .
        END.
        
        ASSIGN 
            iProcessed = iProcessed + 1
            b-oe-bolh.bol-date = DATE(ttBol.BolDate).
            
    END. /* AVAIL bolh */
    ELSE DO:
        PUT STREAM sOutput UNFORMATTED 
            "Could not process BOL# " + ttBol.bol# + " (not found)" + CHR(10).
    END.
            
END. /* EACH ttBOL */

OUTPUT STREAM sOutput CLOSE. 

MESSAGE 
    "Imported: " iImported SKIP
    "Processed: " iProcessed
    VIEW-AS ALERT-BOX.
