/*------------------------------------------------------------------------
    File        : CorrectRMInventoryCosts.p
    Author(s)   : MYT 
    Created     : Tue Apr 19 15:22:47 EDT 2022
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttCartonTag 
    FIELD cTag AS CHAR 
    FIELD cItem AS CHAR 
    FIELD deCostPerMSF AS DECIMAL 
    FIELD deCorrUnitCost AS DECIMAL 
    FIELD deDollar AS DECIMAL 
    FIELD iVendorPO AS INT 
    FIELD cWhse AS CHAR 
    FIELD cBin AS CHAR 
    FIELD deDiff AS DECIMAL 
    .

DEF VAR cRaw AS CHAR. 
    
INPUT FROM "c:\tmp\accord cartonroll inventory cost correction file.csv".
IMPORT cRaw.

REPEAT:
    IMPORT UNFORMATTED cRaw.
    CREATE ttCartonTag.
    ASSIGN 
        ttCartonTag.cTag = ENTRY(1,cRaw)
        ttCartonTag.cItem = ENTRY(2,cRaw) 
        ttCartonTag.deCostPerMSF = DECIMAL(ENTRY(3,cRaw))
        ttCartonTag.deCorrUnitCost = DECIMAL(ENTRY(4,cRaw))
        ttCartonTag.deDollar = DECIMAL(ENTRY(5,cRaw))
        ttCartonTag.iVendorPO = INTEGER(ENTRY(6,cRaw))
        ttCartonTag.cWhse = ENTRY(7,cRaw)
        ttCartonTag.cBin = ENTRY(8,cRaw)
        ttCartonTag.deDiff = DECIMAL(ENTRY(9,cRaw))
        .    
END.
INPUT CLOSE.

FOR EACH ttCartonTag:
    FOR EACH rm-rctd WHERE 
        rm-rctd.company EQ "001" AND 
        rm-rctd.i-no EQ ttCartonTag.cItem AND 
        rm-rctd.tag EQ cTag:
        ASSIGN 
            rm-rctd.cost = ttCartonTag.deCostPerMSF.
    END.
    FOR EACH rm-rdtlh WHERE 
        rm-rdtlh.company EQ "001" AND 
        rm-rdtlh.i-no EQ ttCartonTag.cItem AND 
        rm-rdtlh.tag EQ cTag:
        ASSIGN 
            rm-rdtlh.cost = ttCartonTag.deCostPerMSF.
    END.
    FOR EACH rm-bin WHERE 
        rm-bin.company EQ "001" AND 
        rm-bin.i-no EQ ttCartonTag.cItem AND 
        rm-bin.tag EQ cTag:
        ASSIGN 
            rm-bin.cost = ttCartonTag.deCostPerMSF.
    END.
END.
    
            