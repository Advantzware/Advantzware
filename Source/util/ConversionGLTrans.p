/*------------------------------------------------------------------------
    File        : util/ConversionGLTrans.p
    Purpose     : GLTrans table conversion in GLHist 
    Syntax      : 
    Description : 
    Author(s)   : Sewa Singh, Mark Tyndall
    Created     : 01.07.2021
    Notes       :   Updated 21.04.20 - MYT
----------------------------------------------------------------------*/
    DEFINE TEMP-TABLE ttPeriod LIKE period.
    DEFINE TEMP-TABLE ttOpenPeriodStartDateByCompany
        FIELD company LIKE period.company
        FIELD daStartDate LIKE period.pst.
        
    DEFINE VARIABLE iLocked AS INT NO-UNDO.
    
    /* Optional - prevents create of audit records */
    DISABLE TRIGGERS FOR LOAD OF glhist.
    
    /* If no glTrans, just leave */
    FIND FIRST glTrans NO-LOCK NO-ERROR.
    IF NOT AVAIL glTrans THEN RETURN.

    /* First, copy the period table into temp-table to prevent further DB reads */
    FOR EACH period NO-LOCK:
        CREATE ttPeriod.
        IMPORT ttPeriod.
    END.
    
    /* Next, find the first open period start date for each company */
    companyLoop:
    FOR EACH company NO-LOCK:
        CREATE ttOpenPeriodStartDateByCompany.
        ASSIGN 
            ttOpenPeriodStartDateByCompany.company = company.company.
        FOR EACH ttPeriod WHERE 
            ttPeriod.company EQ company.company:
            IF ttPeriod.pstat EQ TRUE THEN DO:
                ASSIGN 
                    ttOpenPeriodStartDateByCompany.daStartDate = ttPeriod.pst.
                NEXT companyLoop.
            END.
        END.
    END.  
            
    FOR EACH GLTrans NO-LOCK:
        /* On the off-chance that the gltrans is in-use, skip it and report later */
        FIND CURRENT glTrans EXCLUSIVE NO-WAIT.
        IF LOCKED(glTrans) THEN DO:
            ASSIGN iLocked = iLocked + 1.
            NEXT.
        END.
        
        /* Find the first open period start date from the temp-table */
        FIND FIRST ttOpenPeriodStartDateByCompany WHERE 
            ttOpenPeriodStartDateByCompany.company EQ glTrans.company
            NO-ERROR.
        CREATE glhist.
        ASSIGN 
            glhist.company   = gltrans.company
            glhist.actnum    = gltrans.actnum
            glhist.jrnl      = gltrans.jrnl
            glhist.period    = gltrans.period
            glhist.tr-dscr   = gltrans.tr-dscr
            glhist.tr-date   = gltrans.tr-date
            glhist.tr-num    = gltrans.trnum
            glhist.tr-amt    = gltrans.tr-amt
            glhist.entryType = "A"
            glhist.glYear    = YEAR(gltrans.tr-date)
            glhist.createdBy = gltrans.createdBy
            glhist.curr-code = gltrans.curr-code
            glhist.ex-rate   = gltrans.ex-rate
            glhist.posted    = IF gltrans.tr-date LT ttOpenPeriodStartDateByCompany.daStartDate THEN TRUE ELSE FALSE
            glhist.postedby  = IF glhist.posted THEN USERID(LDBNAME(1)) ELSE ""
            .
        DELETE GLTrans.        
    END.
    RELEASE glhist.
    
    IF iLocked GT 0 THEN MESSAGE 
        "There were " + STRING(iLocked) + " locked record(s) found during this " +
        "conversion.  Please re-run this function to complete the process."
        VIEW-AS ALERT-BOX INFO.  
