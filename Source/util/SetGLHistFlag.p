/*------------------------------------------------------------------------
    File        : util/SetGLHistFlag.p
    Purpose     : populate year field in GLHist and fix posted field in old data
    Syntax      : 
    Description :  
    Author(s)   : Sewa Singh, Mark Tyndall
    Created     : 03.01.2021
    Notes       :   Updated 21.04.20 - MYT
  ----------------------------------------------------------------------*/
    DEFINE TEMP-TABLE ttPeriod LIKE period.
    DEFINE TEMP-TABLE ttOpenPeriodStartDateByCompany
        FIELD company LIKE period.company
        FIELD daStartDate LIKE period.pst.
            
    DEFINE BUFFER b-glhist FOR glhist.
    
    DEFINE VARIABLE iLocked AS INT NO-UNDO.
        
    /* Optional - prevents create of audit records */
    DISABLE TRIGGERS FOR LOAD OF glhist.
    DISABLE TRIGGERS FOR LOAD OF b-glhist.
        
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
            IF ttPeriod.pstat EQ TRUE THEN 
            DO:
                ASSIGN 
                    ttOpenPeriodStartDateByCompany.daStartDate = ttPeriod.pst.
                NEXT companyLoop.
            END.
        END.
    END.  

    /* Only process records where the glyear has not been set OR record is not posted 
       Otherwise, this will try to lock and process EVERY glhist */
    FOR EACH b-glhist EXCLUSIVE WHERE
        b-glhist.glYear EQ 0 OR 
        b-glhist.posted EQ NO:
       
        FIND FIRST ttPeriod NO-LOCK
            WHERE ttperiod.company EQ b-glhist.company
            AND ttPeriod.pst  LE b-glhist.tr-date
            AND ttPeriod.pend GE b-glhist.tr-date          
            NO-ERROR.             
         IF AVAIL ttPeriod THEN ASSIGN  
            b-glhist.glYear = ttPeriod.yr.
         
         FIND FIRST ttOpenPeriodStartDateByCompany NO-LOCK
            WHERE ttOpenPeriodStartDateByCompany.company EQ b-glhist.company
            NO-ERROR.
         IF (AVAIL ttOpenPeriodStartDateByCompany AND b-glhist.tr-date LT ttOpenPeriodStartDateByCompany.daStartDate) 
         OR NOT AVAIL ttOpenPeriodStartDateByCompany THEN ASSIGN
             b-glhist.posted = YES
             b-glhist.entryType = "A".      
         
    END.
    RELEASE b-glhist.
