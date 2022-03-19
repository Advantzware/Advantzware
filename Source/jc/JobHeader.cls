 
 /*------------------------------------------------------------------------
    File        : jc/JobHeader.cls
    Purpose     : To get and set the contexts of a job
    Syntax      : 
    Description : 
    Author(s)   : Rahul Rawat
    Created     : Tue Dec 22 02:23:46 EST 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS jc.JobHeader:
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-job     FOR job.
    
    DEFINE PRIVATE VARIABLE lIsAvailable AS LOGICAL NO-UNDO.
    
	CONSTRUCTOR PUBLIC JobHeader (INPUT ipcCompany AS CHARACTER, INPUT ipcJobNo AS CHARACTER, INPUT ipiJobNo2 AS INTEGER, INPUT ipiFormNo AS INTEGER, INPUT ipiBlankNo AS INTEGER):
        THIS-OBJECT:SetContext (
            INPUT ipcCompany,
            INPUT ipcJobNo,
            INPUT ipiJobNo2,
            INPUT ipiFormNo,
            INPUT ipiBlankNo
            ).
    END CONSTRUCTOR.
    
    CONSTRUCTOR PUBLIC JobHeader():
    
    END CONSTRUCTOR.
    
    METHOD PUBLIC LOGICAL SetContext (INPUT ipcCompany AS CHARACTER, INPUT ipcJobNo AS CHARACTER, INPUT ipiJobNo2 AS INTEGER, INPUT ipiFormNo AS INTEGER, INPUT ipiBlankNo AS INTEGER):
        lIsAvailable = FALSE.

        FIND FIRST bf-job NO-LOCK
             WHERE bf-job.company EQ ipcCompany
               AND TRIM(bf-job.job-no) EQ TRIM(ipcJobno)
               AND bf-job.job-no2 EQ ipiJobNo2
             NO-ERROR.
        IF AVAILABLE bf-job THEN
            FIND FIRST bf-job-hdr NO-LOCK
                 WHERE bf-job-hdr.company   EQ ipcCompany
                   AND bf-job-hdr.job-no    EQ bf-job.job-no
                   AND bf-job-hdr.job-no2   EQ bf-job.job-no2
                   AND (bf-job-hdr.frm      EQ ipiFormNo  OR ipiFormNo  EQ ?)
                   AND (bf-job-hdr.blank-no EQ ipiBlankNo OR ipiBlankNo EQ ?)
                 NO-ERROR.
        
        lIsAvailable = AVAILABLE bf-job AND AVAILABLE bf-job-hdr.    

        RETURN lIsAvailable.    
    END METHOD.
    
	METHOD PUBLIC CHARACTER GetValue(INPUT ipcType AS CHARACTER):	    
	    
	    IF NOT AVAILABLE bf-job-hdr THEN
	       RETURN "".
	       	    
	    CASE ipcType:
	        WHEN "Company" THEN
	            RETURN bf-job-hdr.company. 
	        WHEN "Job" THEN 
	            RETURN STRING(bf-job-hdr.job).
	        WHEN "JobNo" THEN
	            RETURN STRING(bf-job-hdr.job-no).
	        WHEN "JobNo2" THEN 
                RETURN STRING(bf-job-hdr.job-no2).
            WHEN "FormNo" THEN 
                RETURN STRING(bf-job-hdr.frm).
            WHEN "BlankNo" THEN 
                RETURN STRING(bf-job-hdr.blank-no).            
	        WHEN "Quantity" THEN 
	            RETURN STRING(bf-job-hdr.qty).	
	        WHEN "FGItem" THEN
	            RETURN bf-job-hdr.i-no.           
	        WHEN "Customer" THEN
	            RETURN bf-job-hdr.cust-no.
	        OTHERWISE
	            RETURN "".
	    END.
	END METHOD.

	METHOD PUBLIC LOGICAL IsAvailable ():
        RETURN AVAILABLE bf-job-hdr.	 
	END METHOD.

    
END CLASS.
