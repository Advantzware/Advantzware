 
 /*------------------------------------------------------------------------
    File        : jc/JobMaterial.cls
    Purpose     : To get and set the contexts of a job
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Mon Feb 22 12:52:45 EST 2022
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS jc.JobMaterial:
    DEFINE BUFFER bf-job-mat FOR job-mat.
    DEFINE BUFFER bf-job     FOR job.
    
    DEFINE PRIVATE VARIABLE lIsAvailable AS LOGICAL NO-UNDO.
    
	CONSTRUCTOR PUBLIC JobMaterial (INPUT ipcCompany AS CHARACTER, INPUT ipcJobNo AS CHARACTER, INPUT ipiJobNo2 AS INTEGER, INPUT ipiFormNo AS INTEGER, INPUT ipiBlankNo AS INTEGER):
        THIS-OBJECT:SetContext (
            INPUT ipcCompany,
            INPUT ipcJobNo,
            INPUT ipiJobNo2,
            INPUT ipiFormNo,
            INPUT ipiBlankNo
            ).
    END CONSTRUCTOR.
    
    CONSTRUCTOR PUBLIC JobMaterial():
    
    END CONSTRUCTOR.
    
    METHOD PUBLIC LOGICAL SetContext (INPUT ipcCompany AS CHARACTER, INPUT ipcJobNo AS CHARACTER, INPUT ipiJobNo2 AS INTEGER, INPUT ipiFormNo AS INTEGER, INPUT ipiBlankNo AS INTEGER):
        lIsAvailable = FALSE.

        FIND FIRST bf-job NO-LOCK
             WHERE bf-job.company EQ ipcCompany
               AND bf-job.job-no  EQ ipcJobno
               AND bf-job.job-no2 EQ ipiJobNo2
             NO-ERROR.
        IF AVAILABLE bf-job THEN
            FIND FIRST bf-job-mat NO-LOCK
                 WHERE bf-job-mat.company   EQ ipcCompany
                   AND bf-job-mat.job-no    EQ ipcJobNo
                   AND bf-job-mat.job-no2   EQ ipiJobNo2
                   AND (bf-job-mat.frm      EQ ipiFormNo  OR ipiFormNo  EQ ?)
                   AND (bf-job-mat.blank-no EQ ipiBlankNo OR ipiBlankNo EQ ?)
                 NO-ERROR.
        
        lIsAvailable = AVAILABLE bf-job AND AVAILABLE bf-job-mat.    

        RETURN lIsAvailable.    
    END METHOD.
    
	METHOD PUBLIC CHARACTER GetValue(INPUT ipcType AS CHARACTER):	    
	    
	    IF NOT AVAILABLE bf-job-mat THEN
	       RETURN "".
	       	    
	    CASE ipcType:
	        WHEN "Company" THEN
	            RETURN bf-job-mat.company. 
	        WHEN "Job" THEN 
	            RETURN STRING(bf-job-mat.job).
	        WHEN "JobNo" THEN
	            RETURN STRING(bf-job-mat.job-no).
	        WHEN "JobNo2" THEN 
                RETURN STRING(bf-job-mat.job-no2).
            WHEN "FormNo" THEN 
                RETURN STRING(bf-job-mat.frm).
            WHEN "BlankNo" THEN 
                RETURN STRING(bf-job-mat.blank-no).            
	        WHEN "Quantity" THEN 
	            RETURN STRING(bf-job-mat.qty).	
	        WHEN "FGItem" THEN
	            RETURN bf-job-mat.i-no.           
	        OTHERWISE
	            RETURN "".
	    END.
	END METHOD.

	METHOD PUBLIC LOGICAL IsAvailable ():
        RETURN AVAILABLE bf-job-mat.	 
	END METHOD.

    
END CLASS.
