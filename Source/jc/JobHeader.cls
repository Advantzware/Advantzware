 
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
    
    METHOD PUBLIC LOGICAL SetContext (INPUT ipcCompany AS CHARACTER, INPUT ipcJobNo AS CHARACTER, INPUT ipiJobNo2 AS INTEGER, INPUT ipiFormNo AS INTEGER, INPUT ipiBlankNo AS INTEGER):
        lIsAvailable = FALSE.
        
        FIND FIRST bf-job NO-LOCK
             WHERE bf-job.company EQ ipcCompany
               AND bf-job.job-no  EQ ipcJobno
               AND bf-job.job-no2 EQ ipiJobNo2
             NO-ERROR.
        IF AVAILABLE bf-job THEN
            FIND FIRST bf-job-hdr NO-LOCK
                 WHERE bf-job-hdr.company  EQ ipcCompany
                   AND bf-job-hdr.job-no   EQ ipcJobNo
                   AND bf-job-hdr.job-no2  EQ ipiJobNo2
                   AND bf-job-hdr.frm      EQ ipiFormNo
                   AND bf-job-hdr.blank-no EQ ipiBlankNo
                 NO-ERROR.
        IF AVAILABLE bf-job-hdr THEN
            lIsAvailable = TRUE.        
    END METHOD.
    
	METHOD PUBLIC CHARACTER GetValue(INPUT ipcType AS CHARACTER):	    
	    
	    IF NOT AVAILABLE bf-job-hdr THEN
	       RETURN "".
	       	    
	    CASE ipcType:
	       WHEN "Job" THEN 
	           RETURN STRING(bf-job-hdr.job).
	       WHEN "JobNo" THEN
	           RETURN STRING(bf-job-hdr.job-no).
	       WHEN "JobNo2" THEN 
               RETURN STRING(bf-job-hdr.job-no2).
	       WHEN "Quantity" THEN 
	           RETURN STRING(bf-job-hdr.qty).	           
	       OTHERWISE
	           RETURN "".
	    END.
	END METHOD.

	METHOD PUBLIC LOGICAL IsAvailable ():
        RETURN lIsAvailable.	 
	END METHOD.

    
END CLASS.
