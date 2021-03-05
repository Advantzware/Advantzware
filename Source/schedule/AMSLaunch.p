
/*------------------------------------------------------------------------
    File        : AMSLaunch.p
    Purpose     : 

    Syntax      :

    Description : For launching AMS Scheduling, SSO	

    Author(s)   : BV
    Created     : Thu Mar 04 23:58:50 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cRunURL AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetURL RETURNS CHARACTER PRIVATE
	() FORWARD.


/* ***************************  Main Block  *************************** */

cRunURL = fGetURL().

IF cRunURL NE "" THEN 
    OS-COMMAND NO-WAIT START VALUE(cRunURL).



/* ************************  Function Implementations ***************** */


FUNCTION fGetURL RETURNS CHARACTER PRIVATE
  ():
    /*------------------------------------------------------------------------------
     Purpose: Returns NK1 value of the JobQueueURL setting
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cURL   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserID AS CHARACTER NO-UNDO.
    
    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    RUN spGetSessionParam ("UserID", OUTPUT cUserID).
    RUN sys/ref/nk1look.p (
        cCompany,"JobQueueURL","C",NO,NO,"","",
        OUTPUT cURL, OUTPUT lFound).
    IF cUserID NE "" THEN 
        cURL = cURL + "?UserId=" + cUserID.
        
    RETURN cURL.
		
END FUNCTION.

