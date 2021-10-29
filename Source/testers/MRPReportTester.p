
/*------------------------------------------------------------------------
    File        : MRPReportTester.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Fri Oct 08 15:07:33 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE ghSession AS HANDLE.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).

FIND FIRST eb NO-LOCK 
    WHERE eb.company EQ '001'
    AND eb.est-no EQ '  103093'.

RUN est\MRPReport.p(ROWID(eb)).