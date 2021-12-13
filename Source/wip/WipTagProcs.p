
/*------------------------------------------------------------------------
    File        : wipTagProcs.p
    Purpose     : 

    Syntax      :

    Description : Builds the wiptag Printout report		

    Author(s)   : Sewa Singh
    Created     : Fri Dec 03 15:41:37 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{Inventory/ttPrintInventoryStock.i}

DEFINE VARIABLE init-dir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCountPallet AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCompany     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation    AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmp-dir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE list-name    AS CHARACTER NO-UNDO.

RUN spGetSessionParam("Company", OUTPUT cCompany).
RUN spGetSessionParam("Location", OUTPUT cLocation). 
{custom/xprint.i}

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */
  
    
/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */

PROCEDURE pPrintView:
    DEFINE INPUT PARAMETER TABLE FOR ttPrintInventoryStock.
    DEFINE INPUT PARAMETER ipcCasLabel AS CHARACTER .

    DEFINE VARIABLE cEmail         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPhone         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFax           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lBussFormModle AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE ls-full-img1   AS CHARACTER NO-UNDO FORMAT "x(200)".

    RUN pGetNk1Setting(OUTPUT lBussFormModle, OUTPUT ls-full-img1).
         
    {sys/inc/print1.i}
    {sys/inc/outprint.i value(85)}
    
    SESSION:SET-WAIT-STATE ("general").
   
    
    IF NOT lBussFormModle THEN
        PUT "<PREVIEW><MODAL=NO></PROGRESS>" FORM "x(50)".
    ELSE
        PUT "<PREVIEW></PROGRESS>" FORM "x(50)".      

    iCountPallet = 0 .
    FOR EACH ttPrintInventoryStock NO-LOCK BREAK
        BY ttPrintInventoryStock.jobID 
        BY ttPrintInventoryStock.rmItemID:
                                 
                                 
        FIND FIRST loadtag NO-LOCK
            WHERE loadtag.company   EQ cCompany
            AND loadtag.item-type EQ YES
            AND loadtag.tag-no EQ TRIM(ttPrintInventoryStock.tag)
            USE-INDEX tag NO-ERROR.
        iCountPallet = iCountPallet + 1 .
                                
        IF ipcCasLabel EQ "wiptag.xpr" THEN 
        DO:   
            {wip/wipTagXprnt.i}
        END.                     
        ELSE  
        DO:
            {wip/wipTagXprnt.i}
        END.
    
        IF NOT LAST(ttPrintInventoryStock.rmItemID) THEN PAGE .
    END.
   
    OUTPUT CLOSE.
    SESSION:SET-WAIT-STATE ("").

    FILE-INFO:FILE-NAME = list-name.
    RUN printfile (FILE-INFO:FILE-NAME).
 
        
END PROCEDURE.

PROCEDURE pGetNk1Setting:
    DEFINE OUTPUT PARAMETER oplBusinessFormModal AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opclsFullImg1        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRtnChar  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFound    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE ls-image1 AS CHARACTER NO-UNDO.

    RUN sys/ref/nk1look.p (INPUT cCompany, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cRtnChar, OUTPUT lRecFound).

    IF lRecFound THEN
        oplBusinessFormModal = LOGICAL(cRtnChar) NO-ERROR.  
    RUN sys/ref/nk1look.p (INPUT cCompany,
        INPUT "LoadTagXprintImage",
        INPUT "C",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT ls-image1,
        OUTPUT lFound).

    FILE-INFO:FILE-NAME = ls-image1.
    opclsFullImg1 = FILE-INFO:FULL-PATHNAME + ">".


END PROCEDURE.




/* ************************  Function Implementations ***************** */




