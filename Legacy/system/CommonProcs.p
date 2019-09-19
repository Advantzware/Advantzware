/*------------------------------------------------------------------------
    File        : CommonProcs.p
    Purpose     : 

    Syntax      :

    Description : This will hold procedures and functions like “RoundUp” and some other common,
                  non-DB related functions

    Author(s)   : Sewa Singh
    Created     : Thur sept 12 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Property Variables*/
            
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

 FUNCTION GetNumberOfDaysInMonth RETURNS INTEGER 
    (ipcMonth AS INTEGER) FORWARD.

/* ***************************  Main Block  *************************** */
/*Initialize Constants and Property Defaults*/

/* **********************  Internal Procedures  *********************** */


/* ************************  Function Implementations ***************** */

FUNCTION GetNumberOfDaysInMonth RETURNS INTEGER 
    ( ipcMonth AS INTEGER ):
    /*------------------------------------------------------------------------------
     Purpose: return day in a month
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cDaya AS INTEGER EXTENT 12 INIT [31,28,31,30,31,30,31,31,30,31,30,31] NO-UNDO .
    DEFINE VARIABLE iReturn AS INTEGER NO-UNDO .

    if (ipcMonth NE 2) THEN  iReturn =  cDaya[ipcMonth ].
    ELSE if (year(TODAY) / 4 NE 0) THEN  iReturn =  cDaya[2].
    ELSE if (year(TODAY) / 100 EQ 0 AND year(TODAY) / 400 NE 0) THEN  iReturn = cDaya[2].
    ELSE iReturn = cDaya[2] + 1.
 
    RETURN iReturn .   
		
END FUNCTION.


