/*----------------------------------------------------------------------
Program     : custom/getregvalue.p

Description : Wrapper for lookup of value from windows registry

Copyright(c): Advanced Software Services Inc. 2013
Author      : Wade Kaldawi
Created     : 03/06/2013 
Notes       :

Sample of Usage:

/*     RUN custom/getregvalue.p (INPUT "HKEY_LOCAL_MACHINE",   */
/*                               INPUT "SOFTWARE",             */
/*                               INPUT "Teklynx\Label Matrix", */
/*                               INPUT "PATH",                 */
/*                               OUTPUT cPath).                */
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions     ----------------------------------------- */
DEF INPUT PARAMETER ipcBaseKey AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcFolder  AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcSection AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcKey     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER opcValue  AS CHAR NO-UNDO.

/* Local Variable Definitions ----------------------------------------- */

/* Include Files              ----------------------------------------- */

/* Function Forwards          ----------------------------------------- */

/* ***************************  Main Block  *************************** */

LOAD ipcFolder BASE-KEY ipcBaseKey.

USE ipcFolder.

IF ipcKey EQ "" THEN
    GET-KEY-VALUE SECTION ipcSection
      KEY DEFAULT
      VALUE opcValue.
ELSE  
    GET-KEY-VALUE SECTION ipcSection
      KEY ipcKey
      VALUE opcValue.

UNLOAD ipcFolder.


/* **********************  Internal Procedures  *********************** */
