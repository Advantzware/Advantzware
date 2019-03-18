/***************************************************************************************
Program Name : util/CurrDir.p
          By : Shashi On 1/1/507
Parameters   : Output Parameter chrDirectoryName - gives the current Directory         
****************************************************************************************/
DEFINE VARIABLE intBufferSize    AS INTEGER   NO-UNDO INITIAL 256 .
DEFINE VARIABLE intResult        AS INTEGER   NO-UNDO.
DEFINE VARIABLE ptrToString      AS MEMPTR    NO-UNDO. 
DEFINE OUTPUT PARAMETER chrDirectoryName AS CHARACTER FORMAT "X(256)" NO-UNDO.

PROCEDURE GetCurrentDirectoryA EXTERNAL "KERNEL32.DLL":    
  DEFINE INPUT        PARAMETER intBufferSize AS LONG.    
  DEFINE INPUT-OUTPUT PARAMETER ptrToString   AS MEMPTR.    
  DEFINE RETURN       PARAMETER intResult     AS SHORT.
END PROCEDURE. 

&IF DEFINED(FWD-VERSION) > 0 &THEN
ASSIGN chrDirectoryName = get-working-directory().
&ELSE

SET-SIZE(ptrToString) = 256. 

RUN GetCurrentDirectoryA (INPUT        intBufferSize,                          
													INPUT-OUTPUT ptrToString,                          
													OUTPUT       intResult). 

ASSIGN chrDirectoryName = GET-STRING(ptrToString,1). 
IF intResult = 0 THEN    
MESSAGE "Function call failed, not sure why" VIEW-AS ALERT-BOX.
ELSE IF intResult = LENGTH(chrDirectoryName) THEN .       
/* MESSAGE chrDirectoryName VIEW-AS ALERT-BOX.     */
ELSE MESSAGE "Buffer size is too small.  Must be at least " + STRING(intResult) VIEW-AS ALERT-BOX. 
SET-SIZE(ptrToString) = 0.
&ENDIF
