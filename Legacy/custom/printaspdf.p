/*Prints text report to pdf using pdfcamp */

DEFINE INPUT PARAMETER p_File AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p_FontNumber AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p_PageLayout AS CHARACTER NO-UNDO.

DEFINE VARIABLE result AS INTEGER.
DEFINE VARIABLE oldPrinter AS CHARACTER.
DEFINE VARIABLE p_PageSize    AS INTEGER   NO-UNDO INITIAL 20.
DEFINE VARIABLE p_PageCount   AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE p_Printed     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lv_PageLayout AS INTEGER NO-UNDO.
define variable WshNetwork as com-handle.

IF p_PageLayout BEGINS "L" THEN lv_PageLayout = 2.
ELSE lv_PageLayout = 0.

/* Get original printer settings */
RUN getKey ("Windows", "Device", "", OUTPUT oldPrinter).

CREATE "WScript.Network" WshNetwork NO-ERROR.
  IF NOT(VALID-HANDLE(WshNetwork)) THEN
  DO :
    MESSAGE "Unable to Create Wscript.Network" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.

WshNetwork:SetDefaultPrinter("PDFCamp Printer").

RELEASE OBJECT WshNetwork NO-ERROR.

SESSION:PRINTER-CONTROL-HANDLE = 0.

RUN adecomm\_osprint.p (INPUT  ?,
                        INPUT  p_File,
                        INPUT  p_FontNumber,                             
                        INPUT  lv_PageLayout,
                        INPUT  p_PageSize,                            
                        INPUT  p_PageCount,                            
                        OUTPUT p_Printed). 

/* Reset to original printer settings */
RUN setKey ("Windows", "Device", oldPrinter).

PROCEDURE GetKey:
DEFINE INPUT PARAMETER pSection AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pEntry AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pDefault AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER pString AS CHAR NO-UNDO.
DEFINE VAR result AS INT NO-UNDO.
DEFINE VAR wbuf AS MEMPTR NO-UNDO.

SET-SIZE(wbuf) = 255.
RUN GetProfileStringA(pSection,pEntry,pDefault,wbuf,254,OUTPUT result).
IF result = 0 THEN
   pString = ?.
ELSE
   pString = GET-STRING(wbuf,1).

SET-SIZE(wbuf) = 0.
END PROCEDURE.

PROCEDURE SetKey:
DEFINE INPUT PARAMETER pSection AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pEntry AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pString AS CHAR NO-UNDO.
RUN WriteProfileStringA(pSection,pEntry,pString, OUTPUT result).
END PROCEDURE.

PROCEDURE GetProfileStringA EXTERNAL "KERNEL32.DLL":
DEFINE INPUT PARAMETER lpszSection AS CHAR. /* address of section    */
DEFINE INPUT PARAMETER lpszEntry AS CHAR.    /* address of entry    */
DEFINE INPUT PARAMETER lpszDefault AS CHAR.
DEFINE INPUT PARAMETER lpszReturnBuffer AS MEMPTR.
DEFINE INPUT PARAMETER cbReturnBuffer AS LONG.
DEFINE RETURN PARAMETER result AS LONG.
END.

PROCEDURE WriteProfileStringA EXTERNAL "KERNEL32.DLL":
DEFINE INPUT PARAMETER lpszSection AS CHAR.
DEFINE INPUT PARAMETER lpszEntry AS CHAR.   
DEFINE INPUT PARAMETER lpszString AS CHAR.
DEFINE RETURN PARAMETER result AS LONG.
END.
