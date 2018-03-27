/*
 
ID: P3688 
Title: "How to set the default printer without using SYSTEM-DIALOG PRINTER-SETUP" 
Created: 20-Jun-2002             Last Modified: 3-Mar-2004 
Status: Verified 

Goal(s): 
How to set the default printer without using SYSTEM-DIALOG PRINTER-SETUP 
 

Fix:  
The following sample code shows how to call the following MS-Windows API functions: GetProfileStringA, WriteProfileStringA and SendMessageA.

-GetProfileStringA is used just to exemplify this function.

-WriteProfileStringA is used to set the printer DEVICE of the WINDOWS section in the WIN.INI file. The DEVICE information is available in the PRINTERPORTS section.

-SendMessageA is used to notify all others application that this section has been changed.

-SESSION:PRINTER-CONTROL-HANDLE = 0, says Progress to use the the default MS-Windows printer. If the SESSION:PRINTER-CONTROL-HANDLE is nonzero Progress will use the actual printer context set by the SYSTEM-DIALOG PRINTER-SETUP statement.
*/

&SCOPED-DEFINE HWND_BROADCAST 65535
&SCOPED-DEFINE WM_WININICHANGE 26
DEF VAR result AS INT.
DEF VAR X AS CHAR FORMAT "X(50)".

RUN SetKey("WINDOWS","DEVICE","BitFax Driver,BFDRV,COM2:").
RUN GetKey("WINDOWS","DEVICE",?,OUTPUT X).
RUN SendMessageA( {&HWND_BROADCAST},
                  {&WM_WININICHANGE},0,"WINDOWS",
                  OUTPUT result).
MESSAGE X "SM:". SESSION:PRINTER-CONTROL-HANDLE = 0.
DISPLAY SESSION:PRINTER-CONTROL-HANDLE
        SESSION:PRINTER-NAME FORMAT "X(50)".

PROCEDURE SendMessageA EXTERNAL "USER32.DLL":
    DEF INPUT PARAMETER p1 AS LONG.
    DEF INPUT PARAMETER p2 AS LONG.
    DEF INPUT PARAMETER p3 AS LONG.
    DEF INPUT PARAMETER p4 AS CHAR.
    DEF RETURN PARAMETER result AS LONG.
END.

PROCEDURE GetKey:
    DEF INPUT PARAMETER pSection AS CHAR NO-UNDO.
    DEF INPUT PARAMETER pEntry AS CHAR NO-UNDO.
    DEF INPUT PARAMETER pDefault AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER pString AS CHAR NO-UNDO.

    DEF VAR result AS INT NO-UNDO.
    DEF VAR wbuf AS MEMPTR NO-UNDO.

    SET-SIZE(wbuf) = 255.

    RUN GetProfileStringA(pSection,pEntry,pDefault,wbuf,254,
                          OUTPUT result).

    IF result = 0 THEN pString = ?.
    ELSE pString = GET-STRING(wbuf,1).

    SET-SIZE(wbuf) = 0.
END PROCEDURE.

PROCEDURE SetKey:
    DEF INPUT PARAMETER pSection AS CHAR NO-UNDO.
    DEF INPUT PARAMETER pEntry AS CHAR NO-UNDO.
    DEF INPUT PARAMETER pString AS CHAR NO-UNDO.

    RUN WriteProfileStringA(pSection,pEntry,pString,
                            OUTPUT result).
END PROCEDURE.

PROCEDURE GetProfileStringA EXTERNAL "KERNEL32.DLL":
    DEF INPUT PARAMETER lpszSection AS CHAR.
    /* address of section    */ 
    DEF INPUT PARAMETER lpszEntry AS CHAR.    
    /* address of entry    */ 
    DEF INPUT PARAMETER lpszDefault AS CHAR. 
    DEF INPUT PARAMETER lpszReturnBuffer AS MEMPTR. 
    DEF INPUT PARAMETER cbReturnBuffer AS LONG.
    DEF RETURN PARAMETER result AS LONG.
END.

PROCEDURE WriteProfileStringA EXTERNAL "KERNEL32.DLL":
    DEF INPUT PARAMETER lpszSection AS CHAR.
    DEF INPUT PARAMETER lpszEntry AS CHAR.    
    DEF INPUT PARAMETER lpszString AS CHAR.
    DEF RETURN PARAMETER result AS LONG.
END.

 
