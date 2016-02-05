/* print raw data file to printer */

DEF VAR prt-port AS cha NO-UNDO.
DEF VAR lv-file AS cha NO-UNDO.

lv-file = "c:/tmp/fax2.txt".
/*RUN custom/printapi.p (OUTPUT prt-port, OUTPUT prt-names).*/
RUN custom/d-print2.w (OUTPUT prt-port).





/*==== startprt 3.p ===*/
DEFINE VAR /*INPUT PARAMETER */ PrinterName AS CHAR NO-UNDO. /* As set in Printer properties */
DEFINE VAR /*INPUT PARAMETER*/ FILENAME AS CHAR NO-UNDO.
 
ASSIGN printername = prt-port
       FILENAME = lv-file.

DEFINE VARIABLE X AS INT NO-UNDO.
DEFINE VARIABLE hPrinter AS INT NO-UNDO.
DEFINE VARIABLE hFile AS INT NO-UNDO.
DEFINE VARIABLE pBuf AS MEMPTR NO-UNDO.
DEFINE VARIABLE FileSize AS INT NO-UNDO.
DEFINE VARIABLE iSize AS INT NO-UNDO.
DEFINE VARIABLE xSize AS INT NO-UNDO.
DEFINE VARIABLE pFileName AS MEMPTR NO-UNDO.
DEFINE VARIABLE OutFileName AS CHAR  NO-UNDO.
DEFINE VARIABLE pOutFileName AS MEMPTR NO-UNDO.
DEFINE VARIABLE DataType AS CHAR  NO-UNDO.
DEFINE VARIABLE pDataType AS MEMPTR NO-UNDO.
DEFINE VARIABLE pDocInfo AS MEMPTR NO-UNDO.
 
   RUN OpenPrinterA (PrinterName,OUTPUT hPrinter,0, OUTPUT X).
   MESSAGE "printer: " printername X VIEW-AS ALERT-BOX.
   IF X = 0
   THEN MESSAGE "Error opening printer: " PrinterName VIEW-AS ALERT-BOX.
   ELSE DO:
     RUN CreateFileA (FILENAME , -2147483648,0,0,3,128,0,OUTPUT hFile). /* -2147483648 = $80000000 */
     MESSAGE 2 hfile VIEW-AS ALERT-BOX.

     IF hFile = -1
     THEN MESSAGE "Error opening file: " FILENAME VIEW-AS ALERT-BOX.
     ELSE DO:
       RUN GetFileSize (hFile,0,OUTPUT FileSize).
       IF FileSize = -1
       THEN MESSAGE "Wrong file size" VIEW-AS ALERT-BOX.
       ELSE DO:
         SET-SIZE(pBuf) = FileSize.
 
         RUN ReadFile(hFile,pBuf,FileSize,OUTPUT iSize,0, OUTPUT X).
         IF X = 0
         THEN MESSAGE "Error reading file: " FILENAME VIEW-AS ALERT-BOX.
         ELSE DO:
           IF iSize = 0
           THEN MESSAGE "Attempt to read beyond end of file:" FILENAME VIEW-AS ALERT-BOX.
           ELSE DO:
             OutFileName = "".
             DataType = "RAW".
             SET-SIZE(pDocInfo) = 12.
             SET-SIZE(pFileName) = LENGTH(FILENAME) + 1.
             PUT-STRING(pFileName,1) = FILENAME.
             SET-SIZE(pOutFileName) = LENGTH(OutFileName) + 1.
             PUT-STRING(pOutFileName,1) = OutFileName.
             SET-SIZE(pDataType) = LENGTH(DataType) + 1.
             PUT-STRING(pDataType,1) = DataType.
             PUT-LONG(pDocInfo,1) = GET-POINTER-VALUE(pFileName).
             PUT-LONG(pDocInfo,5) = GET-POINTER-VALUE(pOutFileName).
             PUT-LONG(pDocInfo,9) = GET-POINTER-VALUE(pDataType).
 
             RUN StartDocPrinterA (hPrinter,1,pDocInfo,OUTPUT X).
             IF X = 0 THEN DO:
                 RUN GetLastError(OUTPUT X).
                 MESSAGE "Error : " X VIEW-AS ALERT-BOX.
             END.
 
             RUN WritePrinter(hPrinter,pBuf,iSize,OUTPUT xSize,OUTPUT X).
             IF X = 0 THEN DO:
                 RUN GetLastError(OUTPUT X).
                 MESSAGE "Error writing to printer: " PrinterName iSize xsize X VIEW-AS ALERT-BOX.
             END.
 
             RUN EndDocPrinter(hPrinter,OUTPUT X).
           END.
         END.
       END.
       RUN CloseHandle(hFile,OUTPUT X).
       IF X = 0 THEN MESSAGE "Error closing file: " FILENAME.
     END.
 
 
     RUN ClosePrinter(hPrinter,OUTPUT X).
     IF X = 0
     THEN MESSAGE "Error closing printer: " PrinterName VIEW-AS ALERT-BOX.
   END.
 
   SET-SIZE(pBuf) = 0.
   SET-SIZE(pDocInfo) = 0.
   SET-SIZE(pFileName) = 0.
   SET-SIZE(pOutFileName) = 0.
   SET-SIZE(pDataType) = 0.
/*==== end of main startprt 3.p ===*/

 
/******************/
/* DLL Procedures */
/******************/
PROCEDURE GetLastError EXTERNAL "kernel32.dll" :
    DEFINE RETURN PARAMETER X AS LONG.
END PROCEDURE.
 
PROCEDURE StartDocPrinterA EXTERNAL "winspool.drv" :
    DEFINE INPUT PARAMETER hPrinter AS LONG.
    DEFINE INPUT PARAMETER Level AS LONG.
    DEFINE INPUT PARAMETER pDocInfo AS MEMPTR.
    DEFINE RETURN PARAMETER X AS LONG.
END PROCEDURE.
 
PROCEDURE EndDocPrinter EXTERNAL "winspool.drv" :
    DEFINE INPUT PARAMETER hPrinter AS LONG.
    DEFINE RETURN PARAMETER X AS LONG.
END PROCEDURE.
 
PROCEDURE CreateFileA EXTERNAL "kernel32.dll" :
    DEFINE INPUT PARAMETER lpFileName AS CHAR.
    DEFINE INPUT PARAMETER dwDesiredAccess AS LONG.
    DEFINE INPUT PARAMETER dwShareMode AS LONG.
    DEFINE INPUT PARAMETER lpSecurityAttributes AS LONG.
    DEFINE INPUT PARAMETER dwCreationDistribution AS LONG.
    DEFINE INPUT PARAMETER dwFlagsAndAttributes AS LONG.
    DEFINE INPUT PARAMETER hTemplateFile AS LONG.
    DEFINE RETURN PARAMETER hFile AS LONG.
END PROCEDURE.
 
PROCEDURE ReadFile EXTERNAL "kernel32.dll" :
    DEFINE INPUT PARAMETER hFile AS LONG.
    DEFINE INPUT PARAMETER lpBuffer AS MEMPTR.
    DEFINE INPUT PARAMETER nNumberOfBytesToRead AS LONG.
    DEFINE OUTPUT PARAMETER  lpNumberOfBytesRead AS LONG.
    DEFINE INPUT PARAMETER lpOverlapped AS LONG.
    DEFINE RETURN PARAMETER X AS LONG.
END PROCEDURE.
 
PROCEDURE WritePrinter EXTERNAL "winspool.drv" :
    DEFINE INPUT PARAMETER hPrinter AS LONG.
    DEFINE INPUT PARAMETER  pBuf AS MEMPTR.
    DEFINE INPUT PARAMETER cbBuf AS LONG.
    DEFINE OUTPUT PARAMETER lpdwWritten AS LONG.
    DEFINE RETURN PARAMETER X AS LONG.
END PROCEDURE.
 
PROCEDURE OpenPrinterA EXTERNAL "winspool.drv" :
    DEFINE INPUT PARAMETER pPrinterName AS CHAR.
    DEFINE OUTPUT PARAMETER phPrinter AS LONG.
    DEFINE INPUT PARAMETER pDefault AS LONG.
    DEFINE RETURN PARAMETER X AS LONG.
END PROCEDURE.
 
PROCEDURE ClosePrinter EXTERNAL "winspool.drv" :
    DEFINE INPUT PARAMETER hPrinter AS LONG.
    DEFINE RETURN PARAMETER X AS LONG.
END PROCEDURE.
 
PROCEDURE GetFileSize EXTERNAL "kernel32.dll" :
    DEFINE INPUT PARAMETER hFile AS LONG.
    DEFINE INPUT PARAMETER lpFileSizeHigh AS LONG.
    DEFINE RETURN PARAMETER FileSize AS LONG.
END PROCEDURE.
 
PROCEDURE CloseHandle EXTERNAL "kernel32.dll" :
    DEFINE INPUT PARAMETER hObject AS LONG.
    DEFINE RETURN PARAMETER X AS LONG.
END PROCEDURE.

