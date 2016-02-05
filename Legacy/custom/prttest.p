{custom/windows.i}  /* March 28, 1998 or later */
 
DEF VAR hKey        AS INTEGER NO-UNDO.
DEF VAR hPrinterkey AS INTEGER NO-UNDO.
DEF VAR subkey      AS CHAR    NO-UNDO.
DEF VAR port        AS MEMPTR  NO-UNDO.
DEF VAR lth         AS INTEGER NO-UNDO.
DEF VAR reslt       AS INTEGER NO-UNDO.
DEF VAR datatype    AS INTEGER NO-UNDO.
DEF VAR ITEM        AS INTEGER NO-UNDO.
 
RUN RegOpenKeyA IN hpApi( {&HKEY_LOCAL_MACHINE},
                          "System\CurrentControlSet\control\Print\Printers",
                          OUTPUT hKey,
                          OUTPUT reslt).
 
ASSIGN ITEM  = 0
       reslt = 0.
 
DO WHILE reslt NE {&ERROR_NO_MORE_ITEMS} :
 
   ASSIGN lth     = {&MAX_PATH} + 1
          subkey  = FILL("x", lth).
 
   RUN RegEnumKeyA IN hpApi (hKey, 
                             ITEM, 
                             OUTPUT subkey, 
                             INPUT LENGTH(subkey), 
                             OUTPUT reslt).
 
   IF reslt NE {&ERROR_NO_MORE_ITEMS} THEN DO:
 
      /* get the printer port (or description..) */
      RUN RegOpenKeyA IN hpApi ( hKey,
                                 subkey,
                                 OUTPUT hPrinterkey,
                                 OUTPUT reslt).
      lth  = {&MAX_PATH} + 1.
      SET-SIZE(port) = lth.
      RUN RegQueryValueExA IN hpApi (hPrinterkey,
                                     "port",
                                     0,  /* reserved, must be 0 */
                                     OUTPUT datatype,
                                     GET-POINTER-VALUE(port),
                                     INPUT-OUTPUT lth,
                                     OUTPUT reslt).
      RUN RegCloseKey IN hpApi (hPrinterkey,OUTPUT reslt).
 
      MESSAGE "printer name=" subkey SKIP 
              "port="         GET-STRING(port,1)
              VIEW-AS ALERT-BOX.
 
   END.
 
   ITEM = ITEM + 1.
END. /* do while not ERROR_NO_MORE_ITEMS */
 
SET-SIZE(port)=0.       
RUN RegCloseKey IN hpApi (hKey,OUTPUT reslt).

 
