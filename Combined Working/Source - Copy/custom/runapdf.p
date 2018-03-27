/* custom/runapdf.p  Run Acrobat Reader 5.0 */
/*OS-COMMAND Does not support running a start *.pdf command when spaces are in the name*/

DEFINE OUTPUT PARAMETER opcCommand AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLicense AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAcrobatPath AS CHARACTER NO-UNDO.

   /*====  find from registry   ====== */
FUNCTION get-reg-data RETURNS CHARACTER ().
      cLicense = "Free".
      DEF VAR DATA AS CHARACTER NO-UNDO.
      LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
                      
      USE  "SOFTWARE".

      GET-KEY-VALUE SECTION "Adobe\Acrobat Reader\5.0\InstallPath"
                    KEY     DEFAULT                        
                    VALUE   DATA.
      UNLOAD "SOFTWARE".
      RETURN (DATA).                                          
END FUNCTION.

FUNCTION get-reg-data2 RETURNS CHARACTER ().
      cLicense = "Purchase".

      DEF VAR DATA AS CHARACTER NO-UNDO.
      LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
                      
      USE  "SOFTWARE".

      GET-KEY-VALUE SECTION "Adobe\Adobe Acrobat\5.0\InstallPath"
                    KEY     DEFAULT                        
                    VALUE   DATA.
      UNLOAD "SOFTWARE".
      RETURN (DATA).
END FUNCTION.

FUNCTION get-reg-data-V6 RETURNS CHARACTER ().
      cLicense = "Acrobat V6".

      DEF VAR DATA AS CHARACTER NO-UNDO.
      LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
                      
      USE  "SOFTWARE".

      GET-KEY-VALUE SECTION "Adobe\Acrobat Reader\6.0\InstallPath"
                    KEY     DEFAULT                        
                    VALUE   DATA.
      UNLOAD "SOFTWARE".
      RETURN (DATA).
END FUNCTION.

FUNCTION get-reg-data-V7 RETURNS CHARACTER ().
      cLicense = "Acrobat V7".

      DEF VAR DATA AS CHARACTER NO-UNDO.
      LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
                      
      USE  "SOFTWARE".

      GET-KEY-VALUE SECTION "Adobe\Acrobat Reader\7.0\InstallPath"
                    KEY     DEFAULT                        
                    VALUE   DATA.
      UNLOAD "SOFTWARE".
      RETURN (DATA).
END FUNCTION.

FUNCTION get-reg-data-V8 RETURNS CHARACTER ().
      cLicense = "Acrobat V8".

      DEF VAR DATA AS CHARACTER NO-UNDO.
      LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
                      
      USE  "SOFTWARE".

      GET-KEY-VALUE SECTION "Adobe\Acrobat Reader\8.0\InstallPath"
                    KEY     DEFAULT                        
                    VALUE   DATA.
      UNLOAD "SOFTWARE".
      RETURN (DATA).
END FUNCTION.

FUNCTION get-reg-data-V9 RETURNS CHARACTER ().
      cLicense = "Acrobat V9".

      DEF VAR DATA AS CHARACTER NO-UNDO.
      LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
                      
      USE  "SOFTWARE".

      GET-KEY-VALUE SECTION "Adobe\Acrobat Reader\9.0\InstallPath"
                    KEY     DEFAULT                        
                    VALUE   DATA.
      UNLOAD "SOFTWARE".
      RETURN (DATA).
END FUNCTION.

FUNCTION get-reg-data-V9Pro RETURNS CHARACTER ().
      
      cLicense = "purchase".

      DEF VAR DATA AS CHARACTER NO-UNDO.
      LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
                      
      USE  "SOFTWARE".

      GET-KEY-VALUE SECTION "Adobe\Adobe Acrobat\9.0\InstallPath"
                    KEY     DEFAULT                        
                    VALUE   DATA.
      UNLOAD "SOFTWARE".
      RETURN (DATA).
END FUNCTION.

FUNCTION get-reg-data-V10 RETURNS CHARACTER ().
      cLicense = "Acrobat V10".

      DEF VAR DATA AS CHARACTER NO-UNDO.
      LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
                      
      USE  "SOFTWARE".

      GET-KEY-VALUE SECTION "Adobe\Acrobat Reader\10.0\InstallPath"
                    KEY     DEFAULT                        
                    VALUE   DATA.
      UNLOAD "SOFTWARE".
      RETURN (DATA).
END FUNCTION.

FUNCTION get-reg-data-V10Pro RETURNS CHARACTER ().
      
      cLicense = "purchase".

      DEF VAR DATA AS CHARACTER NO-UNDO.
      LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
                      
      USE  "SOFTWARE".

      GET-KEY-VALUE SECTION "Adobe\Adobe Acrobat\10.0\InstallPath"
                    KEY     DEFAULT                        
                    VALUE   DATA.
      UNLOAD "SOFTWARE".
      RETURN (DATA).
END FUNCTION.

FUNCTION get-reg-data-V11 RETURNS CHARACTER ().
      cLicense = "Acrobat V11".

      DEF VAR DATA AS CHARACTER NO-UNDO.
      LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
                      
      USE  "SOFTWARE".

      GET-KEY-VALUE SECTION "Adobe\Acrobat Reader\11.0\InstallPath"
                    KEY     DEFAULT                        
                    VALUE   DATA.
      UNLOAD "SOFTWARE".
      RETURN (DATA).
END FUNCTION.

FUNCTION get-reg-data-V11Pro RETURNS CHARACTER ().
      
      cLicense = "purchase".

      DEF VAR DATA AS CHARACTER NO-UNDO.
      LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
                      
      USE  "SOFTWARE".

      GET-KEY-VALUE SECTION "Adobe\Adobe Acrobat\11.0\InstallPath"
                    KEY     DEFAULT                        
                    VALUE   DATA.
      UNLOAD "SOFTWARE".
      RETURN (DATA).
END FUNCTION.

FUNCTION get-reg-data-DC RETURNS CHARACTER ().
      cLicense = "DC".

      DEF VAR DATA AS CHARACTER NO-UNDO.
      LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
                      
      USE  "SOFTWARE".

      GET-KEY-VALUE SECTION "Classes\Software\Adobe\Acrobat\Exe"
                    KEY     DEFAULT                        
                    VALUE   DATA.
      UNLOAD "SOFTWARE".
      RETURN (DATA).
END FUNCTION.


cAcrobatPath = get-reg-data() .
IF cAcrobatPath = ? THEN DO:
   cAcrobatPath = get-reg-data2().
   IF cAcrobatPath = ? THEN cAcrobatPath = get-reg-data-DC().
   IF cAcrobatPath = ? THEN cAcrobatPath = get-reg-data-V11().
   IF cAcrobatPath = ? THEN cAcrobatPath = get-reg-data-V11Pro().
   IF cAcrobatPath = ? THEN cAcrobatPath = get-reg-data-V10().
   IF cAcrobatPath = ? THEN cAcrobatPath = get-reg-data-V10Pro().
   IF cAcrobatPath = ? THEN cAcrobatPath = get-reg-data-V9().
   IF cAcrobatPath = ? THEN cAcrobatPath = get-reg-data-V9Pro().
   IF cAcrobatPath = ? THEN cAcrobatPath = get-reg-data-V8().
   IF cAcrobatPath = ? THEN cAcrobatPath = get-reg-data-V7().
   IF cAcrobatPath = ? THEN cAcrobatPath = get-reg-data-V6().
   IF cAcrobatPath = ? THEN DO:
      /*MESSAGE "Acrobat Reader versions 5.0 through DC are supported." VIEW-AS ALERT-BOX ERROR.*/
      RETURN.
   END.
END.

IF cLicense EQ "purchase" THEN cAcrobatPath = cAcrobatPath + '\Acrobat.exe'.
ELSE IF cLicense NE "DC" THEN cAcrobatPath = cAcrobatPath + '\AcroRd32.exe'.
ELSE cAcrobatPath = TRIM(cAcrobatPath,'"').

opcCommand = ('"' + cAcrobatPath + '"') .
