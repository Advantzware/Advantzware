/* custom/runlmw.p  Run Labal Matrix */

/* run acrobat reader

DEFINE VARIABLE cAcroRead AS CHARACTER  NO-UNDO
     INITIAL '"C:\Program Files\Adobe\Acrobat 5.0\Reader\AcroRd32.exe" /t
    "%1" "%2" "%3" "%4"'.
    /* The correct value for cAcroRead can be retrieved from registry key
        HKEY_CLASSES_ROOT\AcroExch.Document\shell\printto\command */

    cAcroRead = REPLACE(cAcroRead,'%1','C:\Docs\start.pdf'). /* PDF file
                */
    cAcroRead = REPLACE(cAcroRead,'%2','Insert Name Here').  /* Windows
    printer name */
    cAcroRead = REPLACE(cAcroRead,'%3','').                  /* Printer
    driver       */
    cAcroRead = REPLACE(cAcroRead,'%4','').                  /* Printer
    port         */

    OS-COMMAND NO-CONSOLE  VALUE(cAcroRead).
*/

DEF outPUT param op-cmd AS cha NO-UNDO.
DEF VAR lv-liscence AS cha NO-UNDO.

   /*====  find from registry   ====== */
FUNCTION get-reg-data RETURNS CHARACTER ().
      /* lmw.exe %1 */
      DEF VAR DATA AS CHARACTER NO-UNDO.
      LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
                      
      USE  "SOFTWARE".

      GET-KEY-VALUE SECTION "classes\Label Matrix Document\shell\open\command"
                    KEY     DEFAULT                        
                    VALUE   DATA.
      UNLOAD "SOFTWARE".
      RETURN (DATA).                                          
END FUNCTION.

FUNCTION get-reg-data2 RETURNS CHARACTER ().
      /* lmwprint.exe %1 */

      DEF VAR DATA AS CHARACTER NO-UNDO.
      LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
                      
      USE  "SOFTWARE".

      GET-KEY-VALUE SECTION "classes\Label Matrix Document\shell\print\command"
                    KEY     DEFAULT                        
                    VALUE   DATA.
      UNLOAD "SOFTWARE".
      RETURN (DATA).
END FUNCTION.


DEF VAR v-cmd-path AS cha NO-UNDO.

v-cmd-path = get-reg-data() .
IF v-cmd-path = ? THEN 
IF SEARCH("c:\program files\lmw32\lmw.exe") <> ? THEN v-cmd-path = '"c:\program files\lmw32\lmw.exe"'.

IF v-cmd-path = ? THEN DO:
/*   v-cmd-path = get-reg-data2().
   IF v-cmd-path = ? THEN v-cmd-path = get-reg-data-V6().
   IF v-cmd-path = ? THEN do:
     
   END.
   */
    
    MESSAGE "Please install Label Matrix 7.0 or later and try again... " VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

IF INDEX(v-cmd-path,"%") > 0 THEN
   v-cmd-path = SUBSTRING(v-cmd-path,1,INDEX(v-cmd-path,"%") - 1).

/*
OS-COMMAND VALUE('"' + '"' + v-cmd-path + op-cmd + '"' + " r:\asi_gui9\source\boximage\accordfg.pdf" + '"' ).
*/


op-cmd = v-cmd-path  /*('"' + v-cmd-path + '"') */ .
/*=== */
