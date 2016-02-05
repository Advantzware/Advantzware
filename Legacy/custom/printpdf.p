/* pdf_print.p  YSK
prints selected pdf doc to designated printer for batch printing module
""AcroRd32.exe" /t "%1" "%2" "%3" "%4""
%1 - file name
%2 - windows printer name
%3 - printer driver
%4 - printer port                          */

 /*
  def Input Param ip-filename as char.  /* the full path */
*/
DEF VAR ip-filename AS cha NO-UNDO.

  def Var ip-printer as Char No-undo.  /* format \\xxx\xxx */
  Def Var lv-Command As Char No-undo.
  Def Var lv-int As Int No-undo.
  Def Var hold-printer As Char No-undo.
  Def Var lv-printer-port As Char No-undo.
  Def Var lv-printer-server As Char No-undo.
  Def Var lv-printer-name As Char No-undo.
  Def Var lv-section As Char No-undo.
  Def Var lv-printer-driver As Char No-undo.


  Assign
   lv-printer-name = "HP2100TN"
   ip-printer = "\\ntserver1\HP2100TN"
   lv-printer-driver = "HP LaserJet 2100 Series PCL 6" /*printerdriver*/
   lv-printer-port = "\\ntserver1\hp2100tn" 
   ip-filename = "c:\temp\accordfg.pdf"   .

    /* Get command for PDF printing */
    load "AcroExch.Document" BASE-KEY "HKEY_CLASSES_ROOT".
    use  "AcroExch.Document".
    get-key-value section "shell\printto\command"
                  key   default
                  VALUE lv-command.
    unload "AcroExch.Document".

    lv-command = REPLACE(lv-command, "%1", ip-filename).
    lv-command = REPLACE(lv-command, "%2", ip-printer).         /* Windows
printer name */
    lv-command = REPLACE(lv-command, "%3", lv-printer-driver).  /* Printer
driver  i.e. HP LaserJet 4 */
    lv-command = REPLACE(lv-command, "%4", lv-printer-port).    /* Printer port
i.e. Ne07: */

    /* Enclose in quotes */
    lv-command = "~"" + lv-command + "~"" + " /h".

   MESSAGE lv-command VIEW-AS ALERT-BOX.

       /* Print the pdf  */
   os-command no-wait value(lv-command).  

