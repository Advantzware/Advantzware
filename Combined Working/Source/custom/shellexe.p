
/* pdf_print_scrn.p
   prints selected pdf doc to screen using adobe reader */
   
 Def var vTempFile As Char No-undo.
 Def Var tInt As Int No-undo.
 vtempfile = "c:\tmp\orderbal.pdf".

 Run ShellExecuteA (0,"print":u,vTempFile,"","",1,output tInt).


 PROCEDURE ShellExecuteA EXTERNAL "shell32":u :
      define input parameter hwnd as long.
      define input parameter lpOperation as char.
      define input parameter lpFile as char.
      define input parameter lpParameters as char.
      define input parameter lpDirectory as char.
      define input parameter nShowCmd as long.
      define return parameter hInstance as long.
 END PROCEDURE.
