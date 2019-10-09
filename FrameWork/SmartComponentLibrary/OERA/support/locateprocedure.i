/*************************************************************/
/* Copyright (c) 2007 by Progress Software Corporation      */
/*************************************************************/
/*------------------------------------------------------------------------
    File        : locateprocedure.i
    Purpose     : locate peristent procedure or start one that is locatable
    Description : defines a locateProcedure function that locates or starts 
                  a persistent procedure.  
    Notes       : Avoids loop thru session.
                - Allows separate instances of same procedure not started
                  by this.
                - Requires isRunning with this-procedure as output 
                  parameter being implemented in the procedure we're searching
  ----------------------------------------------------------------------*/
function locateProcedure returns handle private
  (pcName as character):
    
  define variable hProc as handle    no-undo.
  define variable cMsg  as character no-undo.
  cMsg = "isRunning" + pcName.
  publish cMsg (output hProc).
  if not valid-handle(hProc) then
  do on stop undo, return ?:   
    run value("{&OERASI}/" + pcName + ".p") persistent set hProc.
    subscribe procedure hProc to cMsg anywhere run-procedure "isRunning".       
  end.  
  return hProc.  
end.  
 
 