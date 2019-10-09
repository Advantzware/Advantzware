/*------------------------------------------------------------------------
    File        : startsuper.i
    Purpose     : 
    Description : startsuper function  
    Author(s)   : hdaniels
    Created     : Wed May 23 09:18:14 EDT 2007
    Notes       :
  ----------------------------------------------------------------------*/

{ {&OERASI}/locateprocedure.i }

function startSuper returns logical private
  (pcProcedure as character):
  define variable hProc as handle    no-undo.
  hProc = locateProcedure(pcProcedure).
  this-procedure:add-super-procedure(hProc).          
end.  

