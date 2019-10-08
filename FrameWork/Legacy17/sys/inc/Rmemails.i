/*************************************************************************
 *  Programe Name :  sys/inc/Rmemails.i                                 * 
    Author        :  Amit Jadon                                          * 
    Date          :  06/19/2008                                          * 
    Desc          :  Include file to crate System code parameter for     * 
                     RMPost                                              * 
    Modify By     :                                                      *
*************************************************************************/

def var rmemails like sys-ctrl.char-fld no-undo.
DEF VAR rmemail-dlg-box AS LOG NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "RMEMAILS"
                      no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "RMEMAILS"
   sys-ctrl.log-fld  = yes
   sys-ctrl.char-fld = "NONE"
   sys-ctrl.descrip  = "RM post to automatically Email Customer Service for Hot Customers.".
end.

ASSIGN
   rmemails = sys-ctrl.char-fld
   rmemail-dlg-box = sys-ctrl.log-fld.
