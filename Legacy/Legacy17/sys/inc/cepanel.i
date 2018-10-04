
def var v-int as int no-undo.
def var v-panel as int no-undo.
def var v-cepanel-log like sys-ctrl.log-fld no-undo.
def var v-cepanel-cha like sys-ctrl.char-fld no-undo.
def var v-prev as char no-undo.
def var v-next as char no-undo.
def var v-dec as dec no-undo.

do transaction:
  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "CEPANEL"
      no-error.
  if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "CEPANEL"
     sys-ctrl.log-fld  = no
     sys-ctrl.descrip  = "Check Panel sizes against minimums on Machine File".
   
    
  end.

  if lookup(sys-ctrl.char-fld,"WminLmin,PminPmax") eq 0 then
    sys-ctrl.char-fld = if sys-ctrl.char-fld eq ""      or
                           sys-ctrl.char-fld eq "Score" then "WminLmin"
                                                        else "PminPmax".

  assign
   v-cepanel-log = sys-ctrl.log-fld
   v-cepanel-cha = sys-ctrl.char-fld.

  release sys-ctrl.
end.

