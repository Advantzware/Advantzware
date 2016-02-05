
def var ou-log like sys-ctrl.log-fld INIT NO no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CustomerList"
    no-lock no-error.
if avail sys-ctrl then do:
    FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE 
        sys-ctrl-shipto.char-fld = "OU1" NO-LOCK NO-ERROR.
    
    IF NOT AVAIL sys-ctrl-shipto THEN do:

        create sys-ctrl-shipto .
        assign
            sys-ctrl-shipto.company = cocode
            sys-ctrl-shipto.name = "CustomerList"
            sys-ctrl-shipto.module = sys-ctrl.module
            sys-ctrl-shipto.char-fld = "OU1"
            sys-ctrl-shipto.log-fld  = no           .
    end.
ou-log = sys-ctrl-shipto.log-fld.
END.


