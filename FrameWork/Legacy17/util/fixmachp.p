/* fixmachp.p  Create mach-attach-pat record from mach-attach*/

FOR EACH mach-attach NO-LOCK:
    /*DISP company att-type m-code style. */
    IF NOT CAN-FIND(mach-attach-pat OF mach-attach) THEN
    DO:
       CREATE mach-attach-pat.
       ASSIGN mach-attach-pat.company = mach-attach.company
              mach-attach-pat.att-type = mach-attach.att-type
              mach-attach-pat.m-code = mach-attach.m-code
              mach-attach-pat.style = mach-attach.style
              .

    END.
END.

MESSAGE "Initial Machine Attachement Partion records are created."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
