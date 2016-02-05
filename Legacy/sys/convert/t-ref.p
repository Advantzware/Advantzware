/* t-ref.p  create records for stack pattern image */
        
  DEF BUFFER bf-ref FOR reftable.
  def buffer strap for reftable.

  FOR EACH reftable WHERE TRUE
      AND reftable.reftable = "STACK" and
          ASI.reftable.company = ""
          AND reftable.loc = "" .

    DISP CODE.
    CREATE bf-ref.
    ASSIGN bf-ref.reftable = "STACKPAT"
           bf-ref.CODE = reftable.CODE
           bf-ref.company = ""
           bf-ref.loc = "".

    find first strap where strap.reftable = "stackstrap" and
        strap.code = reftable.code no-error.
    if not available strap then do:
           create strap.
           assign strap.reftable = "STACKSTRAP"
               strap.company = ""
               strap.loc = ""
               strap.code = reftable.code.
               
    end.
  END.
