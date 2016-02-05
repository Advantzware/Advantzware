/* Local delete for po-ord */
FOR EACH po-ordl WHERE po-ordl.company EQ po-ord.company
   AND po-ordl.po-no EQ po-ord.po-no 
   NO-LOCK:
    FOR EACH  reftable 
      where reftable.reftable eq "ORDERPO"
        AND reftable.company  eq po-ordl.company
        AND reftable.code2    eq po-ordl.i-no
        AND reftable.val[1] = po-ordl.po-no
      USE-INDEX code2. 
      DELETE reftable.
    END.
END.
