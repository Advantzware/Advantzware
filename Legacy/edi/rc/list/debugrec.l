...edi/rc/debugrec.p                    12/03/99 16:39:14                                                       PROGRESS(R) Page 1   

{} Line Blk
-- ---- ---
      1     def input param p_text as char no-undo.
      2     def input param p_rec as recid no-undo.
      3     find {1} where recid({1}) = p_rec no-lock no-error.
      4     run rc/debugmsg.p (p_text + (if avail {1} then "" else "(Unavailable)" ) ).
...edi/rc/debugrec.p                    12/03/99 16:39:14                                                       PROGRESS(R) Page 2   

     File Name       Line Blk. Type Tran            Blk. Label            
-------------------- ---- --------- ---- --------------------------------
...edi/rc/debugrec.p    0 Procedure No                                    
