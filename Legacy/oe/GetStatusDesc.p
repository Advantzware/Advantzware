/* Obtain Order Status Description  */
  
    DEFINE INPUT PARAMETER ipStatus AS CHARACTER NO-UNDO. /* oe-ord.s-code */
    DEFINE OUTPUT PARAMETER opStatDesc AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cStatDesc AS CHARACTER NO-UNDO.
   

    CASE ipStatus:
      when "N" then cStatDesc = "N- New".
      when "D" then cStatDesc = "D- Deleted".
      when "H" then cStatDesc = "H- Credit Hold".
      when "A" then cStatDesc = "A- Approved".     
      when "R" then cStatDesc = "R- Release".  
      when "I" then cStatDesc = "I- Invoiced".
      when "O" then cStatDesc = "O- Original Invoice".
      when "S" then cStatDesc = "S- Ship Only".
      when "X" then cStatDesc = "X- InvPrinted".     
      when "P" then cStatDesc = "P- Partial".
      when "C" then cStatDesc = "C- Closed".
      when "U" then cStatDesc = "U- Updated".
      when "F" then cStatDesc = "F- Fill".
    END CASE.

    ASSIGN opStatDesc = cStatDesc .
 
