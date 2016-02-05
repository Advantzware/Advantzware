 
 def buffer bf-hlp for hlp-head.
  def var li-next-num as int no-undo.
  
  find last bf-hlp use-index mess-num no-lock no-error.
  if avail bf-hlp then li-next-num = bf-hlp.msg-num + 1.
  else li-next-num = 1.
  
  
  /*     
  hlp-head.msg-num = li-next-num.
  
  display hlp-head.msg-num with frame {&frame-name}.
  
  
       */

  FOR EACH SYS-CTRL:
      CREATE hlp-head.
      hlp-head.msg-num = li-next-num.
      hlp-head.fld-name = CAPS(sys-ctrl.NAME).
      hlp-head.fil-name = sys-ctrl.company.
      hlp-head.help-txt = "Help for " + sys-ctrl.NAME.

      DISP NAME.
      li-next-num = li-next-num + 1.
  END.
