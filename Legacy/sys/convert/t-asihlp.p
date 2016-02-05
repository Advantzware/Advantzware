/* t-asihlp.p  convert help data base if need   help-msg - not use any more */

   for each help-msg no-lock break by help-msg.msg-number :
       if first-of(help-msg.msg-number) then do:
          find first hlp-head where hlp-head.msg-num = help-msg.msg-number no-error.
          if not avail hlp-head then message help-msg.msg-number view-as alert-box.
          else hlp-head.help-txt = "".
       end.

       if avail hlp-head then   hlp-head.help-txt = hlp-head.help-txt + help-msg.msg-txt + chr(13).
  end.     
