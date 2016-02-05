def var li-num as int.
def buffer bf-hlp for hlp-head.

find last hlp-head use-index mess-num no-lock .
li-num = hlp-head.msg-num.

for each hlp-head where fld-name = "windows/itemfg.r" .
  disp fld-name msg-num.
  
  buffer-copy hlp-head except hlp-head.msg-num hlp-head.fld-name to bf-hlp.   
  assign bf-hlp.msg-num = li-num + 1
         bf-hlp.fld-name = "D".
       
    
    end.
    pause.
