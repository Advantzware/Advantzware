/* sys/inc/f3help.i */
on f3 of frame {&frame-name}
anywhere
do:
   
   if not connected("asihlp") then do:
      if search("asihelp.pf") <> ? then connect -pf value(search("asihelp.pf")).
      else if search("asihlp.pf") <> ? then connect -pf value(search("asihlp.pf")).
   end.
   if not connected("asihlp") then do:
      message "ASI Help Database is not connected. Contact System Administrator." view-as alert-box error.
      return no-apply.
   end.
message "field:" frame-field ",  frame-file:" frame-file skip
        "frame-name: " frame-name skip 
        program-name(1) skip
        program-name(2) view-as alert-box.
         
   run sys/ref/hlp.p (frame-field, frame-file, frame-db,frame-name, "English") .
   return no-apply.
end.
