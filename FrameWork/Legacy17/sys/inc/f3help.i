/* sys/inc/f3help.i */
on f3 of frame {&frame-name}
anywhere
do:
   def var ls-prog-name as cha no-undo.   
   if not connected("asihlp") then do:
      if search("asihelp.pf") <> ? then connect -pf value(search("asihelp.pf")).
      else if search("asihlp.pf") <> ? then connect -pf value(search("asihlp.pf")).
   end.
   if not connected("asihlp") then do:
      message "ASI Help Database is not connected. Contact System Administrator." view-as alert-box error.
      return no-apply.
   end.
   
   
   /* frame-field,frame-file and frame-db are not working when f3 key pressed in a row */
   ls-prog-name = if program-name(1) begins "user" then entry(2,program-name(1)," ")      
                  else program-name(1).

      if can-do("Browse,Frame",self:type) THEN DO:

       IF ls-prog-name MATCHES "*viewers/sys-ctrl*" THEN
            RUN sys/ref/hlp-ctrl.w (FRAME-FIELD,sys-ctrl.company,sys-ctrl.NAME,ls-prog-name, "English") .
                            /* self:name or focus:name */
       ELSE run sys/ref/hlp.w (self:name, frame-file, frame-db,ls-prog-name, "English") .
   END.
   else run sys/ref/hlp.w (focus:name, focus:table, focus:dbname,"{&frame-name}", "English") .
   
   return no-apply.
end.
