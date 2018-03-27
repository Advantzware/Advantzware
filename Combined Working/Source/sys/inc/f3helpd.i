/* sys/inc/f3helpd.i for dialog-box help */
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
/*
message "Help Object Debug: self - "  self:name "," self:type skip
        "                   Focus - " focus:name "," focus:type skip 
        "          Frame-file,db "           frame-file "," frame-db  ", {&frame-name} " skip
        "self frame,file "              self:frame-name self:table
         view-as alert-box.
*/

                      /* frame-field,frame-file and frame-db are not working when f3 key pressed in a row */
   ls-prog-name = if program-name(1) begins "user" then entry(2,program-name(1)," ")      
                  else program-name(1).

   if can-do("Browse,Frame",self:type) then
                         /* self:name or focus:name */
            run sys/ref/hlpd.p (self:name, frame-file, frame-db,ls-prog-name, "English") .
   else run sys/ref/hlpd.p (focus:name, focus:table, focus:dbname,"{&frame-name}", "English") .
   return no-apply.
end.
