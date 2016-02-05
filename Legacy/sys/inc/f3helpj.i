/* sys/inc/f3helpw.i for window/menu */
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

   ls-prog-name = if program-name(1) begins "user" then entry(2,program-name(1)," ")      
                  else program-name(1).
/*
message "Help Win Debug: "  self:name "," self:type skip
        "          focus:" focus:name focus:type skip
        "          Frame: " frame-file "," frame-db "," frame-name
    SKIP "Program: " ls-prog-name view-as alert-box.
*/
 if can-do("Browse,Frame",self:type) OR can-do("fi_jobStartDate",self:name) THEN
                         /* self:name or focus:name */
            run sys/ref/hlpd.w (self:name, frame-file, frame-db,ls-prog-name, "English") .
   else run sys/ref/hlpd.w (focus:name, focus:table, focus:dbname,"{&frame-name}", "English") .             
   return no-apply.
end.
