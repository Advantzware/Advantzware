   /* f3helpmn.i for menu-item help */
   
   def var ls-prog-name as cha no-undo.
   
   if not connected("asihlp") then do:
      if search("asihelp.pf") <> ? then connect -pf value(search("asihelp.pf")).
      else if search("asihlp.pf") <> ? then connect -pf value(search("asihlp.pf")).
   end.
   if not connected("asihlp") then do:
      message "ASI Help Database is not connected. Contact System Administrator." view-as alert-box error.
      return no-apply.
   end.

   ls-prog-name = /*if program-name(1) begins "user" then entry(2,program-name(1)," ")      
                  else program-name(1).*/
                  callprgm + "r" .
 
/*
   message focus:name ","
           frame-field ","
           frame-file ","
           frame-name ","
           ls-prog-name
           view-as alert-box.
*/                  
   run sys/ref/hlpd.w (focus:name, frame-file, frame-db,ls-prog-name, "English") .
   
