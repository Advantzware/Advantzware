/* custom/askdelss.i  for shartshooter*/

 /*message "Delete Currently Selected Record?" view-as alert-box question
          button yes-no update ll-ans as log.          
 if not ll-ans then return error.             */
 
 RUN custom/d-msg.w ("Warning","","Delete Currently Selected Record? ","",2,"Yes,No", OUTPUT v-msgreturn).         
 IF v-msgreturn >= 2 THEN RETURN ERROR.
