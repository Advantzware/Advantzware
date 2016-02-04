/* custom/askdel.i */

 message "Delete Currently Selected Record?" view-as alert-box question
          button yes-no update ll-ans as log.
 if not ll-ans then return error.
