/* custom/tabnext.i  like disable column in browser in update mode*/

 IF NOT adm-new-record THEN DO:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
 END.
