&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME ap-inv

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.


{sys/inc/apsecure.i}

IF ap-inv.posted THEN DO:
  MESSAGE "Record already posted, no deletion allowed!"
      VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

IF apsecure-log AND ap-inv.user-id NE USERID("nosweat") THEN DO:
  MESSAGE "This invoice may only be deleted by UserID: " +
          TRIM(ap-inv.user-id) + "..."
       VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

for each ap-invl exclusive-lock where ap-invl.i-no = ap-inv.i-no:
      find first po-ordl
          where po-ordl.company   eq ap-inv.company
            and po-ordl.po-no     eq ap-invl.po-no
            and po-ordl.line      eq {ap/invlline.i -1}
            and po-ordl.item-type eq no
          no-lock no-error.

      if avail po-ordl then
      for each fg-rcpth
          where fg-rcpth.company   eq ap-inv.company
            and fg-rcpth.i-no      eq po-ordl.i-no
            and fg-rcpth.po-no     eq trim(string(po-ordl.po-no,">>>>>>>>>>"))
            and fg-rcpth.rita-code eq "R"
            and fg-rcpth.b-no      eq ap-invl.i-no
          use-index item-po:
               
        fg-rcpth.b-no = 0.
      end.
      
      delete ap-invl.
   end
