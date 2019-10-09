/* emailcod.i */

&Scoped-define ITEM1 Phone
&Scoped-define LABEL1 &Phone Information
&Scoped-define PROC1 ~
RUN Get_Procedure IN Persistent-Handle ('phone.',OUTPUT run-proc,no). ~
IF run-proc NE '' THEN DO: ~
  CASE lvPhoneType: ~
    WHEN 'Customer' THEN DO: ~
      FIND cust NO-LOCK WHERE ROWID(cust) EQ lvRowID. ~
      ~{methods/smartrun.i (cust.rec_key,~"~{methods/headers/cust.i}~")} ~
    END. ~
    WHEN 'Vendor' THEN DO: ~
      FIND vend NO-LOCK WHERE ROWID(vend) EQ lvRowID. ~
      ~{methods/smartrun.i (vend.rec_key,~"~{methods/headers/vend.i}~")} ~
    END. ~
    OTHERWISE ~
    SESSION:SET-WAIT-STATE(''). ~
  END CASE. ~
END.
&Scoped-define ITEM2 
&Scoped-define LABEL2
&Scoped-define PROC2
&Scoped-define ITEM3
&Scoped-define LABEL3
&Scoped-define PROC3
&Scoped-define ITEM4
&Scoped-define LABEL4
&Scoped-define PROC4
&Scoped-define ITEM5
&Scoped-define LABEL5
&Scoped-define PROC5
&Scoped-define ITEM6
&Scoped-define LABEL6
&Scoped-define PROC6
&Scoped-define ITEM7
&Scoped-define LABEL7
&Scoped-define PROC7
&Scoped-define ITEM8
&Scoped-define LABEL8
&Scoped-define PROC8

{methods/menus/options.i}
