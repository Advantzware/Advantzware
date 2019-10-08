/* emplogi_.p */
/* this code has been custmized */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME emplogi_.
&Scoped-define LISTORDER Emp ID
&Scoped-define WHERE-STATEMENT employee.company = selected-company
&Scoped-define SHOWNOTES no
&Scoped-define SHOWMISCFLDS no
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY emplogin-query FOR emplogin.

DEFINE VARIABLE selected-company AS CHARACTER FORMAT "X(3)" LABEL "Company" NO-UNDO.
DEFINE VARIABLE begin_employee AS CHARACTER FORMAT "X(5)" LABEL "Beginning Employee" NO-UNDO.
DEFINE VARIABLE end_employee AS CHARACTER FORMAT "X(5)" LABEL "Ending Employee" NO-UNDO.
DEFINE VARIABLE begin_end_date AS DATE FORMAT "99/99/9999" LABEL "Beginning Date" NO-UNDO.
DEFINE VARIABLE end_end_date AS DATE FORMAT "99/99/9999" LABEL "Ending Date" NO-UNDO.
DEFINE VARIABLE begin_note-date AS DATE FORMAT "99/99/9999" LABEL "Beginning Note Date" NO-UNDO.
DEFINE VARIABLE end_note-date AS DATE FORMAT "99/99/9999" LABEL "Ending Note Date" NO-UNDO.
DEFINE VARIABLE lv-labor-only AS LOGICAL FORMAT "yes/no" LABEL "Print Labor" NO-UNDO.
DEFINE VARIABLE lv-note-only AS LOGICAL FORMAT "yes/no" LABEL "Print Notes" NO-UNDO.


{methods/lstlogic/lstlogic.i}
/*==========
PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY emplogin-query FOR EACH emplogin NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        emplogin.employee GE begin_employee AND
        emplogin.employee LE end_employee.
  END CASE.
  GET FIRST emplogin-query.
  DO WHILE AVAILABLE(emplogin)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/emplogi_.i}
    DOWN.
    GET NEXT emplogin-query.
  END.
END PROCEDURE. /* List-Logic */
================*/

/* edited =========*/
def temp-table tt-note field employee like emplogin.employee
                       field rec_key like ASI.notes.rec_key
                       field note_date like ASI.notes.note_date
                       field note_title like ASI.notes.note_title
                       field note_src as cha .

PROCEDURE list-logic:
   {methods/lstlogic/custom/emplogi_.i}
end procedure.
 /* ======== end of edit =-*/
PROCEDURE Import-Values:
  IMPORT selected-company.
  IMPORT begin_employee.
  IMPORT end_employee.
  IMPORT begin_end_date.
  IMPORT end_end_date.
  IMPORT begin_note-date.
  IMPORT end_note-date.
  IMPORT lv-labor-only.
  IMPORT lv-note-only.
END PROCEDURE.

PROCEDURE Show-Selections:
  IF output-option = 4 THEN
  DO:
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Company:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",selected-company).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Beginning Employee:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",begin_employee).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Ending Employee:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",end_employee).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Beginning Date:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",begin_end_date).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Ending Date:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",end_end_date).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Beginning Note Date:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",begin_note-date).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Ending Note Date:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",end_note-date).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Print Labor:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",lv-labor-only).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Print Notes:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",lv-note-only).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
  END.
  ELSE
  DISPLAY
    selected-company COLON 40
    begin_employee COLON 40
    end_employee COLON 40
    begin_end_date COLON 40
    end_end_date COLON 40
    begin_note-date COLON 40
    end_note-date COLON 40
    lv-labor-only COLON 40
    lv-note-only COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
