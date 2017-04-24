/* titlcod_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME titlcod_.
&Scoped-define LISTORDER Title Code,Description
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY titlcode-query FOR titlcode.

DEFINE VARIABLE begin_titlcode AS CHARACTER FORMAT "X(10)" LABEL "Beginning  Title Code" NO-UNDO.
DEFINE VARIABLE begin_titlcode_description AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_titlcode AS CHARACTER FORMAT "X(10)" LABEL "Ending Title Code" NO-UNDO.
DEFINE VARIABLE end_titlcode_description AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY titlcode-query FOR EACH titlcode NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        titlcode.titlcode GE begin_titlcode AND
        titlcode.titlcode LE end_titlcode.
    WHEN 2 THEN
    OPEN QUERY titlcode-query FOR EACH titlcode NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        titlcode.description GE begin_titlcode_description AND
        titlcode.description LE end_titlcode_description.
  END CASE.
  GET FIRST titlcode-query.
  DO WHILE AVAILABLE(titlcode)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/titlcod_.i}
    DOWN.
    GET NEXT titlcode-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_titlcode.
  IMPORT begin_titlcode_description.
  IMPORT end_titlcode.
  IMPORT end_titlcode_description.
END PROCEDURE.

PROCEDURE Show-Selections:
  IF output-option = 4 THEN
  DO:
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Beginning  Title Code:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",begin_titlcode).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Beginning Description:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",begin_titlcode_description).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Ending Title Code:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",end_titlcode).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Ending Description:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",end_titlcode_description).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
  END.
  ELSE
  DISPLAY
    begin_titlcode COLON 40
    begin_titlcode_description COLON 40
    end_titlcode COLON 40
    end_titlcode_description COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
