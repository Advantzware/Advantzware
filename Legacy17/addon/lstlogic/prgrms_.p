/* prgrms_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME prgrms_.
&Scoped-define LISTORDER Program,Title
&Scoped-define SHOWNOTES no
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY prgrms-query FOR prgrms.

DEFINE VARIABLE begin_prgmname AS CHARACTER FORMAT "X(10)" LABEL "Beginning Program Name" NO-UNDO.
DEFINE VARIABLE begin_prgrms_prgtitle AS CHARACTER FORMAT "X(30)" LABEL "Beginning Program Title" NO-UNDO.
DEFINE VARIABLE end_prgmname AS CHARACTER FORMAT "X(10)" LABEL "Ending Program Name" NO-UNDO.
DEFINE VARIABLE end_prgrms_prgtitle AS CHARACTER FORMAT "X(30)" LABEL "Ending Program Title" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY prgrms-query FOR EACH prgrms NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        prgrms.prgmname GE begin_prgmname AND
        prgrms.prgmname LE end_prgmname.
    WHEN 2 THEN
    OPEN QUERY prgrms-query FOR EACH prgrms NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        prgrms.prgtitle GE begin_prgrms_prgtitle AND
        prgrms.prgtitle LE end_prgrms_prgtitle.
  END CASE.
  GET FIRST prgrms-query.
  DO WHILE AVAILABLE(prgrms)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/prgrms_.i}
    DOWN.
    GET NEXT prgrms-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_prgmname.
  IMPORT begin_prgrms_prgtitle.
  IMPORT end_prgmname.
  IMPORT end_prgrms_prgtitle.
END PROCEDURE.

PROCEDURE Show-Selections:
  IF output-option = 4 THEN
  DO:
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Beginning Program Name:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",begin_prgmname).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Beginning Program Title:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",begin_prgrms_prgtitle).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Ending Program Name:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",end_prgmname).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Ending Program Title:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",end_prgrms_prgtitle).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
  END.
  ELSE
  DISPLAY
    begin_prgmname COLON 40
    begin_prgrms_prgtitle COLON 40
    end_prgmname COLON 40
    end_prgrms_prgtitle COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
