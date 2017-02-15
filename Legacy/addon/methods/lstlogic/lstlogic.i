/* lstlogic.i */

DEFINE INPUT PARAMETER parameter-list AS CHARACTER NO-UNDO.

&Global-define FRAME-NAME f-logic

{methods/defines/hndldefs.i}
{methods/defines/listdefs.i}

DEFINE BUFFER bprgrms FOR prgrms.

DEFINE VARIABLE output-option AS INTEGER NO-UNDO.
DEFINE VARIABLE spooled AS LOGICAL NO-UNDO.
DEFINE VARIABLE spool-date AS DATE NO-UNDO.
DEFINE VARIABLE spool-time AS CHARACTER NO-UNDO.
DEFINE VARIABLE spool-ampm AS CHARACTER NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER NO-UNDO.
DEFINE VARIABLE orientation AS CHARACTER NO-UNDO.
DEFINE VARIABLE show-notes AS LOGICAL LABEL "Show Notes" NO-UNDO.
DEFINE VARIABLE notes-type AS INTEGER NO-UNDO.
DEFINE VARIABLE show-parameters AS LOGICAL NO-UNDO.
DEFINE VARIABLE show-misc-fields AS LOGICAL LABEL "Show Misc Fields" NO-UNDO.
DEFINE VARIABLE show-addresses AS LOGICAL LABEL "Show Addresses" NO-UNDO.
DEFINE VARIABLE show-phones AS LOGICAL LABEL "Show Phones" NO-UNDO.
DEFINE VARIABLE list-order AS INTEGER NO-UNDO.
DEFINE VARIABLE cnt AS INTEGER NO-UNDO.
DEFINE VARIABLE misc-label AS CHARACTER FORMAT "X(70)" NO-UNDO.
DEFINE VARIABLE x-pos AS INTEGER NO-UNDO.
DEFINE VARIABLE y-pos AS INTEGER NO-UNDO.

DEFINE VARIABLE h_Viper AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE h_VReport AS WIDGET-HANDLE NO-UNDO.

FIND bprgrms WHERE bprgrms.prgmname = "{&PROGNAME}" NO-LOCK NO-ERROR.
IF NOT AVAILABLE bprgrms THEN
RETURN.

{methods/lstlogic/header.i}
{methods/lstlogic/footer.i}

RUN Import-Selections.
OUTPUT TO VALUE(parameter-list) PAGE-SIZE VALUE(lines-per-page) PAGED.
/*
IF output-option = 4 THEN
DO:
  RUN viper.p PERSISTENT SET h_Viper.
  RUN vreport.p PERSISTENT SET h_VReport.
  RUN SetParam IN h_VReport (h_Viper,'viper/lists.vfl',False).
  RUN LoadReport IN h_VReport (orientation).
  RUN SetWindowPos IN h_Viper (0,0,800,600).
  RUN List-Header.
END.
ELSE */
DO:
  VIEW FRAME f-header.
  VIEW FRAME f-continued.
END.
IF show-parameters THEN
DO:
  RUN Show-Parameters.
/*  IF output-option = 4 THEN
  DO:
    RUN List-Footer.
    RUN NewPage IN h_Viper.
    RUN List-Header.
  END. */
END.
RUN List-Logic.
/*
IF output-option = 4 THEN
DO:
  OUTPUT CLOSE.
  RUN Show-List-Lines.
  IF show-parameters THEN
  DO:
    RUN NewPage IN h_Viper.
    RUN List-Header.
    RUN Show-Parameters.
  END.
  RUN List-End.
  RUN EndDoc IN h_Viper.
  RUN ShowPreview IN h_Viper.
END.
ELSE  */
DO:
  HIDE FRAME f-continued NO-PAUSE.
  VIEW FRAME f-end-footer.
  OUTPUT CLOSE.
END.

PROCEDURE Import-Selections:
  INPUT FROM VALUE(parameter-list) NO-ECHO.
  IMPORT output-option.
  IMPORT spooled.
  IMPORT spool-date.
  IMPORT spool-time.
  IMPORT spool-ampm.
  IMPORT lines-per-page.
  /*IMPORT orientation. */
  IMPORT show-notes.
  IMPORT notes-type.
  IMPORT show-parameters.
  IMPORT show-misc-fields.
  IMPORT show-addresses.
  IMPORT show-phones.
  IMPORT list-order.
  RUN Import-Values.
  INPUT CLOSE.
END PROCEDURE.
/*
PROCEDURE List-End:
  RUN InitGroups IN h_VReport ('Footer').
  RUN SetCellText IN h_Viper ('Footer-Cell','Footer','End of List.').
  RUN FlushGroup IN h_Viper ('Footer').  
END PROCEDURE.

PROCEDURE List-Footer:
  DEFINE VARIABLE rpt_page AS INTEGER NO-UNDO.

  RUN GetPageNo IN h_Viper (OUTPUT rpt_page).
  RUN InitGroups IN h_VReport ('Footer').
  RUN SetCellText IN h_Viper ('Footer-Cell','Footer','Continued on Page: ' + STRING(rpt_page + 1)).
  RUN FlushGroup IN h_Viper ('Footer').
END PROCEDURE.

PROCEDURE List-Header:
  DEFINE VARIABLE rpt_page AS INTEGER NO-UNDO.
  DEFINE VARIABLE rpt_title AS CHARACTER FORMAT "x(36)" NO-UNDO.
  DEFINE VARIABLE rpt_name AS CHARACTER FORMAT "x(20)" NO-UNDO.

  FIND users WHERE users.user_id = USERID("NOSWEAT") NO-LOCK.

  RUN GetPageNo IN h_Viper (OUTPUT rpt_page).
  RUN SetCellText IN h_Viper ('Date-Cell','Header1','Date: ' + STRING(TODAY,'99/99/9999')).
  RUN SetCellText IN h_Viper ('Title-Cell','Header1',bprgrms.prgtitle).
  RUN SetCellText IN h_Viper ('Page-Cell','Header1','Page: ' + STRING(rpt_page)).
  RUN InitGroups IN h_VReport ('Header1').
  RUN FlushGroup IN h_Viper ('Header1').
  RUN SetCellText IN h_Viper ('Time-Cell','Header2','Time: ' + STRING(TIME,'HH:MM:SS am')).
  RUN SetCellText IN h_Viper ('Program-Cell','Header2','Program: ' + bprgrms.prgmname).
  RUN SetCellText IN h_Viper ('User-Cell','Header2','User: ' + users.user_id).
  RUN InitGroups IN h_VReport ('Header2').
  RUN FlushGroup IN h_Viper ('Header2').
END PROCEDURE.


PROCEDURE Show-List-Lines:
  DEFINE VARIABLE list-line AS CHARACTER NO-UNDO.

  RUN InitGroups IN h_VReport ('Detail').
  INPUT FROM VALUE(parameter-list) NO-ECHO.
  REPEAT:
    IMPORT UNFORMATTED list-line.
    IF ASC(SUBSTR(list-line,1,1)) = 12 THEN
    DO:
      list-line = SUBSTR(list-line,2).
      RUN List-Footer.
      RUN NewPage IN h_Viper.
      RUN List-Header.
    END.
    RUN SetCellText IN h_Viper ('List-Line-Cell','Detail',list-line).
    RUN SetCellFont IN h_Viper ('List-Line-Cell','Detail',8,0,'Regular','Courier New').
    RUN FlushGroup IN h_Viper ('Detail').
    list-line = ''.
  END.
  INPUT CLOSE.
  OS-DELETE VALUE(parameter-list).
END PROCEDURE.
*/

PROCEDURE Show-Parameters:
  DEFINE VARIABLE notetype-descrip AS CHARACTER FORMAT "X(20)" NO-UNDO.

  ASSIGN
    notetype-descrip = IF notes-type = 1 THEN "(All)"
                  ELSE IF notes-type = 2 THEN "(Viewed Only)"
                  ELSE "(Not Viewed Only)"
    y-pos = 500.
/*  IF output-option = 4 THEN
  DO:
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","SELECTED PARAMETERS:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","List Order:").
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",ENTRY(list-order,"{&LISTORDER}")).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN Show-Selections.
&IF "{&SHOWNOTES}" = "yes" &THEN
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Show Notes:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,100,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",show-notes).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    IF show-notes THEN
    DO:
      RUN SetCellPos IN h_Viper("Cell","Parameter",970,y-pos,700,0).
      RUN SetCellText IN h_Viper("Cell","Parameter",notetype-descrip).
      RUN FlushGroup IN h_Viper ("Parameter").
    END.
&ENDIF
&IF "{&SHOWMISCFLDS}" = "yes" &THEN
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Show Misc Fields:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,100,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",show-misc-fields).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
&ENDIF
&IF "{&SHOWADDRESSES}" = "yes" &THEN
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Show Addresses:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,100,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",show-addresses).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
&ENDIF
&IF "{&SHOWPHONES}" = "yes" &THEN
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Show Phones:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,100,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",show-phones).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
&ENDIF
  END.
  ELSE */
  DO:
    DISPLAY SKIP(3)
      "SELECTED PARAMETERS:" TO 40 SKIP(1)
      ENTRY(list-order,"{&LISTORDER}") FORMAT "X(40)"
          LABEL "List Order" COLON 40 SKIP(1)
        WITH FRAME f1-parameters SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
    RUN Show-Selections.
    DISPLAY SKIP(1)
&IF "{&SHOWNOTES}" = "yes" &THEN
      show-notes COLON 40
      notetype-descrip WHEN show-notes NO-LABEL SKIP
&ENDIF
&IF "{&SHOWMISCFLDS}" = "yes" &THEN
      show-misc-fields COLON 40 SKIP
&ENDIF
&IF "{&SHOWADDRESSES}" = "yes" &THEN
      show-addresses COLON 40 SKIP
&ENDIF
&IF "{&SHOWPHONES}" = "yes" &THEN
      show-phones COLON 40 SKIP
&ENDIF
        WITH FRAME f2-parameters SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
    PAGE.
  END.
END PROCEDURE.
