/* lstshell.i */

DEFINE INPUT PARAMETER progname AS CHARACTER NO-UNDO.
DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE text-line AS CHARACTER NO-UNDO.

IF SEARCH("lstlogic/" + progname + "p") NE ? THEN
DO:
  MESSAGE "File '" + "lstlogic/" + progname + "p'"
    "already Exists, Continue?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE file-exists AS LOGICAL.
  IF NOT file-exists THEN
  RETURN.
  INPUT FROM VALUE("lstlogic/" + progname + "p") NO-ECHO.
  IMPORT ^.
  IMPORT UNFORMATTED text-line.
  INPUT CLOSE.
  IF INDEX(text-line,"remove this comment") = 0 THEN
  DO:
    MESSAGE "File '" + "lstlogic/" + progname + "p' has been customized!" SKIP(1)
        "Continue to Create Shell Anyway?" VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO UPDATE custom-override AS LOGICAL.
    IF NOT custom-override THEN
    RETURN.
  END.
END.
OUTPUT TO VALUE("lstlogic/" + progname + "p").
PUT UNFORMATTED
  "/* " progname "p */" SKIP
  "/* remove this comment to prevent auto creating, saving custom changes */" SKIP(1)
  "~&Scoped-define PROGNAME " progname SKIP
  "~&Scoped-define LISTORDER {&LISTORDER}" SKIP
&IF "{&WHERE-STATEMENT}" NE "" &THEN
  "~&Scoped-define WHERE-STATEMENT {&WHERE-STATEMENT}" SKIP
&ENDIF
  "~&Scoped-define SHOWNOTES {&SHOWNOTES}" SKIP
  "~&Scoped-define SHOWMISCFLDS {&SHOWMISCFLDS}" SKIP
  "~&Scoped-define SHOWADDRESSES {&SHOWADDRESSES}" SKIP
  "~&Scoped-define SHOWPHONES {&SHOWPHONES}" SKIP(1).
&IF "{&TABLENAME}" NE "" &THEN
PUT UNFORMATTED
  "DEFINE QUERY {&FIRST-EXTERNAL-TABLE}-query "
  "FOR {&FIRST-EXTERNAL-TABLE}." SKIP(1).
&ENDIF
ASSIGN
  current-widget = FRAME {&FRAME-NAME}:HANDLE
  current-widget = current-widget:FIRST-CHILD
  current-widget = current-widget:FIRST-CHILD.
DO WHILE current-widget NE ?:
  IF CAN-DO("save",current-widget:PRIVATE-DATA) THEN
  DO:
    PUT UNFORMATTED "DEFINE VARIABLE " current-widget:NAME " AS "
      current-widget:DATA-TYPE " FORMAT ~"".
    IF CAN-DO("COMBO-BOX,FILL-IN,TEXT,TOGGLE-BOX",current-widget:TYPE) THEN
    PUT UNFORMATTED current-widget:FORMAT.
    ELSE
      IF NUM-ENTRIES(current-widget:TOOLTIP) GT 1 THEN
      PUT UNFORMATTED ENTRY(2,current-widget:TOOLTIP).
      ELSE
      PUT UNFORMATTED "X(40)".
    PUT UNFORMATTED  "~" LABEL ~"".
    IF current-widget:LABEL NE ? THEN
    PUT UNFORMATTED current-widget:LABEL.
    ELSE
    PUT UNFORMATTED ENTRY(1,current-widget:TOOLTIP).
    PUT UNFORMATTED "~" NO-UNDO." SKIP.
  END.
  current-widget = current-widget:NEXT-SIBLING.
END.
PUT UNFORMATTED SKIP(1)
  "~{methods/lstlogic/lstlogic.i}" SKIP(1)
  "PROCEDURE List-Logic:" SKIP.
&IF "{&QUERYDEFAULT}" = "yes" AND "{&TABLENAME}" NE "" &THEN
PUT UNFORMATTED
  "  CASE list-order:" SKIP.
{methods/lstlogic/for_each.i 1}
{methods/lstlogic/for_each.i 2}
{methods/lstlogic/for_each.i 3}
{methods/lstlogic/for_each.i 4}
{methods/lstlogic/for_each.i 5}
PUT UNFORMATTED
  "  END CASE." SKIP.
PUT UNFORMATTED
  "  GET FIRST {&FIRST-EXTERNAL-TABLE}-query." SKIP
  "  DO WHILE AVAILABLE({&FIRST-EXTERNAL-TABLE})" SKIP
  "      WITH FRAME ~{&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:" SKIP
  "    ~{methods/lstlogic/custom/" + progname + "i}" SKIP
  "    DOWN." SKIP
  "    GET NEXT {&FIRST-EXTERNAL-TABLE}-query." SKIP
  "  END." SKIP.
&ELSE
PUT UNFORMATTED
  "  ~{methods/lstlogic/custom/" + progname + "i}" SKIP.
&ENDIF
PUT UNFORMATTED
  "END PROCEDURE. /* List-Logic */" SKIP(1)
  "PROCEDURE Import-Values:" SKIP.
ASSIGN
  current-widget = FRAME {&FRAME-NAME}:HANDLE
  current-widget = current-widget:FIRST-CHILD
  current-widget = current-widget:FIRST-CHILD.
DO WHILE current-widget NE ?:
  IF CAN-DO("save",current-widget:PRIVATE-DATA) THEN
  PUT UNFORMATTED "  IMPORT " current-widget:NAME "." SKIP.
  current-widget = current-widget:NEXT-SIBLING.
END.
PUT UNFORMATTED
  "END PROCEDURE." SKIP(1)
  "PROCEDURE Show-Selections:" SKIP
  "  DISPLAY" SKIP.
ASSIGN
  current-widget = FRAME {&FRAME-NAME}:HANDLE
  current-widget = current-widget:FIRST-CHILD
  current-widget = current-widget:FIRST-CHILD.
DO WHILE current-widget NE ?:
  IF CAN-DO("save",current-widget:PRIVATE-DATA) THEN
  PUT UNFORMATTED "    " current-widget:NAME " COLON 40" SKIP.
  current-widget = current-widget:NEXT-SIBLING.
END.
PUT UNFORMATTED
  "        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO." SKIP
  "END PROCEDURE." SKIP.
OUTPUT CLOSE.
IF SEARCH("methods/lstlogic/custom/" + progname + "i") = ? THEN
DO:
  OUTPUT TO VALUE("methods/lstlogic/custom/" + progname + "i").
  PUT UNFORMATTED
    "/* " progname "i */" SKIP(1)
    "/*- Programming Notes -------------------------------------------------------" SKIP(1)
    "1) fields with no frame reference are scoped to 'FRAME ~{&FRAME-NAME}'" SKIP(1)
    "2) if internal procedures are needed, add'em to 'lstlogic/persist.p'" SKIP
    "   alphabetically and use the following syntax:" SKIP
    "   RUN <ip-name> IN ListLogic-Handle [(<parameters>)]." SKIP
    "   The naming convention used for <ip-name> should begin with '"
    SUBSTR(progname,1,LENGTH(progname) - 1) "'." SKIP(1)
    "3) if notes are desired for this listing use the following syntax:" SKIP
    "   ~{methods/lstlogic/shownote.i ~&db_table=~"{&FIRST-EXTERNAL-TABLE}~" ~&col=~"5~" ~&frame-name=~"f-notes~"}" SKIP(1)
    "4) if misc fields are desired for this listing use the following syntax:" SKIP
    "   ~{methods/lstlogic/showmisc.i ~&db_table=~"{&FIRST-EXTERNAL-TABLE}~" ~&col=~"5~" ~&frame-name=~"f-miscflds~"}" SKIP(1)
    "5) if addresses are desired for this listing use the following syntax:" SKIP
    "   ~{methods/lstlogic/showaddr.i ~&db_table=~"{&FIRST-EXTERNAL-TABLE}~" ~&col=~"5~" ~&frame-name=~"f-addresses~"}" SKIP(1)
    "6) if phones are desired for this listing use the following syntax:" SKIP
    "   ~{methods/lstlogic/showphon.i ~&db_table=~"{&FIRST-EXTERNAL-TABLE}~" ~&col=~"5~" ~&frame-name=~"f-phones~"}" SKIP(1)
    "---------------------------------------------------------------------------*/" SKIP(1)
    "DISPLAY" SKIP
    "  {&DBFIELD1}"
&IF INDEX("{&LISTORDER}",",") NE 0 &THEN
    SKIP "  {&DBFIELD2}"
&ENDIF
    "." SKIP(1)
    &IF "{&SHOWNOTES}" = "yes" &THEN
    "~{methods/lstlogic/shownote.i ~&db_table=~"{&FIRST-EXTERNAL-TABLE}~" ~&col=~"5~" ~&frame-name=~"f-notes~"}" SKIP
    &ENDIF
    &IF "{&SHOWMISCFLDS}" = "yes" &THEN
    "~{methods/lstlogic/showmisc.i ~&db_table=~"{&FIRST-EXTERNAL-TABLE}~" ~&col=~"5~" ~&frame-name=~"f-miscflds~"}" SKIP
    &ENDIF
    &IF "{&SHOWADDRESSES}" = "yes" &THEN
    "~{methods/lstlogic/showaddr.i ~&db_table=~"{&FIRST-EXTERNAL-TABLE}~" ~&col=~"5~" ~&frame-name=~"f-addresses~"}" SKIP
    &ENDIF
    &IF "{&SHOWPHONES}" = "yes" &THEN
    "~{methods/lstlogic/showphon.i ~&db_table=~"{&FIRST-EXTERNAL-TABLE}~" ~&col=~"5~" ~&frame-name=~"f-phones~"}" SKIP
    &ENDIF
    .
  OUTPUT CLOSE.
  MESSAGE "Created Default 'methods/lstlogic/custom/" + progname + "i'." SKIP(1)
    "Add additional logic and Display <field> statements within this file."
        VIEW-AS ALERT-BOX INFORMATION.
END.
ELSE
MESSAGE "Default 'methods/lstlogic/custom/" + progname + "i' already exists!" SKIP(1)
    "Modify any logic and Display <field> statements within this file."
        VIEW-AS ALERT-BOX INFORMATION.
