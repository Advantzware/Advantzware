/* zipcode_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME zipcode_.
&Scoped-define LISTORDER Zip/Postal Code,City
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY zipcode-query FOR zipcode.

DEFINE VARIABLE begin_zipcode AS CHARACTER FORMAT "X(10)" LABEL "Beginning  Zipcode" NO-UNDO.
DEFINE VARIABLE begin_zipcode_city AS CHARACTER FORMAT "X(30)" LABEL "Beginning City" NO-UNDO.
DEFINE VARIABLE end_zipcode AS CHARACTER FORMAT "X(10)" LABEL "Ending Zipcode" NO-UNDO.
DEFINE VARIABLE end_zipcode_city AS CHARACTER FORMAT "X(30)" LABEL "Ending City" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY zipcode-query FOR EACH zipcode NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        zipcode.zipcode GE begin_zipcode AND
        zipcode.zipcode LE end_zipcode.
    WHEN 2 THEN
    OPEN QUERY zipcode-query FOR EACH zipcode NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        zipcode.city GE begin_zipcode_city AND
        zipcode.city LE end_zipcode_city.
  END CASE.
  GET FIRST zipcode-query.
  DO WHILE AVAILABLE(zipcode)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/zipcode_.i}
    DOWN.
    GET NEXT zipcode-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_zipcode.
  IMPORT begin_zipcode_city.
  IMPORT end_zipcode.
  IMPORT end_zipcode_city.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_zipcode COLON 40
    begin_zipcode_city COLON 40
    end_zipcode COLON 40
    end_zipcode_city COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
