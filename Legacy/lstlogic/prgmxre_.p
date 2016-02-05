/* prgmxre_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME prgmxre_.
&Scoped-define LISTORDER Table Name
&Scoped-define SHOWNOTES no
&Scoped-define SHOWMISCFLDS no
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY prgmxref-query FOR prgmxref.

DEFINE VARIABLE begin_table_name AS CHARACTER FORMAT "X(10)" LABEL "Beginning Table Name" NO-UNDO.
DEFINE VARIABLE end_table_name AS CHARACTER FORMAT "X(10)" LABEL "Ending Table Name" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  OPEN QUERY prgmxref-query FOR EACH prgmxref WHERE
      prgmxref.table_name GE begin_table_name AND
      prgmxref.table_name LE end_table_name NO-LOCK USE-INDEX pi-prgmxref.
  GET FIRST prgmxref-query NO-LOCK.
  DO WHILE AVAILABLE(prgmxref)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/prgmxre_.i}
    DOWN.
    GET NEXT prgmxref-query NO-LOCK.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_table_name.
  IMPORT end_table_name.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_table_name COLON 40
    end_table_name COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
