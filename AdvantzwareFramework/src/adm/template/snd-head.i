/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/* snd-head.i - 7/23/95 */
  DEFINE INPUT PARAMETER p-tbl-list AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER p-rowid-list AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE link-handle  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE rowid-string AS CHARACTER NO-UNDO.

  /* Mike Fechner, Consultingwerk Ltd. 11.03.2017
     https://github.com/advantzwareWinKit/Advantzware/issues/38 */
  &IF "{&PROCEDURE-TYPE}" EQ "SmartBrowser" &THEN
  IF NOT AVAILABLE {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN
      GET FIRST {&BROWSE-NAME} .
  &ENDIF

  DO i = 1 TO NUM-ENTRIES(p-tbl-list):
      IF i > 1 THEN p-rowid-list = p-rowid-list + ",":U.
      CASE ENTRY(i, p-tbl-list):
