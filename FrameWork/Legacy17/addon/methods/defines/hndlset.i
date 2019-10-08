/* hndlset.i */

DEFINE SHARED VARIABLE g_groups AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE g_rec_key AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE g_pageno AS INTEGER NO-UNDO.

{methods/defines/hndldefs.i &NEW="NEW"}

Persistent-Handle = SESSION:FIRST-PROCEDURE.
DO WHILE VALID-HANDLE(Persistent-Handle):
  hsignature = Persistent-Handle:GET-SIGNATURE("Get_Procedure").
  IF hsignature NE "" THEN
  LEAVE.
  Persistent-Handle = Persistent-Handle:NEXT-SIBLING.
END.
IF Persistent-Handle = ? THEN
RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.
