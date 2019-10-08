/* spoolchk.p */

DEFINE NEW GLOBAL SHARED VARIABLE g_track_usage AS LOGICAL NO-UNDO.
{methods/defines/hndldefs.i &NEW="NEW GLOBAL"}

IF SEARCH("splchkup") NE ? THEN
OS-DELETE splchkup.

RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.
RUN Get_Procedure IN Persistent-Handle ("chkspool.",OUTPUT run-proc,yes).
{methods/nowait.i}
QUIT.
