
DEF VAR notepad-log AS LOG NO-UNDO.
DEF VAR notepad-chr AS char NO-UNDO.

DEFINE VARIABLE gcNotepadFlag AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcNotepadValue AS CHARACTER NO-UNDO.

RUN spGetSettingByName ("Notepad", OUTPUT gcNotepadFlag).
ASSIGN notepad-log = LOGICAL(gcNotepadFlag) NO-ERROR.


RUN spGetSettingByName ("NotepadValue", OUTPUT gcNotepadValue).
ASSIGN notepad-chr = gcNotepadValue NO-ERROR.


