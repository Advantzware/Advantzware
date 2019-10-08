/* notes.i */

IF AVAILABLE notes THEN
DISPLAY STRING(notes.updateTime,"HH:MM:SS am") FORMAT "X(11)" @ notes.updateTime
        STRING(notes.createTime,"HH:MM:SS am") FORMAT "X(11)" @ notes.createTime
    WITH FRAME {&FRAME-NAME}. 
