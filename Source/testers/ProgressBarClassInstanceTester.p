USING Progress.Lang.*.
USING testers.ProgressBarTester.*.

DEFINE VARIABLE classInstance AS testers.ProgressBarTester NO-UNDO.
DEFINE BUTTON bMax LABEL "Display Progress Bar".
DEFINE FRAME f1 bMax.

 classInstance = NEW testers.ProgressBarTester().

ON CHOOSE OF bMax in frame f1 DO: /* BEGIN Trigger Block */
    
    classInstance = NEW testers.ProgressBarTester().   
    WAIT-FOR System.Windows.Forms.Application:Run(classInstance).
END.


ENABLE bMax WITH FRAME f1.
WAIT-FOR WINDOW-CLOSE OF FRAME f1.

DELETE OBJECT classInstance NO-ERROR.
QUIT.
