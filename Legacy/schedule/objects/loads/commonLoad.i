/* commonLoad.i - common code for all load programs */

DEFINE VARIABLE udfField AS CHARACTER NO-UNDO EXTENT {&udfExtent}.
DEFINE VARIABLE userField AS CHARACTER NO-UNDO EXTENT {&userExtent}.
DEFINE VARIABLE jobStatus AS LOGICAL NO-UNDO EXTENT {&statusExtent}.
DEFINE VARIABLE statusTimeStamp AS CHARACTER NO-UNDO EXTENT {&statusExtent}.
DEFINE VARIABLE liveUpdate AS LOGICAL NO-UNDO INITIAL YES.
DEFINE VARIABLE loadDate AS DATE NO-UNDO.

{{&loads}/resourceList.i}

/* RUN {&prompts}/loadDate.w (OUTPUT loadDate). */

&IF '{&Board}' NE 'View' &THEN
EXPORT STREAM sPending '{&version}' {{&includes}/asOfTime.i}.
EXPORT STREAM sScenario '{&version}' {{&includes}/asOfTime.i}.
&ENDIF
