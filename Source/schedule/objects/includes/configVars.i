/* configVars.i */

DEFINE VARIABLE allResources            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE alphaSort               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE autoSize                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE boardDatePrompt         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE colorPriority           AS INTEGER   NO-UNDO EXTENT 28. /* customColor X 2 */
DEFINE VARIABLE completedCheckoff       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE completedHide           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE customCheckoff          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE customBGColor           AS INTEGER   NO-UNDO EXTENT 14.
DEFINE VARIABLE customFGColor           AS INTEGER   NO-UNDO EXTENT 14.
DEFINE VARIABLE customLabel             AS CHARACTER NO-UNDO EXTENT 14.
DEFINE VARIABLE customValue             AS CHARACTER NO-UNDO EXTENT 14.
DEFINE VARIABLE datePrompt              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE detailWindow            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE downtimeBlock           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE downtimeConflictBGColor AS INTEGER   NO-UNDO.
DEFINE VARIABLE downtimeConflictFGColor AS INTEGER   NO-UNDO.
DEFINE VARIABLE downtimePrompt          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE downtimeSize            AS INTEGER   NO-UNDO.
DEFINE VARIABLE downtimeTop             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE downtimeWarning         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dueDateUsed             AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE endDateBuffer           AS INTEGER   NO-UNDO.
DEFINE VARIABLE flashLightColor         AS INTEGER   NO-UNDO.
DEFINE VARIABLE flashLight              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE flashGridLine           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE flashTimeLine           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE fullBoard               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gap                     AS INTEGER   NO-UNDO.
DEFINE VARIABLE gridLineColor           AS INTEGER   NO-UNDO.
DEFINE VARIABLE gridBGColor             AS INTEGER   NO-UNDO.
DEFINE VARIABLE hpixels                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE htmlCapacityDays        AS INTEGER   NO-UNDO.
DEFINE VARIABLE htmlPageLocation        AS CHARACTER NO-UNDO.
DEFINE VARIABLE htmlPopupHeight         AS INTEGER   NO-UNDO.
DEFINE VARIABLE htmlPopupWidth          AS INTEGER   NO-UNDO.
DEFINE VARIABLE intervalInit            AS INTEGER   NO-UNDO.
DEFINE VARIABLE jobBGColor              AS INTEGER   NO-UNDO EXTENT 14.
DEFINE VARIABLE jobFGColor              AS INTEGER   NO-UNDO EXTENT 14.
DEFINE VARIABLE jobBlock                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE jobConflictBGColor      AS INTEGER   NO-UNDO.
DEFINE VARIABLE jobConflictFGColor      AS INTEGER   NO-UNDO.
DEFINE VARIABLE jobLabel                AS CHARACTER NO-UNDO EXTENT 14.
DEFINE VARIABLE jobPrompt               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE jobWarning              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lightBulbColor          AS INTEGER   NO-UNDO.
DEFINE VARIABLE lockButtons             AS INTEGER   NO-UNDO.
DEFINE VARIABLE loadCapacity            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE monitorInterval         AS INTEGER   NO-UNDO.
DEFINE VARIABLE moveUndoRedo            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE noteButtons             AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE packOption              AS INTEGER   NO-UNDO INITIAL 5.
DEFINE VARIABLE packOptionPrompt        AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE pendingDays             AS INTEGER   NO-UNDO.
DEFINE VARIABLE pendingLastDay          AS INTEGER   NO-UNDO INITIAL 365.
DEFINE VARIABLE pendingOver             AS INTEGER   NO-UNDO INITIAL 180.
DEFINE VARIABLE popupBottom             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE priority1               AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE priority2               AS INTEGER   NO-UNDO INITIAL 2.
DEFINE VARIABLE priority3               AS INTEGER   NO-UNDO INITIAL 3.
DEFINE VARIABLE reloadReport            AS LOGICAL   NO-UNDO INITIAL ?.
DEFINE VARIABLE reloadStatus            AS LOGICAL   NO-UNDO INITIAL ?.
DEFINE VARIABLE resourceBGColor         AS INTEGER   NO-UNDO.
DEFINE VARIABLE resourceBrowseAction    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE resourceJobDetail       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE saveInterval            AS INTEGER   NO-UNDO.
DEFINE VARIABLE showDowntime            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE showStatus              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE statusCheckoffType      AS CHARACTER NO-UNDO.
DEFINE VARIABLE threeD                  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE threeDBottom            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE threeDLeft              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE threeDRight             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE threeDTop               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE timeInit                AS INTEGER   NO-UNDO.
DEFINE VARIABLE timeLineColor           AS INTEGER   NO-UNDO.
DEFINE VARIABLE version                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE viewRefresh             AS INTEGER   NO-UNDO.
