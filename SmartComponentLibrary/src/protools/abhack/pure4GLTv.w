&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
/* Procedure Description
"pure4glTv.w is a Treeview Smart Object in pure 4GL developed by Sébastien Lacroix October 2004

See the definition block to see more details"
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win
/*********************************************************************
pure4glTv.w is a Treeview Smart Object in pure 4GL by Sébastien Lacroix October 2004

Note on 29-MAR-2005: we may now consider this object as a *perfect* clone of the
MS ActiveX.  Its main strengths are:
A] It is up to 5 times faster to load nodes in it.
B] It causes less deployment problems.  Can even run on Linux with Wine
C] It is customizable


slacroix 7-MAR-2005:
1) Implemented 2 preprocessors APIRtnParm and APIRtnParmINT to allow
 pure4gltv to run with the windows.p and winfunc.p API librairies that
 are shipped with Dynamics
    => See the definition block for more information.

2) Implemented a new MSkeyScrollForcePaint instance property to force
 a repaint when scrolling with the keyboard.
    =>  See the MSkeyScrollForcePaint function for more information.
Note the initial value of the MSkeyScrollForcePaint property to YES

3) call setWindowsSkin a the beginnig of initializePure4glTv to set
gWindowsSkin correctly when WindowsSkin is set to 'automatic' at design time.
Note there was not problem at runTime, but at design time, when just dropping
pure4gltv on a container, the demo TV was showing a classic scrollbar in a
Luna environment

4) Changed code to rely more on the HIDDEN attribute instead of VISIBLE, as
suggested by Aris Schlingmann


slacroix 21-MAR-2005:
1) Fixed minor cosmetic issue: in some random circumstances with intense resizing,
resizeVertScrollBar could bring up a set of native scroll buttons at the bottom of
the frame of the emulated vertical scrollbar.  Fixed by calling ShowScrollBar IN hppure4gltvApi

2) Moved the vertical scrollbar by one pixel on the right on the request of a user.
Beware, one cannot make a 4GL frame smaller than 19 pixels width.

3) Fixed cosmetic bug with edtSunken (Editor used for 3D effect) that was badly
resized down in resizeObject



slacroix & Nicolas Andricq MAR-2006
1) Support of Royale skin.  Restructuration to support multiple skins

2) Review of the vertical scrollbar to avoid a hole of two pixels on the right.

3) new getSelectedNodeKey() API



slacroix later in MAR-2006
1) New expandBranch API that is fired on '*' key trigger

2) New enableObject and disableObject API's

3) New expandAll functionality on '*' key

4) Made a new version of Prospy that uses pure4gl tv for greater performance


 09-JUN-2007 sla:
1) implemented a new nodeDisplay event a bit similar to a Browse ROW-DISPLAY event:
  /* 08-APR-2007 sla: new nodeDisplay hook to give the ability to refine a node just before it gets displayed
    it's up to the container to subscribe to this event.  It is a bit like a ROW-DISPLAY trigger in a browse
    This way, one can  refine node labels just before they get displayed.  One nice example is a new prospy++
    that displays a huge trace in a treeview with time information.  This time info can be either an absolute time
    or a relative time, or a node cost or branch cost.  In the past, without this hook, I had to reupdate the labels
    of the all treview when the end user was choosing a different display mode, which was very slow when the treeview was
    containing half a million of nodes....  Now, it is just *instant* since I refresh I prepare the node label just
    when it is about to be displayed ;) */

2) Provided additional pictures to be used in a new prospy++ and in ABHack


16-JUN-2007 sla:
 1) New ApplyEntry strong override to really apply entry to the treeview by using the
 focusCatcher combo.

 2) New search by keyStroke feature.  When the focus is in the treeview (actually
 in the hidden focusCatcher static combo), pure4gltv now maintains a string composed
 by the last keystrokes with up to 1500ms between each strike (above this time, the
 string is reset to blank), then searches the next brother node (same parent as current
 node) which label begins with the mentioned string.  The search uses an index against
 parent + label that has always been present, so this feature is very fast and cool.
 When we cannot find a next brother based on the searched label, then pure4gltv tries
 to find a first brother node.  Of course, when the search is successful, then the
 found brother node is selected and brought into the viewPort if necessary with the
 selectNode API

 3) Added protection against unsupported skins like aero now for WinVista (until) someone sends
 me the necessary pictures).  When I find out the theme is not supported in the
 tvSkins directory, pure4gltv goes into classic mode and gives some details in the
 tooltip of the EMPTY square on the right bottom corner.


19-JUN-2007 sla:
New option in addNode of pure4glTv so superceed the autoSort TV option for one single
node, and made abhack use it for the parameters.  See header comment of addNode() for more info


05-JUL-2007 sla:
Review to remove errors returned by functions for compliance with more consistent
error handling introduced in 10.1C
Note: In the past, I had a few FUNCTION capable of returning an ERROR: CollapseNode, ExpandNode and NodeInCollapsedBranch
Actually, the point was more to show my design intention for cases that should not occur.  The AVM
could not raise an error in the caller at this time (it will in 10.1C), so it was not a problem to
just return NO or unknow instead

25-JUL-2007 sla:
Added support of the aero skin for WinVista

04-AUG-2007 sla:
Fixed regression introduced with the search on keyStroke for the '-', '+' and '*' key events
Indeed, they were caught by the focusCatcher combo-box to perform search on keyStroke
instead of letting the fMainKeyEvent procedure doing the initial collapse,
expand or expandAll functionalities

05-AUG-2007 sla:
Fixed little regression due to the previous fix... the CAN-DO('+,-,*', LAST-EVENT:LABEL) could catch
any printable event due to the '*' item  Solution was to use CAN-DO('+,-,~~~*', LAST-EVENT:LABEL)...


03-OCT-2007 sla:
Changed include files and procedure with windows API's' so they remain independent
from the ones that can be supplied with Dynamics or OE 10.
Made a special version of pure4gltv that will use a path relative to {&pure4gltvrootpath}
Introduced a few new preprocessors to manage the path of tvpics and tvskins
all relative to {&pure4gltvrootpath}}

Two consequences:
1)you will obtain the following errors when opening the pure4gltv.w file itself in the AppBuilder
because the AppBuilder does not like to have preprocessors in image file path...
The file (Classic/scrollbuttondown.bmp) for LOAD-IMAGE-UP on the BUTTON widget is invalid on the current system. (206)
 => just ignore them

2) If you save it in graphical mode, then the hardcoded preprocessor will be removed from the three buttons bellow:
DEFINE BUTTON SbtnScrollDown
     IMAGE-UP FILE "{&tvskinpath}Classic/scrollbuttondown.bmp":U NO-FOCUS
     LABEL ""
     SIZE 3.4 BY .81.

DEFINE BUTTON SbtnScrollUp
     IMAGE-UP FILE "{&tvskinpath}Classic/scrollbuttonup.bmp":U NO-FOCUS
     LABEL ""
     SIZE 3.4 BY .81.

DEFINE IMAGE imgsv
     FILENAME "{&tvskinpath}Classic/vscrollbg.jpg":U

 => please modify the generated code to reinclude the preprocessor there


09-OCT-2007 SLP: (Simon L. Prinsoo) Add a double-click event on the nodes in the bottom of the
tree, provided that the "AddOnExpand" is not set for the node.

17-OCT-2007 SLP: Address the following needs:
1) Change the node colour
2) Change the node font

These are achieved by incorporating the code submitted by Dries Feys for changing the font
and at the same time introducing similar code to manage the colour.

30-OCT-2007 SLP:
1) Apply Dries Feys' fix to the bug I introduced on 17-OCT-2007 in "deselectcurrentnodelabel".
2) Move the "DoubleClick" code of 9 Oct to a position after entering the focus catcher.
3) Do the same to existing code in "labLeftMouseEvent"
4) Add code to provide tooltip functionality on nodes.

28-NOV-2007 sla: little review of the code submitted by Simon and packaging

25-JAN-2008 sla: Improved the creation of dyn popup menu on right click by first deleting
a possible existing dyn popup menu.  Before that, it was somtimes necessary to do a double
right-click in order to obtain a wanted popup menu.

05-AUG-2008 sla: fixed the updateNode API so it can also handle the new node options introduced in october last year
*********************************************************************/

/* Create an unnamed pool to store all the widgets created
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

  &GLOB ADMClass pure4glTv

/* 20-AUG-2007 sla: changed window.i and winfunc.i to pure4gltvwindows.i and pure4gltvwinfunc.i
 to remain independent from those Dynamics OE10 issues... */
&SCOPED-DEFINE APIRtnParm  ,OUTPUT DummyAPIRtnVal
&SCOPED-DEFINE APIRtnParmINT  ,OUTPUT DummyAPIRtnValINT

/* 20-AUG-2007 sla: use the following to have an independent version of pure4gltv for abhack */
/* &SCOPED-DEFINE pure4gltvrootpath protools/abhack/ */
&SCOPED-DEFINE pure4gltvrootpath protools/abhack/
/* &SCOPED-DEFINE pure4gltvrootpath */
&SCOPED-DEFINE tvskinpath {&pure4gltvrootpath}tvskins/
&SCOPED-DEFINE tvpicpath {&pure4gltvrootpath}tvpics/

DEFINE VARIABLE DummyAPIRtnVal AS CHARACTER  NO-UNDO.
DEFINE VARIABLE DummyAPIRtnValINT AS INT  NO-UNDO.

{{&pure4gltvrootpath}pure4gltvwindows.i}


/* Local Variable Definitions ---                                       */

  &IF "{&xcInstanceProperties}":U NE "":U &THEN
    &GLOB xcInstanceProperties {&xcInstanceProperties},
  &ENDIF
  &GLOB xcInstanceProperties {&xcInstanceProperties}~
wineMode,windowsSkin,picCacheCoef,labCacheCoef,tvIterationHeight,TreeStyle,~
FocSelNodeBgColor,UnfSelNodeBgColor,tvnodeDefaultFont,~
FocSelNodeFgColor,UnfSelNodeFgColor,resizeVertical,resizeHorizontal,~
DragSource,autoSort,MSkeyScrollForcePaint


  &IF "{&xcTranslatableProperties}":U NE "":U &THEN
    &GLOB xcTranslatableProperties {&xcTranslatableProperties},
  &ENDIF
  &GLOB xcTranslatableProperties {&xcTranslatableProperties}~

&IF DEFINED (ADM-PROPERTY-DLG) = 0 &THEN
  &SCOP ADM-PROPERTY-DLG {&pure4gltvrootpath}pure4glTvInstanceProp.w
&ENDIF


/* sadly, the following one is not in windows.i */
&SCOP BM_SETSTATE 243

&SCOPED-DEFINE fsvWIDTH-PIXELS 17
&SCOPED-DEFINE bsvGraberHEIGHT-PIXELS 8

/* 16-oct-2004 do not need that anymore
(&SCOPED-DEFINE ApplyEntryToFocusCatcher IF FOCUS <> focusCatcher:HANDLE IN FRAME fMain ~
 THEN  APPLY 'ENTRY' TO focusCatcher IN FRAME fMain.*/
&SCOPED-DEFINE ApplyEntryToFocusCatcher APPLY 'ENTRY' TO focusCatcher IN FRAME fMain.
&SCOPED-DEFINE unvalidOptionsInNodeOptn "expanded,selected,addMode=after,addMode=before,refresh"


CREATE WIDGET-POOL.

/* The point of using two differnet widget pools to handle labels and pictures
  is to get best performance with Wine on Linux */
DEFINE VARIABLE gWPNodeLabels AS CHARACTER  NO-UNDO.
DEFINE VARIABLE gWPNodeIcons  AS CHARACTER  NO-UNDO.
gWPNodeLabels = STRING(THIS-PROCEDURE) + "nodeLabels".
gWPNodeIcons  = STRING(THIS-PROCEDURE) + "nodeIcons".
CREATE WIDGET-POOL gWPNodeLabels PERSISTENT.
CREATE WIDGET-POOL gWPNodeIcons PERSISTENT.

/* 15-JUN-2007 sla: support of search with keystrokes */
DEFINE VARIABLE gcKeySearch          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gdLastKeySearchEtime AS DECIMAL     NO-UNDO.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE gid         AS INTEGER           NO-UNDO. /* kind of sequence, we reset to 0 it when emptying the tv */

DEF TEMP-TABLE node NO-UNDO
 FIELD Id        LIKE gid  /* Node ID for treeview, can change without notice */
 FIELD ke          AS CHAR /* node key as char.  Can be ROWID, or key field or whatever like tableName=<ROWID>
                             free key, developpers should use it to search a node from the outside */
 FIELD Par         AS INT  /* id of parent node, 0 if level is 0 */
 /* yeh, having these two pre and nex field leads to redondancy, but it is far better for performance */
 FIELD pre         AS INT  /* prev sibling node at same level, 0 if first child */
 FIELD nex         AS INT  /* next sibling node at same level, 0 if last child */
 FIELD level       AS INT  /* level, 0 if root */
 FIELD lab         AS CHAR /* label */
 FIELD nodeFont    AS INT  /* the fontnumber of the node - SLP 17-OCT-2007 */
 FIELD nodeFGCol   AS INT  /* Color of the node's text - SLP 17-OCT-2007 */
 FIELD nodeBGCol   AS INT  /* Color of the background behind the node's text - SLP 17-OCT-2007 */
 FIELD nodeToolTip AS CHAR /* Tooltip for the node - SLP 30-OCT-2007 */
 FIELD labWidthPixels  AS INT  /* width of label in pixels */
 FIELD ico         AS CHAR /* icone pic, blank if none */
 FIELD expanded    AS LOG  /* yes if expanded */
 FIELD expChildren AS INT  /* number of expanded children, needed to work out height of view port */
 FIELD colChildren AS INT  /* number of collapsed children, needed to work out height of view port */
 FIELD optn        AS CHAR /* open CHR(1) separated list of various options, see addNode API */
 FIELD VWP         AS INT  /*Vitural WidthPixels = labWidthPixels + X (level * gTvLevelWidth + gTvPicWidth) */
 FIELD VWPexpChildren AS INT   /*to manage virtual width-pixels of a branch, mechanism a bit similar to FIELD expChildren to maintain this data (updated at the same time)
                                    => see procedures expandNode and collapseNode  */
 FIELD VWPcolChildren AS INT   /*see expChildrenVWP, field set to value of expChildrenVWP when collapsing the node, and before puting VWP in expChildren VWP*/
 INDEX id IS PRIMARY UNIQUE id /* unique for safety */
 INDEX ke IS UNIQUE ke
 INDEX parPre IS UNIQUE par pre
 INDEX parNex IS UNIQUE par nex
 INDEX pre pre
 INDEX nex nex
 INDEX labWordIndex IS WORD-INDEX lab
 INDEX lab lab
 INDEX parLab par lab /* used for sorting nodes */
 INDEX optn IS WORD-INDEX optn
 INDEX parVWPExpChildren par VWPexpChildren.  /*used when collapsing a or removing a node so we
                                               can find the new largest expChildrenVWP for a branch */

DEFINE VARIABLE gWindowsSkin  AS CHARACTER  INITIAL "Classic" NO-UNDO.
DEFINE VARIABLE gExpChildren  LIKE node.expChildren NO-UNDO. /* TOTAL number of expanded (visible) nodes,
                                                               i.e. sum of all nodes at level 0 + their expChildren */
DEFINE VARIABLE gVWP          AS INTEGER    NO-UNDO. /* virtual width of the the all tree.  The mechanism to update this
                                                        info is a bit similar to the one of gExpChildren, see node.VWP and node.expChildrenVWP*/
DEFINE VARIABLE glPicWithPlusMinus        AS LOG INITIAL YES NO-UNDO. /* yes to use the set of pictures that have a plus/minus on the left
                                                                BEWARE: IT IS FALSE when treestyle is plus/minus + text (no picture) */
DEFINE VARIABLE glOnlyPlusMinus    AS LOGICAL    NO-UNDO.  /* True when treestyle = tvwPlusMinusText*/
DEFINE VARIABLE gCurNode           LIKE node.id NO-UNDO.
DEFINE VARIABLE gCurhLab           AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE gcDefaultIco       AS CHARACTER INITIAL "{&tvpicpath}Folder" NO-UNDO.
DEFINE VARIABLE gRefreshFromNode   LIKE node.id NO-UNDO.
DEFINE VARIABLE gTvIterations      AS INTEGER    NO-UNDO.
DEFINE VARIABLE gPicNumMax         AS INTEGER    NO-UNDO.
DEFINE VARIABLE gLabNumMax         AS INTEGER    NO-UNDO.
DEFINE VARIABLE gPicCacheCoef      AS DECIMAL  INITIAL 1  NO-UNDO. /* >= 1 ; a way to have a large set of pictures than number of Treeview iterations to make a bigger cache*/
DEFINE VARIABLE gLabCacheCoef      AS DECIMAL  INITIAL 1  NO-UNDO. /* same as gPicCacheCoef for labels */
DEFINE VARIABLE gVisibleIterations AS INTEGER    NO-UNDO. /* = gTvIterations or gTvIterations - 1 if horizScrollBar visible*/
DEFINE VARIABLE gTvIterationHeight AS INTEGER  INITIAL 17 NO-UNDO.  /*in pixels */
DEFINE VARIABLE gTvPicWidth        AS INTEGER  INITIAL 32 NO-UNDO.
DEFINE VARIABLE gTvLevelWidth      AS INTEGER  INITIAL 17 NO-UNDO.
DEFINE VARIABLE gIterOffset        AS INTEGER    NO-UNDO. /* To manage scrolling, 0 means first visual iteration is also first
                                                      first virtual iteration, <n> means that there <n> visible (expanded)
                                                      iteration above node at top of the viewport */
DEFINE VARIABLE gHorzScrollingActive  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE gFtvHP                AS INTEGER    NO-UNDO.
DEFINE VARIABLE gFtvWP                AS INTEGER    NO-UNDO. /* WIDTH-FIXELS of frame ftv*/
DEFINE VARIABLE gFtvVWP               AS INTEGER    NO-UNDO. /* VIRTUAL-WIDTH-FIXELS of frame ftv*/
DEFINE VARIABLE gVertScrollingActive  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE gSmallerVertScrollBar AS LOGICAL    NO-UNDO.
DEFINE VARIABLE gBsvY                 AS INTEGER    NO-UNDO. /* Y of bsv vert scroll button*/
DEFINE VARIABLE gBsvHP                AS INTEGER    NO-UNDO. /*Height-pixels of vert scroll button */
DEFINE VARIABLE gFsvHP                AS INTEGER    NO-UNDO.
DEFINE VARIABLE gFsvimgHP             AS INTEGER    NO-UNDO. /*Height-pixels of imgsv in frame fsv (vert scrollbar)*/
DEFINE VARIABLE gFsvimgY              AS INTEGER    NO-UNDO. /*Y of imgsv in frame fsv (vert scrollbar)*/
DEFINE VARIABLE gDoNotRenderVerScrollBar AS LOGICAL NO-UNDO.
DEFINE VARIABLE gGoToNodeAtNextRefresh    LIKE node.id NO-UNDO.
DEFINE VARIABLE glWineMode            AS LOGICAL       NO-UNDO.
DEFINE VARIABLE glMSkeyScrollForcePaint AS LOGICAL     NO-UNDO.

/* handles for widgets that might be either dynamic or static depending on the skin */
DEFINE VARIABLE ghbtnScrollUp   AS HANDLE     NO-UNDO.
DEFINE VARIABLE ghbtnScrollDown AS HANDLE     NO-UNDO.
DEFINE VARIABLE ghbsv           AS HANDLE     NO-UNDO.
DEFINE VARIABLE ghbsvGraber     AS HANDLE     NO-UNDO.

DEF TEMP-TABLE tviter NO-UNDO LABEL "tviter (rendering iteration in treeview)"
 FIELD id             LIKE node.id  /* if unknown then this iteration is empty (hidden)*/
 FIELD iter           LIKE gTvIterations
 FIELD picImg         AS CHAR /* image file-name, worked out here once */
 FIELD hpic           AS WIDGET-HANDLE
 FIELD picX           AS INT
 FIELD picY           AS INT
 FIELD hlab           AS WIDGET-HANDLE
 FIELD labX           AS INT
 FIELD labY           AS INT
 FIELD nodeFont       AS INT  /* SLP 17-OCT-2007 */
 FIELD nodeFGCol      AS INT  /* SLP 17-OCT-2007 */
 FIELD nodeBGCol      AS INT  /* SLP 17-OCT-2007 */
 FIELD nodeTooltip    AS CHAR /* SLP 30-OCT-2007 */
 FIELD labScreenValue AS CHAR
 FIELD labWidthPixels AS INT
 INDEX iter IS UNIQUE PRIMARY iter
 INDEX id   IS UNIQUE id.

DEF TEMP-TABLE tvPic NO-UNDO LABEL "tvPic (rendering picture for a node in treeview)"
 FIELD iter           LIKE gTvIterations
 FIELD hpic           AS WIDGET-HANDLE
 FIELD picX           AS INT
 FIELD picY           AS INT
 FIELD picImg         AS CHAR /* image file-name */
 FIELD picVisible     AS LOG
 INDEX iter IS UNIQUE  iter
 INDEX hpic IS UNIQUE hpic
 INDEX picVisible picVisible
 INDEX picX picX
 INDEX picY picY
 INDEX perf1 picImg picX picY picVisible /* these indices help to gain few ms in optimizationProcedures... */
 INDEX perf2 picImg picX
 INDEX perf3 picImg picY
 INDEX perf4 picImg picVisible
 INDEX perf5 picX picY picVisible.

DEF TEMP-TABLE tvLab NO-UNDO LABEL "tvLab (rendering label for a node in treeview)"
 FIELD iter           LIKE gTvIterations
 FIELD hlab           AS WIDGET-HANDLE
 FIELD labX           AS INT
 FIELD labY           AS INT
 FIELD labVisible     AS LOG
 FIELD labScreenValue AS CHAR
 FIELD labWidthPixels AS INT
 INDEX iter IS UNIQUE iter
 INDEX hlab IS UNIQUE hlab
 INDEX labX labX
 INDEX labY labY
 INDEX labVisible labVisible
 INDEX labScreenValue labScreenValue
 INDEX perf1 labScreenValue labX labY labVisible /* these indices help to gain few ms inoptimizationProcedures... */
 INDEX perf2 labScreenValue labX
 INDEX perf3 labScreenValue labY
 INDEX perf4 labScreenValue labVisible
 INDEX perf5 labX labY labVisible.

DEF TEMP-TABLE copyNode LIKE node LABEL "temp-table copyNode used to hold a copies of node records".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE pure4glTv
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS focusCatcher
&Scoped-Define DISPLAYED-OBJECTS focusCatcher edtSunken

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD collapseNode C-Win
FUNCTION collapseNode RETURNS LOGICAL
  (INPUT pcNodeKe AS CHAR
  ,INPUT pcOptn AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createDynWidgetForVertScrollBar C-Win
FUNCTION createDynWidgetForVertScrollBar RETURNS LOGICAL PRIVATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD deleteBranchOf C-Win
FUNCTION deleteBranchOf RETURNS LOGICAL PRIVATE
  (nodeId AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD deselectCurrentNode C-Win
FUNCTION deselectCurrentNode RETURNS LOG FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD deselectCurrentNodeLabel C-Win
FUNCTION deselectCurrentNodeLabel RETURNS LOGICAL PRIVATE
  ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD expandNode C-Win
FUNCTION expandNode RETURNS LOGICAL
  (INPUT pcNodeKe AS CHAR
  ,INPUT pcOptn   AS CHAR  /*list of option like "refresh" */)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAutoSort C-Win
FUNCTION getAutoSort RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBsvY C-Win
FUNCTION getBsvY RETURNS CHARACTER PRIVATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCharOptValue C-Win
FUNCTION getCharOptValue RETURNS CHARACTER
  ( INPUT pcOptList AS CHARACTER, INPUT pcOption AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDragSource C-Win
FUNCTION getDragSource RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFocSelNodeBgColor C-Win
FUNCTION getFocSelNodeBgColor RETURNS INTEGER FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFocSelNodeFgColor C-Win
FUNCTION getFocSelNodeFgColor RETURNS INTEGER FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIntOptValue C-Win
FUNCTION getIntOptValue RETURNS INTEGER
  ( INPUT pcOptList AS CHARACTER, INPUT pcOption AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLabCacheCoef C-Win
FUNCTION getLabCacheCoef RETURNS DECIMAL FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMSkeyScrollForcePaint C-Win
FUNCTION getMSkeyScrollForcePaint RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNodeId C-Win
FUNCTION getNodeId RETURNS INTEGER
  (pcKe AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNodeKey C-Win
FUNCTION getNodeKey RETURNS CHAR
  (piId AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNodeLocatedAtXY C-Win
FUNCTION getNodeLocatedAtXY RETURNS CHARACTER
  (ipX AS INT
  ,ipY AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNodeParentKey C-Win
FUNCTION getNodeParentKey RETURNS CHAR
  (pcKe AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPicCacheCoef C-Win
FUNCTION getPicCacheCoef RETURNS DECIMAL FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPicturePath C-Win
FUNCTION getPicturePath RETURNS CHARACTER
  ( ic-Pic-Type     AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSelectedNodeKey C-Win
FUNCTION getSelectedNodeKey RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTreeStyle C-Win
FUNCTION getTreeStyle RETURNS INTEGER FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD gettvIterationHeight C-Win
FUNCTION gettvIterationHeight RETURNS INTEGER FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD gettvnodeDefaultFont C-Win
FUNCTION gettvnodeDefaultFont RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUnfSelNodeBgColor C-Win
FUNCTION getUnfSelNodeBgColor RETURNS INTEGER FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUnfSelNodeFgColor C-Win
FUNCTION getUnfSelNodeFgColor RETURNS INTEGER FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getwindowsSkin C-Win
FUNCTION getwindowsSkin RETURNS CHARACTER FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWineMode C-Win
FUNCTION getWineMode RETURNS CHARACTER FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD goToNode C-Win
FUNCTION goToNode RETURNS LOGICAL
  ( ipGoToNode AS INT,
    pcMode AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD greyCurrentSelectedNodeLabel C-Win
FUNCTION greyCurrentSelectedNodeLabel RETURNS LOGICAL PRIVATE
  ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD lastVisibleNode C-Win
FUNCTION lastVisibleNode RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LockfMain C-Win
FUNCTION LockfMain RETURNS LOGICAL PRIVATE
  (lockIt AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MSkeyScrollForcePaint C-Win
FUNCTION MSkeyScrollForcePaint RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD nodeAtVirtualIter C-Win
FUNCTION nodeAtVirtualIter RETURNS INTEGER
  (VIterToGo AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD nodeInCollapsedBranch C-Win
FUNCTION nodeInCollapsedBranch RETURNS LOGICAL
  (ipNode AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD picFileName C-Win
FUNCTION picFileName RETURNS CHARACTER
  (cCode AS CHAR,
   ico   AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD renderVertScrollBar C-Win
FUNCTION renderVertScrollBar RETURNS LOGICAL PRIVATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD resizeVertScrollBar C-Win
FUNCTION resizeVertScrollBar RETURNS LOGICAL PRIVATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD selectNode C-Win
FUNCTION selectNode RETURNS LOGICAL
  (pcNodeKe AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD selectNodeLabel C-Win
FUNCTION selectNodeLabel RETURNS LOGICAL PRIVATE
  (hLab AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAutoSort C-Win
FUNCTION setAutoSort RETURNS LOGICAL
  (lautoSort AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDragSource C-Win
FUNCTION setDragSource RETURNS LOGICAL
  (cDragSource AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFocSelNodeBgColor C-Win
FUNCTION setFocSelNodeBgColor RETURNS LOGICAL
  (iFocSelNodeBgColor AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFocSelNodeFgColor C-Win
FUNCTION setFocSelNodeFgColor RETURNS LOGICAL
  (iFocSelNodeFgColor AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setLabCacheCoef C-Win
FUNCTION setLabCacheCoef RETURNS LOGICAL
  (dlabCacheCoef AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMSkeyScrollForcePaint C-Win
FUNCTION setMSkeyScrollForcePaint RETURNS LOGICAL
  (lMSkeyScrollForcePaint AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPicCacheCoef C-Win
FUNCTION setPicCacheCoef RETURNS LOGICAL
  (dpicCacheCoef AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setScrollBarWidgets C-Win
FUNCTION setScrollBarWidgets RETURNS LOGICAL PRIVATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTreeStyle C-Win
FUNCTION setTreeStyle RETURNS LOGICAL
  (iTreeStyle AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD settvIterationHeight C-Win
FUNCTION settvIterationHeight RETURNS LOGICAL
  (itvIterationHeight AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTvnodeDefaultFont C-Win
FUNCTION setTvnodeDefaultFont RETURNS LOGICAL
  ( itvnodeDefaultFont AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUnfSelNodeBgColor C-Win
FUNCTION setUnfSelNodeBgColor RETURNS LOGICAL
  (iUnfSelNodeBgColor AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUnfSelNodeFgColor C-Win
FUNCTION setUnfSelNodeFgColor RETURNS LOGICAL
  (iUnfSelNodeFgColor AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setwindowsSkin C-Win
FUNCTION setwindowsSkin RETURNS LOGICAL
  (cwindowsSkin AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWineMode C-Win
FUNCTION setWineMode RETURNS LOGICAL
  ( cWineMode AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD topNode C-Win
FUNCTION topNode RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD tvRefresh C-Win
FUNCTION tvRefresh RETURNS LOGICAL
  (/* char def */)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD tvScroll C-Win
FUNCTION tvScroll RETURNS LOGICAL
  ( ipScrollBy AS INT,
    scrollAsMuchAsPossible AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE focusCatcher AS CHARACTER FORMAT "X(256)":U INITIAL "Waiting"
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "MouseWheelUp","Waiting","MouseWheelDown"
     DROP-DOWN-LIST
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE edtSunken AS CHARACTER
     VIEW-AS EDITOR NO-WORD-WRAP
     SIZE 3.6 BY .71 NO-UNDO.

DEFINE BUTTON emptySquare  NO-FOCUS FLAT-BUTTON
     LABEL ""
     SIZE 3.4 BY .76.

DEFINE BUTTON Sbsv  NO-FOCUS
     LABEL ""
     SIZE 3.4 BY .81.

DEFINE BUTTON SbtnScrollDown
     IMAGE-UP FILE "{&tvskinpath}Classic/scrollbuttondown.bmp":U NO-FOCUS
     LABEL ""
     SIZE 3.4 BY .81.

DEFINE BUTTON SbtnScrollUp
     IMAGE-UP FILE "{&tvskinpath}Classic/scrollbuttonup.bmp":U NO-FOCUS
     LABEL ""
     SIZE 3.4 BY .81.

DEFINE IMAGE imgsv
     FILENAME "{&tvskinpath}Classic/vscrollbg.jpg":U
     SIZE 3.4 BY 1.19.

DEFINE RECTANGLE blackClickDownRectangle
     EDGE-PIXELS 0
     SIZE 2.6 BY .57
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     focusCatcher AT ROW 1.24 COL 2 NO-LABEL
     edtSunken AT ROW 2.19 COL 2 NO-LABEL NO-TAB-STOP
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 22 BY 3.14.

DEFINE FRAME fsv
     emptySquare AT ROW 3.14 COL 1.2
     Sbsv AT ROW 1.76 COL 1.2
     SbtnScrollDown AT ROW 2.52 COL 1.2
     SbtnScrollUp AT ROW 1 COL 1.2
     blackClickDownRectangle AT ROW 1.62 COL 1.6
     imgsv AT ROW 1.71 COL 1.4
    WITH 1 DOWN NO-BOX OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 18.8 ROW 1.1
         SIZE 3.4 BY 2.9.

DEFINE FRAME ftv
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1.05
         SCROLLABLE SIZE 18 BY 3.1
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: pure4glTv
   Allow: Basic
   Frames: 0
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB)
  CREATE WINDOW C-Win ASSIGN
         HEIGHT             = 3.33
         WIDTH              = 23.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win
/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME fsv:FRAME = FRAME fMain:HANDLE
       FRAME ftv:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE FRAME-NAME                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME fsv:MOVE-BEFORE-TAB-ITEM (focusCatcher:HANDLE IN FRAME fMain)
       XXTABVALXX = FRAME ftv:MOVE-BEFORE-TAB-ITEM (FRAME fsv:HANDLE)
/* END-ASSIGN-TABS */.

ASSIGN
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR edtSunken IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX focusCatcher IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME fsv
                                                                        */
/* SETTINGS FOR RECTANGLE blackClickDownRectangle IN FRAME fsv
   NO-ENABLE                                                            */
ASSIGN
       blackClickDownRectangle:HIDDEN IN FRAME fsv           = TRUE.

/* SETTINGS FOR BUTTON emptySquare IN FRAME fsv
   NO-ENABLE                                                            */
ASSIGN
       emptySquare:HIDDEN IN FRAME fsv           = TRUE.

/* SETTINGS FOR BUTTON Sbsv IN FRAME fsv
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON SbtnScrollDown IN FRAME fsv
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON SbtnScrollUp IN FRAME fsv
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME ftv
                                                                        */
ASSIGN
       FRAME ftv:HEIGHT           = 3.1
       FRAME ftv:WIDTH            = 18.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME fMain
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain C-Win
ON * OF FRAME fMain
ANYWHERE DO:
    RUN fMainKeyEvent ("*").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain C-Win
ON + OF FRAME fMain
ANYWHERE DO:
    RUN fMainKeyEvent ("+").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain C-Win
ON - OF FRAME fMain
ANYWHERE DO:
    RUN fMainKeyEvent ("-").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain C-Win
ON CURSOR-DOWN OF FRAME fMain
ANYWHERE DO:
  RUN fMainKeyEvent ("CURSOR-DOWN").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain C-Win
ON CURSOR-LEFT OF FRAME fMain
ANYWHERE DO:
    RUN fMainKeyEvent ("CURSOR-LEFT").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain C-Win
ON CURSOR-RIGHT OF FRAME fMain
ANYWHERE DO:
    RUN fMainKeyEvent ("CURSOR-RIGHT").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain C-Win
ON CURSOR-UP OF FRAME fMain
ANYWHERE DO:
  DEFINE VARIABLE mKeyboardState       AS MEMPTR     NO-UNDO.

  SET-SIZE(mKeyboardState) = 256.

  /* Did we get here because of a mouse wheel action or CURSOR-DOWN ? */
  RUN GetKeyboardState(GET-POINTER-VALUE(mKeyboardState) {&APIRtnParmINT}).

  IF GET-BITS(GET-BYTE(mKeyboardState,38) ,8, 1) <> 0  THEN DO:
     focusCatcher:SCREEN-VALUE = "MouseWheelDown".
     APPLY "VALUE-CHANGED" TO focusCatcher.
  END.
  ELSE RUN fMainKeyEvent ("CURSOR-UP").

  SET-SIZE(mKeyboardState) = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain C-Win
ON END OF FRAME fMain
ANYWHERE DO:
    RUN fMainKeyEvent ("END").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain C-Win
ON END-RESIZE OF FRAME fMain
DO:
  /* wanted to trapped frame resized at design time, but the AB does not let
   me trap it.  It may define its own override... */

  MESSAGE 'Design END-RESIZE' SELF:HEIGHT-PIXELS
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  RUN resizeObject(FRAME fMain:HEIGHT-PIXELS, FRAME fMain:WIDTH-PIXELS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain C-Win
ON HOME OF FRAME fMain
ANYWHERE DO:
    RUN fMainKeyEvent ("HOME").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain C-Win
ON LEAVE OF FRAME fMain
ANYWHERE DO:
  DEFINE VARIABLE hLeave AS HANDLE     NO-UNDO.
  DEF BUFFER btviter FOR tviter.

  hLeave = LAST-EVENT:WIDGET-LEAVE.

  /* grey current selected label */
  IF VALID-HANDLE (hLeave)
   AND hLeave:TYPE = "COMBO-BOX"
   AND hLeave:FRAME = FRAME fMain:HANDLE
   AND hLeave:NAME  = "focusCatcher"
   AND gCurNode <> 0 THEN DO:
      FIND btviter WHERE btviter.id = gCurNode NO-ERROR.
      IF AVAIL btviter THEN greyCurrentSelectedNodeLabel().
  END.


RETURN. /* well ... do not care...*/

  /* we are still in the TV */
  IF VALID-HANDLE(FOCUS)
   AND FOCUS:TYPE = "COMBO-BOX"
   AND FOCUS:FRAME = FRAME fMain:HANDLE
   AND FOCUS:NAME = "focusCatcher"
   THEN RETURN.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain C-Win
ON LEFT-MOUSE-DOWN OF FRAME fMain
DO:
  {&ApplyEntryToFocusCatcher}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain C-Win
ON PAGE-DOWN OF FRAME fMain
ANYWHERE DO:
    RUN fMainKeyEvent ("PAGE-DOWN").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain C-Win
ON PAGE-UP OF FRAME fMain
ANYWHERE DO:
    RUN fMainKeyEvent ("PAGE-UP").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain C-Win
ON RETURN OF FRAME fMain
ANYWHERE DO:
    /* SLP 09-OCT-2007 - Add trigger for selecting with ENTER as well */
    APPLY "MOUSE-SELECT-DBLCLICK" TO gCurhlab.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fsv
&Scoped-define SELF-NAME emptySquare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emptySquare C-Win
ON RIGHT-MOUSE-DBLCLICK OF emptySquare IN FRAME fsv
DO:
/* goal: output a message about pure4glTv on right-mouse-double click if
the 3 keys ctrl alt and shift are pressed */
DEFINE VARIABLE mKeyboardState  AS MEMPTR     NO-UNDO.
SET-SIZE(mKeyboardState) = 256.

RUN GetKeyboardState(GET-POINTER-VALUE(mKeyboardState) {&APIRtnParmINT}).

IF    GET-BITS(GET-BYTE(mKeyboardState,17) ,8, 1) = 1 /* shift */
  AND GET-BITS(GET-BYTE(mKeyboardState,18) ,8, 1) = 1 /* ctrl */
  AND GET-BITS(GET-BYTE(mKeyboardState,19) ,8, 1) = 1 /* alt */
 THEN MESSAGE "Pure4gltv.w, a treeview Smart Object in pure 4GL/ABL" SKIP(2)
  "This object as a *perfect* clone of the MS ActiveX.  Its main strengths are:" SKIP
  "  A] It is up to 5 times faster to load nodes in it." SKIP
  "  B] It causes less deployment problems.  Can even run on Linux with Wine" SKIP
  "  C] It is customizable" SKIP(2)
  "Developped by Sébastien Lacroix in October 2004.  Refined on 06/04/2006" SKIP(1)
  "Many thanks to Nicolas Andricq for restructuring the support of multiple skins" SKIP(1)
  "Great Contribution of Simon L Prinsloo and Dries Feys in OCT 2007 to bring the support of node fonts, fgcolor, bgColor and tooltips" SKIP(2)
  "And many thanks to my wife Amandine for her patience regarding the time spent at home;)"
   VIEW-AS ALERT-BOX INFO BUTTONS OK.

SET-SIZE(mKeyboardState) = 0.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME focusCatcher
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL focusCatcher C-Win
ON ANY-PRINTABLE OF focusCatcher IN FRAME fMain
DO:
    DEFINE BUFFER node    FOR node.
    DEFINE BUFFER curNode FOR node.

    /* 04-AUG-2007 sla: fixed regression on + - or * that are caught by this widget now */
    IF CAN-DO("-,+,~~~*", LAST-EVENT:LABEL) THEN DO:
        RUN fMainKeyEvent (LAST-EVENT:LABEL).
        RETURN.
    END.

    IF DECIMAL(ETIME) - gdLastKeySearchEtime > 1500 THEN gcKeySearch = "".
    gcKeySearch = gcKeySearch + LAST-EVENT:LABEL.
    gdLastKeySearchEtime = DECIMAL(ETIME).

    FIND curNode WHERE curNode.id = gCurNode NO-ERROR.
    IF NOT AVAILABLE curNode THEN RETURN.

    FIND NEXT node WHERE node.Par = curNode.Par AND node.lab BEGINS gcKeySearch NO-ERROR.
    IF NOT AVAILABLE node THEN DO:
        FIND FIRST node WHERE node.Par = curNode.Par AND node.lab BEGINS gcKeySearch NO-ERROR.
        RETURN.
    END.

    IF AVAILABLE node THEN selectNode(node.ke).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL focusCatcher C-Win
ON CURSOR-DOWN OF focusCatcher IN FRAME fMain
DO:
  /* need to implement this trigger because the mouseWheel action first
   tries to fire a CURSOR-DOWN or CURSOR-UP, which I have defined at the frame
   level.  If I did not define this trigger at the frame level, then it would
   direclty select the item above and fire VALUE-CHANGED for the COMBO focusCatcher */

  DEFINE VARIABLE mKeyboardState       AS MEMPTR     NO-UNDO.

  SET-SIZE(mKeyboardState) = 256.

  /* Did we get here because of a mouse wheel action or CURSOR-DOWN ? */
  RUN GetKeyboardState(GET-POINTER-VALUE(mKeyboardState) {&APIRtnParmINT}).


  /* 41 = cursor-down */
  IF GET-BITS(GET-BYTE(mKeyboardState,41) ,8, 1) = 0  THEN DO:
     focusCatcher:SCREEN-VALUE = "MouseWheelDown".
     APPLY "VALUE-CHANGED" TO focusCatcher.
  END.
  ELSE RUN fMainKeyEvent ("CURSOR-DOWN").

  SET-SIZE(mKeyboardState) = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL focusCatcher C-Win
ON CURSOR-UP OF focusCatcher IN FRAME fMain
DO:
  /* need to implement this trigger because the mouseWheel action first
   tries to fire a CURSOR-DOWN or CURSOR-UP, which I have defined at the frame
   level.  If I did not define this trigger at the frame level, then it would
   direclty select the item above and fire VALUE-CHANGED for the COMBO focusCatcher */

  DEFINE VARIABLE mKeyboardState       AS MEMPTR     NO-UNDO.

  SET-SIZE(mKeyboardState) = 256.

  /* Did we get here because of a mouse wheel action or CURSOR-DOWN ? */
  RUN GetKeyboardState(GET-POINTER-VALUE(mKeyboardState) {&APIRtnParmINT}).


  IF GET-BITS(GET-BYTE(mKeyboardState,39) ,8, 1) = 0  THEN DO:
     focusCatcher:SCREEN-VALUE = "MouseWheelUp".
     APPLY "VALUE-CHANGED" TO focusCatcher.
  END.
  ELSE RUN fMainKeyEvent ("CURSOR-UP").

  SET-SIZE(mKeyboardState) = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL focusCatcher C-Win
ON ENTRY OF focusCatcher IN FRAME fMain
DO:
  /* Note I need to call this one often to be sure that the mouse wheel support
    works OK.  Indeed, there is a stranger issue at the core level (or windows
    level) that can lead to a situation where FOCUS:focusCatcher but it is like
    if the focus was not really there (mouseWheel action does not fire
    Value-change in focus catcher...
    I have noticed that by doing APPLY ENTRY to focusCatcher even when the focus
    is already there solves this problem.
    That being said, I avoid to call SelectNodeLabel when I find out that the
    wanted node is already selected */

  DEF BUFFER btviter FOR tviter.

  IF gCurNode <> 0 THEN DO:
      FIND btviter WHERE btviter.id = gCurNode NO-ERROR.
      IF AVAIL btviter
       AND gCurhLab <> btviter.hLab /* do not call this function if not needed */
       THEN SelectNodeLabel(btviter.hLab).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL focusCatcher C-Win
ON VALUE-CHANGED OF focusCatcher IN FRAME fMain
DO:
  /* the purpose of this rigger is to support the mouse wheel in the tree */
  DEFINE VARIABLE selfIsFocus AS LOGICAL    NO-UNDO.

  selfIsFocus = FOCUS = {&SELF-NAME}:HANDLE.

  IF SELF:SCREEN-VALUE = "MouseWheelUp" THEN DO:
      tvScroll(-3, YES).
      IF NOT glWineMode AND glMSkeyScrollForcePaint THEN MSkeyScrollForcePaint().
  END.
  IF SELF:SCREEN-VALUE = "MouseWheelDown " THEN DO:
      tvScroll(3, YES).
      IF NOT glWineMode AND glMSkeyScrollForcePaint THEN MSkeyScrollForcePaint().
  END.

  /* reset it so we can reuse it */
  SELF:SCREEN-VALUE = "Waiting".

  /* The following trick seems to solve a problem in the core, at least it makes
  it more reliable, especially when you roll the mouse wheel very fast:
  Sometimes, the focus is no longer in the focusCatcher combo... or it is when you
  query it a first time with widgetWalker, but the mouseWheel does not work anymore,
  and if you query FOCUS a seccond time, then it gives you SESSION:HANDLE.... weird
  Now, the good news is that the FOCUS event does not fire the ENTRY event...  so
  it does not run the code I have put there to handle the selection ;) */
  IF selfIsFocus THEN APPLY "FOCUS" TO {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fsv
&Scoped-define SELF-NAME imgsv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL imgsv C-Win
ON MOUSE-SELECT-DOWN OF imgsv IN FRAME fsv
DO:

/* 23-Oct-2004, added this code to scroll when clikcing above or bellow the scroll thumb
keep scrolling until mouse button is released or until the scroll button is reached*/

  DEFINE VARIABLE cLastEvent           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE mKeyboardState       AS MEMPTR     NO-UNDO.
  DEFINE VARIABLE iFirstClickEtime     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFirstClickProcessed AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iLastEventY          AS INTEGER    NO-UNDO.


  iLastEventY = LAST-EVENT:Y.  /* We consifer the original last-event:Y until the mouse button is released */

  SET-SIZE(mKeyboardState) = 256.
  iFirstClickEtime = ETIME.

  DO WHILE TRUE:
      /* WAIT-FOR a dummy event, what I want is the PAUSE 0 otherwize the UI
         never finds out that we realease the mouse button....*/
      WAIT-FOR 'LEFT-MOUSE-DOWN' OF focusCatcher IN FRAME fMain  PAUSE 0.
      IF LAST-EVENT:FUNCTION <> '' THEN cLastEvent = LAST-EVENT:FUNCTION.

      /* do we still keep the left mouse button pressed ? */
      RUN GetKeyboardState(GET-POINTER-VALUE(mKeyboardState) {&APIRtnParmINT}).

      IF GET-BITS(GET-BYTE(mKeyboardState,2) ,8, 1) = 0  THEN LEAVE. /* 2 is actually 1 + 1 (2nd byte but the first is coded as 0)*/

      /* hidde blackClickDownRectangle after 100s when waiting to continue after first click */
      IF iFirstClickProcessed
       AND ETIME > iFirstClickEtime + 100
       AND ETIME < iFirstClickEtime + 500
       AND blackClickDownRectangle:VISIBLE
       THEN blackClickDownRectangle:VISIBLE = NO.
      /* wait a bit before processing next scroll's */
      IF iFirstClickProcessed AND ETIME < iFirstClickEtime + 500 THEN NEXT.

      /* scroll one page up */
      IF iLastEventY < ghbsv:Y THEN DO:
          blackClickDownRectangle:Y = gFsvimgY.
          blackClickDownRectangle:HEIGHT-PIXELS = gbsvY.
          blackClickDownRectangle:MOVE-TO-TOP().
          blackClickDownRectangle:VISIBLE = YES.
          tvScroll(- gVisibleIterations + 1, YES).
          IF gbsvY > gFsvimgY THEN blackClickDownRectangle:HEIGHT-PIXELS = gbsvY.
      END.
      /* give up because the button is now on the original iLstEventY */
      ELSE IF iLastEventY < ghbsv:Y + gbsvHP THEN LEAVE.
      /* scroll one page down */
      ELSE DO:
          ASSIGN
           blackClickDownRectangle:Y = gbsvY + gbsvHP + gFsvimgY
           blackClickDownRectangle:HEIGHT-PIXELS = MAX(1,gFsvimgHP - gbsvY - gbsvHP) /* when the button is just few pixels above the very bottom, we could get a result smaller than 1  */
           NO-ERROR. /* slacroix 04/04/2006 NO-ERROR required since buttons have been moved by few pixels */
          blackClickDownRectangle:MOVE-TO-TOP().
          blackClickDownRectangle:VISIBLE = YES.
          tvScroll(gVisibleIterations - 1, YES).
          IF gbsvY + gbsvHP + gFsvimgY < gFsvimgHP THEN ASSIGN
           blackClickDownRectangle:HEIGHT-PIXELS = MAX(1,gFsvimgHP - gbsvY - gbsvHP) /*see reson for MAX(1, above)*/
           blackClickDownRectangle:Y = gbsvY + gbsvHP + gFsvimgY
           NO-ERROR.  /* slacroix 04/04/2006 NO-ERROR required since buttons have been moved by few pixels */
      END.

      iFirstClickProcessed = YES.

      IF cLastEvent <> "" THEN LEAVE. /* just in case, but actually, we should find out that the button was released earlier */
  END.

  blackClickDownRectangle:VISIBLE = NO.
  blackClickDownRectangle:Y = 1.
  blackClickDownRectangle:HEIGHT-PIXELS = 5.

  /* The above handling of blackClickDownRectangle can hide the scroll button */
  IF ghbsv:VISIBLE THEN ghbsv:MOVE-TO-TOP().
  IF VALID-HANDLE(ghbsvGraber) AND ghbsvGraber:VISIBLE THEN ghbsvGraber:MOVE-TO-TOP().

  SET-SIZE(mKeyboardState) = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Sbsv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Sbsv C-Win
ON MOUSE-SELECT-DOWN OF Sbsv IN FRAME fsv
DO:
  RUN vertScrollFollowMouse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SbtnScrollDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SbtnScrollDown C-Win
ON MOUSE-SELECT-DOWN OF SbtnScrollDown IN FRAME fsv
DO:
  RUN MouseSelectDownBtnScrollDown.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SbtnScrollUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SbtnScrollUp C-Win
ON MOUSE-SELECT-DOWN OF SbtnScrollUp IN FRAME fsv
DO:
  RUN MouseSelectDownBtnScrollUp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win


/* ***************************  Main Block  *************************** */

  /* pure4glTv-specific properties which are in the property temp-table. */

  &GLOBAL-DEFINE xpwineMode
  &GLOBAL-DEFINE xpwindowsSkin
  &GLOBAL-DEFINE xppicCacheCoef
  &GLOBAL-DEFINE xplabCacheCoef
  &GLOBAL-DEFINE xptvIterationHeight
  &GLOBAL-DEFINE xpTreeStyle
  &GLOBAL-DEFINE xpFocSelNodeBgColor
  &GLOBAL-DEFINE xpFocSelNodeFgColor
  &GLOBAL-DEFINE xpUnfSelNodeBgColor
  &GLOBAL-DEFINE xpUnfSelNodeFgColor
  &GLOBAL-DEFINE xptvnodeDefaultFont
  &GLOBAL-DEFINE xpresizeVertical
  &GLOBAL-DEFINE xpresizeHorizontal
  &GLOBAL-DEFINE xpdragSource
  &GLOBAL-DEFINE xpautoSort
  &GLOBAL-DEFINE xpMSkeyScrollForcePaint


  /* Now include the other props files which will start the ADMProps def. */
  {src/adm2/visprop.i}

  &GLOBAL-DEFINE tvwTextOnly                      0
  &GLOBAL-DEFINE tvwPictureText                   1
  &GLOBAL-DEFINE tvwPlusMinusText                 2
  &GLOBAL-DEFINE tvwPlusPictureText               3
  &GLOBAL-DEFINE tvwTreelinesText                 4
  &GLOBAL-DEFINE tvwTreelinesPictureText          5
  &GLOBAL-DEFINE tvwTreelinesPlusMinusText        6
  &GLOBAL-DEFINE tvwTreelinesPlusMinusPictureText 7

  /* and then we add our pure4gl property defs to that... */
  ghADMProps:ADD-NEW-FIELD('wineMode':U,    'CHAR':U, 0, ?, 'Automatic').
  ghADMProps:ADD-NEW-FIELD('windowsSkin':U, 'CHAR':U, 0, ?, 'Automatic').
  ghADMProps:ADD-NEW-FIELD('picCacheCoef':U, 'DEC':U, 0, ?, 1).
  ghADMProps:ADD-NEW-FIELD('labCacheCoef':U, 'DEC':U, 0, ?, 1).
  ghADMProps:ADD-NEW-FIELD('tvIterationHeight':U, 'INT':U, 0, ?, 17).
  ghADMProps:ADD-NEW-FIELD('TreeStyle':U, 'INT':U, 0, ?, {&tvwPlusPictureText}).
  ghADMProps:ADD-NEW-FIELD('FocSelNodeBgColor':U  , 'INT':U, 0, ?, 1).
  ghADMProps:ADD-NEW-FIELD('FocSelNodeFgColor':U  , 'INT':U, 0, ?, 15).
  ghADMProps:ADD-NEW-FIELD('UnfSelNodeBgColor':U, 'INT':U, 0, ?, 8).
  ghADMProps:ADD-NEW-FIELD('UnfSelNodeFgColor':U, 'INT':U, 0, ?, 0).
  ghADMProps:ADD-NEW-FIELD('tvnodeDefaultFont':U, 'INT':U, 0, ?, 1).
  ghADMProps:ADD-NEW-FIELD('dragSource':U,  'CHAR':U, 0, ?, 'none').
  ghADMProps:ADD-NEW-FIELD('autoSort':U,     'LOG':U, 0, ?, NO).
  ghADMProps:ADD-NEW-FIELD('MSkeyScrollForcePaint':U,     'LOG':U, 0, ?, YES).


  /* Now include our parent class file for visual objects. */
  {src/adm2/visual.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addNode C-Win
PROCEDURE addNode :
/*------------------------------------------------------------------------------
  Purpose:    Add a node to the treeview.  A node is added to a parent node as
             a last child.  We cannot insert t between two children for now
             (easy to implement an option later...)


  Parameters:

  pcKe:  User key used for the node to add
         if blank, then we generate a key with "generatedKey-<bideId>>"

  pcKePar: user key of parent, or "#par=<id of parent node>"

  pcLab: label of the node

  pcIco: icon or set of image.  If blank then classic folder is used.
     => see picFileName() function to see how implement a new set of images


  pcOptn:   An extendable CHR(1) separated list of options, for the node to add,
           or for the addNode API itself (such as "refresh"):
            Actually, options that are not valid for the addNode API itself
           are considered as options for the node record to be added
            Unknown options are taken as custom options and are stored in node.optn

    --- node options considered when adding the node, but not kept in node.optn -------
    expanded       node initially expanded

    selected       Go to that node and select it at the next tvRefresh()


    --- node options stored in node.optn for later use -------------------------
    addOnExpand     Handle it as a collapsed node even when it has no child
                     expanding it will result in publish "tvnodeEvent" with
                     event "addOnExpand" then it will expand the node

    InViewPortIfPossible   In next tvRefresh() will try to make the node
                          visible in the view port by scrolling the treeview
                          if necessary

    dragSource    When the property DragSource is set to "some", then a drag
                  operation is allowed for nodes that have the option
                  "dragSource"


    noDragSource  Disable the drag event even when the property DragSource is set
                  to "all"
                  => Note also that another way to disable a drag operation
                     is to make tvNodeEvent "dragBegin" RETURN "cancelDrag"


    --- option for the addNode API itself (not for the node to add) ------------
    AddMode=after   Instead of adding the node as a child of pcKePar, the
                   node is added as a next sibling of node pcKePar
                    => to do that, we find the parent of pcKePar, add the node
                  to it then run MoveNode to move after the wanted node
           ** if autoSort is set, then this option is used only to retrieve the parent node
              but the new node is added at a place that is compliant with the sort

    AddMode=before  Instead of adding the node as a child of pcKePar, the
                   node is added as a prev sibling of node pcKePar
                    => to do that, we find the parent of pcKePar, add the node
                  to it then run MoveNode to move before the wanted node
           ** if autoSort is set, then this otion is used only to retrieve the parent node
              but the new node is added at a place that is compliant with the sort


    refresh       Calls tvrefresh() at the end of the addNode process


    AutoSort=yes/no  19-JUN-2007 sla new feature.  New option to superceed the autoSort TV
                     option for one single node

    font=n      17-OCT-2007 SLP New option to set the node text font, where n is the font
                number in the font table.

    fgcolor=n   17-OCT-2007 SLP New option to set the node text color, where n is the
                colour number in the colour table.

    bgcolor=n   17-OCT-2007 SLP New option to set the background colour behind the
                node text, where n is the colour number in the colour table.

    tooltip=xxx 30-OCT-2007 SLP New option to set the text of the node tooltip.

------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER pcKe    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcKePar AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcLab   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcIco   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcOptn  AS CHARACTER  NO-UNDO.

DEFINE VARIABLE ipar       LIKE node.par   INITIAL 0  NO-UNDO.
DEFINE VARIABLE iLevel     LIKE node.level INITIAL 0  NO-UNDO. /* level, = parentNode.level + 1 or 0 if no pcKePar is blank (root node) */
DEFINE VARIABLE cIco       LIKE node.ico   INITIAL "" NO-UNDO.
/* 17-OCT-2007 SLP: Rename the variable, because it may now carry either the default font
 * or the custom font.
DEFINE VARIABLE itvnodeDefaultFont AS INTEGER INIT ?  NO-UNDO.*/
DEFINE VARIABLE itvnodeFont    AS INTEGER INIT ?  NO-UNDO. /* SLP 17-OCT-2007 */
DEFINE VARIABLE lAutoSort      AS LOGICAL         NO-UNDO.
DEFINE VARIABLE cAfterBeforeKe AS CHARACTER       NO-UNDO.
DEFINE VARIABLE iFGCol         AS INTEGER         NO-UNDO. /* SLP 17-OCT-2007 */
DEFINE VARIABLE iBGCol         AS INTEGER         NO-UNDO. /* SLP 17-OCT-2007 */
DEFINE VARIABLE cTooltip       AS CHARACTER       NO-UNDO. /* SLP 30-OCT-2007 */
DEFINE VARIABLE icount      AS INTEGER    NO-UNDO.
DEFINE VARIABLE oneOptn     AS CHARACTER  NO-UNDO.

DEF BUFFER bnode      FOR node.
DEF BUFFER parentNode FOR node.
/* Not avail prevSibling node when first child of a parent node, or very first root node
   If avail prevSibling, then we assign node.pre = prevSibling.id */
DEF BUFFER prevSibling FOR node.

/* SLP 17-OCT-2007 - No need to get the default if an option was specified
{get tvnodeDefaultFont itvnodeDefaultFont}.
{get AutoSort lAutoSort}.
*/

/* SLP 17-OCT-2007 */
ASSIGN itvnodeFont = getIntOptValue(pcOptn,"font")    /* May return ? */
       iFGCol      = getIntOptValue(pcOptn,"fgcolor") /* May return ? */
       iBGCol      = getIntOptValue(pcOptn,"bgcolor") /* May return ? */
       cTooltip    = getCharOptValue(pcOptn,"tooltip") /* SLP 30-OCT-2007 */
       .
IF itvnodeFont = ?
THEN DO:
    {get tvnodeDefaultFont itvnodeFont}.
END.
/* SLP 17-OCT-2007 - End*/


/* 19-JUN-2007 sla: new autoSort option */
IF LOOKUP("autoSort=no", pcOptn, CHR(1)) > 0 THEN lAutoSort = NO.
ELSE IF LOOKUP("autoSort=yes", pcOptn, CHR(1)) > 0 THEN lAutoSort = YES.
/* SLP 17-OCT-2007 - No need to get the default unless sorting was not specified */
ELSE DO:
    {get AutoSort lAutoSort}.
END.
/* SLP 17-OCT-2007 - End */

/* insert node after or before pcKePar */
IF  LOOKUP("AddMode=after", pcOptn, CHR(1)) <> 0
 OR LOOKUP("AddMode=before", pcOptn, CHR(1)) <> 0
 THEN DO:
    cAfterBeforeKe = pcKePar.
    FIND bnode WHERE bnode.ke = cAfterBeforeKe NO-ERROR.
    IF NOT AVAIL bnode THEN RETURN ERROR "addNode failed:  Cannot find previous or next node with key = " + pcKePar.
    ASSIGN
     ipar = bnode.par
     iLevel = bnode.level.
END.
/* Attach that node to a parent*/
ELSE IF pcKePar <> "" THEN DO:
    IF pcKePar BEGINS "#par=" THEN
     FIND FIRST parentNode WHERE parentNode.id = INT(ENTRY(2,pcKePar,"=")) NO-ERROR.
    ELSE FIND FIRST parentNode WHERE parentNode.ke = pcKePar NO-ERROR.
    IF NOT AVAIL parentNode THEN RETURN ERROR "addNode failed:  Cannot find parent node with key = " + pcKePar.
    ASSIGN
     ipar = parentNode.id
     iLevel = parentNode.level + 1.
END.

/* prepare to connect new node to last child of parent (or last child at level 0)
 if autoSort is set, then we will move the node at the end of the process  */
/* There is a unique index on par + nex, so do not need the FIRST option */
FIND prevSibling WHERE prevSibling.par = ipar
                   AND prevSibling.nex = 0 NO-ERROR.

/*===========================================================================
 At this point:
1) if not avail prevSibling, then it means we are either adding a very first
child to a parent or creating the very first root node
2) if ipar is 0, then it means we are adding the node at level 0
============================================================================*/

/* Place to handle default for ico, plusMinus depending on pcOptn  (will refine in later dev) */
cIco = pcIco.
IF pcIco = "" THEN cIco = gcDefaultIco. /* default */

IF AVAIL prevSibling THEN prevSibling.nex = -1. /* temporary value to pass the VALIDATE on node buffer*/

CREATE bnode.
ASSIGN
 gid                  = gid + 1
 bnode.id             = gid
 bnode.par            = ipar
 bnode.pre            = IF AVAIL prevSibling THEN prevSibling.id ELSE 0
 bnode.ke             = IF pcKe = "" THEN "generatedKey-" + STRING(gid) ELSE pcKe
 bnode.lab            = pcLab
 bnode.nodeFont       = itvnodeFont /* SLP 17-OCT-2007 */
 bnode.nodeFGCol      = IF iFGCol <> ? /* SLP 17-OCT-2007 */
                        THEN iFGCol
                        ELSE FRAME ftv:FGCOLOR
 bnode.nodeBGCol      = IF iBGCol <> ? /* SLP 17-OCT-2007 */
                        THEN iBGCol
                        ELSE FRAME ftv:BGCOLOR
 bnode.nodeTooltip    = cTooltip /* 30-OCT-2007 */
 bnode.Level          = iLevel
 bnode.expanded       = LOOKUP("expanded", pcOptn, CHR(1)) <> 0
 /* SLP 17-OCT-2007
 bnode.labWidthPixels = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(bnode.lab, itvnodeDefaultFont) + 4*/
 bnode.labWidthPixels = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(bnode.lab, bnode.nodeFont) + 4 /* SLP 17-OCT-2007 */
 bnode.VWP            = bnode.labWidthPixels + ilevel * gTvLevelWidth + gTvPicWidth + 3 /* 3 = 2 + 1 (cosmetic) ,see how I work out picx and Labx in tvRefresh() */
 bnode.VWPexpChildren = bnode.VWP     /* start with that since this is the one we pass to the parent */
 bnode.VWPcolChildren = bnode.VWP   /* start with that too... */
 bnode.ico            = cIco NO-ERROR.
VALIDATE bnode NO-ERROR. /* Any problem with unique index such as ke not being unique ? */

IF ERROR-STATUS:ERROR THEN DO:
    DELETE bnode. /* clean it up */
    IF AVAIL prevSibling THEN prevSibling.nex = 0. /* put 0 back */
    RETURN ERROR
     "AddNode failed to assign fields for new node with pcKe=" + pcKe + " pcKePar=" + pcKePar
     + " pcLab=" + pcLab + " pcIco=" + pcIco + " pcOptn=" + pcOptn
     + "~n Progress Error:" + ERROR-STATUS:GET-MESSAGE(1).
END.

/* handle known options that are not stored in node.optn ("expanded" has already been handled) */
IF LOOKUP("selected", pcOptn, CHR(1)) <> 0 THEN gGoToNodeAtNextRefresh = gid.

/* handle options to store in node.optn */
DO iCount = NUM-ENTRIES(pcOptn, CHR(1)) TO 1 BY -1: /* go backward for performance sake (NUM-ENTRIES is evaluated only once)*/
    oneOptn = ENTRY(iCount,pcOptn, CHR(1)).
    IF LOOKUP(oneOptn, {&unvalidOptionsInNodeOptn}) = 0
    AND (oneOptn BEGINS "font=")    = FALSE     /* SLP 170-OCT-2007 */
    AND (oneOptn BEGINS "fgcolor=") = FALSE     /* SLP 17-OCT-2007 */
    AND (oneOptn BEGINS "bgcolor=") = FALSE     /* SLP 17-OCT-2007 */
    AND (oneOptn BEGINS "tooltip=") = FALSE     /* SLP 30-OCT-2007 */
    THEN bnode.optn = bnode.optn + CHR(1) + oneOptn.
END.

IF bnode.optn <> "" THEN bnode.optn = SUBSTR(bnode.optn,2). /* remove leading comma (dummy empty entry)*/

/* now connect prev sibling to new node (we have avoided a unique index conflict) */
IF AVAIL prevSibling THEN prevSibling.nex = bnode.id. /* was temporary set to -1 */


/* update colChildren of parent if this parent is collapsed
  update expChildren of all parents ( parent of aprent and so on)
  until we reach a root node (then update gExpChildren variable)
  or a collapsed parent
   also update VWPcolChildren and VWPexpChildren of parents to handle virtual width */

DO WHILE TRUE:
    IF bnode.par = 0 THEN DO:
        ASSIGN
         gExpChildren = gExpChildren + 1
         gVWP = MAX(gVWP, bnode.VWPexpChildren).
        LEAVE.
    END.

    FIND parentNode WHERE parentNode.id = bnode.par.

    IF NOT parentNode.expanded THEN DO:
        ASSIGN
         parentNode.colChildren = parentNode.colChildren + 1
         parentNode.VWPcolChildren = MAX(parentNode.VWPcolChildren,bnode.VWPexpChildren).
        LEAVE.
    END.
    ASSIGN
     parentNode.expChildren = parentNode.expChildren + 1
     parentNode.VWPexpChildren = MAX(parentNode.VWPexpChildren,bnode.VWPexpChildren).
    FIND bnode WHERE bnode.id = parentNode.id.
END.


/* move node after/before/sorted position depending on option and autoSort */
IF lautoSort THEN DO:
    FIND bnode WHERE bnode.ke = pcKe. /* refetch the new node in bnode*/
    FIND LAST prevsibling WHERE prevsibling.par = bnode.par
     AND prevsibling.lab < bnode.lab
     USE-INDEX parlab NO-ERROR.

    /* Have to move the node after another one */
    IF AVAIL prevsibling
     AND prevsibling.id <> bnode.pre /*well, the node is already at the right place ;)*/
     THEN RUN moveNode (pcKe, prevsibling.ke, "after" ,"").

    /* perhaps the new node should be the first child */
    IF NOT AVAIL prevsibling THEN DO:

        FIND prevsibling WHERE prevsibling.par = bnode.par AND prevsibling.pre = 0. /*cannot make any error*/
        IF prevsibling.id <> bnode.nex /*otherwise, already next-sibling, we should not happen since the new node was added as last child */
         THEN RUN moveNode (pcKe, prevsibling.ke, "before" ,"").
    END.
END.
ELSE DO:
    IF LOOKUP("AddMode=after", pcOptn ,CHR(1)) <> 0 THEN RUN moveNode (pcKe, cAfterBeforeKe, "after" ,"").
    IF LOOKUP("AddMode=before",pcOptn ,CHR(1)) <> 0 THEN RUN moveNode (pcKe, cAfterBeforeKe, "before" ,"").
END.


IF LOOKUP("refresh", pcOptn ,CHR(1)) <> 0 THEN tvRefresh().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddNToLevelOfBranchOf C-Win
PROCEDURE AddNToLevelOfBranchOf PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER toAdd AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER parId LIKE node.id  NO-UNDO.

DEF BUFFER bnode  FOR node.
DEF BUFFER bnode2 FOR node.


FOR EACH bnode WHERE bnode.par = parId:
    IF CAN-FIND(FIRST bnode2 WHERE bnode2.par = bnode.id)
     THEN RUN AddNToLevelOfBranchOf (toAdd, bnode.id).
    ASSIGN
     bnode.level = bnode.level + toAdd
     bnode.VWP   = bnode.VWP + toAdd * gTvLevelWidth
     bnode.VWPexpChildren = bnode.VWPexpChildren + toAdd * gTvLevelWidth
     bnode.VWPcolChildren = bnode.VWPcolChildren + toAdd * gTvLevelWidth.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects C-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyEntry C-Win
PROCEDURE applyEntry :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:      15-JUN-2007 sla:  new applyEntry strong override to force the entry
            INTO focusCatcher
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcDummy AS CHARACTER   NO-UNDO.

APPLY 'ENTRY' TO focusCatcher IN FRAME fMain.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildAndOpenPopupMenu C-Win
PROCEDURE buildAndOpenPopupMenu :
/*------------------------------------------------------------------------------
  Purpose:     build and Open Popup Menu parented by pcKey
               This API is called automatically from labLeftMouseEvent when
               the publish of tvNodeEvent "rightclick" gets a RETURN-VALUE back
               from the linked object

  Parameters:
    pcKey  the key of the node that is going to parent the popup menu

    pcLabelsEvents  Comma separated list of item label and item event to fire
                    when the menu is chosen.

                    Example:
                    "Hello world,MenuHelloWorld,RULE,,Add Customer,MenuAddCust"
                    => Choosing the Item with label "Add Customer" will fire:
                    PUBLISH "tvNodeEvent" ("MenuAddCust" , pcMenuParentNodeKey).

                    Note that if an item label is set to RULE (in *capital* case),
                    then a the item is taken as a RULE subtype menu item



  Notes:       It is a good practice to prefix the events with something like
               "Menu" or "PopupMenu" so it is easier to handle it in the
               tvNodeEvent target procedure

               I do not handle the case when a pair is missing.  In this
               situation one will hit:
                ERROR Entry <entry#> is outside the range of list <list-string>. (560)

               If a blank or invalid key is passed, then the menu is simply not
               parented to any node (We actually parent it to the frame).

               This API should normally be called from labLeftMouseEvent on
               right mouse click with key of the current selected node (bellow
               the mouse pointer) but nothing prevents you from calling directly
               One little drawback is that it will open the menu at the current
               location of the mouse pointer...
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcKey          AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcLabelsEvents AS CHARACTER  NO-UNDO.

DEFINE VARIABLE iCount           AS INTEGER    NO-UNDO.
DEFINE VARIABLE cLabel           AS CHARACTER  CASE-SENSITIVE NO-UNDO.
DEFINE VARIABLE cEvent           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hPopupMenu       AS HANDLE     NO-UNDO.
DEFINE VARIABLE hPopupItem       AS HANDLE     NO-UNDO.
DEFINE VARIABLE cPopupWidgetPool AS CHARACTER  NO-UNDO.

cPopupWidgetPool = STRING(THIS-PROCEDURE) + "PopupMenu".

DELETE WIDGET-POOL cPopupWidgetPool NO-ERROR.
CREATE WIDGET-POOL cPopupWidgetPool PERSISTENT.

CREATE MENU hPopupMenu IN WIDGET-POOL cPopupWidgetPool
 ASSIGN POPUP-ONLY = TRUE.

ON 'MENU-DROP':U OF hPopupMenu PERSISTENT RUN PopupMenuDrop IN THIS-PROCEDURE.

DO iCount = 1 TO NUM-ENTRIES(pcLabelsEvents) BY 2:
    cLabel = ENTRY(icount     ,pcLabelsEvents).
    cEvent = ENTRY(iCount + 1 ,pcLabelsEvents). /* Will make error if no corresponding pair */

    IF cLabel = "RULE" THEN DO: /* case sensitive test, so one can make a item with label of 'Rule' */
        CREATE MENU-ITEM hPopupItem IN WIDGET-POOL cPopupWidgetPool
         ASSIGN
          SUBTYPE = "RULE"
          PARENT = hPopupMenu.
        NEXT.
    END.

    CREATE MENU-ITEM hPopupItem IN WIDGET-POOL cPopupWidgetPool
     ASSIGN
      PARENT = hPopupMenu
      LABEL = cLabel.

    ON 'CHOOSE':U OF hPopupItem PERSISTENT RUN PopupMenuItemChoosen
     IN THIS-PROCEDURE ("tvNodeEvent", cEvent, pcKey).
END.

FRAME ftv:POPUP-MENU     = hPopupMenu.

RUN SendMessage{&A} IN hppure4gltvApi (FRAME ftv:HWND, 517, 0, 0 {&APIRtnParmINT}).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createLab C-Win
PROCEDURE createLab PRIVATE :
/*------------------------------------------------------------------------------
  Purpose: Creates btvlab record
           Create a TEXT widget with handle in btvlab.hlab

  Parameters: see bellow
  Notes:
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER piX              AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER piY              AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER plVisible        AS LOGICAL    NO-UNDO.
DEFINE INPUT  PARAMETER pcLabScreenValue AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER piLabWidthPixels AS INTEGER    NO-UNDO.

DEFINE VARIABLE itvnodeDefaultFont AS INTEGER    NO-UNDO.

{get tvnodeDefaultFont itvnodeDefaultFont}.

DEF BUFFER btvlab FOR tvlab.


IF gLabNumMax > gLabCacheCoef * gTvIterations THEN DO:
    MESSAGE PROGRAM-NAME(1) "in" THIS-PROCEDURE:FILE-NAME SKIP
     "attempt to create a new rendering dynamic TEXT (node label) whereas there are already"
     gLabNumMax "created TEXTs for " gTvIterations "Treeview iterations with a"
     "gLabCacheCoef (cache coefficient see definition block) of" gLabCacheCoef SKIP
     "A RETURN ERROR is going to occur"
     VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Debug message".
    RETURN ERROR.
END.

gLabNumMax = gLabNumMax + 1.


CREATE btvlab.
ASSIGN
 btvlab.iter = ?
 btvlab.labX = piX
 btvlab.labY = piY
 btvlab.labVisible = plVisible
 btvlab.LabScreenValue = pcLabScreenValue
 btvlab.labWidthPixels = piLabWidthPixels.


CREATE TEXT btvlab.hlab IN WIDGET-POOL gWPNodeLabels ASSIGN
 FRAME = FRAME ftv:HANDLE
 NAME = "tvlab.hlab #" + STRING(gLabNumMax)
 X = btvlab.labX
 Y = btvlab.labY
 FONT = itvnodeDefaultFont
 FORMAT = "X(256)"
 HEIGHT-PIXELS = gTvIterationHeight
 WIDTH-PIXELS = btvlab.labWidthPixels
 SENSITIVE = YES
 SCREEN-VALUE = btvlab.LabScreenValue
 HIDDEN = NOT btvlab.labVisible
 TRIGGERS:
   ON 'LEFT-MOUSE-DOWN'       PERSISTENT RUN labLeftMouseEvent IN THIS-PROCEDURE ('LEFT-MOUSE-DOWN').
   ON 'RIGHT-MOUSE-CLICK'     PERSISTENT RUN labLeftMouseEvent IN THIS-PROCEDURE ('RIGHT-MOUSE-CLICK').
   ON 'MOUSE-SELECT-DBLCLICK' PERSISTENT RUN labLeftMouseEvent IN THIS-PROCEDURE ('MOUSE-SELECT-DBLCLICK').
 END TRIGGERS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createPic C-Win
PROCEDURE createPic PRIVATE :
/*------------------------------------------------------------------------------
  Purpose: Creates btvpic record
           Create an IMAGE widget with handle in btvpic.hpic

  Parameters: see bellow
  Notes:
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER piX           AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER piY           AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER plVisible     AS LOGICAL    NO-UNDO.
DEFINE INPUT  PARAMETER pcPicFileName AS CHARACTER  NO-UNDO.

DEFINE VARIABLE lMthRtn AS LOGICAL    NO-UNDO.

DEF BUFFER btvpic FOR tvpic.

IF gPicNumMax > gPicCacheCoef * gTvIterations THEN DO:
    MESSAGE PROGRAM-NAME(1) "in" THIS-PROCEDURE:FILE-NAME SKIP
     "attempt to create a new rendering dynamic IMAGE whereas there are already"
     gPicNumMax "created pictures for " gTvIterations "Treeview iterations with a"
     "gPicCacheCoef (cache coefficient see definition block) of" gPicCacheCoef SKIP
     "A RETURN ERROR is going to occur"
     VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Debug message".
    RETURN ERROR.
END.

gPicNumMax = gPicNumMax + 1.

CREATE btvpic.
ASSIGN
 btvpic.iter = ?
 btvpic.picX = piX
 btvpic.picY = piY
 btvpic.picImg = pcPicFileName
 btvpic.picVisible = plVisible.


CREATE IMAGE btvpic.hpic IN WIDGET-POOL gWPNodeIcons ASSIGN
 FRAME = FRAME ftv:HANDLE
 NAME = "tvpic.hpic #" + STRING(gPicNumMax)
 X = btvpic.picX
 Y = btvpic.picY
 HEIGHT-PIXELS = gTvIterationHeight
 WIDTH-PIXELS = gtvpicWidth /* + 1  I need this extra pixel  */
 HIDDEN = NOT btvpic.picVisible
 SENSITIVE = YES
 TRIGGERS:
   ON 'LEFT-MOUSE-DOWN'       PERSISTENT RUN picLeftMouseEvent IN THIS-PROCEDURE ('LEFT-MOUSE-DOWN').
   ON 'RIGHT-MOUSE-CLICK'     PERSISTENT RUN picLeftMouseEvent IN THIS-PROCEDURE ('RIGHT-MOUSE-CLICK').
   ON 'MOUSE-SELECT-DBLCLICK' PERSISTENT RUN picLeftMouseEvent IN THIS-PROCEDURE ('MOUSE-SELECT-DBLCLICK').
 END TRIGGERS.

lMthRtn = btvpic.hpic:LOAD-IMAGE(btvpic.picImg) NO-ERROR.

IF NOT lMthRtn THEN DO:
    /* Do that if we cannot find the picture */
    btvpic.hpic:LOAD-IMAGE("{&tvpicpath}missingPicFile.bmp") NO-ERROR.
    btvpic.hpic:TOOLTIP = "Cannot find picture file " + btvpic.picImg.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteNode C-Win
PROCEDURE deleteNode :
/*------------------------------------------------------------------------------
     Purpose:     Delete a given node, and the branch it parents if any
  Parameters:
      pcKe:   User key used for the node to delete

    pcOptn: extendable comma separated list of options:
       refresh       Calls tvrefresh() at the end of the deletion process

      Notes:
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER pcKe    AS CHARACTER  NO-UNDO. /* user key to put in node.ke */
DEFINE INPUT  PARAMETER pcOptn  AS CHARACTER  NO-UNDO.

DEFINE VARIABLE inex  LIKE node.nex   INITIAL 0  NO-UNDO.
DEFINE VARIABLE ipre  LIKE node.pre   INITIAL 0  NO-UNDO.
DEFINE VARIABLE iiter AS INTEGER      NO-UNDO.
DEFINE VARIABLE iterOfNodeToDelete AS INTEGER    NO-UNDO.
DEFINE VARIABLE nextNodeToShow AS INTEGER    NO-UNDO.
DEFINE VARIABLE prevNodeToShow AS INTEGER    NO-UNDO.


DEF BUFFER nodeToDelete  FOR node.
DEF BUFFER bnode         FOR node.
DEF BUFFER parentNode    FOR node.
DEF BUFFER widestBrother FOR node.
DEF BUFFER btviter FOR tviter.


FIND FIRST nodeToDelete WHERE nodeToDelete.ke = pcKe NO-ERROR.
IF NOT AVAIL nodeToDelete THEN RETURN ERROR "deleteNode failed:  Cannot find node to delete with key = " + pcKe.
FIND bnode WHERE bnode.id = nodeToDelete.id.

/* Now update all parents about colChildren and expChildren as well
 as VWPexpChildren about the removal of nodeToDelete
 The algorythm is similar to the one of collapseNode */
DO WHILE TRUE:
    /* widestBrother is not AVAIL only when no brother */
    FIND LAST widestBrother WHERE
     widestBrother.par = bnode.par
     AND widestBrother.id <> nodeToDelete.id
     USE-INDEX parVWPExpChildren NO-ERROR.

    IF bnode.par = 0 THEN DO:
        gExpChildren = gExpChildren - nodeToDelete.expChildren - 1.
        gVWP = IF AVAIL widestBrother THEN widestBrother.VWPexpChildren ELSE 0.
        LEAVE.
    END.

    FIND parentNode WHERE parentNode.id = bnode.par.

    IF NOT parentNode.expanded THEN DO:
        ASSIGN
         parentNode.colChildren    = parentNode.colChildren - nodeToDelete.expChildren - 1
         parentNode.VWPcolChildren = MAX(parentNode.VWP,IF AVAIL widestBrother
                                                         THEN widestBrother.VWPexpChildren
                                                         ELSE 0).
        LEAVE.
    END.
    ASSIGN
     parentNode.expChildren = parentNode.expChildren - nodeToDelete.expChildren - 1
     parentNode.VWPexpChildren = MAX(parentNode.VWP,IF AVAIL widestBrother
                                                     THEN widestBrother.VWPexpChildren
                                                     ELSE 0).
    FIND bnode WHERE bnode.id = parentNode.id.
END.

/* keep track of prev sibling and next sibling to link them after the deletion */
ASSIGN
 ipre = nodeToDelete.pre
 inex = nodeToDelete.nex.

/* deleteBranchOf() can create holes in the viewport.
If it does, then we need to make sure the next refresh will clean these holes.*/
RUN findPrevNodeToShow(nodeToDelete.id, NO, OUTPUT prevNodeToShow).

/* We can now delete the node and its children */
deleteBranchOf(nodeToDelete.id). /* note it will remove the visible node from tviter */
FIND FIRST btviter WHERE btviter.iter = 1.
FIND bnode WHERE bnode.id = btviter.id NO-ERROR.
/* The node at the top of the view port was part of the branch that has beeb removed */
IF NOT AVAIL bnode THEN DO:
    IF inex <> 0 THEN gRefreshFromNode = inex. /*there is a next node for the top*/
    ELSE IF prevNodeToShow <> 0 THEN gRefreshFromNode = prevNodeToShow. /*there was no next node, take the previous*/
    /* This situation happens only when tv has been made completelty cleared (empty) */
    ELSE ASSIGN
     gid = 0  /* This will means that tv is clear to some API's, and it is quite good to reset it anyway */
     gRefreshFromNode = 0.
END.

/* link former prev sibling and next sibling */
IF ipre <> 0 THEN DO:
    FIND bnode WHERE bnode.id = ipre.
    bnode.nex = inex.
END.
IF inex <> 0 THEN DO:
    FIND bnode WHERE bnode.id = inex.
    bnode.pre = ipre.
END.

IF CAN-DO(pcOptn,"refresh") THEN tvRefresh().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject C-Win
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

    DELETE WIDGET-POOL "pure4GlTvVertSrcollB" + STRING(THIS-PROCEDURE) NO-ERROR.
    DELETE WIDGET-POOL gWPNodeIcons NO-ERROR.
    DELETE WIDGET-POOL gWPNodeLabels NO-ERROR.
    DELETE WIDGET-POOL STRING(THIS-PROCEDURE) + "PopupMenu" NO-ERROR.

    RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableObject C-Win
PROCEDURE disableObject :
/*------------------------------------------------------------------------------
  Purpose:     Provide a standard ADM2 API to disable pure4gltv
  Parameters:  <none>
  Notes:       Implemented in March 2006
------------------------------------------------------------------------------*/
FRAME fMain:SENSITIVE = NO.

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME fMain.
  HIDE FRAME fsv.
  HIDE FRAME ftv.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dragNode C-Win
PROCEDURE dragNode PRIVATE :
/*------------------------------------------------------------------------------
    Starts a drag operation when the property dragSource is set to "some" or "all"
  (see the two node options dragSource and noDragSource in the addNode internal
  procedure)

    We first check that the left mouse button is indeed pressed, then we fire a
  dragBegin event when the mouse is being moved outside of the node to be dragged.
    A drag frame and text follow the mouse pointer until the left mouse button is
  released.

    When tvNodeEvent "dragBegin" is fired, then the linked object can optionally
  return one of the following RETURN-VALUE to affect some parameters that will be
  passed by tvnodeEvent "DropEnd":
  -Nothing
     => DropEnd will then just pass the X and Y of the mouse pointer (the one of
      MS Windows)

  -STRING(of a given target frame:HANDLE)
     => DropEnd will then pass the X and Y of the mouse pointer relative to the
      Target Frame (0,0 = top left corner of the frame)
        + a list of widget handles that are located on (X,Y) on this target frame

      Note that the target frame can be located in an other window than the one
      that contains the treeview

  -"cancelDrag"
     =>  Will cancel the drag operation.  No DropEnd will be fired

  -"dropOnYourself"
     => DropEnd will then pass the X and Y of the mouse pointer relative to the
      TreeView Frame
        + the key of the node where the dragged node was dropped

About the DropEnd event, note that we actually pass a list in the
pcEvent parameter as shown bellow:

PUBLISH "tvNodeEvent" ("DropEnd,<X>,<Y>,<WidgetList>", draggedNodeKey)
   or
PUBLISH "tvNodeEvent" ("DropEnd,<X>,<Y>,<targetNodeKey>", draggedNodeKey)

  => Therefore one shall test pcEvent BEGINS "DropEnd," to handle this event
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER piNodeId LIKE node.id   NO-UNDO.

DEFINE VARIABLE hWin                      AS HANDLE     NO-UNDO.
DEFINE VARIABLE mKeyboardState            AS MEMPTR     NO-UNDO.
DEFINE VARIABLE mMousePosOrig             AS MEMPTR     NO-UNDO.
DEFINE VARIABLE mMousePosWin              AS MEMPTR     NO-UNDO.
DEFINE VARIABLE mMousePos                 AS MEMPTR     NO-UNDO.
DEFINE VARIABLE mouseX                    AS INTEGER    NO-UNDO.
DEFINE VARIABLE mouseY                    AS INTEGER    NO-UNDO.
DEFINE VARIABLE winMouseX                 AS INTEGER    NO-UNDO.
DEFINE VARIABLE winMouseY                 AS INTEGER    NO-UNDO.
DEFINE VARIABLE lDragStarted              AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lfollowPointerWidgetReady AS LOGICAL    NO-UNDO.
DEFINE VARIABLE hTargetFrame              AS HANDLE     NO-UNDO.
DEFINE VARIABLE cDropWidgetsAtList        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dropOnYourself            AS LOGICAL    NO-UNDO.
DEFINE VARIABLE hpointerText              AS HANDLE     NO-UNDO.
DEFINE VARIABLE hpointerFrame             AS HANDLE     NO-UNDO.
/*DEFINE VARIABLE itvDefaultFont            AS INTEGER    NO-UNDO. SLP 17-OCT-2007 */
DEFINE VARIABLE truncatedLab              AS CHARACTER  NO-UNDO.

/* the following guys are used when the target frame is in ANOTHER window */
DEFINE VARIABLE hWinBis                   AS HANDLE     NO-UNDO.
DEFINE VARIABLE winMouseXBis              AS INTEGER    NO-UNDO.
DEFINE VARIABLE winMouseYBis              AS INTEGER    NO-UNDO.
DEFINE VARIABLE hPointerTextBis           AS HANDLE     NO-UNDO.
DEFINE VARIABLE hpointerFrameBis          AS HANDLE     NO-UNDO.
DEFINE VARIABLE mMousePosWinBis           AS MEMPTR     NO-UNDO.


DEF BUFFER bnode   FOR node.
DEF BUFFER btviter FOR tviter.

/* I use static widgets to follow the mouse pointer in the window of the treeview
  however, if the drop target is located on another window, then dynamic widgets
  will be used to follow the pointer */
DEFINE VARIABLE pointerText AS CHARACTER FORMAT "X(256)":U INITIAL "labText"
 VIEW-AS TEXT SIZE-PIXELS 70 BY 17 NO-UNDO.

DEFINE FRAME pointerFrame pointerText AT Y 0 X 0 NO-LABEL
 WITH 1 DOWN OVERLAY SIDE-LABELS TOP-ONLY NO-UNDERLINE THREE-D
  AT COL 1 ROW 1 SIZE-PIXELS 80 BY 17 BGCOLOR 15.

SET-SIZE(mKeyboardState) = 256.

/* Are we really pressing the left mouse button ?
   => We can indeed get here from labLeftMouseEvent with just a keyboard action
   that did an APPLY 'LEFT-MOUSE-DOWN' to hLab
     => The following code will quickly find it out */
RUN GetKeyboardState(GET-POINTER-VALUE(mKeyboardState) {&APIRtnParmINT}).
IF GET-BITS(GET-BYTE(mKeyboardState,2) ,8, 1) = 0 THEN DO: /* 2 is actually 1 + 1 (2nd byte but the first is coded as 0)*/
    SET-SIZE(mKeyboardState) = 0.
    RETURN.
END.

FIND bnode WHERE bnode.id = piNodeId.     /*should always be avail, else raise a bad error ;)*/
FIND btviter WHERE btviter.id = piNodeId. /*same as above*/

ASSIGN
 hWin = FRAME ftv:WINDOW
 hpointerText = pointerText:HANDLE IN FRAME pointerFrame
 hpointerFrame = FRAME pointerFrame:HANDLE
 hpointerText:HEIGHT-PIXELS = 5
 hpointerText:WIDTH-PIXELS  = 3. /* shrink hpointerText to allow a small pointerFrame later */

SET-SIZE(mMousePos)      = 16.  SET-SIZE(mMousePosOrig)  = 16.  SET-SIZE(mMousePosWin)   = 16.

/* monitor the left mouse button */
DO WHILE TRUE:
    WAIT-FOR 'MOUSE-SELECT-CLICK' OF FRAME fmain  PAUSE 0. /* MOUSE-SELECT-CLICK is a *dummy* event */

    /* track location of the mouse pointer */
    RUN GetCursorPos( INPUT-OUTPUT mMousePosOrig).
    mMousePos    = mMousePosOrig.

    RUN ScreenToClient (INPUT FRAME ftv:HWND, INPUT mMousePos ).
    ASSIGN
     mouseX = GET-LONG( mMousePos,1)
     mouseY = GET-LONG( mMousePos,5).

    /* Has the drag been started ? Check if the mouse pointer is 1 pixels away
      from the picture+Label */
    IF NOT lDragStarted
     THEN lDragStarted =
         mouseX > btviter.labWidthPixels + btviter.labX + 1
      OR mouseX < btviter.picX - 1
      OR mouseY > btviter.picY + gTvIterationHeight + 1
      OR mouseY < btviter.picY - 1.

    /* If we just started to drag, then prepare the Widgets that are going to follow the pointer
      Note that I first wanted to just change the mouse-pointer, but MS Windows does not let
      me change the mouse-pointer while the left button remains pressed (wine does ;) */
    IF NOT lfollowPointerWidgetReady AND lDragStarted THEN DO:
        /* itvDefaultFont = gettvnodeDefaultFont(). SLP 17-10-2007 */
        truncatedLab = bnode.lab.
        /* If the label is too large, then truncate it */
        /* SLP 17-OCT-2007
        DO WHILE FONT-TABLE:GET-TEXT-WIDTH-PIXELS(truncatedLab,itvDefaultFont) > 200:*/
        DO WHILE FONT-TABLE:GET-TEXT-WIDTH-PIXELS(truncatedLab,bnode.nodeFont) > 200:
            truncatedLab = SUBSTR(truncatedLab,1,LENGTH(truncatedLab) - 1).
        END.
        IF truncatedLab <> bnode.lab THEN truncatedLab = truncatedLab + "...".

        ASSIGN
         /*hpointerText:FONT = itvDefaultFont  SLP 17-OCT-2007 */
         hpointerText:FONT = bnode.nodeFont
         hpointerText:SCREEN-VALUE   = truncatedLab
         hpointerFrame:HEIGHT-PIXELS = gTvIterationHeight + 2 /* + 2 because of the box*/
         hpointerFrame:WIDTH-PIXELS  = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(truncatedLab) + 2
         hpointerFrame:VIRTUAL-HEIGHT-PIXELS = hpointerFrame:HEIGHT-PIXELS
         hpointerFrame:VIRTUAL-WIDTH-PIXELS  = hpointerFrame:WIDTH-PIXELS
         hpointerText:HEIGHT-PIXELS = hpointerFrame:HEIGHT-PIXELS - 2
         hpointerText:WIDTH-PIXELS  = hpointerFrame:WIDTH-PIXELS - 2.

        /* required the first time to attach the frame to this windows */
        IF hpointerFrame:PARENT = ? THEN VIEW FRAME pointerFrame IN WINDOW hWin.
        ELSE hpointerFrame:VISIBLE = YES.

        lfollowPointerWidgetReady = YES.

        /* The DragBegin event also gives the ability to show the widgets that can be
           a drop target (changing their color or changing their mouse pointer)
           One can also return
              'cancelDrag' to cancel the drag operation
           or 'dropOnYourself' to use the TV Frame ftv as targetFrame
           or the handle of any frame to be used as drop target */
        PUBLISH "tvNodeEvent" ("DragBegin", bnode.ke).
        IF RETURN-VALUE = "cancelDrag" THEN DO:
            lDragStarted = NO. /* we keep lfollowPointerWidgetReady set so we hide/delete the pointer widgets */
            LEAVE.
        END.
        IF RETURN-VALUE = "dropOnYourself" THEN ASSIGN
         hTargetFrame   = FRAME ftv:HANDLE
         dropOnYourself = YES.
        ELSE hTargetFrame = WIDGET-HANDLE(RETURN-VALUE) NO-ERROR.
        IF VALID-HANDLE(hTargetFrame) AND hTargetFrame:TYPE <> "FRAME"
         THEN hTargetFrame = ?. /* we want a frame, and noting else, period !*/

        /* If target frame is on another window, then let start to handle it */
        IF VALID-HANDLE(hTargetFrame) AND hTargetFrame:WINDOW <> hWin
         THEN DO:
            hWinBis = hTargetFrame:WINDOW.
            CREATE FRAME hpointerFrameBis ASSIGN
             PARENT = hWinBis    X = 1            Y = 1            VISIBLE = NO
             TOP-ONLY = YES      OVERLAY = YES    THREE-D = YES    BOX = YES
             WIDTH-PIXELS = hpointerFrame:WIDTH-PIXELS
             HEIGHT-PIXELS = hpointerFrame:HEIGHT-PIXELS
             BGCOLOR = hpointerFrame:BGCOLOR.

            CREATE TEXT hpointerTextBis ASSIGN
             FRAME = hpointerFrameBis     X = 0           Y = 0
             FORMAT = FILL("X",256)       HIDDEN = NO     FONT = hpointerText:FONT
             WIDTH-PIXELS = hpointerText:WIDTH-PIXELS
             HEIGHT-PIXELS = hpointerText:HEIGHT-PIXELS
             SCREEN-VALUE = hpointerText:SCREEN-VALUE.
        END.
    END.

    /* The point of this block is to make some widgets follow the mouse pointer */
    IF lfollowPointerWidgetReady THEN DO:
        mMousePosWin = mMousePosOrig.
        RUN ScreenToClient (INPUT hWin:HWND, INPUT mMousePosWin ).
        ASSIGN
         winMouseX = GET-LONG( mMousePosWin,1)
         winMouseY = GET-LONG( mMousePosWin,5).
        hpointerFrame:X = MAX(0,MIN(winMouseX, hWin:WIDTH-PIXELS  - hpointerFrame:WIDTH-PIXELS)).
        hpointerFrame:Y = MAX(0,MIN(winMouseY - hpointerFrame:HEIGHT-PIXELS + 6, hWin:HEIGHT-PIXELS - hpointerFrame:HEIGHT-PIXELS)).
        /* if drop target frame is in another windows, then hidde the pointerWidget if the pointer
          is on this other window */
        IF VALID-HANDLE(hWinBis) THEN DO:
            mMousePosWinBis = mMousePosOrig.
            RUN ScreenToClient (INPUT hWinBis:HWND, INPUT mMousePosWinBis).
            ASSIGN
             winMouseXBis = GET-LONG( mMousePosWinBis,1)
             winMouseYBis = GET-LONG( mMousePosWinBis,5).

            IF    winMouseXBis > 0
              AND winMouseXBis < hWinBis:WIDTH-PIXELS
              AND winMouseYBis > 0
              AND winMouseYBis < hWinBis:HEIGHT-PIXELS
              THEN DO:
                /* We are above the target other window, hidde the local pointerFrame
                  and show the pointerFrameBis */
                hpointerFrame:VISIBLE    = NO.
                hpointerFrameBis:VISIBLE = YES.
                hpointerFrameBis:X = MAX(0,MIN(winMouseXBis, hWinBis:WIDTH-PIXELS  - hpointerFrameBis:WIDTH-PIXELS)).
                hpointerFrameBis:Y = MAX(0,MIN(winMouseYBis - hpointerFrameBis:HEIGHT-PIXELS + 6, hWinBis:HEIGHT-PIXELS - hpointerFrameBis:HEIGHT-PIXELS)).
            END.
            ELSE DO:
                /* We are NOT above the target other window, hidde the pointerFrameBis
                  and show the local pointerFrame */
                hpointerFrame:VISIBLE    = YES.
                hpointerFrameBis:VISIBLE = NO.
            END.
        END. /* IF VALID-HANDLE(hWinBis) */
    END. /* IF lfollowPointerWidgetReady */


    /* Did we release the left mouse button ? */
    RUN GetKeyboardState(GET-POINTER-VALUE(mKeyboardState) {&APIRtnParmINT}).
    IF GET-BITS(GET-BYTE(mKeyboardState,2) ,8, 1) = 0 THEN LEAVE. /* 2 is actually 1 + 1 (2nd byte but the first is coded as 0)*/


    /* If dropOnYourself is set, and if the mouse pointer is above or bellow the view
     port, then scroll down or up by a number that is propertionnal to the gap */
    IF dropOnYourself AND mouseY > gFtvHP THEN tvScroll( INT((mouseY - gFtvHP) / 30 + .4999), YES).
    IF dropOnYourself AND mouseY < 0      THEN tvScroll( INT(mouseY / 30 - 0.5), YES).

    IF LAST-EVENT:FUNCTION <> "" THEN LEAVE. /* The user did something */
END.  /* monitor the left mouse button */


/* hide the dragFollowPointer widgets and make them small in a corner */
IF lfollowPointerWidgetReady THEN ASSIGN
 hpointerText:HEIGHT-PIXELS = 5
 hpointerText:WIDTH-PIXELS  = 3
 hpointerFrame:HEIGHT-PIXELS = 5
 hpointerFrame:WIDTH-PIXELS = 3
 hpointerFrame:VISIBLE = NO.
IF VALID-HANDLE(hpointerTextBis)  THEN DELETE WIDGET hpointerTextBis.
IF VALID-HANDLE(hpointerFrameBis) THEN DELETE WIDGET hpointerFrameBis.

IF lDragStarted THEN DO:
    /* If we received the handle of a DropTargetFrame back from DragStart,
     then convert mouseX and mouseY to this frame */
    IF VALID-HANDLE(hTargetFrame) THEN DO:
        RUN ScreenToClient (INPUT hTargetFrame:HWND, INPUT mMousePosOrig ).
        ASSIGN
         mouseX = GET-LONG( mMousePosOrig,1)
         mouseY = GET-LONG( mMousePosOrig,5).
        RUN widgetsAt (hTargetFrame, mouseX, mouseY, OUTPUT cDropWidgetsAtList).
    END.
    PUBLISH "tvNodeEvent" ("DropEnd," + STRING(mouseX) + "," + STRING(mouseY)
                             + IF cDropWidgetsAtList = "" THEN ""
                              ELSE "," + cDropWidgetsAtList
                          ,bnode.ke).
END.

SET-SIZE(mKeyboardState) = 0.
SET-SIZE(mMousePosOrig)  = 0.
SET-SIZE(mMousePosWin)   = 0.
SET-SIZE(mMousePos)      = 0.


IF lDragStarted THEN RETURN "NodeDragged".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dumpNodeTable C-Win
PROCEDURE dumpNodeTable :
/*------------------------------------------------------------------------------
  Purpose:     API to use for debugging purposes, either for an App that uses
               the pure4GlTv of for  the pure4GlTv itself
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    OUTPUT TO dumpNodeTable.txt.

    DEFINE VARIABLE c AS CHAR.

    PUT UNFORM "gExpChildren:" gExpChildren "       gVWP: " gVWP "    gftvVWP:" gftvVWP
      "  gftvWP:" gftvWP "  FRAME ftv:VIRTUAL-WIDTH-PIXELS:" FRAME ftv:VIRTUAL-WIDTH-PIXELS "~n".
    FOR EACH node:
        FIND tviter OF node NO-ERROR.
        c = IF AVAIL tviter THEN string(tviter.iter) ELSE "".
        DISPLAY c  WITH WIDTH 300 FRAME f 100 DOWN.
        DISPLAY node WITH WIDTH 300 FRAME f.
/*        DISPLAY c node.lab node.level node.id WITH WIDTH 300 FRAME f 100 DOWN.*/
    END.

    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dumptviter C-Win
PROCEDURE dumptviter :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

OUTPUT TO dumptviter.txt APPEND.

FOR EACH tviter, EACH node WHERE node.Id = tviter.id:
    DISPLAY tviter EXCEPT hpic hlab WITH WIDTH 320.
    /*DISPLAY node.*/
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE emptyTree C-Win
PROCEDURE emptyTree :
/*------------------------------------------------------------------------------
  Purpose:     delete all node and display an empty tree
  Parameters:  <none>
  Notes:       will call tvRefresh() at the end of the process
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE node.

deselectCurrentNode(). /* clean up gCurNode gCurhLab and the selected label widget */

ASSIGN
 gid = 0 /* will mean that tv is empty in some places, I also avoid the limit of 2Gig */
 gExpChildren = 0
 gIterOffset = 0
 gVWP = 0.

tvRefresh().

/* just in case if tvRefresh() did not do this job */
RUN ShowScrollBar IN hppure4gltvApi (FRAME ftv:HWND, {&SB_VERT},  0 {&APIRtnParmINT}).
RUN ShowScrollBar IN hppure4gltvApi (FRAME ftv:HWND, {&SB_HORZ},  0 {&APIRtnParmINT}).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableObject C-Win
PROCEDURE enableObject :
/*------------------------------------------------------------------------------
  Purpose:     Provide a standard ADM2 API to enable pure4gltv
  Parameters:  <none>
  Notes:       Implemented in March 2006
------------------------------------------------------------------------------*/

FRAME fMain:SENSITIVE = YES.

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY focusCatcher edtSunken
      WITH FRAME fMain.
  ENABLE focusCatcher
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW FRAME ftv.
  {&OPEN-BROWSERS-IN-QUERY-ftv}
  ENABLE imgsv
      WITH FRAME fsv.
  {&OPEN-BROWSERS-IN-QUERY-fsv}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE expandBranch C-Win
PROCEDURE expandBranch :
/*------------------------------------------------------------------------------
  Purpose:     Expand all the nodes of a branch starting from a given node
  Parameters:  pcKe : node key the branch to expand starts from
  Notes:       Typically fired when the '*' key is pressed, but can be called
                directly
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcKe LIKE node.ke NO-UNDO.

DEFINE VARIABLE nodeId AS INTEGER    NO-UNDO.

DEFINE BUFFER bnode FOR node.

FIND bnode WHERE bnode.ke = pcKe.
nodeId = bnode.id.
/* perhaps time to add nodes on the fly */
IF LOOKUP("addOnExpand", bnode.optn, CHR(1)) > 0 THEN DO:
    PUBLISH "tvNodeEvent" ("addOnExpand", bnode.ke).
    ASSIGN
     bnode.optn = REPLACE(bnode.optn ,"addOnExpand" + CHR(1), "")
     bnode.optn = REPLACE(bnode.optn ,CHR(1) + "addOnExpand", "")
     bnode.optn = REPLACE(bnode.optn ,"addOnExpand"         , "").
END.
ELSE IF NOT bnode.expanded THEN expandNode(pcKe, "").

FOR EACH bnode WHERE bnode.par = nodeId:
    /* perhaps time to add nodes on the fly */
    IF LOOKUP("addOnExpand", bnode.optn, CHR(1)) > 0
     THEN DO:
        PUBLISH "tvNodeEvent" ("addOnExpand", bnode.ke).
        ASSIGN
         bnode.optn = REPLACE(bnode.optn ,"addOnExpand" + CHR(1), "")
         bnode.optn = REPLACE(bnode.optn ,CHR(1) + "addOnExpand", "")
         bnode.optn = REPLACE(bnode.optn ,"addOnExpand"         , "").
    END.

    /* if it has childrens, then expand the branch */
    IF bnode.expChildren + bnode.colChildren > 0
     THEN RUN expandBranch(bnode.ke).
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EXTERNALPROCEDURES C-Win
PROCEDURE EXTERNALPROCEDURES PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:   Dummy procedure.  This is just a place holder for external procedures
  Parameters:  <none>
  Notes:    All this procedures work with Linux/Wine
------------------------------------------------------------------------------*/

END PROCEDURE.

PROCEDURE GetCursorPos EXTERNAL "user32.dll" :
    DEFINE INPUT-OUTPUT PARAMETER lRect AS MEMPTR.
END PROCEDURE.

PROCEDURE ScreenToClient EXTERNAL "user32.dll" :
   DEFINE INPUT  PARAMETER hWnd     AS LONG.
   DEFINE INPUT  PARAMETER lpPoint  AS MEMPTR.
END PROCEDURE.

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll":
   DEFINE INPUT  PARAMETER piWindowHwnd AS LONG NO-UNDO.
   DEFINE RETURN PARAMETER piResult     AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE GetKeyboardState EXTERNAL "user32.dll":
   DEFINE INPUT  PARAMETER KBState AS LONG. /* memptr */
&IF DEFINED(APIRtnParm) &THEN
   DEFINE RETURN PARAMETER RetVal  AS LONG. /* bool   */
&ENDIF

END PROCEDURE.


PROCEDURE Sleep EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT PARAMETER intMilliseconds AS LONG.
END PROCEDURE.

PROCEDURE GetCurrentThemeName EXTERNAL "uxtheme.dll":
    DEFINE OUTPUT PARAMETER mThemeName       AS MEMPTR.
    DEFINE INPUT  PARAMETER dwMaxNameChars   AS LONG.
    DEFINE OUTPUT PARAMETER mThemeColor      AS MEMPTR.
    DEFINE INPUT  PARAMETER cchMaxColorChars AS LONG.
    DEFINE OUTPUT PARAMETER mThemeSize       AS MEMPTR.
    DEFINE INPUT  PARAMETER cchMaxSizeChars  AS LONG.
    DEF RETURN PARAMETER lrtn AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findNextNodeToShow C-Win
PROCEDURE findNextNodeToShow :
/*------------------------------------------------------------------------------
     Purpose: Find the Next available node of a given node in the treeview
            => can be first child or next sibling, or an uncle ;) (next sibling
             of a parent or grant parent)

  Parameters: INPUT  Node ID we want to find the Next node of

              INPUT  plIgnoreChild  mode
                    YES => Ignore expanded children nodes bellow the given node
                    NO  => This mode is like finding the node in the next visible
                           iteration

              OUTPUT Node id of the wanted Next node.
                  Equals to 0 when no available next node
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piOfNodeId    LIKE node.id           NO-UNDO.
  DEFINE INPUT  PARAMETER plIgnoreChild AS LOG                 NO-UNDO.
  DEFINE OUTPUT PARAMETER opiNextNodeId LIKE node.id INITIAL 0 NO-UNDO.

  DEF BUFFER ofNode     FOR node.
  DEF BUFFER firstChild FOR node.
  DEF BUFFER parentNode FOR node.

  FIND ofNode WHERE ofNode.id = piOfNodeId NO-ERROR.
  /* insane case */
  IF NOT AVAIL ofNode THEN RETURN ERROR "There is no node with id " + STRING(piOfNodeId).

  /* If expanded and has a child and not plIgnoreChild then return it */
  IF ofNode.expanded
   AND NOT plIgnoreChild THEN DO:
      FIND firstChild WHERE firstChild.par = ofNode.id
                        AND firstChild.pre = 0 NO-ERROR.
      /* This node should not have been expanded */
      IF NOT AVAIL firstChild THEN ofNode.expanded = NO.
      ELSE DO:
        opiNextNodeId = firstChild.id.
        RETURN.
      END.
  END.

  /* Else if has a next sibling then return it */
  IF ofNode.nex <> 0 THEN DO:
    opiNextNodeId = ofNode.nex.
    RETURN.
  END.

  /* Else determine the next sibling of the first parent on the way that is not a last child*/
  opiNextNodeId = ofNode.par.
  DO WHILE TRUE:
      IF opiNextNodeId = 0 THEN RETURN. /* We were actually at the bottom of the tree */

      FIND parentNode WHERE parentNode.id = opiNextNodeId.
      IF parentNode.nex <> 0 THEN LEAVE.
      opiNextNodeId = parentNode.par.
  END.
  opiNextNodeId = parentNode.nex.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findPrevNodeToShow C-Win
PROCEDURE findPrevNodeToShow :
/*------------------------------------------------------------------------------
     Purpose: Find the Previous available node of a given node
       Similar to the findNextNodeToShow API, but to get a previous node
       (in the iteration above a given node)

        => see comments in findNextNodeToShow

------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piOfNodeId    LIKE node.id           NO-UNDO.
  DEFINE INPUT  PARAMETER plIgnoreChild AS LOG                 NO-UNDO.
  DEFINE OUTPUT PARAMETER opiPrevNodeId LIKE node.id INITIAL 0 NO-UNDO.

  DEF BUFFER ofNode     FOR node.
  DEF BUFFER prevNode   FOR node.
  DEF BUFFER lastChild   FOR node.


  FIND ofNode WHERE ofNode.id = piOfNodeId NO-ERROR.
  /* insane case */
  IF NOT AVAIL ofNode THEN RETURN ERROR "There is no node with id " + STRING(piOfNodeId).

  /* If first child (no prev sibling) then return parent */
  IF ofNode.pre = 0 THEN DO:
      opiPrevNodeId = ofNode.par.
      RETURN.
  END.


  /* Otherwise determine the last child of the last child of the....  of the prev sibling*/
  opiPrevNodeId = ofNode.pre.

  IF NOT plIgnoreChild THEN DO WHILE TRUE:
      FIND prevNode WHERE prevNode.id = opiPrevNodeId.
      IF NOT prevNode.expanded THEN LEAVE.

      FIND lastChild WHERE lastChild.par = opiPrevNodeId
                       AND lastChild.nex = 0 NO-ERROR.
      IF NOT AVAIL lastChild THEN LEAVE.
      opiPrevNodeId = lastChild.id.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fMainKeyEvent C-Win
PROCEDURE fMainKeyEvent :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  Key function in the frame.
              Can be a navigation key or "+" or "-"
  Notes:   Was designed to be called internally, but it should work fine to
          be called from anywhere, so I unchecked the PRIVATE option
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cFunction AS CHARACTER  NO-UNDO.

DEF BUFFER curNode     FOR node.
DEF BUFFER goToNode    FOR node.
DEF BUFFER btviter     FOR tviter.
DEF BUFFER curTviter   FOR tviter.

DEFINE VARIABLE goToNodeId         LIKE node.id   NO-UNDO.
DEFINE VARIABLE newRefreshFromNode LIKE node.id   NO-UNDO.
DEFINE VARIABLE nodeOnTheWay       LIKE node.id   NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER    NO-UNDO.

FIND curNode WHERE curNode.id = gCurNode NO-ERROR.
FIND curTviter WHERE curTviter.id = gCurNode NO-ERROR. /* not avail if no selected node
                                                         or if it is outside of the viewport */

/* for some actions, it is important to have the selected node already in
 the view port (sometimes not the case when scrolled somewhere with scroll bar) */
IF AVAIL curNode
 AND CAN-DO("CURSOR-DOWN,CURSOR-UP,PAGE-UP,PAGE-DOWN,CURSOR-LEFT,CURSOR-RIGHT,+,-,*", cFunction)
 AND NOT AVAIL curTviter THEN DO:
    gRefreshFromNode = gCurNode.
    tvRefresh().
    FIND curTviter WHERE curTviter.id = gCurNode. /* should be there now */
    selectNodeLabel(curTviter.hLab).
END.


/*========= These actions applies only when there is a selected node ==========*/
IF AVAIL curNode THEN
CASE cFunction:
WHEN "CURSOR-DOWN" THEN DO:
  /* is there a next node ? */
  RUN findNextNodeToShow(curNode.id, NO, OUTPUT goToNodeId).
  IF goToNodeId = 0 THEN RETURN. /* no was at last node, forget it*/

  /* can navigate to a net node in the view port ? */
  FIND btviter WHERE btviter.id = goToNodeId NO-ERROR.

  /* no, need to scroll */
  IF NOT AVAIL btviter THEN DO:
      tvScroll(1, YES). /*scroll up by 1 iteration */
      FIND btviter WHERE btviter.id = goToNodeId. /* should be there now */
      IF NOT glWineMode AND glMSkeyScrollForcePaint THEN MSkeyScrollForcePaint().
  END.
  APPLY 'LEFT-MOUSE-DOWN' TO btviter.hLab.
END.

WHEN "CURSOR-UP" THEN DO:
  /* is there a previous node ? */
  RUN findPrevNodeToShow(curNode.id, NO, OUTPUT goToNodeId).
  IF goToNodeId = 0 THEN RETURN. /* no was at last node, forget it*/

  /* can navigate to a net node in the view port ? */
  FIND btviter WHERE btviter.id = goToNodeId NO-ERROR.

  /* no, then need to scroll */
  IF NOT AVAIL btviter THEN DO:
      tvScroll(-1, YES). /*scroll down by 1 iteration */
      FIND btviter WHERE btviter.id = goToNodeId. /* should be there now */
      IF NOT glWineMode AND glMSkeyScrollForcePaint THEN MSkeyScrollForcePaint().
  END.
  APPLY 'LEFT-MOUSE-DOWN' TO btviter.hLab.
END.

WHEN "CURSOR-LEFT" OR WHEN "-" THEN DO:
  IF curNode.expanded THEN DO:  /* Collapse that node */
      FIND btviter WHERE btviter.id = curNode.id. /* error would mean corrupted TV*/
      APPLY 'MOUSE-SELECT-DBLCLICK' TO btviter.hPic.
      RETURN.
  END.
  IF cFunction = "-" THEN RETURN.

  IF curNode.par = 0 THEN RETURN. /* No parent */

  /* select parent node */
  FIND goToNode WHERE goToNode.id = curNode.par.
  FIND btviter WHERE btviter.id = goToNode.id NO-ERROR. /* if in view port then go there */
  /* if not in view port then scroll there */
  IF NOT AVAIL btviter THEN DO:
    gRefreshFromNode = goToNode.id.
    tvRefresh().
    FIND btviter WHERE btviter.id = goToNode.id. /* should be there now */
  END.
  APPLY 'LEFT-MOUSE-DOWN' TO btviter.hLab.
END.

WHEN "CURSOR-RIGHT" OR WHEN "+" THEN DO:
  IF NOT curNode.expanded THEN DO: /* Attempt to collapse that node (if not
                                  collapsible then nothing will happen) */
      FIND btviter WHERE btviter.id = curNode.id.  /* error would mean corrupted TV*/
      APPLY 'MOUSE-SELECT-DBLCLICK' TO btviter.hPic.
      RETURN.
  END.
  IF cFunction = "+" THEN RETURN.

  /* Select first child */
  FIND goToNode WHERE goToNode.par = curNode.id
                  AND goToNode.pre = 0.  /* error would mean corrupted TV*/
  FIND btviter WHERE btviter.id = goToNode.id NO-ERROR.  /* if in view port then go there */
  /* if not in view port then scroll one up */
  IF NOT AVAIL btviter THEN DO:
      tvScroll(1,NO). /* About the NO, actually it should always be possible to scroll one up in this
                        case.  I set scrollAsPossible to NO to show my intentions */
      FIND btviter WHERE btviter.id = goToNode.id NO-ERROR.  /* if in view port then go there */
      IF NOT glWineMode AND glMSkeyScrollForcePaint THEN MSkeyScrollForcePaint().
  END.
  APPLY 'LEFT-MOUSE-DOWN' TO btviter.hLab.
END.

WHEN "PAGE-DOWN" THEN DO:
  RUN dumpTviter.
  /* already at bottom of the page, try to go down (scroll up) one page */
  IF curTviter.iter = gVisibleIterations THEN DO:
      goToNodeId = curNode.id.
      /* try to go (gVisibleIterations - 1) iterations down, stop when no more node*/
      DO iLoop = 1 TO gVisibleIterations - 1:
          RUN findNextNodeToShow(goToNodeId, NO, OUTPUT nodeOnTheWay).
          IF nodeOnTheWay = 0 THEN LEAVE.
          goToNodeId = nodeOnTheWay.
      END.
      /* Now go back (gVisibleIterations - 1) iterations up to determine new top Node
         => the point is to avoid empty space at the bottom of the tree when reaching
         the last page*/
      newRefreshFromNode = goToNodeId.
      DO iLoop = 1 TO gVisibleIterations - 1:
          RUN findPrevNodeToShow(newRefreshFromNode, NO, OUTPUT nodeOnTheWay).
          IF nodeOnTheWay = 0 THEN LEAVE.
          newRefreshFromNode = nodeOnTheWay.
      END.

      gRefreshFromNode = newRefreshFromNode.
      tvRefresh().
      FIND btviter WHERE btviter.id = goToNodeId.
      IF NOT glWineMode AND glMSkeyScrollForcePaint THEN MSkeyScrollForcePaint().
  END.
  /* just select last node of the view port */
  ELSE DO:
      FIND FIRST btviter WHERE btviter.id = ? USE-INDEX iter NO-ERROR.
      IF AVAIL btviter THEN FIND PREV btviter USE-INDEX iter.
      ELSE FIND btviter WHERE btviter.iter = gVisibleIterations.
  END.
  APPLY 'LEFT-MOUSE-DOWN' TO btviter.hLab. /*select that node */
END.

WHEN "PAGE-UP" THEN DO:
  /* already at bottom of the page, try to go down (scroll up) one page */
  IF curTviter.iter = 1 THEN DO:
      newRefreshFromNode = curTviter.id.
      /* try to go (gVisibleIterations - 1) iterations up, stop when no more node*/
      DO iLoop = 1 TO gVisibleIterations - 1:
          RUN findPrevNodeToShow(newRefreshFromNode, NO, OUTPUT goToNodeId).
          IF goToNodeId = 0 THEN LEAVE.
          newRefreshFromNode = goToNodeId.
      END.
      gRefreshFromNode = newRefreshFromNode.
      tvRefresh().
      FIND btviter WHERE btviter.id = gRefreshFromNode.
      IF NOT glWineMode AND glMSkeyScrollForcePaint THEN MSkeyScrollForcePaint().
  END.
  /* just select last node of te view port */
  ELSE FIND btviter WHERE btviter.iter = 1.
  APPLY 'LEFT-MOUSE-DOWN' TO btviter.hLab. /*select that node */
END.

END CASE. /* case cFunction , actions that require a selected node*/

/*========= These actions DO NOT require a selected node ==========*/
CASE cfunction:
WHEN "HOME" THEN DO:
  gRefreshFromNode = topNode(). /*setting it to 0 would result in the same in tvRefresh()*/
  tvRefresh().
  FIND btviter WHERE btviter.id = gRefreshFromNode. /* should be there now */
  APPLY 'LEFT-MOUSE-DOWN' TO btviter.hLab.
END.

WHEN "END" THEN DO:
  goToNodeId = lastVisibleNode().
  gRefreshFromNode = goToNodeId.
  /* tvRehfresh() will scroll up by itself to avoid empty space at the bottom */
  tvRefresh().
  FIND btviter WHERE btviter.id = goToNodeId. /* should be there now */
  APPLY 'LEFT-MOUSE-DOWN' TO btviter.hLab.
END.

WHEN "*" THEN DO:
    RUN expandBranch(curNode.ke).
    /* trick to bring the branch as much as possible in the view port */
    APPLY '-' TO curTviter.hLab.
    APPLY '+' TO curTviter.hLab.
END.

END CASE. /* case cFunction , actions that DO NOT require a selected node*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getNodeDetails C-Win
PROCEDURE getNodeDetails :
/*------------------------------------------------------------------------------
  Purpose:  To obtain the details of a given node

  Parameters:  pcKe, node key of the node we want the details
               One can also pass "nodeId=<nodeId>" instead of a node key

  Notes:       This API is rather powerful because it returns the HANDLE of a
               DYNAMIC buffer that we create for the occasion

               Advantages:
                Any field in the node table is available (with dynamic programming)

                The buffer can be reused to walk though the tree with a dynamic
                FIND-FIRST() method or a Dynamic Query, with very high performance

               One disadvantage however:
                IT IS UP TO THE DEVELOPPER TO DELETE the dynamic buffer object
                when it is not needed anymore otherwise, one may get a memory
                leak.
                Note that the dynamic buffer is created in the unnamed widget pool
                so it will be deleted with the pure4glTreeview Object


Example:

    RUN getNodeDetails IN h_pure4gltv
    ( INPUT mynodeKey    /* CHARACTER */,
      OUTPUT hNodeBuffer /* HANDLE */).

    MESSAGE
    "id:"           hNodeBuffer:BUFFER-FIELD("id"):BUFFER-VALUE       SKIP
    "lab:"          hNodeBuffer:BUFFER-FIELD("lab"):BUFFER-VALUE      SKIP
    "ico:"          hNodeBuffer:BUFFER-FIELD("ico"):BUFFER-VALUE      SKIP
    "level:"        hNodeBuffer:BUFFER-FIELD("level"):BUFFER-VALUE    SKIP
    "par:"          hNodeBuffer:BUFFER-FIELD("par"):BUFFER-VALUE      SKIP
    "prev-sibling:" hNodeBuffer:BUFFER-FIELD("pre"):BUFFER-VALUE      SKIP
    "next-sibling:" hNodeBuffer:BUFFER-FIELD("nex"):BUFFER-VALUE      SKIP
    "expanded:"     hNodeBuffer:BUFFER-FIELD("expanded"):BUFFER-VALUE SKIP
    "optn:"         hNodeBuffer:BUFFER-FIELD("optn"):BUFFER-VALUE
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    DELETE OBJECT hNodeBuffer.

------------------------------------------------------------------------------*/
/* you can actually pass nodeId=<nodeId> instead of a node key */
DEFINE INPUT  PARAMETER pcKe    AS CHARACTER          NO-UNDO.
DEFINE OUTPUT PARAMETER hNodeBuffer     AS HANDLE     NO-UNDO.


DEF BUFFER bnode FOR node.

IF pcKe BEGINS "nodeId=" THEN
 FIND FIRST bnode WHERE bnode.id = INT(ENTRY(2,pcKe,"=")) NO-ERROR.

ELSE FIND FIRST bnode WHERE bnode.ke = pcKe NO-ERROR.

IF NOT AVAIL bnode THEN RETURN ERROR "getNodeDetails failed:  Cannot find node with key = " + pcKe.

CREATE BUFFER hNodebuffer FOR TABLE "node".
hNodeBuffer:FIND-BY-ROWID(ROWID(bnode)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject C-Win
PROCEDURE initializeObject :
/* -----------------------------------------------------------
      Purpose:     Local version of the initialize method which starts up
                   the pure4glTv object..
      Parameters:  <none>
      Notes:
-------------------------------------------------------------*/

    /* DEFINE VARIABLE lObjectInitialized AS LOGICAL    NO-UNDO.
      {get ObjectInitialized lObjectInitialized}.
      /* Why initializing twice.... */
      IF lObjectInitialized THEN RETURN.
    */

    DEFINE VARIABLE cUIBMode AS CHARACTER  NO-UNDO.
    {get UIBMode cUIBMode}.

    RUN initializePure4glTv.

    RUN SUPER.

    IF cUIBMode = "Design" THEN DO:
        RUN loadDemoTv NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.
        /* called by resizeObject when in Desing mode: */
        tvRefresh().
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializePure4glTv C-Win
PROCEDURE initializePure4glTv PRIVATE :
/* -----------------------------------------------------------
      Purpose:     Creates the widget for pure4gltv and resize all
      Parameters:  <none>
      Notes:       Run automatically as part of folder startup.
    -------------------------------------------------------------*/

    DEFINE VARIABLE char-hdl           AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE hContainer         AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lHidden            AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE container-hdl      AS HANDLE     NO-UNDO.

    /* trick to set gWindowsSkin to correct value when the property WindowsSkin = 'Automatic' */
    DEFINE VARIABLE cWindowsSkin AS CHARACTER  NO-UNDO.
    {get WindowsSkin cWindowsSkin}.
    DYNAMIC-FUNCTION('setWindowsSkin':U, cWindowsSkin).
    /* Get the pure4glTv's CONTAINER for triggers.
       Note that in design mode the CONTAINER may not be specified;
       the code takes this into account. Also the broker will not
       be available in design mode. */

    ASSIGN
     char-hdl = DYNAMIC-FUNCTION('linkHandles':U, 'Container-Source':U)
     container-hdl = WIDGET-HANDLE(char-hdl).
    {get ContainerSource hContainer}.
    IF VALID-HANDLE(hContainer) THEN
    DO:
     /* if the frame is hidden by its container, we unhide it now. as
       there's a performance overhead of doing this after all widgets
       have been created. */
      {get ObjectHidden lHidden hContainer}.
    END.

    IF lHidden THEN FRAME {&FRAME-NAME}:HIDDEN = FALSE.
    ELSE FRAME {&FRAME-NAME}:HIDDEN = TRUE.

    /* will handle that as a property later, for now only the height is
     handled as a variable property*/
    gTvPicWidth = IF glPicWithPlusMinus THEN 32 ELSE 16.

    ASSIGN
     edtSunken:X IN FRAME fMain = 0
     edtSunken:Y = 0
     FRAME ftv:X = edtSunken:X + 2
     FRAME ftv:Y = edtSunken:Y + 2
     edtSunken:BGCOLOR = FRAME ftv:BGCOLOR
     FRAME fsv:Y = 2
     FRAME fsv:BGCOLOR = FRAME ftv:BGCOLOR
     imgsv:SENSITIVE = YES
     ghbsv:SENSITIVE = YES
     emptySquare:SENSITIVE = YES /* 06/04/2006 to display a 'about' message */
     gBsvY = -1 /* The goal is to force to have gBsvY <> BsvY in the next call of tvrefresh() */
     NO-ERROR.

    RUN resizeObject (FRAME {&FRAME-NAME}:HEIGHT, FRAME {&FRAME-NAME}:WIDTH).

    /* well, perhaps there is a good reason to keep that */
    IF FRAME {&FRAME-NAME}:HIDDEN THEN
       FRAME {&FRAME-NAME}:HIDDEN = FALSE.
    FRAME fsv:MOVE-TO-BOTTOM().
    edtSunken:MOVE-TO-TOP().
    FRAME ftv:MOVE-TO-TOP().
    FRAME ftv:CAREFUL-PAINT = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeTviter C-Win
PROCEDURE initializeTviter PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     To run when the treeview is initialized or resized
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

DEFINE VARIABLE iiter AS INTEGER    NO-UNDO.

DEF BUFFER btviter FOR tviter.

/* If the Tv has been skrinked down, then reduce the cache of dyn pictures and labels  */
gPicNumMax = 0.
FOR EACH tvPic BY tvPic.iter:
    IF gPicNumMax < gTvIterations * gPicCacheCoef THEN DO:
        gPicNumMax = gPicNumMax + 1.
        NEXT.
    END.
    IF VALID-HANDLE(tvPic.hPic) THEN DELETE WIDGET tvPic.hPic.
    DELETE tvPic.
END.

gLabNumMax = 0.
FOR EACH tvLab BY tvLab.iter:
    IF gLabNumMax < gTvIterations * gLabCacheCoef THEN DO:
        gLabNumMax = gLabNumMax + 1.
        NEXT.
    END.
    IF VALID-HANDLE(tvLab.hLab) THEN DELETE WIDGET tvLab.hLab.
    DELETE tvLab.
END.

FOR EACH btviter WHERE btviter.iter > gTvIterations:
    DELETE btviter.
END.


/* The following makes sense at startup or when increasing the number of iterations in the TV */
DO iiter = 1 TO gTvIterations:
    IF CAN-FIND(tviter WHERE tviter.iter = iiter) THEN NEXT.
    CREATE btviter.
    ASSIGN
     btviter.iter = iiter
     btviter.id   = ?. /* beware unique index */

    RUN createPic(/*X*/ 2 + 2 /*bnode.level*/ * gTvLevelWidth
                 ,/*Y*/ 1 + (btviter.iter - 1) * gtviterationHeight
                 ,/*VISIBLE*/ NO
                 ,/*IMAGE file-name */ picFileName("collapsed","{&tvpicpath}Folder")).

    RUN createLab(/*X*/ 2 + 2 /*bnode.level*/ * gTvLevelWidth + gTvPicWidth + 1
                 ,/*Y*/ 1 + (btviter.iter - 1) * gtviterationHeight
                 ,/*VISIBLE*/ NO
                 ,/*SCREEN-VALUE */ ""
                 ,15).
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE labLeftMouseEvent C-Win
PROCEDURE labLeftMouseEvent PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     Event procedure for actions done on node.
  Parameters:  <none>
  Notes:       This procedure can be called from the event procedure for a node
               picture via an APPLY '<Event>' TO btviter.hLab.
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cFunction AS CHARACTER  NO-UNDO.

    DEF BUFFER btviter FOR tviter.
    DEF BUFFER bnode FOR node.
    DEFINE VARIABLE cDragSource AS CHARACTER  NO-UNDO.

    FIND btviter WHERE btviter.hlab = SELF.

    /* double click may mean expand */
    IF cFunction = "MOUSE-SELECT-DBLCLICK" THEN DO:
        /*APPLY 'MOUSE-SELECT-DBLCLICK' TO btviter.hPic. SLP 30-OCT-2007 */
        {&ApplyEntryToFocusCatcher}
        APPLY 'MOUSE-SELECT-DBLCLICK' TO btviter.hPic. /* SLP 30-OCT-2007 */
        RETURN.
    END.

    /* select new node */
    FIND bnode WHERE bnode.id = btviter.id.
    selectNode(bnode.ke).

    /* The selectNode() above publishes "tvNodeEvent", so an object can catch it and
      decide to few things like adding few nodes and delete the current node (case
      of a "More" node)
      In this situation, then the current bnode record is deleted and we should not go
      any further, hence the following that can sound surprising */
    IF NOT AVAIL bnode THEN RETURN.


    /* right-click to raise a popup menu ? */
    IF cFunction = "RIGHT-MOUSE-CLICK" THEN DO:
        /* If the following returns a value, then it is taken as a popup menu to
          build, see the code of buildAndOpenPopupMenu for more details */
        PUBLISH "tvNodeEvent" ("rightClick", bnode.ke).
        IF RETURN-VALUE = "" THEN RETURN.
        RUN buildAndOpenPopupMenu (bnode.ke, RETURN-VALUE).
        RETURN.
    END.

    {get DragSource cDragSource}.
    IF (cDragSource  = "some" AND LOOKUP("dragSource",bnode.optn, CHR(1))  <> 0
        OR cDragSource  = "all")
       AND LOOKUP("noDragSource",bnode.optn, CHR(1)) = 0
     THEN DO:
        /* If we got there from a keyboard event then dragNode will find it
         out at the very beginning and return immediately */
        RUN dragNode (bnode.id).
        IF RETURN-VALUE = "nodeDragged" THEN RETURN.
    END.

    {&ApplyEntryToFocusCatcher}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadDemoTv C-Win
PROCEDURE loadDemoTv :
/*------------------------------------------------------------------------------
  Purpose:     Guess what...
  Parameters:  <none>
  Notes:       Called internally at design time in order to show a cute little
               treeview
------------------------------------------------------------------------------*/

    /* Clear existing nodes */
    IF CAN-FIND(FIRST node) THEN RUN emptyTree.

    DO ON ERROR UNDO, LEAVE:
        RUN addNode ("n1"   ,""    ,"node 1"   ,""                    ,"")         NO-ERROR.
        RUN addNode ("n2"   ,""    ,"node 2"   ,""                    ,"expanded") NO-ERROR.
        RUN addNode ("n21"  ,"n2"  ,"node 21"  ,""                    ,"")         NO-ERROR.
        RUN addNode ("n22"  ,"n2"  ,"node 22"  ,"{&tvpicpath}book02.bmp"   ,"expanded") NO-ERROR.
        RUN addNode ("n221" ,"n22" ,"node 221" ,"{&tvpicpath}book02.bmp"   ,"")         NO-ERROR.
        RUN addNode ("n222" ,"n22" ,"node 222" ,"{&tvpicpath}book02.bmp"   ,"")         NO-ERROR.
        RUN addNode ("n3"   ,""    ,"node 3"   ,"{&tvpicpath}present1.bmp" ,"expanded") NO-ERROR.
        RUN addNode ("n31"  ,"n3"  ,"node 31"  ,"{&tvpicpath}$.bmp"        ,"")         NO-ERROR.
        RUN addNode ("n4"   ,""    ,"node 4"   ,"{&tvpicpath}smile56.bmp"  ,"")         NO-ERROR.
    END.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE THIS-PROCEDURE:FILE-NAME PROGRAM-NAME(1) SKIP
         "Problem to realize a demo treeview in design mode.  addNode raised the following error:" SKIP
          RETURN-VALUE
         "~n~nAbout to do a RETURN ERROR"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN ERROR.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MouseSelectDownBtnScrollDown C-Win
PROCEDURE MouseSelectDownBtnScrollDown PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:  MouseSelectDownBtnScrollDown   event procedure when choosing the
           button to scroll down in the emulated scrollbar
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLastEvent           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE mKeyboardState       AS MEMPTR     NO-UNDO.
  DEFINE VARIABLE iFirstClickEtime     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFirstClickProcessed AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE tvScrollRtn          AS LOGICAL    NO-UNDO.

  IF gWindowsSkin <> "Classic" THEN DO:
      ghbtnScrollDown:LOAD-IMAGE ( getPicturePath("scrollButtonDoPressed.bmp")).
      /* strange, loading an image in ghbtnScrollDown makes the thumb go to the bottom...
       The following fixes that*/
      ghbsv:MOVE-TO-TOP().
      IF VALID-HANDLE(ghbsvGraber) THEN ghbsvGraber:MOVE-TO-TOP().
  END.
  ELSE RUN SendMessageA IN hppure4gltvApi ( ghbtnScrollDown:HWND,
                                   {&BM_SETSTATE},
                                   1,  /* TRUE, set BST_PUSHED style */
                                   0
                                    {&APIRtnParmINT}).

  SET-SIZE(mKeyboardState) = 256.
  iFirstClickEtime = ETIME.

  DO WHILE TRUE:
      /* WAIT-FOR a dummy event, what I want is the PAUSE 0 otherwise the UI
         never finds out that we release the mouse button....*/
      WAIT-FOR 'LEFT-MOUSE-DOWN' OF focusCatcher IN FRAME fMain  PAUSE 0.
      IF LAST-EVENT:FUNCTION <> '' THEN cLastEvent = LAST-EVENT:FUNCTION.

      /* do we still keep the left mouse button pressed ? */
      RUN GetKeyboardState(GET-POINTER-VALUE(mKeyboardState)  {&APIRtnParmINT}).

      IF GET-BITS(GET-BYTE(mKeyboardState,2) ,8, 1) = 0  THEN LEAVE. /* 2 is actually 1 + 1 (2nd byte but the first is coded as 0)*/

      /* wait a bit before processing next scroll's */
      IF iFirstClickProcessed AND ETIME < iFirstClickEtime + 500 THEN NEXT.

      tvScrollRtn = tvScroll(1,NO).
      IF NOT tvScrollRtn THEN LEAVE.
      iFirstClickProcessed = YES.
      IF cLastEvent <> "" THEN LEAVE. /* just in case, but actually, we should find out that the button was released earlier */
  END.
  SET-SIZE(mKeyboardState) = 0.

  IF gWindowsSkin <> "Classic" THEN DO:
      ghbtnScrollDown:LOAD-IMAGE(getPicturePath("scrollButtonDo.bmp")).
      /* strange, loading an image in ghbtnScrollDown makes the thumb go to the bottom...
       The following fixes that*/
      ghbsv:MOVE-TO-TOP().
      IF VALID-HANDLE(ghbsvGraber) THEN ghbsvGraber:MOVE-TO-TOP().
  END.


  ELSE RUN SendMessageA IN hppure4gltvApi ( ghbtnScrollDown:HWND,
                              {&BM_SETSTATE},
                              0,   /* FALSE, remove BST_PUSHED style */
                              0
                              {&APIRtnParmINT}).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MouseSelectDownBtnScrollUp C-Win
PROCEDURE MouseSelectDownBtnScrollUp PRIVATE :
/*------------------------------------------------------------------------------
   Purpose:  MouseSelectDownBtnScrollUp   event procedure when choosing the
           button to scroll up in the emulated scrollbar
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLastEvent           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE mKeyboardState       AS MEMPTR     NO-UNDO.
  DEFINE VARIABLE iFirstClickEtime     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFirstClickProcessed AS LOGICAL    NO-UNDO.


  IF gWindowsSkin <> "Classic" THEN DO:
      ghbtnScrollUp:LOAD-IMAGE(getPicturePath("scrollButtonUpPressed.bmp")).
      /* strange, loading an image in ghbtnScrollDown makes the thumb go to the bottom...
       The following fixes that*/
      ghbsv:MOVE-TO-TOP().
      IF VALID-HANDLE(ghbsvGraber) THEN ghbsvGraber:MOVE-TO-TOP().
  END.

  ELSE RUN SendMessageA IN hppure4gltvApi ( ghbtnScrollUp:HWND,
                                  {&BM_SETSTATE},
                                  1,  /* TRUE, set BST_PUSHED style */
                                  0
                                   {&APIRtnParmINT}).

  SET-SIZE(mKeyboardState) = 256.
  iFirstClickEtime = ETIME.

  DO WHILE TRUE:
      /* WAIT-FOR a dummy event, what I want is the PAUSE 0 otherwise the UI
         never finds out that we release the mouse button....*/
      WAIT-FOR 'LEFT-MOUSE-DOWN' OF focusCatcher IN FRAME fMain  PAUSE 0.
      IF LAST-EVENT:FUNCTION <> '' THEN cLastEvent = LAST-EVENT:FUNCTION.

      /* do we still keep the left mouse button pressed ? */
      RUN GetKeyboardState(GET-POINTER-VALUE(mKeyboardState)  {&APIRtnParmINT}).

      IF GET-BITS(GET-BYTE(mKeyboardState,2) ,8, 1) = 0  THEN LEAVE. /* 2 is actually 1 + 1 (2nd byte but the first is coded as 0)*/

      /* wait a bit before processing next scroll's */
      IF iFirstClickProcessed AND ETIME < iFirstClickEtime + 500 THEN NEXT.

      IF NOT tvScroll(-1,NO) THEN LEAVE.
      iFirstClickProcessed = YES.

      IF cLastEvent <> "" THEN LEAVE. /* just in case, but actually, we should find out that the button was released earlier */
  END.

  SET-SIZE(mKeyboardState) = 0.

  IF gWindowsSkin <> "Classic" THEN DO:
      ghbtnScrollUp:LOAD-IMAGE(getPicturePath("scrollButtonUp.bmp")).
      /* strange, loading an image in ghbtnScrollDown makes the thumb go to the bottom...
       The following fixes that*/
      ghbsv:MOVE-TO-TOP().
      IF VALID-HANDLE(ghbsvGraber) THEN ghbsvGraber:MOVE-TO-TOP().
  END.
  ELSE RUN SendMessageA IN hppure4gltvApi ( ghbtnScrollUp:HWND,
                                  {&BM_SETSTATE},
                                  0,   /* FALSE, remove BST_PUSHED style */
                                  0
                                   {&APIRtnParmINT}).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveNode C-Win
PROCEDURE moveNode :
/*------------------------------------------------------------------------------
  Purpose:     Move a given node to wherever

  Parameters:  pcNodeKey  Key of node to move

               pcToKe     Key of relative node to move to (after it, before it, last child of it)

               pcMode     One of the 3 following values
                           "after"
                           "before"
                           "parent"

               pcOtpn    extendable comma separated list of options
                          "refresh"  => calls tvRefresh() at the end of the process

  Notes:       IF pcMode = "parent" and toNode.Ke is blank THEN it means
               'move it so it becomes the last guy at level 0'
                 => In this case, the buffer toNode will hold no record
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcNodeKe AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcToKe   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcMode   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcoptn   AS CHARACTER NO-UNDO.


DEF BUFFER moveNode    FOR node.
DEF BUFFER toNode      FOR node.
DEF BUFFER parentNode  FOR node.
DEF BUFFER bnode       FOR node.
DEF BUFFER widestChild FOR node.
DEF BUFFER btviter     FOR tviter.
DEF BUFFER bcopyNode   FOR copyNode.


DEFINE VARIABLE par       LIKE node.par NO-UNDO.
DEFINE VARIABLE origLevel LIKE node.level NO-UNDO.
DEFINE VARIABLE lKeepSameParent AS LOGICAL    NO-UNDO. /* used to avoid to update parent level*/

/*--------------- Sanity checks  ------------------------*/
IF pcNodeKe = pcToKe THEN RETURN ERROR "Trying to move a node to itself. (Key " + pcnodeKe + ") which does not make sense".
IF LOOKUP(pcMode, "after,before,parent") = 0 OR NUM-ENTRIES(pcMode) <> 1
 THEN RETURN ERROR "Invalid pcMode '" + pcMode + "' passed.  Valid values are 'after' or 'before' or 'parent'".

FIND moveNode WHERE moveNode.ke = pcnodeKe NO-ERROR.
IF NOT AVAIL moveNode THEN RETURN ERROR "Cannot find node to move with key " + pcnodeKe.

FIND toNode WHERE toNode.ke = pcToKe NO-ERROR.
IF NOT AVAIL toNode
 AND NOT (pcMode = "parent" AND pcToKe = "") /*this case  means move it so it becomes the last guy at level 0*/
 THEN RETURN ERROR "Cannot find reference node with key " + pcToKe.

/* Walk to root to check that we do not try to move a node to one of its child
  (This does not make sense) */
IF AVAIL toNode THEN FIND parentNode WHERE parentNode.id = toNode.id NO-ERROR.  /* not avail if pcMode = parent and toNode.Ke is blank then it means move it so it becomes the last guy at level 0 */
IF AVAIL parentNode THEN DO WHILE TRUE:
    IF parentNode.par = 0 THEN LEAVE.
    IF parentNode.par = moveNode.id THEN RETURN ERROR "Tring to move node '" + pcNodeKe + "' to one of its child '"
     + pcToKe + "', which does not make sense.  You should first move this child somewhere else".
    par = parentNode.par.
    FIND parentNode WHERE parentNode.id = par.
END.
/*--------------- End of Sanity checks  ------------------------*/


IF moveNode.par = toNode.id AND pcMode = "parent" THEN lKeepSameParent = YES.
IF moveNode.par = toNode.par AND CAN-DO("after,before", pcMode) THEN lKeepSameParent = YES.

/*----------- Update current relatives of moveNode -------------*/
IF NOT lKeepSameParent THEN DO:
    /* Update the parents about colChildren exp Children and all the VWP stuffs
     The algorithm is very similar to the one of deleteNode*/
    FIND bnode WHERE bnode.id = moveNode.id.
    UpdateParerentsOfmoveNode:
    DO WHILE TRUE:
        /* widestBrother is not AVAIL only when no brother */
        FIND LAST widestChild WHERE
         widestChild.par = bnode.par
         AND widestChild.id <> moveNode.id
         USE-INDEX parVWPExpChildren NO-ERROR.

        IF bnode.par = 0 THEN DO:
            gExpChildren = gExpChildren - moveNode.expChildren - 1.
            gVWP = widestChild.VWPexpChildren.
            LEAVE.
        END.

        FIND parentNode WHERE parentNode.id = bnode.par.

        IF NOT parentNode.expanded THEN DO:
            ASSIGN
             parentNode.colChildren    = parentNode.colChildren - moveNode.colChildren - 1.
             parentNode.VWPcolChildren = MAX(parentNode.VWP,IF AVAIL widestChild THEN widestChild.VWPexpChildren ELSE 0).
            LEAVE UpdateParerentsOfmoveNode.
        END.
        ASSIGN
         parentNode.expChildren = parentNode.expChildren - moveNode.expChildren - 1
         parentNode.VWPexpChildren = MAX(parentNode.VWP,IF AVAIL widestChild THEN widestChild.VWPexpChildren ELSE 0).
        FIND bnode WHERE bnode.id = parentNode.id.
    END.
END. /* IF NOT lKeepSameParent */

/* keep a copy of modeNode and delete it to avoid unique index problems */
origLevel = moveNode.level.
CREATE bcopyNode.   BUFFER-COPY moveNode TO bcopyNode.  DELETE moveNode.

/* Update prev-sibling and next-sibling of moveNode
  We have to connect them to each other or to 0 (if they become first or last child)*/
IF bcopyNode.pre <> 0 THEN DO:
    FIND bnode WHERE bnode.id = bcopyNode.pre.
    bnode.nex = bcopyNode.nex. /*can be 0 if moveNode was last child*/
END.
IF bcopyNode.nex <> 0 THEN DO:
    FIND bnode WHERE bnode.id = bcopyNode.nex.
    bnode.pre = bcopyNode.pre.  /*can be 0 if moveNode was first child*/
END.
/*----------- End of Update current relatives of moveNode -------------*/

/* Refine bcopyNode with new parent, pre, nex, level, VWP, VWPcolChildren and VWPExpChildren
   and update the new prev and next sibling if any*/
CASE pcMode:
    WHEN "after" THEN DO:
        /* update next sibling of toNode so it becomes the new next sibling of moveNode */
        IF toNode.nex <> 0 THEN DO:
            FIND bnode WHERE bnode.id = toNode.nex.
            bnode.pre = bcopyNode.id.
        END.
        ASSIGN
         bcopyNode.par   = toNode.par
         bcopyNode.pre   = toNode.id
         bcopyNode.nex   = toNode.nex
            toNode.nex   = bcopyNode.id
         bcopyNode.level = toNode.level.
    END.
    WHEN "before" THEN DO:
        /* update prev sibling of toNode so it becomes the new prev sibling of moveNode */
        IF toNode.pre <> 0 THEN DO:
            FIND bnode WHERE bnode.id = toNode.pre.
            bnode.nex = bcopyNode.id.
        END.
        ASSIGN
         bcopyNode.par   = toNode.par
         bcopyNode.nex   = toNode.id
         bcopyNode.pre   = toNode.pre
            toNode.pre   = bcopyNode.id
         bcopyNode.level = toNode.level.
    END.
    WHEN "parent" THEN DO:
        FIND parentNode WHERE ROWID(parentNode) = ? NO-ERROR. /* empty parentNode buffer*/
        IF AVAIL toNode THEN FIND parentNode WHERE parentNode.id = toNode.id NO-ERROR.  /* not avail if pcMode = parent and toNode.Ke is blank then it means move it so it becomes the last guy at level 0 */
        /* find last child of parent */
        FIND bnode WHERE
         bnode.par = (IF AVAIL parentNode THEN parentNode.id ELSE 0)
         AND bnode.nex = 0
         NO-ERROR. /* Not avail means that the parent has no node yet.
          Note we do not need to handle the two cases when there is no node at all in the tree
          or when we are trying to move the node to one of its child, because these two cases
          cannot pass the sanity checks of the beginning*/

        /* update last child of parent */
        IF AVAIL bnode THEN bnode.nex = bcopyNode.id.
        ASSIGN
         bcopyNode.par   = IF AVAIL parentNode THEN parentNode.id ELSE 0
         bcopyNode.nex   = 0
         bcopyNode.pre   = IF AVAIL bnode THEN bnode.id ELSE 0
         bcopyNode.level = IF AVAIL parentNode THEN parentNode.level + 1 ELSE 0.
    END.
END CASE.

ASSIGN
 bcopyNode.VWP   = bcopyNode.VWP + (bcopyNode.level - origLevel) * gTvLevelWidth
 bcopyNode.VWPexpChildren = bcopyNode.VWPexpChildren + (bcopyNode.level - origLevel) * gTvLevelWidth
 bcopyNode.VWPcolChildren = bcopyNode.VWPcolChildren + (bcopyNode.level - origLevel) * gTvLevelWidth.


/* Recreate moveNode with updated values in bcopyNode*/
CREATE moveNode.
BUFFER-COPY bcopyNode TO moveNode.

/* Update the level of the children of moveNode*/
IF bcopyNode.level <> origLevel
 AND CAN-FIND(FIRST node WHERE node.par = bcopyNode.id)
 THEN RUN AddNToLevelOfBranchOf (bcopyNode.level - origLevel, bcopyNode.id).


IF NOT lKeepSameParent THEN DO:
    /* Update the new parents about colChildren exp Childen and all the VWP stuffs
     The algorythm has some similarities with the one of expand Node */
    FIND bnode WHERE bnode.id = moveNode.id.
    UpdateNewParerentsOfmoveNode:
    DO WHILE TRUE:
        /* widestBrother is not AVAIL only when no brother */
        FIND LAST widestChild WHERE widestChild.par = bnode.par USE-INDEX parVWPExpChildren.

        IF bnode.par = 0 THEN DO:
            gExpChildren = gExpChildren + bcopyNode.expChildren + 1.
            gVWP = widestChild.VWPexpChildren.
            LEAVE UpdateNewParerentsOfmoveNode.
        END.

        FIND parentNode WHERE parentNode.id = bnode.par.

        IF NOT parentNode.expanded THEN DO:
            ASSIGN
             parentNode.colChildren    = parentNode.colChildren + bcopyNode.expChildren + 1.
             parentNode.VWPcolChildren = MAX(parentNode.VWP,widestChild.VWPexpChildren).
            LEAVE UpdateNewParerentsOfmoveNode.
        END.
        ASSIGN
         parentNode.expChildren = parentNode.expChildren + bcopyNode.expChildren + 1
         parentNode.VWPexpChildren = MAX(parentNode.VWP,widestChild.VWPexpChildren).
        FIND bnode WHERE bnode.id = parentNode.id.
    END.
END. /* IF NOT lKeepSameParent */


/* The following RELEASE's are required because of some unique node INDEXes
   Not doing them disturbs the next call of tvRefresh() */
RELEASE moveNode.
IF AVAIL toNode THEN RELEASE toNode.
IF AVAIL parentNode THEN RELEASE parentNode.

IF CAN-DO(pcoptn, "refresh") THEN tvRefresh().

DELETE bcopyNode.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE optimizeTviterMSWin C-Win
PROCEDURE optimizeTviterMSWin PRIVATE :
/*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
optimizeTviterMSWin to optimize the rendering of tvrefresh() on native MS Windows
by reusing iteration widget at best.  See comment in optimizeTviterWine to see
more details.
Note the two optimization procedures use the same method, but for different
attributes because the cost of writing to some UI attributes is not the same in
Linux/Wine as on MS Windows.
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*/

DEF BUFFER bnode       FOR node.
DEF BUFFER childnode   FOR node.
DEF BUFFER btviter     FOR tviter.
DEF BUFFER btvpic      FOR tvpic.
DEF BUFFER btvlab      FOR tvlab.

  /* First pass:
    IMAGE  image x y visible
     TEXT  screen-value x y visible  */
  DEFINE VARIABLE lAllIterationsReady AS LOGICAL    NO-UNDO.
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picimg     = btviter.picimg
                        AND btvpic.picY       = btviter.picY
                        AND btvpic.picX       = btviter.picX
                        AND btvpic.picVisible = YES
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labScreenValue = btviter.labScreenValue
                        AND btvlab.labY           = btviter.labY
                        AND btvlab.labX           = btviter.labX
                        AND btvlab.labVisible     = YES
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */

  /* 2nd pass
    IMAGE  image x y
     TEXT x y visible */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picimg     = btviter.picimg
                        AND btvpic.picY       = btviter.picY
                        AND btvpic.picX       = btviter.picX
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN  btvpic.iter  = btviter.iter   btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labY           = btviter.labY
                        AND btvlab.labX           = btviter.labX
                        AND btvlab.labVisible     = YES
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN  btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */

  /* 3rd pass:
    IMAGE  image (x or y) visible
     TEXT  screen-value x y */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picimg     = btviter.picimg
                        AND (btvpic.picY      = btviter.picY
                             OR btvpic.picX   = btviter.picX)
                        AND btvpic.picVisible = YES
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labScreenValue = btviter.labScreenValue
                        AND btvlab.labY           = btviter.labY
                        AND btvlab.labX           = btviter.labX
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */


  /* 4th pass:
    IMAGE  image (x or y)
     TEXT  screen-value (x or y) visible */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picimg     = btviter.picimg
                        AND (btvpic.picY      = btviter.picY
                             OR btvpic.picX   = btviter.picX)
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labScreenValue = btviter.labScreenValue
                        AND (btvlab.labY          = btviter.labY
                             OR btvlab.labX       = btviter.labX)
                        AND btvlab.labVisible     = YES
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */

  /* 5th pass:
    IMAGE   image visible
     TEXT   x y  */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picimg     = btviter.picimg
                        AND btvpic.picVisible = YES
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labY           = btviter.labY
                        AND btvlab.labX           = btviter.labX
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */

  /* 6th pass:
    IMAGE  image
     TEXT  screen-value visible */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picimg     = btviter.picimg
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labScreenValue = btviter.labScreenValue
                        AND btvlab.labVisible     = YES
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */

    /* 7th pass:
    IMAGE   x y visible
     TEXT  screen-value (x or y)  */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picY       = btviter.picY
                        AND btvpic.picX       = btviter.picX
                        AND btvpic.picVisible = YES
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labScreenValue = btviter.labScreenValue
                        AND (btvlab.labY          = btviter.labY
                             OR  btvlab.labX      = btviter.labX)
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */

  /* 8th pass:
    IMAGE  x y
     TEXT  screen-value */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picY       = btviter.picY
                        AND btvpic.picX       = btviter.picX
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labScreenValue = btviter.labScreenValue
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */

  /* 9th pass:
    IMAGE  x OR y
     TEXT  screen-value x y visible  */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND (btvpic.picY      = btviter.picY
                             OR  btvpic.picX  = btviter.picX)
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND (btvlab.labY          = btviter.labY
                             OR btvlab.labX       = btviter.labX)
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */

  /* Last pass, either create a new cache file, or force the reuse of a remaining widget*/
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          IF gPicCacheCoef * gTvIterations < gPicNumMax
           THEN RUN createPic(/*X*/ btviter.picX
                             ,/*Y*/ btviter.picY
                             ,/*VISIBLE*/ YES
                             ,/*IMAGE file-name */ btviter.picimg).
          ELSE DO:
              FIND FIRST btvpic WHERE btvpic.iter = ?. /* This should never fail by design, anyway if it does then we know it is here... */
              ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          END.
      END.
      IF btviter.hlab = ? THEN DO:
          IF gLabCacheCoef * gTvIterations < gLabNumMax
           THEN RUN createLab(/*X*/ btviter.labX
                             ,/*Y*/ btviter.labY
                             ,/*VISIBLE*/ YES
                             ,/*SCREEN-VALUE */ btviter.labScreenValue
                             ,btviter.labWidthPixels).
          ELSE DO:
              FIND FIRST btvlab WHERE btvlab.iter     = ?. /* This should never fail by design, anyway if it does then we know it is here... */
              ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          END.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE optimizeTviterWine C-Win
PROCEDURE optimizeTviterWine PRIVATE :
/*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
optimizeTviterWine to optimize the rendering of tvrefresh() on Linux/Wine by
reusing iteration widget at best.  The following results have been taken into
account to achieve this procedure.

GUI performance results in milliseconds from quickest to slowest (3 samples)
Test with a loop that does 500 changes of Attributes or Method calls

Note the order is NOT THE SAME on wine and on Native Windows
For instance, on wine, doing a LOAD() is a bit faster than moving
X and Y, whereas on Native Windows, moving X and Y is much faster
than calling LOAD()


On Wine 20040121 with KDE 3.2 Mandrake 10.0:
  IMAGE Widget
        VISIBLE             1380    1357    1457
        Y OR X (one)        3731    3745    3400
        LOAD()              6152    6206    6182
        X AND Y (both)      7296    7526    7312
        LOAD() AND X       10078   10121   10167

   TEXT Widget
        Y OR X (one)         380     394     397
        X AND Y (both)       739     756     766
        VISIBLE             1137    1574    1587
        SCREEN-VALUE        1345    1351    1346
        SCREEN-VALUE AND X  1789    1796    1797


Results on native Windows XP, again with 500 iterations:
  IMAGE Widget
        VISIBLE              141     157     156
        Y OR X (one)         641     609     641
        X AND Y (both)      1235    1234    1219
        LOAD()              4937    4765    4781
        LOAD() AND X        5500    5468    5500

   TEXT Widget
        Y OR X (one)         235     219     218
        VISIBLE              203     219     204
        SCREEN-VALUE         203     187     203
        X AND Y (both)       421     421     437
        SCREEN-VALUE AND X   437     406     578
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*/

DEF BUFFER bnode       FOR node.
DEF BUFFER childnode   FOR node.
DEF BUFFER btviter     FOR tviter.
DEF BUFFER btvpic      FOR tvpic.
DEF BUFFER btvlab      FOR tvlab.

  /* First pass:
    IMAGE  image x y visible
     TEXT  screen-value x y visible  */
  DEFINE VARIABLE lAllIterationsReady AS LOGICAL    NO-UNDO.
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picimg     = btviter.picimg
                        AND btvpic.picY       = btviter.picY
                        AND btvpic.picX       = btviter.picX
                        AND btvpic.picVisible = YES
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labScreenValue = btviter.labScreenValue
                        AND btvlab.labY           = btviter.labY
                        AND btvlab.labX           = btviter.labX
                        AND btvlab.labVisible     = YES
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */


  /* 2nd pass
    IMAGE  image x y
     TEXT  screen-value (x or y) visible  */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picimg     = btviter.picimg
                        AND btvpic.picY       = btviter.picY
                        AND btvpic.picX       = btviter.picX
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN  btvpic.iter  = btviter.iter   btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labScreenValue = btviter.labScreenValue
                        AND (btvlab.labY          = btviter.labY
                             OR btvlab.labX       = btviter.labX)
                        AND btvlab.labVisible     = YES
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN  btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */

  /* 3rd pass:
    IMAGE  image (x or y) visible
     TEXT  screen-value visible  */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picimg     = btviter.picimg
                        AND (btvpic.picY      = btviter.picY
                             OR btvpic.picX   = btviter.picX)
                        AND btvpic.picVisible = YES
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labScreenValue = btviter.labScreenValue
                        AND btvlab.labVisible     = YES
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */


  /* 4th pass:
    IMAGE  image (x or y)
     TEXT  screen-value */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picimg     = btviter.picimg
                        AND (btvpic.picY      = btviter.picY
                             OR btvpic.picX   = btviter.picX)
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labScreenValue = btviter.labScreenValue
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */

  /* 5th pass:
    IMAGE   x y visible
     TEXT   x y visible  */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picY       = btviter.picY
                        AND btvpic.picX       = btviter.picX
                        AND btvpic.picVisible = YES
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labY           = btviter.labY
                        AND btvlab.labX           = btviter.labX
                        AND btvlab.labVisible     = YES
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */

  /* 6th pass:
    IMAGE  image visible
     TEXT  screen-value (x or y)  */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picimg     = btviter.picimg
                        AND btvpic.picVisible = YES
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labScreenValue = btviter.labScreenValue
                        AND (btvlab.labY          = btviter.labY
                             OR  btvlab.labX      = btviter.labX)
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */

    /* 7th pass:
    IMAGE   x y
     TEXT  screen-value x y   */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picY       = btviter.picY
                        AND btvpic.picX       = btviter.picX
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labScreenValue = btviter.labScreenValue
                        AND btvlab.labY           = btviter.labY
                        AND btvlab.labX           = btviter.labX
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */

  /* 8th pass:
    IMAGE  image
     TEXT   x y   */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND btvpic.picimg     = btviter.picimg
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND btvlab.labY           = btviter.labY
                        AND btvlab.labX           = btviter.labX
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.

  /* 9th pass:
    IMAGE  x OR y
     TEXT  screen-value x y visible  */
  lAllIterationsReady = YES.
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          FIND FIRST btvpic WHERE btvpic.iter = ? /* pic not used by another tviter yet */
                        AND (btvpic.picY      = btviter.picY
                             OR  btvpic.picX  = btviter.picX)
           NO-ERROR.
          IF AVAIL btvpic THEN ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          ELSE lAllIterationsReady = NO.
      END.
      IF btviter.hlab = ? THEN DO:
          FIND FIRST btvlab WHERE btvlab.iter     = ? /* lab not used by another tviter yet */
                        AND (btvlab.labY          = btviter.labY
                             OR btvlab.labX       = btviter.labX)
           NO-ERROR.
          IF AVAIL btvlab THEN ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          ELSE lAllIterationsReady = NO.
      END.
  END.
  IF lAllIterationsReady THEN RETURN. /* Well that's all done, no need to go further */

  /* Last pass, either create a new cache file, or force the reuse of a remaining widget*/
  FOR EACH btviter WHERE btviter.id <> ?:  /* do not care about old data in empty iterations*/
      IF btviter.hpic = ? THEN DO:
          IF gPicCacheCoef * gTvIterations < gPicNumMax
           THEN RUN createPic(/*X*/ btviter.picX
                             ,/*Y*/ btviter.picY
                             ,/*VISIBLE*/ YES
                             ,/*IMAGE file-name */ btviter.picimg).
          ELSE DO:
              FIND FIRST btvpic WHERE btvpic.iter = ?. /* This should never fail by design, anyway if it does then we know it is here... */
              ASSIGN btvpic.iter  = btviter.iter     btviter.hpic = btvpic.hpic.
          END.
      END.
      IF btviter.hlab = ? THEN DO:
          IF gLabCacheCoef * gTvIterations < gLabNumMax
           THEN RUN createLab(/*X*/ btviter.labX
                             ,/*Y*/ btviter.labY
                             ,/*VISIBLE*/ YES
                             ,/*SCREEN-VALUE */ btviter.labScreenValue
                             ,btviter.labWidthPixels).
          ELSE DO:
              FIND FIRST btvlab WHERE btvlab.iter     = ?. /* This should never fail by design, anyway if it does then we know it is here... */
              ASSIGN btvlab.iter  = btviter.iter  btviter.hlab = btvlab.hlab.
          END.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE picLeftMouseEvent C-Win
PROCEDURE picLeftMouseEvent PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:    Event Procedure for node picture widgets
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cFunction AS CHARACTER  NO-UNDO.

    DEF BUFFER btviter   FOR tviter.
    DEF BUFFER bnode     FOR node.
    DEF BUFFER childNode FOR node.

    FIND btviter WHERE btviter.hpic = SELF:HANDLE.
    FIND bnode OF btviter.

    /* did user click on plus/minus sign ? */
    /* slacroix 01-JUL-2005 bug when plus/minus & text
    IF (glPicWithPlusMinus AND LAST-EVENT:X <= SELF:X + 8
         AND cFunction = 'LEFT-MOUSE-DOWN'
        OR cFunction = "MOUSE-SELECT-DBLCLICK")

        => glOnlyPlusMinus was forgotten for LEFT-MOUSE-DOWN
     */
    IF ( cFunction = 'LEFT-MOUSE-DOWN'
         AND (glPicWithPlusMinus AND LAST-EVENT:X <= SELF:X + 8
              OR glOnlyPlusMinus)
        OR cFunction = "MOUSE-SELECT-DBLCLICK")
     AND ( CAN-FIND(FIRST childNode WHERE childNode.par = bnode.id)
          OR LOOKUP("addOnExpand", bnode.optn, CHR(1)) <> 0 )
     THEN DO: /* width of plus/minus sign is 8 pixels */
        IF bnode.expanded THEN collapseNode(bnode.ke ,"refresh").
        ELSE DO:
            IF NOT CAN-FIND(FIRST childNode WHERE childNode.par = bnode.id)
             AND LOOKUP("addOnExpand", bnode.optn, CHR(1)) <> 0
             THEN DO:
                PUBLISH "tvNodeEvent" ("addOnExpand", bnode.ke).
                IF NOT AVAIL bnode THEN RETURN. /* node has been deleted ?*/
                ASSIGN
                 bnode.optn = REPLACE(bnode.optn ,"addOnExpand" + CHR(1), "")
                 bnode.optn = REPLACE(bnode.optn ,CHR(1) + "addOnExpand", "")
                 bnode.optn = REPLACE(bnode.optn ,"addOnExpand"         , "").
            END.
            expandNode(bnode.ke ,"refresh").
        END.
    END.
    ELSE DO:
        APPLY 'LEFT-MOUSE-DOWN' TO btviter.hLab.
        IF NOT AVAIL bnode THEN RETURN. /* node has been deleted ?*/
    END.

    IF cFunction  = 'RIGHT-MOUSE-CLICK' THEN DO:
        APPLY 'RIGHT-MOUSE-CLICK' TO btviter.hLab.
        RETURN.
    END.

    {&ApplyEntryToFocusCatcher}

    /* SLP 09-OCT-2007 - Add Double Click Event */
    /* SLP 30-OCT-2007 - Move the code after the Entry to Focus Catcher
       to resolve problems with focus shifts */
    IF cFunction = "MOUSE-SELECT-DBLCLICK"
    AND NOT CAN-FIND(FIRST childNode WHERE childNode.par = bnode.id)
    THEN PUBLISH "tvNodeEvent" ("doubleClick", bnode.ke).
    /* SLP - END */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopupMenuDrop C-Win
PROCEDURE PopupMenuDrop PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:    MENU-DROP Event Procedure for hPopupMenu
  Parameters:  <none>
  Notes:  We do not want this popup menu to come again next time we do a right
         click on the treeview, because we need to recreate it from scratch
------------------------------------------------------------------------------*/

FRAME ftv:POPUP-MENU     = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopupMenuItemChoosen C-Win
PROCEDURE PopupMenuItemChoosen PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     CHOOSE Event procedure for a popup menu item
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pcEvent AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER pcPar1  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER pcPar2  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cPopupWidgetPool AS CHARACTER  NO-UNDO.

    PUBLISH pcEvent (pcPar1, pcPar2).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renderNode C-Win
PROCEDURE renderNode PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     Render the pic and lab of a given tviter
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

DEFINE  PARAMETER BUFFER sbtviter FOR tviter.
DEFINE VARIABLE lMthRtn AS LOGICAL    NO-UNDO.

DEFINE BUFFER btvpic FOR tvpic.
DEFINE BUFFER btvlab FOR tvlab.

FIND btvpic WHERE btvpic.hpic = sbtviter.hpic.
FIND btvlab WHERE btvlab.hlab = sbtviter.hlab.

IF btvpic.picImg <> sbtviter.picImg THEN DO:
    btvpic.picImg = sbtviter.picImg.

    lMthRtn = btvpic.hpic:LOAD-IMAGE(sbtviter.picImg) NO-ERROR.
    IF NOT lMthRtn THEN DO:
        /* Do that if we cannot find the picture */
        btvpic.hpic:LOAD-IMAGE("{&tvpicpath}missingPicFile.bmp") NO-ERROR.
        btvpic.hpic:TOOLTIP = "Cannot find picture file " + sbtviter.picImg.
        /* Yes, this tooltip will remain there forever.  I do not want to clean
          it each time for performance sake, especially on wine.  Anyway, this kind
          of failure should happen only at development time */
    END.

END.

IF btvpic.picY <> sbtviter.picY THEN ASSIGN
 btvpic.picY = sbtviter.picY
 btvpic.hpic:Y = sbtviter.picY.

IF btvlab.labY <> sbtviter.labY THEN ASSIGN
 btvlab.labY    = sbtviter.labY
 btvlab.hlab:Y  = sbtviter.labY.

IF btvpic.picX <> sbtviter.picX THEN ASSIGN
 btvpic.picX = sbtviter.picX
 btvpic.hpic:X = sbtviter.picX.

/* I need to anticipate how far it goes on X axis otherwise it can make horiz scrollbar appear
   just because I mouse something a bit to far on the left when I assign with the NO-ERROR option
   Now I prefer to not use this NO-ERROR option and never push something too far on the left */
IF btvlab.labWidthPixels > sbtviter.labWidthPixels THEN DO:
    /* likely to shrink the width..., so do it first so it won't be pushed too far on the left
      even temporary */
    IF btvlab.labWidthPixels <> sbtviter.labWidthPixels THEN ASSIGN
     btvlab.labWidthPixels    = sbtviter.labWidthPixels
     btvlab.hlab:WIDTH-PIXELS = sbtviter.labWidthPixels.

    IF btvlab.labX <> sbtviter.labX THEN ASSIGN
     btvlab.labX    = sbtviter.labX
     btvlab.hlab:X  = sbtviter.labX.
END.
ELSE DO:
    IF btvlab.labX <> sbtviter.labX THEN ASSIGN
     btvlab.labX    = sbtviter.labX
     btvlab.hlab:X  = sbtviter.labX.

    /* likely to enlarge the width..., so do it at last so it won't be pushed too far on the left
      even temporary */
    IF btvlab.labWidthPixels <> sbtviter.labWidthPixels THEN ASSIGN
     btvlab.labWidthPixels    = sbtviter.labWidthPixels
     btvlab.hlab:WIDTH-PIXELS = sbtviter.labWidthPixels
     NO-ERROR. /* 08-APR-2007 sla: fianlly, it seems I really need this NO-ERROR option when I update node labels on the fly */

END.

IF btvlab.labScreenValue <> sbtviter.labScreenValue THEN ASSIGN
 btvlab.labScreenValue   = sbtviter.labScreenValue
 btvlab.hlab:SCREEN-VALUE = sbtviter.labScreenValue.

/* SLP 17-OCT-2007 */
ASSIGN btvlab.hlab:FONT    = sbtviter.nodeFont
           WHEN btvlab.hlab:FONT    <> sbtviter.nodeFont
       btvlab.hlab:TOOLTIP = sbtviter.nodeTooltip /* SLP 30-OCT-2007 */
           WHEN btvlab.hlab:TOOLTIP  <> sbtviter.nodeTooltip
       btvlab.hlab:FGCOLOR = sbtviter.nodeFGCol
           WHEN btvlab.hlab:FGCOLOR <> sbtviter.nodeFGCol
            AND btvlab.hlab         <> gCurhLab /* Otherwise we loose track of the current node */
       btvlab.hlab:BGCOLOR = sbtviter.nodeBGCol
           WHEN btvlab.hlab:BGCOLOR <> sbtviter.nodeBGCol
            AND btvlab.hlab         <> gCurhLab /* Otherwise we loose track of the current node */
       .
/* SLP 17-OCT-2007 - End */

IF NOT btvpic.picVisible THEN ASSIGN
 btvpic.picVisible   = YES
 btvpic.hpic:HIDDEN = NO.

IF NOT btvlab.labVisible THEN ASSIGN
 btvlab.labVisible   = YES
 btvlab.hlab:HIDDEN = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeObject C-Win
PROCEDURE resizeObject :
/*------------------------------------------------------------------------------
  Purpose:     standard ADM2 API
  Parameters:  height and width in char units

  Notes:       It is up to the developer to
                1) call this API
                2) query the resizeVertical and resizeHorizontal instance
                 properties to supply the expected values

               If a parameter is ? or 0, then it means that the width or
               height has to remain unchanged

Example in window-resize of a container window:

  {get resizeVertical lresizeVertical h_pure4glTv}.
  IF lresizeVertical = ? THEN lresizeVertical = YES.
  {get resizeHorizontal lresizeHorizontal h_pure4glTv}.
  IF lresizeHorizontal = ? THEN lresizeHorizontal = YES.

  iTvWidth = FRAME fMain:COL - 1.7. /* the treeview is on the left border */

  iVerticalGap = SELF:HEIGHT-CHAR - FRAME fContainer:HEIGHT-CHARS.
  iHorizontalGap = SELF:WIDTH-CHAR - FRAME fContainer:WIDTH-CHARS.

  [...] /* resizing of frame in the smart window */

  RUN resizeObject IN h_pure4glTv
    (IF lresizeVertical THEN SELF:HEIGHT-CHARS - 0.2 ELSE ?
    ,IF lresizeHorizontal THEN iTVWidth + iHorizontalGap ELSE ?).

  [...]
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pdHeight         AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pdWidth          AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lfMainEnlargeVertical   AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lfMainEnlargeHorizontal AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE prevTvIterations LIKE gTvIterations   NO-UNDO.
  DEFINE VARIABLE lScrollDown             AS LOGICAL    NO-UNDO.

  DEF BUFFER bnode FOR node.

  IF pdHeight = ? OR pdHeight = 0 THEN pdHeight = FRAME fMain:HEIGHT-CHARS.
  IF pdWidth  = ? OR pdWidth  = 0 THEN pdWidth  = FRAME fMain:WIDTH-CHARS.

  /* minimum values are hardcoded... */
  ASSIGN
   pdHeight = MAX(pdHeight, 3.52)
   pdWidth  = MAX(pdWidth,21.80).
  ASSIGN
   edtSunken:Y = 0  /* Needed because it is not at 0,0 at design time... */
   edtSunken:X = 0 /* and resizeObject can be fired form the AB before initializePure4glTv */
   NO-ERROR. /* NO-ERROR to avoid a dummy warning */

  setScrollBarWidgets().

  ASSIGN
   lfMainEnlargeVertical   = pdHeight > FRAME fMain:HEIGHT-CHARS
   lfMainEnlargeHorizontal = pdWidth  > FRAME fMain:WIDTH-CHARS
   gftvWP = pdWidth  * SESSION:PIXELS-PER-COL - 4
   gFtvHP = pdHeight * SESSION:PIXELS-PER-ROW - 4.

  IF lfMainEnlargeVertical THEN ASSIGN
   FRAME fMain:HEIGHT-CHARS = pdHeight
   edtSunken:HEIGHT-PIXELS  = FRAME fMain:HEIGHT-PIXELS
   FRAME ftv:HEIGHT-PIXELS  = gFtvHP.

  IF lfMainEnlargeHorizontal THEN ASSIGN
   FRAME fMain:WIDTH-CHARS  = pdWidth
   edtSunken:WIDTH-PIXELS   = FRAME fMain:WIDTH-PIXELS
   FRAME ftv:WIDTH-PIXELS   = gFtvWP.

  ASSIGN
   FRAME fsv:WIDTH-PIXELS = {&fsvWIDTH-PIXELS}
   FRAME fsv:X = gftvWP - {&fsvWIDTH-PIXELS} + 2
   gFsvimgY = ghbtnScrollUp:HEIGHT-PIXELS
   imgsv:Y IN FRAME fsv = gFsvimgY.

  resizeVertScrollBar().

  ASSIGN
   prevTvIterations = gTvIterations
   gTvIterations = TRUNCATE((gFtvHP - 1) / gTvIterationHeight,0)
   gVisibleIterations = gTvIterations.


  IF NOT lfMainEnlargeVertical THEN ASSIGN
   FRAME ftv:HEIGHT-PIXELS  = gFtvHP
   edtSunken:HEIGHT-CHARS   = pdHeight
   FRAME fMain:HEIGHT-CHARS = pdHeight.

  IF NOT lfMainEnlargeHorizontal THEN ASSIGN
   FRAME ftv:WIDTH-PIXELS  = gFtvWP
   edtSunken:WIDTH-CHARS   = pdWidth
   FRAME fMain:WIDTH-CHARS = pdWidth.

  /* get rid off scroll-bar (especially for design time) */
  ASSIGN
    FRAME fMain:VIRTUAL-HEIGHT-PIXELS = FRAME fMain:HEIGHT-PIXELS
    FRAME fMain:VIRTUAL-WIDTH-PIXELS  = FRAME fMain:WIDTH-PIXELS
    FRAME ftv:VIRTUAL-HEIGHT-PIXELS   = FRAME ftv:HEIGHT-PIXELS
    gFtvVWP                           = MAX(gFtvWP,gFtvVWP)
    FRAME ftv:VIRTUAL-WIDTH-PIXELS    = gFtvVWP.

  /* if enlarging the TV vertically, then perhaps the scrollbar should
     disappear... */
  IF lfMainEnlargeVertical
   AND gVertScrollingActive THEN DO:
      IF gExpChildren <= gVisibleIterations THEN DO:
          FIND bnode WHERE bnode.par = 0 AND bnode.pre = 0 NO-ERROR.
          IF AVAIL bnode THEN gRefreshFromNode = bnode.id.
      END.

      /* else we probably need to scroll down */
      ELSE lScrollDown = YES.
  END.


  /* 09-oct-2004 Now we display a demo treeview at design time
  DEFINE VARIABLE cUIBMode AS CHARACTER  NO-UNDO.
  {get UIBMode cUIBMode}.
  IF cUIBMode <> "Design" THEN DO:*/
    RUN initializeTviter.
    IF CAN-FIND(FIRST node) THEN tvRefresh().
    IF lScrollDown THEN tvScroll(prevTvIterations - gTvIterations, NO).
  /*END.*/
/*
IF ghbsv:VISIBLE THEN DO:
    FRAME fsv:BGCOLOR = FRAME fsv:BGCOLOR.
END.
  */

/* 12-NOV-2006, if tv is empty, then hide the scrollbars in the tvframe*/
IF NOT CAN-FIND(FIRST node) AND FRAME ftv:HWND <> ?
 THEN RUN ShowScrollBar IN hppure4gltvApi (FRAME ftv:HWND, {&SB_BOTH},  0  {&APIRtnParmINT}).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sortChildren C-Win
PROCEDURE sortChildren :
/*------------------------------------------------------------------------------
  Purpose:     Sorts (or resorts) all the children of a parent by their label

  Parameters:  pcKe  key of the parent node

               pcOptn is an extendable comma separated list of option:
                   refresh  => call tvRefresh() at the end of the sorting process

------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER pcKe          LIKE node.ke   NO-UNDO.
DEFINE INPUT  PARAMETER pcOptn        AS CHARACTER   NO-UNDO.

DEF BUFFER bnode   FOR node.
DEF BUFFER bnode2  FOR node.

DEFINE VARIABLE par LIKE node.par NO-UNDO.
DEFINE VARIABLE pre LIKE node.pre NO-UNDO.


FIND bnode WHERE bnode.ke = pcKe NO-ERROR.
IF NOT AVAIL bnode THEN RETURN ERROR "sortChildren failed: Cannot find the parent node with key " + pcKe.
par = bnode.id.

/* first set temporary all pre and nex to ? to avoid unique index problems */
FOR EACH bnode WHERE bnode.par = par:
    ASSIGN
     bnode.pre = ?
     bnode.nex = ?.
END.

/* Now sort that */
FOR EACH bnode WHERE bnode.par = par
 BY bnode.lab BY bnode.ico BY bnode.id: /* when multiple node with same label, then sort on ico then on id */
    IF AVAIL bnode2 THEN bnode2.nex = bnode.id.
    ASSIGN
     bnode.pre = pre /* note it is equal to 0 on purpose at the first iteration */
     pre = bnode.id.
    FIND bnode2 WHERE bnode2.id = bnode.id.
END.
IF AVAIL bnode2 THEN bnode2.nex = 0.


IF LOOKUP("refresh", pcOptn , CHR(1)) <> 0 THEN tvRefresh().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE swapNodes C-Win
PROCEDURE swapNodes :
/*------------------------------------------------------------------------------
  Purpose: Swap a node with another.

           We do not touch the children nodes, which remain attached to their
           parent, so this API could also have been called swapBranches

  Parameters:  See definition block

  Notes:       It is more efficient to call this API than calling moveNode
               multiple times.
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER pcnodeKe1 LIKE node.ke NO-UNDO.
DEFINE INPUT  PARAMETER pcnodeKe2 LIKE node.ke NO-UNDO.
DEFINE INPUT  PARAMETER pcoptn    AS CHARACTER NO-UNDO. /* list of options like "refresh" */

DEF BUFFER bnode1      FOR node.
DEF BUFFER bnode2      FOR node.
DEF BUFFER swapnode1   FOR copyNode.
DEF BUFFER swapnode2   FOR copyNode.
DEF BUFFER parentNode  FOR node.
DEF BUFFER prenex      FOR node. /* to take care of prev/next-sibling*/
DEF BUFFER widestChild FOR node.
DEF BUFFER btviter FOR tviter.

DEFINE VARIABLE par LIKE node.par NO-UNDO.

IF pcnodeKe1 = pcnodeKe2 THEN RETURN ERROR "Cannot swap a node with itself (key is " + pcnodeKe1 + ")".
FIND bnode1 WHERE bnode1.ke = pcnodeKe1 NO-ERROR.
IF NOT AVAIL bnode1 THEN RETURN ERROR "Cannot find node1 with key " + pcnodeKe1.
FIND bnode2 WHERE bnode2.ke = pcnodeKe2 NO-ERROR.
IF NOT AVAIL bnode2 THEN RETURN ERROR "Cannot find node2 with key " + pcnodeKe2.

/* Walk to root to check that we do not try to swap a node with one of his parent
  (This does not make sense) */
FIND parentNode WHERE parentNode.id = bnode1.id.  /*we will actually go to parent level very soon*/
DO WHILE TRUE:
    IF parentNode.par = 0 THEN LEAVE.
    IF parentNode.par = bnode2.id THEN RETURN ERROR "Node1 with key " + pcnodeKe1 + " is a child of node2, which key is " + pcnodeKe2.
    par = parentNode.par.
    FIND parentNode WHERE parentNode.id = par.
END.

FIND parentNode WHERE parentNode.id = bnode2.id.  /*we will actually go to parent level very soon*/
DO WHILE TRUE:
    IF parentNode.par = 0 THEN LEAVE.
    IF parentNode.par = bnode1.id THEN RETURN ERROR "Node2 with key " + pcnodeKe2 + " is a child of node1, which key is " + pcnodeKe1.
    par = parentNode.par.
    FIND parentNode WHERE parentNode.id = par.
END.


/* keep a copy of the records and delete them to avoid index problems.. */
CREATE swapNode1.   BUFFER-COPY bnode1 TO swapNode1.  DELETE bnode1.
CREATE swapNode2.   BUFFER-COPY bnode2 TO swapNode2.  DELETE bnode2.

/* Update prev-sibling and next-sibling of bnode1, keep in mind the following:
   1) For unique index sake, we have to first set them to unknown
     => Think of the the case when we are swapping to brother nodes
   2) When swapping nodes that are pre and next sibling of each other, then
     few info have to remain unchanged.  It is a bit hard to explain, make
     a drawing with 3 brother nodes and consider their pre and nex */
IF   swapNode1.pre <> swapNode2.id
 AND swapNode1.pre <> 0 THEN DO:
    FIND prenex WHERE prenex.id = swapNode1.pre.
    prenex.nex = ?. /*see comment above*/
END.
IF   swapNode1.nex <> swapNode2.id
 AND swapNode1.nex <> 0 THEN DO:
    FIND prenex WHERE prenex.id = swapNode1.nex.
    prenex.pre = ?.  /*see comment above*/
END.
IF   swapNode2.pre <> swapNode1.id
 AND swapNode2.pre <> 0 THEN DO:
    FIND prenex WHERE prenex.id = swapNode2.pre.
    prenex.nex = ?.  /*see comment above*/
END.
IF   swapNode2.nex <> swapNode1.id
 AND swapNode2.nex <> 0 THEN DO:
    FIND prenex WHERE prenex.id = swapNode2.nex.
    prenex.pre = ?.  /*see comment above*/
END.


/* Recreate bnode1 and put swapNode2 in it, with few original info of swapNode1*/
CREATE bnode1.
BUFFER-COPY swapNode2 TO bnode1 ASSIGN
 bnode1.par   = swapNode1.par
 bnode1.level = swapNode1.level
 bnode1.pre   = /* Beware the case when swapping node that are pre and next sibling of each other */
  IF swapNode1.pre = swapNode2.id THEN swapNode1.id ELSE swapNode1.pre
 bnode1.nex = /* Beware the case when swapping node that are pre and next sibling of each other */
  IF swapNode1.nex = swapNode2.id THEN swapNode1.id ELSE swapNode1.nex
 bnode1.VWP = swapNode2.VWP + (swapNode1.level - swapNode2.level) * gTvLevelWidth
 bnode1.VWPexpChildren = swapNode2.VWPexpChildren + (swapNode1.level - swapNode2.level) * gTvLevelWidth
 bnode1.VWPcolChildren = swapNode2.VWPcolChildren + (swapNode1.level - swapNode2.level) * gTvLevelWidth.
/* Recreate bnode2 and put swapNode1 in it, with few original info of swapNode2*/
CREATE bnode2.
BUFFER-COPY swapNode1 TO bnode2 ASSIGN
 bnode2.par   = swapNode2.par
 bnode2.level = swapNode2.level
 bnode2.pre   = /* Beware the case when swapping node that are pre and next sibling of each other */
  IF swapNode2.pre = swapNode1.id THEN swapNode2.id ELSE swapNode2.pre
 bnode2.nex = /* Beware the case when swapping node that are pre and next sibling of each other */
  IF swapNode2.nex = swapNode1.id THEN swapNode2.id ELSE swapNode2.nex
 bnode2.VWP = swapNode1.VWP + (swapNode2.level - swapNode1.level) * gTvLevelWidth
 bnode2.VWPexpChildren = swapNode1.VWPexpChildren + (swapNode2.level - swapNode1.level) * gTvLevelWidth
 bnode2.VWPcolChildren = swapNode1.VWPcolChildren + (swapNode2.level - swapNode1.level) * gTvLevelWidth.

/* Update the level of children */
IF swapNode1.level <> swapNode2.level THEN DO:
    IF CAN-FIND(FIRST node WHERE node.par = swapNode1.id) THEN
     RUN AddNToLevelOfBranchOf (swapNode2.level - swapNode1.level , swapNode1.id).
    IF CAN-FIND(FIRST node WHERE node.par = swapNode2.id) THEN
     RUN AddNToLevelOfBranchOf (swapNode1.level - swapNode2.level , swapNode2.id).
END.


/* Now we can really update prev-sibling and next-sibling of bnode1
  In other words, there is no problem left with unique index on par+pre/nex now,
  and we can put the final values */
IF   swapNode1.pre <> swapNode2.id
 AND swapNode1.pre <> 0 THEN DO:
    FIND prenex WHERE prenex.id = swapNode1.pre.
    prenex.nex = swapNode2.id.
END.
IF   swapNode1.nex <> swapNode2.id
 AND swapNode1.nex <> 0 THEN DO:
    FIND prenex WHERE prenex.id = swapNode1.nex.
    prenex.pre = swapNode2.id.
END.
IF   swapNode2.pre <> swapNode1.id
 AND swapNode2.pre <> 0 THEN DO:
    FIND prenex WHERE prenex.id = swapNode2.pre.
    prenex.nex = swapNode1.id.
END.
IF   swapNode2.nex <> swapNode1.id
 AND swapNode2.nex <> 0 THEN DO:
    FIND prenex WHERE prenex.id = swapNode2.nex.
    prenex.pre = swapNode1.id.
END.


/* Update the parent about colChildren exp Children and all the VWP stuffs
 The algorithm has some similarities with the one of collapseNode or expandNode
   STARTING FROM THIS POINT bnode1 and bnode2 will hold different records by climbing up
 back to the root */
IF swapNode2.par <> swapNode1.par  /*no need to update the parents of the two swapped nodes share the same one */
 THEN DO:
    UpdateParerentsOfBnode1:
    DO WHILE TRUE:
        /* widestBrother is not AVAIL only when no brother */
        FIND LAST widestChild WHERE widestChild.par = bnode1.par USE-INDEX parVWPExpChildren.

        IF bnode1.par = 0 THEN DO:
            gExpChildren = gExpChildren + swapNode2.expChildren - swapNode1.expChildren.
            gVWP = widestChild.VWPexpChildren.
            LEAVE UpdateParerentsOfBnode1.
        END.

        FIND parentNode WHERE parentNode.id = bnode1.par.

        IF NOT parentNode.expanded THEN DO:
            ASSIGN
             parentNode.colChildren    = parentNode.colChildren + swapNode2.colChildren - swapNode1.colChildren.
             parentNode.VWPcolChildren = MAX(parentNode.VWP,widestChild.VWPexpChildren).
            LEAVE UpdateParerentsOfBnode1.
        END.
        ASSIGN
         parentNode.expChildren = parentNode.expChildren + swapNode2.expChildren - swapNode1.expChildren
         parentNode.VWPexpChildren = MAX(parentNode.VWP,widestChild.VWPexpChildren).
        FIND bnode1 WHERE bnode1.id = parentNode.id.
    END.
    UpdateParerentsOfBnode2:
    DO WHILE TRUE:
        /* widestBrother is not AVAIL only when no brother */
        FIND LAST widestChild WHERE widestChild.par = bnode2.par USE-INDEX parVWPExpChildren NO-ERROR.

        IF bnode2.par = 0 THEN DO:
            gExpChildren = gExpChildren + swapNode1.expChildren - swapNode2.expChildren.
            gVWP = widestChild.VWPexpChildren.
            LEAVE UpdateParerentsOfBnode2.
        END.

        FIND parentNode WHERE parentNode.id = bnode2.par.

        IF NOT parentNode.expanded THEN DO:
            ASSIGN
             parentNode.colChildren    = parentNode.colChildren + swapNode1.colChildren - swapNode2.colChildren.
             parentNode.VWPcolChildren = MAX(parentNode.VWP,widestChild.VWPexpChildren).
            LEAVE.
        END.
        ASSIGN
         parentNode.expChildren = parentNode.expChildren + swapNode1.expChildren - swapNode2.expChildren
         parentNode.VWPexpChildren = MAX(parentNode.VWP,widestChild.VWPexpChildren).
        FIND bnode2 WHERE bnode2.id = parentNode.id.
    END.
END.  /*no need to update the parents of the two swaped nodes share the same one */


/* The following RELEASE's are required because of some unique node INDEXes
   Not doing them disturbs the next call of tvRefresh() */
IF AVAIL bnode1 THEN RELEASE bnode1.
IF AVAIL bnode2 THEN RELEASE bnode2.
IF AVAIL parentNode THEN RELEASE parentNode.

IF CAN-DO(pcoptn, "refresh") THEN DO:
    /* Force a refresh from first iteration.
    If the node seen at the top is one of this node that has just been swapped,
    then anticipate the node that is going to be at the top.
    In the future, we shall provide another option in optn to refresh from
    on particular node, such as a swapped node*/
    FIND btviter WHERE btviter.iter = 1 NO-ERROR.
    gRefreshFromNode = IF AVAIL btviter
                        THEN (IF btviter.id = swapNode2.id
                               THEN swapNode1.id
                               ELSE IF btviter.id = swapNode1.id
                                     THEN swapNode2.id
                                     ELSE btviter.id)
                        ELSE 0. /* 0 mean tvRefresh will find top root node by itself */
    tvRefresh().
END.

DELETE swapNode1.  DELETE swapNode2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateNode C-Win
PROCEDURE updateNode :
/*------------------------------------------------------------------------------
   Purpose:  Update the label, or the icon, or a list of option(s) of a given node.

Parameters:          pcKe:  key of the node to update

             pcfieldNames: Comma separated list of fields
                        => for now the complete list is limited to "lab,ico"
                        the idea is to keep it flexible for possible future needs

             pcFieldValues: CHR(1) separated list of values for the fields listed
                           in pcfieldNames.

             pcOptn:  BEWARE, this one is not only about options for the API
                      itself, but especially to update the options of a node
                      in the node.optn field, as explained bellow:

             pcOptn contains a CHR(1) separated list of option to add, update
             or remove from the node.optn record to update.
               => If the option is not in node.optn yet, then add it

               => If the option is of the form 'name' + CHR(2) + 'value' and there
                is already an option with same 'name' + CHR(2) in node.optn,
                then update the value

               => If the option is in node.optn, but pcOptn has it prefixed with
                "!" (exclamation mark) then it is *removed* from node.optn
                     example "!private" will remove "private<CHR(2)>hello world"

              See the complete list of standard node options in the header
              comments of the addNode API

             If "refresh" is passed in the the option list, then a tvRefresh() is
             called at the end of the update process (I did not want to create yet
             another parameter just for this need...)
              => SO "refresh" IS AN EXCEPTION (don't use it as a custom node option)


    Notes:  Note that this API does not only modify node.lab and node.ico, but
            also takes care of the virtual width of the node to notify the
            treeview if it has to enlarge or shrink its virtual width
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER pcKe          LIKE node.ke   NO-UNDO.
DEFINE INPUT  PARAMETER pcFieldNames  LIKE node.lab  NO-UNDO.
DEFINE INPUT  PARAMETER pcFieldValues LIKE node.ico  NO-UNDO.
/* Beware, see note above */
DEFINE INPUT  PARAMETER pcOptn        AS CHARACTER   NO-UNDO.

DEF BUFFER nodeToUpdate  FOR node.

DEFINE VARIABLE icount         AS INTEGER    NO-UNDO.
DEFINE VARIABLE icount2        AS INTEGER    NO-UNDO.
DEFINE VARIABLE oneOptn        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE oneOptnName    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE newOneOptn     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE newOneOptnName AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFieldName     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFieldValue    AS CHARACTER  NO-UNDO.

/* 05-AUG-2008 sla: those 4 defs were missing */
DEFINE VARIABLE itvnodeFont    AS INTEGER INIT ?  NO-UNDO.
DEFINE VARIABLE iFGCol         AS INTEGER         NO-UNDO.
DEFINE VARIABLE iBGCol         AS INTEGER         NO-UNDO.
DEFINE VARIABLE cTooltip       AS CHARACTER       NO-UNDO.
/* end 05-AUG-2008 sla: those 4 defs were missing */


&SCOPED-DEFINE OptionsForUpdateNodeAPI "refresh"

FIND nodeToUpdate WHERE nodeToUpdate.ke = pcKe NO-ERROR.
IF NOT AVAIL nodeToUpdate THEN RETURN ERROR "updateNode failed: Cannot find node to update with key " + pcKe.


/* 05-AUG-2008 sla: the code to manage the new node options was missing  */
IF INDEX(pcOptn, "font=") > 0 THEN DO:
    itvnodeFont = getIntOptValue(pcOptn,"font").    /* May return ? */
    IF itvnodeFont = ? THEN DO:
        {get tvnodeDefaultFont itvnodeFont}.
    END.
    nodeToUpdate.nodeFont = itvnodeFont.
END.

IF INDEX(pcOptn, "fgcolor=") > 0 THEN DO:
    iFGCol = getIntOptValue(pcOptn,"fgcolor"). /* May return ? */
    nodeToUpdate.nodeFGCol = iFGCol.
END.

IF INDEX(pcOptn, "bgcolor=") > 0 THEN DO:
    iBGCol = getIntOptValue(pcOptn,"bgcolor"). /* May return ? */
    nodeToUpdate.nodeBGCol = iBGCol.
END.

IF INDEX(pcOptn, "tooltip=") > 0 THEN DO:
    cTooltip  = getCharOptValue(pcOptn,"tooltip").
    nodeToUpdate.nodeTooltip = cTooltip.
END.
/* end 05-AUG-2008 sla: the code to manage the new node options was missing  */


/* update field names */
DO iCount = NUM-ENTRIES(pcFieldNames) TO 1 BY -1:
    cFieldName = ENTRY(iCount,pcFieldNames).
    cFieldValue = ENTRY(iCount,pcFieldValues, CHR(1)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR
     "Error in updateNode.  No value is passed in pcFieldValues for fieldName" + cFieldName
      + "~npcFieldNames: " + pcFieldNames + "~npcFieldValues" + pcFieldValues
      + "~npcKe" + pcKe + "~nNote that pcFieldValues has to be a CHR(1) separated list".
    CASE cFieldName:
        WHEN "lab" THEN DO:
            nodeToUpdate.lab    = cFieldValue.
            RUN updateNodeWidth (nodeToUpdate.id).
        END.
        WHEN "ico" THEN nodeToUpdate.ico = cFieldValue.
        OTHERWISE RETURN ERROR "Error in updateNode.  fieldName " + cFieldName
         + " is not in the supported list of updatable fields 'lab,ico'"
         + "~npcFieldNames: " + pcFieldNames + "~npcFieldValues" + pcFieldValues
         + "~npcKe" + pcKe + "~nNote that pcFieldValues has to be a CHR(1) separated list".
    END CASE.
END.

/* update or remove the options in/from nodeToUpdate.optn*/
OneOptnInNodeRec:
DO iCount = NUM-ENTRIES(nodeToUpdate.optn,CHR(1)) TO 1 BY -1: /* go backward for performance sake (NUM-ENTRIES is evaluated only once)
                                                        and to be able to delete an option entry */
    oneOptn = ENTRY(iCount,nodeToUpdate.optn,CHR(1)).
    oneOptnName = IF NUM-ENTRIES(oneOptn,CHR(2)) = 1
                   THEN oneOptn ELSE ENTRY(1,oneOptn,CHR(2)).

    OneOptnInPcOptn:
    DO icount2 = NUM-ENTRIES(pcOptn,CHR(1)) TO 1 BY -1:
        newOneOptn = ENTRY(iCount2, pcOptn ,CHR(1)).
        IF LOOKUP(newOneOptn, {&OptionsForUpdateNodeAPI}) <> 0 THEN NEXT OneOptnInPcOptn. /* this one is for the updateNode API itself, ignore it */

        newOneOptnName = IF NUM-ENTRIES(newOneOptn, CHR(2)) = 1
                          THEN newOneOptn ELSE ENTRY(1,newOneOptn, CHR(2)).

        /* Option to remove ? */
        IF newOneOptnName BEGINS "!" THEN DO:
            IF SUBSTR(newOneOptnName,2) <> oneOptnName THEN NEXT OneOptnInPcOptn. /* Not the right time */

            /* Yes this Option has to be remove d
               the only way to remove an entry without leaving an empty one... */
            ASSIGN
             nodeToUpdate.optn = REPLACE(nodeToUpdate.optn, oneOptn + CHR(1)  , "")
             nodeToUpdate.optn = REPLACE(nodeToUpdate.optn, CHR(1) + oneOptn  , "")
             nodeToUpdate.optn = REPLACE(nodeToUpdate.optn, oneOptn           , "").
            NEXT OneOptnInNodeRec.
        END.

        /* option to update ? */
        ELSE DO:
            IF oneOptnName <> newOneOptnName THEN NEXT OneOptnInPcOptn. /* not the right time */

            /* Yes, option to update */
            ENTRY(icount,nodeToUpdate.optn,CHR(1)) = newOneOptn.
            NEXT OneOptnInNodeRec.
        END. /* option to update ? */
    END. /* OneOptnInPcOptn: */
END. /* OneOptnInNodeRec: */

/* consider the options to add to nodeToUpdate.optn */
OneOptnInPcOptnAdd:
DO icount2 = NUM-ENTRIES(pcOptn,CHR(1)) TO 1 BY -1:
    newOneOptn = ENTRY(iCount2, pcOptn ,CHR(1)).
    IF newOneOptn BEGINS "!" THEN NEXT OneOptnInPcOptnAdd. /*this one is an opion to remove*/
    IF LOOKUP(newOneOptn,{&OptionsForUpdateNodeAPI},CHR(1)) <> 0 THEN NEXT OneOptnInPcOptnAdd. /* this one is for the updateNode API itself, ignore it */

    newOneOptnName = IF NUM-ENTRIES(newOneOptn, CHR(2)) = 1
                      THEN newOneOptn ELSE ENTRY(1,newOneOptn, CHR(2)).

    DO iCount = NUM-ENTRIES(nodeToUpdate.optn,CHR(1)) TO 1 BY -1:
        oneOptn = ENTRY(iCount,nodeToUpdate.optn,CHR(1)).
        oneOptnName = IF NUM-ENTRIES(oneOptn,CHR(2)) = 1
                       THEN oneOptn ELSE ENTRY(1,oneOptn,CHR(2)).

        /* This option is already in node.optn, try the next */
        IF oneOptnName = newOneOptnName THEN NEXT OneOptnInPcOptnAdd.
    END.
    nodeToUpdate.optn = CHR(1) + newOneOptn + nodeToUpdate.optn.
END. /* OneOptnInPcOptnAdd */
IF nodeToUpdate.optn BEGINS CHR(1)
 THEN nodeToUpdate.optn = SUBSTR(nodeToUpdate.optn,2).


IF LOOKUP("refresh", pcOptn , CHR(1)) <> 0 THEN tvRefresh().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateNodeWidth C-Win
PROCEDURE updateNodeWidth :
/*------------------------------------------------------------------------------
  Purpose: Update the width of a node, by taking care of the virtual width of
   the viewport

  Parameters:  see definition block

  Notes: This procedure was designed to be called internally, but it should work
   OK when called form the outside, so I unchecked the PRIVATE option
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipNodeId LIKE node.id NO-UNDO.

DEF BUFFER nodeToUpdate  FOR node.
DEF BUFFER bnode         FOR node.
DEF BUFFER parentNode    FOR node.
DEF BUFFER widestChild   FOR node.

DEFINE VARIABLE itvnodeDefaultFont AS INTEGER         NO-UNDO.

FIND nodeToUpdate WHERE nodeToUpdate.id = ipNodeId NO-ERROR.
IF NOT AVAIL nodeToUpdate THEN RETURN ERROR "updateNodeWidth failed, Cannot find node to update with id " + STRING(ipNodeId).


{get tvnodeDefaultFont itvnodeDefaultFont}.
ASSIGN
 nodeToUpdate.labWidthPixels = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(nodeToUpdate.lab, itvnodeDefaultFont) + 4
 nodeToUpdate.VWP            = nodeToUpdate.labWidthPixels + nodeToUpdate.level * gTvLevelWidth + gTvPicWidth + 3. /* 3 = 2 + 1 (cosmetic) ,see how I work out picx and Labx in tvRefresh() */

FIND LAST widestChild WHERE widestChild.par = nodeToUpdate.id USE-INDEX parVWPexpChildren NO-ERROR.
IF AVAIL widestChild THEN DO:
    IF nodeToUpdate.expanded THEN
     ASSIGN
      nodeToUpdate.VWPexpChildren = MAX(nodeToUpdate.VWP,widestChild.VWPexpChildren)
      nodeToUpdate.VWPcolChildren = nodeToUpdate.VWP.
    ELSE ASSIGN
      nodeToUpdate.VWPcolChildren = MAX(nodeToUpdate.VWP,widestChild.VWPexpChildren)
      nodeToUpdate.VWPexpChildren = nodeToUpdate.VWP.
END.
ELSE ASSIGN
 nodeToUpdate.VWPexpChildren = nodeToUpdate.VWP
 nodeToUpdate.VWPcolChildren = nodeToUpdate.VWP.

/* Now update VWPcolChildren and VWPexpChildren of parents to handle virtual width */
FIND bnode WHERE bnode.id = nodeToUpdate.id.
DO WHILE TRUE:
    FIND LAST widestChild WHERE widestChild.par = bnode.par USE-INDEX parVWPexpChildren.

    IF bnode.par = 0 THEN DO:
         gVWP = widestChild.VWPexpChildren.
        LEAVE.
    END.

    FIND parentNode WHERE parentNode.id = bnode.par.

    IF NOT parentNode.expanded THEN DO:
        parentNode.VWPcolChildren =  widestChild.VWPexpChildren.
        LEAVE.
    END.

    parentNode.VWPexpChildren =  widestChild.VWPexpChildren.
    FIND bnode WHERE bnode.id = parentNode.id.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vertScrollFollowMouse C-Win
PROCEDURE vertScrollFollowMouse PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     To handle the thumb scroll button in the scrollbar.  This procedure
               runs with its own WAIT-FOR until the mouse button is released.  It
               monitors the mouse buttons.

  Parameters:  <none>
  Notes:
  Before, I was letting renderVertScrollBar() move the button bsv but it was
  making problems probably because of bug in the GUI layer...
  Indeed, sometimes the button was no moved to the expected place, but
  querying bsv:Y was reporting the value of that expected place!!!
  Anyway; this method has an advantage for performance.  Indeed, before I was
  determining the position of the button from the one of the mouse then the
  iteration-to-go-to from the position of the button (then node-to-go-to from
  that iteration-to-go-to...), and, at the end of tvrefresh(),
  renderVertScrollBar() was redetermining the position of the button from the
  node-we-just-went-to...
  So now, tvRefresh() will not call renderVertScrollBar if
  gDoNotRenderVerScrollBar is set
------------------------------------------------------------------------------*/

DEFINE VARIABLE mMousePos       AS MEMPTR     NO-UNDO.
DEFINE VARIABLE mKeyboardState  AS MEMPTR     NO-UNDO.
DEFINE VARIABLE xoffset         AS INTEGER    NO-UNDO.
DEFINE VARIABLE yoffset         AS INTEGER    NO-UNDO.
DEFINE VARIABLE mouseX          AS INTEGER    NO-UNDO.
DEFINE VARIABLE mouseY          AS INTEGER    NO-UNDO.
DEFINE VARIABLE cLastEvent      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE deltaScroll     AS INTEGER    NO-UNDO.
DEFINE VARIABLE virtualIterToGo AS INTEGER NO-UNDO.
DEFINE VARIABLE nodeToGo        LIKE node.id  NO-UNDO.
DEFINE VARIABLE prevNodeToGo    LIKE node.id  NO-UNDO.

/* for tracing/debugging */
DEFINE VARIABLE coffsets   AS CHARACTER  NO-UNDO.

IF gWindowsSkin <> "Classic" THEN ghbsv:LOAD-IMAGE(getPicturePath("scrollbuttonPressed.bmp")).

ASSIGN
 gDoNotRenderVerScrollBar = YES
 xoffset = LAST-EVENT:X - ghbsv:X
 yoffset = LAST-EVENT:Y - ghbsv:Y /* gap between original mouse-click:Y and top of button, shall remain the same*/
 coffsets = "x: " + STRING(xoffset) + "   y: " + STRING(yoffset).

SET-SIZE(mMousePos)      = 16.
SET-SIZE(mKeyboardState) = 256.

DO WHILE TRUE:
  WAIT-FOR 'LEFT-MOUSE-DOWN' OF FRAME fsv  PAUSE 0.
  IF LAST-EVENT:FUNCTION <> '' THEN cLastEvent = LAST-EVENT:FUNCTION.

  /* do we still keep the left mouse button pressed ? */
  RUN GetKeyboardState(GET-POINTER-VALUE(mKeyboardState) {&APIRtnParmINT}).
  IF GET-BITS(GET-BYTE(mKeyboardState,2) ,8, 1) = 0  THEN LEAVE. /* 2 is actually 1 + 1 (2nd byte but the first is coded as 0)*/

  /* track location of the mouse pointer */
  RUN GetCursorPos( INPUT-OUTPUT mMousePos).
  RUN ScreenToClient (INPUT FRAME fsv:HWND,
                      INPUT mMousePos ).
  ASSIGN
   mouseX = GET-LONG( mMousePos,1)
   mouseY = GET-LONG( mMousePos,5) - gFsvimgY.

  IF ABS(mouseX - ghbsv:X) < 150
   AND gBsvY <>  MIN(MAX(0,mouseY - yoffset), gFsvimgHP - gbsvHP) THEN DO:
      ASSIGN
       /*Beware gBsvY is NOT the real Y of the button because the image svimg is not at the top of fsv */
       gBsvY           =  MIN(MAX(0,mouseY - yoffset)
                                 ,gFsvimgHP - gbsvHP)
       ghbsv:Y         = gBsvY  + gFsvimgY /* need to add height of the scrollUpButton*/
       virtualIterToGo = DEC(gBsvY) * (gExpChildren - gVisibleIterations) / (gFsvimgHP - gBsvHP) + 1
       nodeToGo        = nodeAtVirtualIter(virtualIterToGo).

      IF VALID-HANDLE(ghbsvGraber)
       AND ghbsvGraber:Y <> gbsvY + (gbsvHP - {&bsvGraberHEIGHT-PIXELS}) / 2 + gFsvimgY
       THEN DO:
          ghbsvGraber:Y = gbsvY + (gbsvHP - {&bsvGraberHEIGHT-PIXELS}) / 2 + gFsvimgY.
      END.

      IF prevNodeToGo <> nodeToGo THEN DO:
          goToNode(nodeToGo, "top").
          prevNodeToGo = nodeToGo.
      END.
  END. /* ABS(mouseX - ghbsv:X) < 150 */
  IF LAST-EVENT:FUNCTION <> "" THEN LEAVE.
END.

SET-SIZE(mMousePos)      = 0.
SET-SIZE(mKeyboardState) = 0.

gDoNotRenderVerScrollBar = NO.

IF gWindowsSkin <> "Classic" THEN ghbsv:LOAD-IMAGE(getPicturePath("scrollbutton.bmp")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewObject C-Win
PROCEDURE viewObject :
/*------------------------------------------------------------------------------
  Purpose:     We have seen a case where a 2nd instance of pure4gltv (more
          than one treeview in the container) could end up with hidden frames
          after calling hideObject and viewObject.
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/


RUN SUPER.

  IF FRAME {&FRAME-NAME}:HIDDEN THEN
     FRAME {&FRAME-NAME}:HIDDEN = FALSE.

  edtSunken:MOVE-TO-TOP().
  FRAME ftv:HIDDEN = NO.
  FRAME ftv:MOVE-TO-TOP().
  IF gVertScrollingActive THEN DO:
      FRAME fsv:HIDDEN = NO.
      FRAME fsv:MOVE-TO-TOP().
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE widgetsAt C-Win
PROCEDURE widgetsAt :
/*------------------------------------------------------------------------------
  Purpose: Return the list of widget handles of the widget(s) that are located
  on a given X and Y

  Parameters:  see definition block

    Notes: This procedure was designed to be called internally, but it should work
   OK when called from the outside, so I unchecked the PRIVATE option
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER hFrame      AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER mouseX      AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER mouseY      AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER cwidgetList AS CHARACTER  NO-UNDO.

DEFINE VARIABLE hfg AS HANDLE     NO-UNDO. /*field group */
DEFINE VARIABLE hw  AS HANDLE     NO-UNDO. /* widget*/

hfg = hFrame:FIRST-CHILD. /* first field group */
hw  = hfg:FIRST-CHILD.
DO WHILE hw <> ?:
    IF   CAN-QUERY(hw, "X")
     AND CAN-QUERY(hw, "Y")
     AND CAN-QUERY(hw, "WIDTH-PIXELS")
     AND CAN-QUERY(hw, "HEIGHT-PIXELS")
     AND mouseX > hw:X
     AND mouseX < hw:X + hw:WIDTH-PIXELS
     AND mouseY > hw:Y
     AND mouseY < hw:Y + hw:HEIGHT-PIXELS
     THEN cWidgetList = cWidgetList + "," + STRING(hw).
    hw = hw:NEXT-SIBLING.
END.

/* remove leading comma */
IF cWidgetList <> "" THEN cWidgetList = SUBSTR(cWidgetList,2).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION collapseNode C-Win
FUNCTION collapseNode RETURNS LOGICAL
  (INPUT pcNodeKe AS CHAR
  ,INPUT pcOptn AS CHAR) :
/*------------------------------------------------------------------------------
    Purpose:  Collapse a given node

Parameters: pcNodeKe: key of the node to collapse

              pcOptn: extendable comma separated list of options:
                 refresh       Calls tvrefresh() at the end of the collapse Process

     Notes: This API returns No in the two following cases:
               -there is no node for the passed pcNodeKe
                 /* 05-JUL-2007 sla: review to remove errors returned by functions for compliance with 10.1C
                    => in this case, it no longer returns an ERROR, but just NO */

               -the node to collapse is already collapsed

            When it is a success, then it does the following:
              PUBLISH "tvNodeEvent" ("collapse", bnode.ke).
              RETURN YES.
------------------------------------------------------------------------------*/


DEF BUFFER bnode              FOR node.
DEF BUFFER parentNode         FOR node.
DEF BUFFER hiddenTviter       FOR tviter.
DEF BUFFER widestBrother      FOR node.

DEFINE VARIABLE iexpChildren   LIKE node.expChildren NO-UNDO.

FIND bnode WHERE bnode.ke = pcNodeKe NO-ERROR.
IF NOT AVAIL bnode THEN RETURN /* ERROR 05-JUL-2007 */ NO.

IF NOT bnode.expanded THEN RETURN NO. /* already collapsed, nothing to do */

ASSIGN
 bnode.expanded   = NO
/* update colChildren of parent if this parent is collapsed
   update expChildren of all parents ( parent of parent and so on) until we
   reach the top root node (then update gExpChildren variable) or a collapsed parent */
 iexpChildren         = bnode.expChildren
 bnode.colChildren    = iexpChildren
 bnode.expChildren    = 0
 bnode.VWPcolChildren = bnode.VWPexpChildren
 bnode.VWPexpChildren = bnode.VWP.

DO WHILE TRUE:
    /* widestBrother is not AVAIL only when at parent level... of course  */
    FIND LAST widestBrother WHERE widestBrother.par = bnode.par USE-INDEX parVWPExpChildren NO-ERROR.

    IF bnode.par = 0 THEN DO:
        gExpChildren = gExpChildren - iexpChildren.
        gVWP = MAX(bnode.VWP, IF AVAIL widestBrother THEN widestBrother.VWPexpChildren ELSE 0).
        LEAVE.
    END.

    FIND parentNode WHERE parentNode.id = bnode.par.

    IF NOT parentNode.expanded THEN DO:
        ASSIGN
         parentNode.colChildren    = parentNode.colChildren + iexpChildren
         parentNode.VWPcolChildren = MAX(parentNode.VWP, widestBrother.VWPexpChildren).
        LEAVE.
    END.
    ASSIGN
     parentNode.expChildren = parentNode.expChildren - iexpChildren
     parentNode.VWPexpChildren = MAX(parentNode.VWP, widestBrother.VWPexpChildren).
    FIND bnode WHERE bnode.id = parentNode.id.
END.

IF CAN-DO(pcOptn,"refresh") THEN DO:
    tvRefresh().

    FIND bnode WHERE bnode.ke = pcNodeKe.

    /*-------------- What to do with the previous selected node ? -----------------*/
    IF gCurNode <> 0
        /* If previously selected node was in the branch that has been collapsed
           then select this parent collapsed node */
     THEN IF nodeInCollapsedBranch(gCurNode)
      THEN selectNode(bnode.ke).
END.

PUBLISH "tvNodeEvent" ("collapse", bnode.ke).

RETURN YES.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createDynWidgetForVertScrollBar C-Win
FUNCTION createDynWidgetForVertScrollBar RETURNS LOGICAL PRIVATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
   DEFINE VARIABLE cVertSBWP AS CHARACTER  NO-UNDO.

   cVertSBWP = "pure4GlTvVertSrcollB" + STRING(THIS-PROCEDURE).

   DELETE WIDGET-POOL cVertSBWP NO-ERROR.
   CREATE WIDGET-POOL cVertSBWP PERSISTENT NO-ERROR.

   imgsv:LOAD-IMAGE(getPicturePath("Vscrollbg.bmp")) IN FRAME fsv.

  /* clone the buttons with flat option */

  /* scroll thumb button */
  CREATE IMAGE ghbsv IN WIDGET-POOL cVertSBWP ASSIGN
   NAME = "ghbsv (scroll thumb button)"
   FRAME = FRAME fsv:HANDLE
   X = Sbsv:X + 1
   Y = Sbsv:Y
   WIDTH-PIXELS = Sbsv:WIDTH-PIXELS
   HEIGHT-PIXELS = Sbsv:HEIGHT-PIXELS
   VISIBLE = YES
   SENSITIVE = YES
   TRIGGERS:
     ON 'MOUSE-SELECT-DOWN' PERSISTENT RUN vertScrollFollowMouse IN THIS-PROCEDURE.
   END TRIGGERS.
  ghbsv:LOAD-IMAGE(getPicturePath("scrollbutton.bmp")).

  ASSIGN
   Sbsv:VISIBLE = NO
   Sbsv:X = 0  /* move it there so it will not make problems... */
   Sbsv:Y = 0. /*...  when resizing FRAME fsv */

  /* show a grabber handle on the button */
  /* I first tried to handle that as an IMAGE, but I could not manage to
    to move it to top, it seems that the GUI wants sensitive widgets like
    buttons to remain at the top */
  CREATE IMAGE ghbsvGraber  IN WIDGET-POOL cVertSBWP ASSIGN
   NAME = "ghbsvGraber (grabber on scroll button when large enough)"
   FRAME = FRAME fsv:HANDLE
   X = Sbsv:X + 6 /* will remain the same forever*/
   Y = Sbsv:Y + 4 /* just to start, will change soon */
   WIDTH-PIXELS = 7
   HEIGHT-PIXELS = 8
   VISIBLE = YES
   SENSITIVE = YES
   TRIGGERS:
     ON 'MOUSE-SELECT-DOWN' PERSISTENT RUN vertScrollFollowMouse IN THIS-PROCEDURE.
   END TRIGGERS.
  ghbsvGraber:MOVE-TO-TOP().
  ghbsvGraber:LOAD-IMAGE(getPicturePath("scrollButtonGraber.bmp")).


  /* scroll up button */
  CREATE IMAGE ghbtnScrollUp  IN WIDGET-POOL cVertSBWP ASSIGN
   NAME = "ghbtnScrollUp (scroll up button)"
   FRAME = FRAME fsv:HANDLE
   X = SbtnScrollUp:X
   Y = SbtnScrollUp:Y
   WIDTH-PIXELS = SbtnScrollUp:WIDTH-PIXELS
   HEIGHT-PIXELS = SbtnScrollUp:HEIGHT-PIXELS
   VISIBLE = YES
   SENSITIVE = YES
   TRIGGERS:
     ON 'MOUSE-SELECT-DOWN' PERSISTENT RUN MouseSelectDownBtnScrollUp IN THIS-PROCEDURE.
   END TRIGGERS.

  ghbtnScrollUp:LOAD-IMAGE(getPicturePath("scrollButtonUp.bmp")).

  ASSIGN
   SbtnScrollUp:VISIBLE = NO
   SbtnScrollUp:X = 0 + 1 /* move it there so it will not make problems... */
   SbtnScrollUp:Y = 0. /*...  when resizing FRAME fsv */

  /* scroll up button */
  CREATE IMAGE ghbtnScrollDown  IN WIDGET-POOL cVertSBWP ASSIGN
   NAME = "ghbtnScrollDown (scroll up button)"
   FRAME = FRAME fsv:HANDLE
   X = SbtnScrollDown:X
   Y = SbtnScrollDown:Y
   WIDTH-PIXELS = SbtnScrollDown:WIDTH-PIXELS
   HEIGHT-PIXELS = SbtnScrollDown:HEIGHT-PIXELS
   VISIBLE = YES
   SENSITIVE = YES
     TRIGGERS:
     ON 'MOUSE-SELECT-DOWN' PERSISTENT RUN MouseSelectDownBtnScrollDown IN THIS-PROCEDURE.
   END TRIGGERS.
  ghbtnScrollDown:LOAD-IMAGE(getPicturePath("scrollButtonDo.bmp")).

  ASSIGN
   SbtnScrollDown:VISIBLE = NO
   SbtnScrollDown:X = 0 + 1  /* move it there so it will not make problems... */
   SbtnScrollDown:Y = 0. /*...  when resizing FRAME fsv */


  RETURN YES.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION deleteBranchOf C-Win
FUNCTION deleteBranchOf RETURNS LOGICAL PRIVATE
  (nodeId AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  Delete all the children nodes (and their children sub-branches...) of
           nodeId, and the node of that nodeId
    Notes: We have to use recursive programming...
           This function does not update colChildren and expChildren for performance sake
           This is done in a more efficient way in the procedure deleteNode
------------------------------------------------------------------------------*/
  DEF BUFFER bnode FOR node.


  FOR EACH bnode WHERE bnode.par = nodeId:
      deleteBranchOf(bnode.id).
  END.

  FIND bnode WHERE bnode.id = nodeId.
  DELETE bnode.

  RETURN YES.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION deselectCurrentNode C-Win
FUNCTION deselectCurrentNode RETURNS LOG:
/*------------------------------------------------------------------------------
  Purpose:  Deselect the node that is currently selected

    Notes:  This API always returns TRUE and sets the global variable gCurNode to 0

    When there is really a node to deselect, then it does:
     PUBLISH "tvNodeEvent" ("deselect", bnode.ke).
------------------------------------------------------------------------------*/
    DEF BUFFER bnode FOR node.

    IF gCurNode = 0 THEN RETURN NO. /* nothing to do */
    deselectCurrentNodeLabel().

    FIND bnode WHERE bnode.id = gCurNode NO-ERROR.
    /* not avail if the current node has been deleted */
    IF AVAIL bnode
    THEN PUBLISH "tvNodeEvent" ("deselect", bnode.ke).

    gCurNode = 0.
    RETURN TRUE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION deselectCurrentNodeLabel C-Win
FUNCTION deselectCurrentNodeLabel RETURNS LOGICAL PRIVATE
  () :
/*------------------------------------------------------------------------------
  Purpose:  deselect the label of the current selected node
    Notes:
------------------------------------------------------------------------------*/
   DEFINE BUFFER tviter FOR tviter. /* SLP 17-OCT-2007 */

  IF NOT VALID-HANDLE(gCurhLab) THEN RETURN FALSE.

  /* SLP 17-OCT-2007 - Start */
  FIND tviter WHERE tviter.hlab = gCurhLab NO-ERROR. /* SLP 30-OCT-2007 */

  IF AVAIL tviter
  THEN ASSIGN
   gCurhLab:BGCOLOR = tviter.nodeBGCol
   gCurhLab:FGCOLOR = tviter.nodeFGCol
   gCurhLab         = ?.

  ELSE /* SLP 17-OCT-2007 - End */
       ASSIGN
   gCurhLab:BGCOLOR = FRAME ftv:BGCOLOR
   gCurhLab:FGCOLOR = FRAME ftv:FGCOLOR
   gCurhLab         = ?.

  RETURN TRUE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION expandNode C-Win
FUNCTION expandNode RETURNS LOGICAL
  (INPUT pcNodeKe AS CHAR
  ,INPUT pcOptn   AS CHAR  /*list of option like "refresh" */) :
/*------------------------------------------------------------------------------
   Purpose:  Expand a given node

Parameters: pcNodeKe: key of the node to expand

              pcOptn: extendable comma separated list of options:
                 refresh       Calls tvrefresh() at the end of the expand Process

     Notes: This API returns No in the two following cases:
               -there is no node for the passed pcNodeKe
                /* 05-JUL-2007 sla: review to remove errors returned by functions for compliance with 10.1C
                    => in this case, it no longer returns an ERROR, but just NO */
               -the node to expand is already expanded

            When it is a success, then it does the following:
              PUBLISH "tvNodeEvent" ("expand", bnode.ke).
              RETURN YES.
------------------------------------------------------------------------------*/


DEF BUFFER nodeToExpand      FOR node.
DEF BUFFER bnode             FOR node.
DEF BUFFER parentNode        FOR node.
DEF BUFFER curSelectedTviter FOR tviter.
DEF BUFFER tviterToExpand    FOR tviter.

DEFINE VARIABLE icolChildren LIKE node.colChildren NO-UNDO.

FIND nodeToExpand WHERE nodeToExpand.ke = pcNodeKe NO-ERROR.
IF NOT AVAIL nodeToExpand THEN RETURN /*ERROR 05-JUL-2007*/ NO.

FIND tviterToExpand WHERE tviterToExpand.id = nodeToExpand.id NO-ERROR. /* no avail if outside of view port ? */

/* already expanded */
IF nodeToExpand.expanded THEN RETURN NO.

IF gCurNode <> 0 THEN
 FIND curSelectedTviter WHERE curSelectedTviter.id = gCurNode NO-ERROR.

nodeToExpand.expanded = YES.

/* update colChildren of parent if this parent collapsed
  update expChildren of all parents ( parent of parent and so on)
  until we reach a root node (then update gExpChildren variable)
  or a collapsed parent */
ASSIGN
 icolChildren                = nodeToExpand.colChildren
 nodeToExpand.expChildren    = icolChildren
 nodeToExpand.colChildren    = 0
 nodeToExpand.VWPexpChildren = nodeToExpand.VWPcolChildren.

FIND bnode WHERE bnode.ke = pcNodeKe.
DO WHILE TRUE:
    IF bnode.par = 0 THEN DO:
        ASSIGN
         gExpChildren = gExpChildren + icolChildren
         gVWP = MAX(gVWP, bnode.VWPexpChildren).
        LEAVE.
    END.

    FIND parentNode WHERE parentNode.id = bnode.par.

    IF NOT parentNode.expanded THEN DO:
        ASSIGN
         parentNode.colChildren    = parentNode.colChildren + icolChildren
         parentNode.VWPcolChildren = MAX(parentNode.VWPcolChildren,bnode.VWPexpChildren).
        LEAVE.
    END.
    ASSIGN
     parentNode.expChildren    = parentNode.expChildren + icolChildren
     parentNode.VWPexpChildren = MAX(parentNode.VWPexpChildren,bnode.VWPexpChildren).
    FIND bnode WHERE bnode.id = parentNode.id.
END.


IF CAN-DO(pcOptn,"refresh") THEN DO:
    /* To better manage a very special case: the node to expand is at the bottom of the
    view port while the Horizontal scrolling is NOT active. The expansion of the node brings
    some wide children node into view, which activates the Horizontal scrollbar, which leads
    to shrink the number of visible iteration by one, which move the expanded node bellow
    the view port ! (amazing isn't it.  The little trouble is the cosmetic part bellow
    will miss this case and the whole expanded  branch will remain out side of the view port
      We keep track of this case to work it around after the refresh if it happens ;) */
    DEFINE VARIABLE nodeToExpandWasAtbottom AS LOGICAL    NO-UNDO.
    IF NOT gHorzScrollingActive THEN DO:
        FIND tviterToExpand WHERE tviterToExpand.id = nodeToExpand.id NO-ERROR. /* no avail if outside of view port.*/
        nodeToExpandWasAtbottom = AVAIL tviterToExpand AND tviterToExpand.iter = gTvIterations.
    END.

    tvRefresh().

    /*------------------------------------------------------------------------------
     Cosmetic: if the newly expanded branch is outside of the viewport
     (or even just partially outside of it) then scroll up to show it better
    -------------------------------------------------------------------------------*/
    FIND tviterToExpand WHERE tviterToExpand.id = nodeToExpand.id NO-ERROR. /* no avail if outside of view port.*/
    /* IF NOT AVAIL, then perhaps we may want to go to ipNodeId...  @@@ToDoLater@@@ */
    IF AVAIL tviterToExpand
     AND tviterToExpand.iter <> 1 /*Well, if expanded node is a top, then we can't do better */
     AND nodeToExpand.expChildren > gVisibleIterations - tviterToExpand.iter
     THEN tvScroll(MIN(nodeToExpand.expChildren + tviterToExpand.iter - gVisibleIterations /* What we want to scroll up...*/
                      ,tviterToExpand.iter - 1) /*... but we cannot scroll more than that!*/
                   ,YES).
    /* see large comment above */
    ELSE IF nodeToExpandWasAtbottom AND gHorzScrollingActive
     THEN tvScroll(MIN(nodeToExpand.expChildren + 1 /* What we want to scroll up...*/
                      ,gTvIterations) /*... but we cannot scroll more than that!*/
                   ,YES).
END.

PUBLISH "tvNodeEvent" ("expand", nodeToExpand.ke).

RETURN YES.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAutoSort C-Win
FUNCTION getAutoSort RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lautoSort AS LOGICAL  NO-UNDO.
  {get autoSort lautoSort}.

  RETURN lautoSort.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBsvY C-Win
FUNCTION getBsvY RETURNS CHARACTER PRIVATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN STRING(gBsvY).   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCharOptValue C-Win
FUNCTION getCharOptValue RETURNS CHARACTER
  (INPUT pcOptList AS CHARACTER
  ,INPUT pcOption AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Extract the value from an option-value pair and return it.
    Notes:  Return "" if the option does not exist.

    SLP: 30-OCT-2007 - New function.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cAns AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE iIdx AS INTEGER       NO-UNDO.

  ASSIGN
   pcOptList = CHR(1) + pcOptList
   iIdx      = INDEX(pcOptList, CHR(1) + pcOption + "=").
  IF iIdx > 0 THEN ASSIGN
   pcOptList = SUBSTR(pcOptList, iIdx + 2 + LENGTH(pcOption)) /* Cut of leading options part */
   cAns      = ENTRY(1, pcOptList, CHR(1)). /* Drop the trailing options */
   /* 28-NOV-2007 sla: the above could not fail
    NO-ERROR. /* Just in case there is no second entry */  */
  RETURN cAns.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDragSource C-Win
FUNCTION getDragSource RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cDragSource AS CHARACTER  NO-UNDO.
  {get DragSource cDragSource}.

  RETURN cDragSource.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFocSelNodeBgColor C-Win
FUNCTION getFocSelNodeBgColor RETURNS INTEGER:
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iFocSelNodeBgColor AS INT NO-UNDO.

  {get FocSelNodeBgColor iFocSelNodeBgColor}.

  RETURN iFocSelNodeBgColor.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFocSelNodeFgColor C-Win
FUNCTION getFocSelNodeFgColor RETURNS INTEGER:
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iFocSelNodeFgColor AS INT NO-UNDO.

  {get FocSelNodeFgColor iFocSelNodeFgColor}.

  RETURN iFocSelNodeFgColor.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIntOptValue C-Win
FUNCTION getIntOptValue RETURNS INTEGER
  (INPUT pcOptList AS CHARACTER
  ,INPUT pcOption  AS CHARACTER):
/*------------------------------------------------------------------------------
  Purpose:  Extract the value from an option-value pair and return it as an integer
    Notes:  Return ? if the option does not exist or the value is not numeric.

    SLP: 17-OCT-2007 - New function.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iAns AS INTEGER INIT ? NO-UNDO.
  DEFINE VARIABLE iIdx  AS INTEGER       NO-UNDO.

  ASSIGN
   pcOptList = CHR(1) + pcOptList /* 28-NOV-2007 sla: the idea is to make sure we do not detect pcOption in the value of another option by searching CHR(1) + pcOption */
   iIdx      = INDEX(pcOptList, CHR(1) + pcOption + "=").
  IF iIdx > 0 THEN ASSIGN
   pcOptList = SUBSTR(pcOptList, iIdx + 1) /* Cut of leading options part */
   pcOptList = ENTRY(1, pcOptList, CHR(1)) /* Drop the trailing options */
   iAns      = INT(ENTRY(2, pcOptList, "="))
   NO-ERROR. /* Just in case the second enrty is not an integer */

  RETURN iAns.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLabCacheCoef C-Win
FUNCTION getLabCacheCoef RETURNS DECIMAL:
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN gLabCacheCoef.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMSkeyScrollForcePaint C-Win
FUNCTION getMSkeyScrollForcePaint RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lMSkeyScrollForcePaint AS LOGICAL  NO-UNDO.
  {get MSkeyScrollForcePaint lMSkeyScrollForcePaint}.

  RETURN lMSkeyScrollForcePaint.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNodeId C-Win
FUNCTION getNodeId RETURNS INTEGER
  (pcKe AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  Returns node.id of node record that has node.ke = pcKe
    Notes:  Returns 0 if no such node
------------------------------------------------------------------------------*/
  DEF BUFFER bnode FOR node.

  FIND bnode WHERE bnode.ke = pcKe NO-ERROR.
  IF AVAIL bnode THEN RETURN bnode.id.

  RETURN 0.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNodeKey C-Win
FUNCTION getNodeKey RETURNS CHAR
  (piId AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  Returns the node key node.ke of node record that has node.id = piId

    Notes:  Returns "" if no such node.
            Note that a node cannot have a blank key, even when a blank node key
            was passed to addNode() to create a node, because addNode then generates
            a key based on the unique id of the node (see code of addNode)
------------------------------------------------------------------------------*/
  DEF BUFFER bnode FOR node.

  FIND bnode WHERE bnode.id = piId NO-ERROR.

  IF AVAIL bnode THEN RETURN bnode.ke.

  RETURN "".   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNodeLocatedAtXY C-Win
FUNCTION getNodeLocatedAtXY RETURNS CHARACTER
  (ipX AS INT
  ,ipY AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  This API returns the node key of the node that is located on a given
           (x,y) in the viewport (FRAME ftv)
           This API is useful to achieve a move node by drag and drop (node dropped
           in the treeview)

    Notes: If there is no node located at the given (x,y) then an empty string is
           returned
------------------------------------------------------------------------------*/

  DEF BUFFER btviter FOR tviter.
  DEF BUFFER bnode   FOR node.

  FOR EACH btviter WHERE btviter.id <> ?:
      IF ipX < btviter.picX THEN NEXT.
      IF ipX > btviter.labX + btviter.labWidthPixels THEN NEXT.
      IF ipY < btviter.LabY THEN NEXT.
      IF ipY > btviter.LabY + gTvIterationHeight THEN NEXT.

      /* if we reach this point, then it means we have found the one*/
      FIND bnode WHERE bnode.id = btviter.id NO-ERROR. /* should always be available */
      IF AVAIL bnode THEN RETURN bnode.ke.
  END.

  RETURN "".   /* return blank if no node available */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNodeParentKey C-Win
FUNCTION getNodeParentKey RETURNS CHAR
  (pcKe AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  Returns key of parent node
    Notes:  Returns blank if the node has no parent (first level in the tree)

         /* 05-JUL-2007 sla: review to remove errors returned by functions for compliance with 10.1C
            => it no longer returns an ERROR if the node does not exist, but just ? */
------------------------------------------------------------------------------*/
  DEF BUFFER bnode FOR node.
  DEF BUFFER parentNode FOR node.


  FIND bnode WHERE bnode.ke = pcKe NO-ERROR.
  IF NOT AVAIL bnode THEN RETURN ?.

  IF bnode.par = 0 THEN RETURN "".
  FIND parentNode WHERE parentNode.id = bnode.par. /* We do not try to handle a corruption like deleted
                                                      parent because this should never happen */

  RETURN parentNode.ke.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPicCacheCoef C-Win
FUNCTION getPicCacheCoef RETURNS DECIMAL:
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN gpicCacheCoef.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPicturePath C-Win
FUNCTION getPicturePath RETURNS CHARACTER
  ( ic-Pic-Type     AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
    XPscrollButton.bmp
    XPscrollButtonDo.bmp
    XPscrollButtonDoPressed.bmp
    XPscrollButtonGraber.bmp
    XPscrollButtonPressed.bmp
    XPscrollButtonUp.bmp
    XPscrollButtonUpPressed.bmp
    XPvscrollbg.bmp
------------------------------------------------------------------------------*/
DEFINE VARIABLE cPicture-Folder AS CHARACTER  NO-UNDO.
ASSIGN
    cPicture-Folder = "{&tvskinpath}".

RETURN cPicture-Folder + gWindowsSkin + "/" + ic-Pic-Type.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSelectedNodeKey C-Win
FUNCTION getSelectedNodeKey RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  returns the node key of the current selected node (or last
            selected node)
    Notes:  If there is no selected node, then we return unknonw value.

------------------------------------------------------------------------------*/
  IF gCurNode = 0 THEN RETURN ?.

  DEF BUFFER bnode FOR node.
  FIND bnode WHERE bnode.id = gCurNode NO-ERROR.

  IF AVAIL bnode THEN RETURN bnode.ke.


  RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTreeStyle C-Win
FUNCTION getTreeStyle RETURNS INTEGER:
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iTreeStyle AS INT NO-UNDO.

  {get TreeStyle iTreeStyle}.

  RETURN iTreeStyle.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION gettvIterationHeight C-Win
FUNCTION gettvIterationHeight RETURNS INTEGER:
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN gTvIterationHeight.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION gettvnodeDefaultFont C-Win
FUNCTION gettvnodeDefaultFont RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:  For now, the same font is used for all the nodes.  We could rather
            easily handle it as a *default* font that could be overridden for a
            given node (as an option in the extendable param option list of addNode).
            To achieve that, we have two options:

             1) The hard way: to handle the font info in the tvLab temp-table and
            perhaps handle that in the rendering optimization procedures, especially
            if we find out that changing the font dynamically cost a non negligible
            time compared to changing the X, Y VISIBLE or SCREEN-VALUE attributes.

             2) State that changing fonts is rather rare and has a rather negligible
             impact on performance.  So we could actually provide a hook to refine
             a lab widget, like a refineLabelProcedure node option in the option list
             parameter of addNode.  (a RUN NO-ERROR in a super proc for example)

            Anyway, I prefer to anticipate this need and I therefore call this
            property tvnodeDefaultFont instead of tvnodeFont
-----------------------------------------------------------------------------*/

  DEFINE VARIABLE itvnodeDefaultFont AS INTEGER    NO-UNDO.

  {get tvnodeDefaultFont itvnodeDefaultFont}.

  RETURN itvnodeDefaultFont.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUnfSelNodeBgColor C-Win
FUNCTION getUnfSelNodeBgColor RETURNS INTEGER:
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iUnfSelNodeBgColor AS INT NO-UNDO.
  {get UnfSelNodeBgColor iUnfSelNodeBgColor}.

  RETURN iUnfSelNodeBgColor.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUnfSelNodeFgColor C-Win
FUNCTION getUnfSelNodeFgColor RETURNS INTEGER:
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iUnfSelNodeFgColor AS INT NO-UNDO.
  {get UnfSelNodeFgColor iUnfSelNodeFgColor}.

  RETURN iUnfSelNodeFgColor.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getwindowsSkin C-Win
FUNCTION getwindowsSkin RETURNS CHARACTER:
/*------------------------------------------------------------------------------
  Purpose:
    Notes:  We do not return gwindowsSkin because it can be different than
    the windowsSkin property when the property is set to 'Automatic'
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cwindowsSkin AS CHARACTER  NO-UNDO.

  {get windowsSkin cwindowsSkin}.

  RETURN cwindowsSkin.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWineMode C-Win
FUNCTION getWineMode RETURNS CHARACTER:
/*------------------------------------------------------------------------------
  Purpose:
    Notes:  We do not return glWineMode because it can be different than
    the WineMode property when the property is set to Automatic
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cWineMode AS CHARACTER    NO-UNDO.
  {get WineMode cWineMode}.

  RETURN cWineMode.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION goToNode C-Win
FUNCTION goToNode RETURNS LOGICAL
  ( ipGoToNode AS INT,
    pcMode AS CHAR ) :
/*------------------------------------------------------------------------------
     Purpose:  Go to a given node.

  Parameters:

      ipGoToNode  node ID we want to go to.

      pcMode:
      - blank (default), will do nothing if the node is already in the viewport.
      - "top" then will force a refresh to put the node a the top of the viewport

    Notes:  We do not test if the node is valid for performance reasons. Up to
            you to call this API for a node that exists
------------------------------------------------------------------------------*/

DEF BUFFER btviter FOR tviter.

/* node is already there, do not scroll */
FIND FIRST btviter WHERE btviter.id = ipGoToNode NO-ERROR.
IF AVAIL btviter AND
 (pcMode <> "top" /*having ipGoToNode in the view port is not enough if pcMode = top...*/
   OR btviter.iter = 1 /*.. but there is nothing to do if ipGoToNode is already at top of view port*/)
 THEN RETURN YES.

IF nodeInCollapsedBranch(ipGoToNode) THEN RETURN NO.

gRefreshFromNode = ipGoToNode.
tvRefresh().

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION greyCurrentSelectedNodeLabel C-Win
FUNCTION greyCurrentSelectedNodeLabel RETURNS LOGICAL PRIVATE
  () :
/*------------------------------------------------------------------------------
  Purpose:  Change the selection bgcolor and fgcolor of the current selected to
            show that the focus has been moved outside of the treeview
    Notes:  These colors are handled as instance properties
------------------------------------------------------------------------------*/

  DEFINE VARIABLE iUnfSelNodeBgColor AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iUnfSelNodeFgColor AS INTEGER    NO-UNDO.

  {get UnfSelNodeBgColor iUnfSelNodeBgColor}.
  {get UnfSelNodeFgColor iUnfSelNodeFgColor}.

  IF NOT VALID-HANDLE(gCurhLab) THEN RETURN FALSE.

  ASSIGN
   gCurhLab:BGCOLOR = iUnfSelNodeBgColor
   gCurhLab:FGCOLOR = iUnfSelNodeFgColor.

  RETURN TRUE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION lastVisibleNode C-Win
FUNCTION lastVisibleNode RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Return node.id of last 'visible' node (available in last non collapsed segment)

    Notes:  Returns 0 if tree is empty

    We start from top level and seek last child of last child of last child...
    and stop if we find a non expanded guy on the way
------------------------------------------------------------------------------*/
  DEF BUFFER bnode    FOR node.
  DEF BUFFER nextNode FOR node.

  FIND bnode WHERE bnode.par = 0 AND bnode.nex = 0 NO-ERROR.
  IF AVAIL bnode THEN DO WHILE TRUE:
      /* guy is collapsed, he is then the last available node at the bottom */
      IF NOT bnode.expanded THEN RETURN bnode.id.

      /* search the last child of ths guy */
      FIND nextNode WHERE nextNode.par = bnode.id
       AND nextNode.nex = 0 NO-ERROR.
      IF NOT AVAIL nextNode THEN RETURN bnode.id.

      FIND bnode WHERE bnode.id = nextNode.id.
  END.

  RETURN 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LockfMain C-Win
FUNCTION LockfMain RETURNS LOGICAL PRIVATE
  (lockIt AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE iReturnCode AS INTEGER    NO-UNDO.

  IF lockIt THEN RUN lockWindowUpdate  (INPUT FRAME fmain:HWND, OUTPUT iReturnCode).
  ELSE RUN lockWindowUpdate (INPUT 0, OUTPUT iReturnCode).

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MSkeyScrollForcePaint C-Win
FUNCTION MSkeyScrollForcePaint RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Force a repaint of the dynamic text labels when scrolling by keeping
  a navigation key (like cursor down/up or page up/dow)
  Note this problem occurs only when pressing the keyboard and not when maintaining
  the arrow up/down buttons in the scrollbar with the mouse.  Somehow, the 4GL
  has a strange way to optimize the painting of widget when the keyboard is
  pressed, which we shall not try to explain but rather just work around.

  This API will be used only if not running with wineMode set (so not on Linux)
  and if the instance property MSkeyScrollForcePaint is set
------------------------------------------------------------------------------*/
/*  this solution produces little expected flickering
  DEF BUFFER btvLab FOR tvLab.
  FOR EACH btvLab WHERE btvLab.labVisible:
      btvLab.hlab:PARENT:VISIBLE = NO.
      btvLab.hlab:PARENT:VISIBLE = YES.
  END.      */

  /* This solution is simple, does not produce any flickering... just smarter ;) */
  FRAME fTV:FGCOLOR = FRAME fTV:FGCOLOR.

  RETURN YES.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION nodeAtVirtualIter C-Win
FUNCTION nodeAtVirtualIter RETURNS INTEGER
  (VIterToGo AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  Returns the node ID of the node located at a given virtual iteration.
            This is quite important to handle scrolling.

            This API returns a node ID and not a node key because it was designed
            to be mainly used internally.  Anyway, it can be used from the outside
            without any problem

            What I call a virtual iteration is the 'line' number of a node if the
            Treeview was big enough to show all the nodes in a very tall viewport
            with first top root node at line number 1.

    Notes: Exceptions:
      Returns 0 when no node in the tree
      Returns -1 when VIterToGo > gExpChildren
      Returns bellow -1 when unexpected error that should never happen since it
       would mean the tree is corrupted (see in the code)
------------------------------------------------------------------------------*/
  DEFINE VARIABLE goToNode LIKE node.id    NO-UNDO.

  DEF BUFFER bnode   FOR node.
  DEF BUFFER tryNode FOR node.

  DEFINE VARIABLE Viter LIKE VIterToGo NO-UNDO.

  IF VIterToGo > gExpChildren THEN RETURN -1.
  IF NOT CAN-FIND(FIRST node) THEN RETURN 0. /* what can we do when no node in the tree ? */

  /* Start searching from top to bottom*/
  VIter = 0.
  FIND bnode WHERE bnode.par = 0 AND bnode.pre = 0. /*top root node*/

  FIND trynode WHERE trynode.id = bnode.id.

  DO WHILE TRUE:
      IF 1 + VIter = VIterToGo THEN RETURN tryNode.id.

      /* The node we want is located beyond the branch of tryNode.id */
      IF 1 + tryNode.expChildren + VIter < VIterToGo THEN DO:
          VIter = VIter + 1 + tryNode.expChildren.
          FIND bnode WHERE bnode.id = tryNode.id.
          FIND tryNode WHERE tryNode.id = bnode.nex NO-ERROR. /* find next brother */
          IF NOT AVAIL tryNode THEN RETURN -2.  /* the tree is broken.  This should never happen */
          NEXT.
      END.

      /* The node we want is located in the branch of tryNode.id  */
      VIter = VIter + 1.
      FIND bnode WHERE bnode.id = tryNode.id.
      FIND tryNode WHERE tryNode.par = bnode.id
                     AND tryNode.pre = 0 NO-ERROR. /* find first child */
      IF NOT AVAIL tryNode THEN RETURN -3.  /* the tree is broken.  This should never happen */
      NEXT.
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION nodeInCollapsedBranch C-Win
FUNCTION nodeInCollapsedBranch RETURNS LOGICAL
  (ipNode AS INT):
/*------------------------------------------------------------------------------
  Purpose:  Retuns yes if a given node (based on node ID) is in a collapsed
            branch, else no
    Notes:  In the case of an invalid node, it RETURNS NO
------------------------------------------------------------------------------*/

  DEF BUFFER bnode FOR node.
  DEFINE VARIABLE parNode LIKE node.par NO-UNDO.

  FIND bnode WHERE bnode.id = ipNode NO-ERROR.
  IF NOT AVAIL bnode THEN RETURN /* ERROR 05-JUL-2007 */  NO. /*can happen when the node has been deleted*/
  DO WHILE TRUE:
      parNode = bnode.par.
      IF parNode = 0 THEN RETURN NO. /* Reached top root node */
      FIND bnode WHERE bnode.id = parNode.
      IF NOT bnode.expanded THEN RETURN YES.
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION picFileName C-Win
FUNCTION picFileName RETURNS CHARACTER
  (cCode AS CHAR,
   ico   AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  Returns a picture file name for a given code and icon/picture file.
    Notes:  Note that ico contains the file name path prefixed by a path, which
            might be relative

    Convention to implement your own set of pictures, called <icoFile> here
    With a Plus/Minus on the left
       noChild   <icoFile>NoSign
       expanded  <icoFile>Minus
       collapsed <icoFile>Plus

    Without Plus/Minus on the left
       noChild   <icoFile>
       expanded  <icoFile>Open
       collapsed <icoFile>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cExt         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFile        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cPath        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iSlashRIndex AS INTEGER     NO-UNDO.

  /* Case of tvwPlusMinusText */
  IF glOnlyPlusMinus THEN CASE cCode:
      WHEN "noChild"   THEN RETURN "{&tvpicpath}blank.bmp".
      WHEN "expanded"  THEN RETURN "{&tvpicpath}minus.bmp".
      WHEN "collapsed" THEN RETURN "{&tvpicpath}plus.bmp".
  END CASE.

  /* The 2 next cases use variable pictures (tvwPictureText tvwPlusPictureText) */
  IF NUM-ENTRIES(ico, '/') = 1 THEN cFile = ico.
  ELSE ASSIGN
   iSlashRIndex = R-INDEX(ico, "/")
   cPath = SUBSTRING(ico, 1, iSlashRIndex)
   cFile = SUBSTRING(ico, iSlashRIndex + 1).

  IF NUM-ENTRIES(cFile, '.') = 2 THEN ASSIGN
   cExt = '.' + ENTRY(2,cFile,'.')
   cFile = ENTRY(1,cFile,'.').

  /* Since Folder will be used a lot, here is a little shortcut for performance sake
    (Perhaps it is not worth it, but I prefered to leave it here) */
  IF ico = "{&tvpicpath}Folder" THEN DO:
      IF glPicWithPlusMinus THEN CASE cCode:
         WHEN "noChild"   THEN RETURN "{&tvpicpath}FoldNoSign.bmp".
         WHEN "expanded"  THEN RETURN "{&tvpicpath}FoldMinus.bmp".
         WHEN "collapsed" THEN RETURN "{&tvpicpath}FoldPlus.bmp".
      END CASE.
      ELSE CASE cCode:
         WHEN "noChild"   THEN RETURN "{&tvpicpath}Fold.bmp".
         WHEN "expanded"  THEN RETURN "{&tvpicpath}FoldOpen.bmp".
         WHEN "collapsed" THEN RETURN "{&tvpicpath}Fold.bmp".
      END CASE.
  END. /* ico = "{&tvpicpath}Folder" */

  IF glPicWithPlusMinus THEN CASE cCode:
       WHEN "noChild"   THEN cFile = cFile + "NoSign".
       WHEN "expanded"  THEN cFile = cFile + "Minus".
       WHEN "collapsed" THEN cFile = cFile + "Plus".
  END CASE.
  ELSE CASE cCode:
       WHEN "expanded"  THEN cFile = cFile + "Open".
  END CASE.

  RETURN cPath + cFile + cExt.

/* The following code handle the case of an invalid picture file:
 RETURN "adeicon/unprog.ico".   /* Function return value. */

  => Now it is obsolete and we handle this problem in renderNode:  If
  LOAD-IMAGE fails, then we do:
     btvpic.hpic:LOAD-IMAGE("{&tvpicpath}missingPicFile.bmp") NO-ERROR.
     btvpic.hpic:TOOLTIP = "Cannot find picture file " + sbtviter.picImg.
         => So it will be easier for a developer to find out what is going wrong
LOAD-PICTURE
  */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION renderVertScrollBar C-Win
FUNCTION renderVertScrollBar RETURNS LOGICAL PRIVATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  DEF BUFFER bTviter FOR tviter.
  DEF BUFFER bnode   FOR node.
  DEFINE VARIABLE bsvY  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE bsvHP AS INTEGER    NO-UNDO.
  DEFINE VARIABLE ipar AS INTEGER    NO-UNDO.
  DEFINE VARIABLE ipre AS INTEGER    NO-UNDO.

  /* tree is now empty, hidde scrollbar */
  IF gid = 0 THEN DO:
     gVertScrollingActive = NO.
     FRAME fsv:MOVE-TO-BOTTOM().
    RETURN YES.
  END.

  /* work out gIterOffset */
  FIND btviter WHERE btviter.iter = 1.
  FIND bnode WHERE bnode.id = btviter.id.

  gIterOffset = 0.
  DO WHILE TRUE:
      ASSIGN
       ipar = bnode.par
       ipre = bnode.pre.
      IF ipar = 0 AND ipre = 0 THEN LEAVE.
      IF ipre = 0 THEN DO:
          FIND bnode WHERE bnode.id = ipar.
          gIterOffset = gIterOffset + 1. /* + 1 for the node itself */
          NEXT.
      END.

      FIND bnode WHERE bnode.id = ipre.
      gIterOffset = gIterOffset + bnode.expChildren + 1. /* + 1 for the node itself */
  END.

  /* enable or disable the scrollBar */
  IF (gIterOffset = 0 AND gExpChildren <= gVisibleIterations)
   AND gVertScrollingActive THEN DO:
     gVertScrollingActive = NO.
     /*make sure we never see the native dummy vertical scrollbar  */
     /* sadly, on wine, it takes a little bit of time to hide this
       dummy annoying scrollbar so it is not very elegant...*/
     IF gHorzScrollingActive THEN RUN ShowScrollBar IN hppure4gltvApi (FRAME ftv:HWND, {&SB_VERT},  0  {&APIRtnParmINT}).

     FRAME fsv:MOVE-TO-BOTTOM().
     RETURN YES.
  END.


  IF (gIterOffset > 0
      OR gExpChildren > gVisibleIterations)
   AND NOT gVertScrollingActive THEN DO:
      gVertScrollingActive = YES.
     FRAME fsv:MOVE-TO-TOP().
  END.

  IF gHorzScrollingActive AND NOT gSmallerVertScrollBar
   OR NOT gHorzScrollingActive AND gSmallerVertScrollBar
   THEN resizeVertScrollBar().


  /* work out size and position of the scroll button */
  ASSIGN
   bsvHP = MIN(gFsvimgHP, MAX(15, gVisibleIterations * gFsvimgHP / gExpChildren))
   bsvY = MIN((gIterOffset) * gFsvimgHP / gExpChildren, gFsvimgHP - bsvHP).


  /* Beware to not make the button go too far to the bottom... */
  IF bsvHP > gbsvHP THEN DO:
      IF bsvY <> gbsvY THEN ASSIGN
       gbsvY = bsvY
       ghbsv:Y = gbsvY + gFsvimgY.
      ASSIGN
       gbsvHP = bsvHP
       ghbsv:HEIGHT-PIXELS = gbsvHP.
      ghbsv:LOAD-IMAGE(ghbsv:IMAGE). /*otherwise image is truncated when enlarged...*/
  END.

  IF bsvHP < gbsvHP THEN DO:
      ASSIGN
       gbsvHP = bsvHP
       ghbsv:HEIGHT-PIXELS = gbsvHP.
      /* bsv:LOAD-IMAGE(bsv:IMAGE). /*othewrise image is truncated when enlarged...*/*/
      IF bsvY <> gbsvY THEN ASSIGN
       gbsvY = bsvY
       ghbsv:Y = gbsvY + gFsvimgY.
  END.

  /* if not done yet because  bsvHP = gbsvHP ... */
  IF bsvY <> gbsvY THEN ASSIGN
   gbsvY = bsvY
   ghbsv:Y = gbsvY + gFsvimgY.

  IF VALID-HANDLE(ghbsvGraber) THEN DO:
      ghbsvGraber:Y = gbsvY + (gbsvHP - {&bsvGraberHEIGHT-PIXELS}) / 2 + gFsvimgY.
      ghbsvGraber:MOVE-TO-TOP().
      ghbsvGraber:VISIBLE = gBsvHP > 16.
  END.

  RETURN YES.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION resizeVertScrollBar C-Win
FUNCTION resizeVertScrollBar RETURNS LOGICAL PRIVATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/


/* The following assign block is necessary to avoid native scroll-bar to pop up
when shrinking the emulated vertical scrollbar */
ASSIGN
 imgsv:HEIGHT-PIXELS IN FRAME fsv = 2
 ghbtnScrollDown:Y  = ghbtnScrollUp:HEIGHT-PIXELS + 1
 emptySquare:Y = 1
 ghbsv:Y =  ghbtnScrollUp:HEIGHT-PIXELS + 1
 gbsvHP = 10  /* size it temporary down to 10 so we are sure it fits in a ...  */
 ghbsv:HEIGHT-PIXELS = gbsvHP. /* ...smaller scrollbar, the next call of renderVertScrollBar() will calculate the right size again */
IF VALID-HANDLE(ghbsvGraber) THEN ghbsvGraber:Y = 1.


IF gHorzScrollingActive THEN ASSIGN
 gFsvHP = gFtvHP - {&fsvWIDTH-PIXELS} - 2 /*note that the height of horzScrollBar =  {&fsvWIDTH-PIXELS}*/
 gFsvimgHP = gFsvHP - 2 * ghbtnScrollUp:HEIGHT-PIXELS + 2
 imgsv:HEIGHT-PIXELS IN FRAME fsv = gFsvimgHP
 ghbtnScrollDown:Y = gFsvHP - ghbtnScrollDown:HEIGHT-PIXELS + 2
/* The following commented line makes a(nother) scrollbar appear on
 this frame ftv, which makes too many scrollbars indeed !!!!
 we do not care after all, what really matters for others is to
 have the right value in ftvHP
 FRAME fsv:HEIGHT-PIXELS = gFsvHP*/
 FRAME fsv:HEIGHT-PIXELS = gFtvHP  /* Note I put gFtvHP instead of gFsvHP*/
 gSmallerVertScrollBar = YES
 NO-ERROR.  /* yes, this is a dirty no-error, required in some cases when
  the tv is resized smaller */

ELSE ASSIGN
 gFsvHP = gFtvHP
 FRAME fsv:HEIGHT-PIXELS = gFsvHP
 gFsvimgHP = gFsvHP - 2 * ghbtnScrollUp:HEIGHT-PIXELS
              - IF gHorzScrollingActive THEN 1 ELSE 0
 imgsv:HEIGHT-PIXELS = gFsvimgHP
 ghbtnScrollDown:Y = gFsvHP - ghbtnScrollDown:HEIGHT-PIXELS * IF gHorzScrollingActive THEN 2 ELSE 1
 gSmallerVertScrollBar = NO.


/* To fix a bug... It seems that Progress does not handle well high thin pictures... */
imgsv:STRETCH-TO-FIT = gFsvimgHP > 490.
imgsv:LOAD-IMAGE(imgsv:IMAGE). /*otherwise image is truncated when enlarged...*/

ASSIGN
 emptySquare:Y = FRAME fsv:HEIGHT-PIXELS - emptySquare:HEIGHT-PIXELS
 FRAME fsv:VIRTUAL-HEIGHT-PIXELS = FRAME fsv:HEIGHT-PIXELS.

emptySquare:VISIBLE = gHorzScrollingActive.

/* 25-MAR-2005 FRAME fsv:HWND seems to be unknown when the frame has not been realized yet
(case when using the classic skin on XP) which can lead to error 12272 and eventually
to a GPF in 10.0B01.  the follwing test solves this problem */
IF FRAME fsv:HWND <> ?
 THEN RUN ShowScrollBar IN hppure4gltvApi (FRAME fsv:HWND, {&SB_BOTH},  0  {&APIRtnParmINT}).

RETURN YES.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION selectNode C-Win
FUNCTION selectNode RETURNS LOGICAL
  (pcNodeKe AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  Select a the node of a given key

    Notes:  For now, if we select a node that is outside of the view port, then
            we try to scroll to it
------------------------------------------------------------------------------*/
    DEF BUFFER btviter FOR tviter.
    DEF BUFFER bnode   FOR node.
    DEF BUFFER collapsedParent  FOR node.
    DEF BUFFER prevSelectedNode FOR node.


    DEFINE VARIABLE iFocSelNodeBgColor AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iFocSelNodeFgColor AS INTEGER    NO-UNDO.
    DEFINE VARIABLE prevSelectedNodeId LIKE node.id  NO-UNDO.

    /* 25-JAN-2008 sla: delete possible existing dynamic popup menu so it does
    not disturb the built and raise of a next popup menu on a next mouse-right-clik */
    DEFINE VARIABLE cPopupWidgetPool AS CHARACTER  NO-UNDO.
    cPopupWidgetPool = STRING(THIS-PROCEDURE) + "PopupMenu".
    DELETE WIDGET-POOL cPopupWidgetPool NO-ERROR.
    /* 25-JAN-2008 sla: end */

    FIND bnode WHERE bnode.ke = pcNodeKe NO-ERROR.
    /* this can happen when a node has just been deleted */
    IF NOT AVAIL bnode THEN RETURN NO.

    {get FocSelNodeBgColor iFocSelNodeBgColor}.
    {get FocSelNodeFgColor iFocSelNodeFgColor}.
    IF gCurNode <> 0 THEN FIND prevSelectedNode WHERE prevSelectedNode.id = gCurNode NO-ERROR.
    IF AVAIL prevSelectedNode THEN prevSelectedNodeId = prevSelectedNode.id.
    gCurNode = bnode.id.

    FIND btviter WHERE btviter.id = bnode.id NO-ERROR.

    /* This node is already selected */
    IF AVAIL btviter
     AND btviter.hLab = gCurhLab
     AND gCurhLab:BGCOLOR = iFocSelNodeBgColor
     AND gCurhLab:FGCOLOR = iFocSelNodeFgColor
     THEN RETURN YES.

    IF NOT AVAIL btviter THEN DO:
         deselectCurrentNodeLabel().
         IF nodeInCollapsedBranch(bnode.id) THEN DO:
             DEFINE VARIABLE parNodeId LIKE node.par NO-UNDO.

             FIND collapsedParent WHERE collapsedParent.id = bnode.id.
             DO WHILE TRUE:
                 parNodeId = collapsedParent.par.
                 IF parNodeId = 0 THEN LEAVE. /* Reached top root node */
                 FIND collapsedParent WHERE collapsedParent.id = parNodeId.
                 IF NOT collapsedParent.expanded THEN expandNode(collapsedParent.ke, "refresh").
             END.
         END.
         ELSE DO:
             gRefreshFromNode = bnode.id.
             tvRefresh().
         END.
         /* slacroix 29-MAR-2006, little bug, actually the above code just expands branches but does not bring
          necesseraly the wanted node into the view port...
         FIND btviter WHERE btviter.id = bnode.id. /* Will be AVAIL because we checked above that it was not in a collapsed branch*/*/
         FIND btviter WHERE btviter.id = bnode.id NO-ERROR. /* perhaps it has been made avail in the view port be just expanding few branches*/
         /* but if not, then bring it to top */
         IF NOT AVAIL btviter THEN DO:
             gRefreshFromNode = bnode.id.
             tvRefresh().
             FIND btviter WHERE btviter.id = bnode.id. /* now this guy should be avail */
         END.
         /* slacroix 29-MAR-2006 end*/
    END.

    selectNodeLabel(btviter.hLab).

    /* a new node is selected, publish the right events */
    IF prevSelectedNodeId <> gCurNode THEN DO:
        IF prevSelectedNodeId <> 0 THEN PUBLISH "tvNodeEvent" ("deselect", prevSelectedNode.ke).
        PUBLISH "tvNodeEvent" ("select", bnode.ke).
    END.

    RETURN YES.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION selectNodeLabel C-Win
FUNCTION selectNodeLabel RETURNS LOGICAL PRIVATE
  (hLab AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  Select the node label of a given label handle (iteration.hlab)
    Notes:
------------------------------------------------------------------------------*/

  IF NOT VALID-HANDLE(hLab) THEN RETURN FALSE.


  DEFINE VARIABLE iFocSelNodeBgColor AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFocSelNodeFgColor AS INTEGER    NO-UNDO.
  {get FocSelNodeBgColor iFocSelNodeBgColor}.
  {get FocSelNodeFgColor iFocSelNodeFgColor}.

  DEF BUFFER btviter FOR tviter.

  FIND btviter WHERE btviter.hlab = hlab.

  IF gCurhLab <> btviter.hLab THEN deselectCurrentNodeLabel().

  ASSIGN
   hlab:BGCOLOR = iFocSelNodeBgColor
   hlab:FGCOLOR = iFocSelNodeFgColor
   gCurhLab = btviter.hLab.

  RETURN TRUE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAutoSort C-Win
FUNCTION setAutoSort RETURNS LOGICAL
  (lautoSort AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  {set autoSort lautoSort}.

  RETURN YES.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDragSource C-Win
FUNCTION setDragSource RETURNS LOGICAL
  (cDragSource AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  {set DragSource cDragSource}.

  RETURN YES.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFocSelNodeBgColor C-Win
FUNCTION setFocSelNodeBgColor RETURNS LOGICAL
  (iFocSelNodeBgColor AS INT) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  {set FocSelNodeBgColor iFocSelNodeBgColor}.

  RETURN YES.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFocSelNodeFgColor C-Win
FUNCTION setFocSelNodeFgColor RETURNS LOGICAL
  (iFocSelNodeFgColor AS INT) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  {set FocSelNodeFgColor iFocSelNodeFgColor}.

  RETURN YES.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setLabCacheCoef C-Win
FUNCTION setLabCacheCoef RETURNS LOGICAL
  (dlabCacheCoef AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

    /* security */
  IF dlabCacheCoef < 1 OR dlabCacheCoef > 20 THEN DO:
       MESSAGE THIS-PROCEDURE:FILE-NAME PROGRAM-NAME(1) SKIP
        "Indecent value" dlabCacheCoef "for labCacheCoef property" SKIP
        "About to do STOP"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       STOP.
  END.

  gLabCacheCoef = dlabCacheCoef.
  {set labCacheCoef dlabCacheCoef}.

  /* Reducing the cache coef can seriously disturb the tree view
    We need to shrink the case in this case, which initializeTviter
    is designed for too */
  DEFINE VARIABLE lShallIReinitialize AS LOGICAL    NO-UNDO.
  lShallIReinitialize = DYNAMIC-FUNCTION('CalledFromPure4GlTvInstPropDialog' IN SOURCE-PROCEDURE) NO-ERROR.

  /* Will be unknown if the caller is not the pure4glTV instance
   property dialog box */
  IF lShallIReinitialize THEN RETURN YES.
  ELSE RUN initializeTviter.


  RETURN YES.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMSkeyScrollForcePaint C-Win
FUNCTION setMSkeyScrollForcePaint RETURNS LOGICAL
  (lMSkeyScrollForcePaint AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  {set MSkeyScrollForcePaint lMSkeyScrollForcePaint}.
  glMSkeyScrollForcePaint = lMSkeyScrollForcePaint.
  RETURN YES.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPicCacheCoef C-Win
FUNCTION setPicCacheCoef RETURNS LOGICAL
  (dpicCacheCoef AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  /* security */
  IF dpicCacheCoef < 1 OR dpicCacheCoef > 20 THEN DO:
       MESSAGE THIS-PROCEDURE:FILE-NAME PROGRAM-NAME(1) SKIP
        "Indecent value" dpicCacheCoef "for picCacheCoef property" SKIP
        "About to do STOP"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       STOP.
  END.

  gPicCacheCoef = dpicCacheCoef.
  {set picCacheCoef dpicCacheCoef}.

  /* Reducing the cache coef can seriously disturb the tree view
    We need to shrink the case in this case, which initializeTviter
    is designed for too */
  DEFINE VARIABLE lShallIReinitialize AS LOGICAL    NO-UNDO.
  lShallIReinitialize = DYNAMIC-FUNCTION('CalledFromPure4GlTvInstPropDialog' IN SOURCE-PROCEDURE) NO-ERROR.

  /* Will be unknown if the caller is not the pure4glTV instance
   property dialog box */
  IF lShallIReinitialize THEN RETURN YES.
  ELSE RUN initializeTviter.


  RETURN YES.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setScrollBarWidgets C-Win
FUNCTION setScrollBarWidgets RETURNS LOGICAL PRIVATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  IF gWindowsSkin <> "Classic" THEN RETURN createDynWidgetForVertScrollBar().
  ELSE ASSIGN
   ghbtnScrollUp   = SbtnScrollUp:HANDLE IN FRAME fsv
   ghbtnScrollDown = SbtnScrollDown:HANDLE IN FRAME fsv
   ghbsv           = Sbsv:HANDLE IN FRAME fsv
   ghbtnScrollUp:SENSITIVE = YES
   ghbtnScrollDown:SENSITIVE = YES
   ghbsv:SENSITIVE = YES
   NO-ERROR.

  RETURN NOT ERROR-STATUS:ERROR.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTreeStyle C-Win
FUNCTION setTreeStyle RETURNS LOGICAL
  (iTreeStyle AS INT) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  {set TreeStyle iTreeStyle}.

  CASE iTreeStyle:
      WHEN {&tvwPictureText} THEN ASSIGN
       glPicWithPlusMinus = NO
       glOnlyPlusMinus    = NO.

      WHEN {&tvwPlusMinusText} THEN ASSIGN
       glPicWithPlusMinus = NO
       glOnlyPlusMinus    = YES.

      WHEN {&tvwPlusPictureText} THEN ASSIGN
       glPicWithPlusMinus = YES
       glOnlyPlusMinus    = NO.
  END CASE.

  RETURN YES.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION settvIterationHeight C-Win
FUNCTION settvIterationHeight RETURNS LOGICAL
  (itvIterationHeight AS INT) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  {set tvIterationHeight itvIterationHeight}.
  gTvIterationHeight = itvIterationHeight.

  RETURN YES.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTvnodeDefaultFont C-Win
FUNCTION setTvnodeDefaultFont RETURNS LOGICAL
  ( itvnodeDefaultFont AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:  see comments about this property in gettvnodeDefaultFont
------------------------------------------------------------------------------*/


  /* Beware, the unknown value makes many problems... */
  IF itvnodeDefaultFont = ?  THEN DO:
      MESSAGE "It seems that setting unknown value in tvnodeDefaultFont"
        "in order to use the default font makes problems in the set and"
        "get pseudo syntax to handle the ADMPRop/ADMPropRepos TEMP-TABLE" SKIP
        "I am going use Font number 1 instead, which results in the same with the"
        "standard environment"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      {set tvnodeDefaultFont 1}.
  END.
  ELSE {set tvnodeDefaultFont itvnodeDefaultFont}.

  RETURN YES.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUnfSelNodeBgColor C-Win
FUNCTION setUnfSelNodeBgColor RETURNS LOGICAL
  (iUnfSelNodeBgColor AS INT) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  {set UnfSelNodeBgColor iUnfSelNodeBgColor}.

  RETURN YES.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUnfSelNodeFgColor C-Win
FUNCTION setUnfSelNodeFgColor RETURNS LOGICAL
  (iUnfSelNodeFgColor AS INT) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  {set UnfSelNodeFgColor iUnfSelNodeFgColor}.

  RETURN YES.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setwindowsSkin C-Win
FUNCTION setwindowsSkin RETURNS LOGICAL
  (cwindowsSkin AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  set the value of the windowsSkinproperty AND the value of the global
           variable gWindowsSkin
    Notes:  gWindowsSkin is equal to the property except in case of "Automatic".
           In this case, we try to determine the value of gWindowsSkin.  For that
           we cannot find the answer in the MS registry because we would have to
           rely on theme file name stored in an unreadable REG_EXPANDE_RZ.

           I tried to rely on few COM-OBJECTS (Theme.Manager.1:SelectedScheme),
           that work fine in XP and XP SP1, but fail with an amazing "access denied"
           error since XP SP2!!  The fact that you are using a particular skin
           can now be taken as a secured info  :o   Thanks MS!
             => see commented code at the bottom

           Found another way with uxtheme.dll that works fine with XP SP2, and
           that will make it even easier to support the theme color "silver"
           (actually "Metallic")
------------------------------------------------------------------------------*/
DEFINE VARIABLE mThemeName  AS MEMPTR     NO-UNDO.
DEFINE VARIABLE mThemeColor AS MEMPTR     NO-UNDO.
DEFINE VARIABLE mThemeSize  AS MEMPTR     NO-UNDO. /* for now, we just ignore the custom sizes... */
DEFINE VARIABLE iRtn        AS INT        NO-UNDO.
DEFINE VARIABLE cThemeName  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cThemeColor AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.

{set windowsSkin cwindowsSkin}.
IF cWindowsSkin <> "Automatic" THEN DO:
    gWindowsSkin = cWindowsSkin.
    RETURN YES.
END.

/* The following code executes only if cWindowsSkin = "Automatic"
  the point is to determine whether gWindowsSkin should be set to classic or Luna */

gWindowsSkin = "Classic". /* First assume classic, will keep it in case of ERROR
                             Will change it if we find a supported skin*/

/* Do not call GetCurrentThemeName on Wine because it can crash,
 assuming classic is good enough for wine */
IF glWineMode THEN RETURN YES.

SET-SIZE(mThemeName) = 255.    SET-SIZE(mThemeColor) = 255.    SET-SIZE(mThemeSize)  = 255.


RUN GetCurrentThemeName(OUTPUT mThemeName, 255, OUTPUT mThemeColor, 255, OUTPUT mThemeSize, 255, OUTPUT iRtn) NO-ERROR.

IF NOT ERROR-STATUS:ERROR /* The RUN GetCurrentThemeName failed because uxtheme.dll is not available (old MS Windows, or wine without multi skin support)*/
 AND iRtn = 0 THEN DO: /* = 0 means that GetCurrentThemeName ran fine, if error, then value is negative*/
    /* The MEMPTR's contain double byte characters.  In our western case, the 2nd
     byte is always the null character, so we can skip it each time */
    DO iCount = 1 TO 255 BY 2:
        IF GET-BYTE(mThemeName,iCount) = 0 THEN LEAVE.
        cThemeName = cThemeName + CHR(GET-BYTE(mThemeName,iCount)).
    END.
    DO iCount = 1 TO 255 BY 2:
        IF GET-BYTE(mThemeColor,iCount) = 0 THEN LEAVE.
        cThemeColor = cThemeColor + CHR(GET-BYTE(mThemeColor,iCount)).
    END.
    ASSIGN
     cThemeName = ENTRY(NUM-ENTRIES(cThemeName,"\"), cThemeName,"\")
     cThemeName = ENTRY(1, cThemeName, ".")
     gWindowsSkin = cThemeName + IF cThemeColor = "NormalColor" THEN "" ELSE cThemeColor.

     /* 16-JUN-2007 sla: protection against unsupported theme */
     FILE-INFO:FILE-NAME = "{&tvskinpath}" + gWindowsSkin.
     IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
         FILE-INFO:FILE-NAME = "{&tvskinpath}".
         emptySquare:TOOLTIP IN FRAME fsv  = "Skin was set to " + QUOTER(cwindowsSkin)
          + " managed to find the current skin is " + QUOTER(gWindowsSkin)
          + " that is currently not supported.~nJust create a directory "
          + QUOTER(gWindowsSkin) + " in the " + QUOTER(FILE-INFO:FULL-PATHNAME)
          + " directory with the necessary pictures~n =>(see what exists in the {&tvskinpath}\Luna directory and provide the same)"
          + "~nUntil this directory exist with the necessary pictures, the classic style will be used for the vertical scrollbar".
         gWindowsSkin = "Classic".
     END.
END.
SET-SIZE(mThemeName) = 0.    SET-SIZE(mThemeColor) = 0.    SET-SIZE(mThemeSize) = 0.
RETURN YES.   /* Function return value. */

END FUNCTION.


/* broken with SP2 on XP,  see comments in header
  DEFINE VARIABLE chTheme              AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE chThemeSel           AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE cThemeSelDisplayName AS CHARACTER  NO-UNDO.

  gWindowsSkin = cWindowsSkin.

  IF gWindowsSkin = "Automatic" THEN DO:
      /* This creates an ERROR if no support for multi skin on Windows
         or if on wine where we have to disable the usage of COM-OBJECTS because
         it can lead to a crash (a good trick is to rename PROX.DLL everywhere) */
      CREATE "Theme.Manager.1" chTheme NO-ERROR.
      IF ERROR-STATUS:ERROR THEN gWindowsSkin = "Classic".
      ELSE DO:
          /*Value for XP skin is "Windows XP style"
            Value for Classic is "Windows Classic style" => I take it as default */
          chThemeSel = chTheme:SelectedScheme NO-ERROR.
          IF ERROR-STATUS:ERROR THEN gWindowsSkin = "Classic".
          ELSE DO:
              cThemeSelDisplayName = chThemeSel:DisplayName.
              CASE cThemeSelDisplayName:
                  WHEN "Windows XP style" THEN gWindowsSkin = "Luna".
                  /* Add other themes you want to support here */
                  OTHERWISE gWindowsSkin = "Classic".
              END CASE.
              RELEASE OBJECT chThemeSel.
              RELEASE OBJECT chTheme.
          END. /* cannot do chTheme:SelectedScheme  => not the expected multi-skin support */
      END. /* ERROR when creating the COM-OBJECT */
  END. /* Automatic, try to determine it */

  {set windowsSkin cwindowsSkin}.

  RETURN YES.   /* Function return value. */
END FUNCTION.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWineMode C-Win
FUNCTION setWineMode RETURNS LOGICAL
  ( cWineMode AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:  Default is Automatic.

    We actually use the global variable glWineMode that defaults to NO, so the
    property can remain set to "Automatic"

    If the property is set to "Automatic", then we use YES if we can find a key
    "RunningOnWine" set to "yes" in the environment (registry or ini file)


------------------------------------------------------------------------------*/

  IF cwineMode = "Automatic" THEN DO:
    /* For now, I rely on a key RunningOnWine set to yes in the ini file */
    DEFINE VARIABLE cKey AS CHARACTER  NO-UNDO.
    GET-KEY-VALUE SECTION "Startup" KEY "RunningOnWine" VALUE cKey.
    glWineMode = cKey = "yes".
  END.
  ELSE glWineMode = LOGICAL(cWineMode).

  {set wineMode cWineMode}.

  RETURN TRUE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION topNode C-Win
FUNCTION topNode RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  returns node.id of node at the top of the tree
    Notes:  Note this node cannot be in a collapsed branch
------------------------------------------------------------------------------*/
  DEF BUFFER bnode FOR node.

  /* Do not search with id = 1, because we could have deleted a top node and
  inserted another top node afterwards */
  FIND bnode WHERE bnode.par = 0 AND bnode.pre = 0 NO-ERROR.
  RETURN IF AVAIL bnode THEN bnode.id ELSE 0.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION tvRefresh C-Win
FUNCTION tvRefresh RETURNS LOGICAL
  (/* char def */) :
/*------------------------------------------------------------------------------
  Purpose:  Refreshes the view port of the treeview.  Used to display the current
            view port

    Notes:  Big change on 01-Nov-2004: we always refresh from iteration 1
            Before, we could refresh starting form the middle of the viewport
            (think of collapsing a node in the middle) for performance sake,
            However it is far simpler and easier to maintain by always refreshing
            the all view port and the impact on performance is ridiculous thanks
            to new optimizeWine/MS procedures that naturally reuse the rendering
            widget at best.
------------------------------------------------------------------------------*/

DEF BUFFER bnode        FOR node.
DEF BUFFER childnode    FOR node.
DEF BUFFER btviter      FOR tviter.
DEF BUFFER btvpic       FOR tvpic.
DEF BUFFER btvlab       FOR tvlab.
DEF BUFFER hiddenTviter FOR tviter.

DEFINE VARIABLE iiter                  LIKE gTvIterations NO-UNDO.
DEFINE VARIABLE nextNodeId             LIKE node.id NO-UNDO.
DEFINE VARIABLE lTopNodeIsAtVirtualTop AS LOGICAL    NO-UNDO.
DEFINE VARIABLE nScrollDown            AS INTEGER    NO-UNDO.  /*if zero then nothing to do */
DEFINE VARIABLE lPrevVertScrollingActive AS LOGICAL    NO-UNDO.
&SCOPED-DEFINE VertSBAdjust (IF gVertScrollingActive THEN {&fsvWIDTH-PIXELS} ELSE 0)

/* if gRefreshFromNode not specified, then find it yourself */
FIND bnode WHERE bnode.id = gRefreshFromNode NO-ERROR.

IF NOT AVAIL bnode THEN DO:
  FIND bnode WHERE bnode.par = 0 AND bnode.pre = 0 NO-ERROR. /* very first root node */
  /* keep track of the top node */
  IF NOT AVAIL bnode AND CAN-FIND(FIRST node) THEN DO:
      MESSAGE PROGRAM-NAME(1) "in" THIS-PROCEDURE:FILE-NAME SKIP
       "corrupted treeview, there are node records, but there is no root node"
       VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Debug message".
      RETURN ERROR.
  END.
  gRefreshFromNode = IF AVAIL bnode THEN bnode.id ELSE 0. /* not avail if tree is completely empty, start refreshing it from 1st iteration */
END.

/* First clear all btviter.id for unique index sake, also set default
hpic and hlab for iter that will remain empty */
FOR EACH btviter:
  ASSIGN
   btviter.id   = ?
   btviter.hpic = ?  /* do not want to remain linked to a valid tvpic*/
   btviter.hlab = ?. /* do not want to remain linked to a valid tvlab*/
END.

/* unlink tvpic and tvlab record from tviterations that are going to be reworked */
FOR EACH btvpic:    btvpic.iter = ?.    END.
FOR EACH btvLab:    btvLab.iter = ?.    END.

/* keep track of gVertScrollingActive, to handle a chicken and egg situation */
lPrevVertScrollingActive = gVertScrollingActive.

/* show/hide horizontal scroll bar and update VIRTUAL-WIDTH-PIXELS of FRAME ftv
   If Horizontal scrolling active then reserve last iteration of view port for Horiz scrollbar */
/* swtich off horz scrollbar */
IF gHorzScrollingActive
 AND gVWP <= gFtvWP - {&VertSBAdjust}
 THEN ASSIGN
 gVisibleIterations        = gTvIterations
 gFtvVWP                   = gFtvWP
 gHorzScrollingActive      = NO.

/* popup horiz scrollbar and/or maintain virtual-width of frame ftv*/
IF gVWP > gFtvWP - {&VertSBAdjust}
 THEN DO:
   IF gVWP <> gFtvVWP - {&VertSBAdjust}
    THEN ASSIGN
     gFtvVWP =  MAX(gVWP + {&VertSBAdjust},gFtvWP)
     FRAME ftv:VIRTUAL-WIDTH-PIXELS = gFtvVWP.
    /* if no vertical scrollbar, then get rid off the dummy unwanted
      vertical scrollbar in frame fsv*/
    IF NOT gVertScrollingActive THEN RUN ShowScrollBar IN hppure4gltvApi (FRAME ftv:HWND, {&SB_VERT},  0  {&APIRtnParmINT}).
    /* else to not let the horz take all the width... */
    ELSE RUN ShowScrollBar IN hppure4gltvApi (FRAME ftv:HWND, {&SB_VERT},  1  {&APIRtnParmINT}).

    ASSIGN
     gVisibleIterations             = gTvIterations - 1
     gHorzScrollingActive           = YES.
END.

/* This makes problems when keeping scrolling (labels not shown)
 so I DISABLED it, anyway, wine ignores it and the gain is not that great on Windows
 19-Oct-2004 no the problem is there anyway since I have optimized the optimizers..., so I re-enable it
 again.  Next step is to find a way to force a refresh... Perhaps I should play with CAREFULL-PAINT */
IF NOT glwineMode THEN lockfMain(YES). /* performance gain of 12.5% by using this trick */

/*@@@@@@@@@@@@@@ Prepare and render iterations in the treeview @@@@@@@@@@@@@@@@@@@*/
IF gRefreshFromNode <> 0 THEN DO:  /* if = 0, then it means we want to clear the TV */

  /* Note that a bnode record should be available when getting here */

  /* for tviter starting from fromIter, determine the node id, work out the image
   file, prepare labScreenValue and width and the coordinate of hpic nd hLab*/
  FOR EACH btviter WHERE btviter.iter <= gVisibleIterations
   BY btviter.iter:
      IF NOT AVAIL bnode THEN LEAVE. /* clearing job done in previous block*/

      /* 08-APR-2007 sla: new nodeDisplay hook to give the ability to refine a node just before it gets displayed
       it's up to the container to subscribe to this event.  It is a bit like a ROW-DISPLAY trigger in a browse
       This way, one can  refine node labels just before they get displayed.  One nice example is a new prospy = prospy +
       that displays a huge trace in a treeview with time information.  This time info can be either an absolute time
       or a relative time, or a node cost or branch cost.  In the past, without this hook, I had to reupdate the labels
       of the all treview when the end user was choosing a different display mode, which was very slow when the treeview was
       containing half a million of nodes....  Now, it is just *instant* since I refresh I prepare the node label just
       when it is about to be displayed ;) */
      DEFINE VARIABLE lNodeUpdated           AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE lOneNodeHasBeenUpdated AS LOGICAL   NO-UNDO.
      PUBLISH "nodeDisplay" (bnode.ke, OUTPUT lNodeUpdated).

      lOneNodeHasBeenUpdated = lOneNodeHasBeenUpdated OR lNodeUpdated. /* keep track of it to refire tvRefresh() if necessary */

      ASSIGN
       btviter.id             = bnode.id
       btviter.hpic           = ? /* not linked to a tvpic yet */
       btviter.hlab           = ? /* not linked to a tvlab yet */
       btviter.nodeFont       = bnode.nodeFont    /* SLP 17-OCT-2007 */
       btviter.nodeFGCol      = bnode.nodeFGCol   /* SLP 17-OCT-2007 */
       btviter.nodeBGCol      = bnode.nodeBGCol   /* SLP 17-OCT-2007 */
       btviter.nodeTooltip    = bnode.nodeTooltip /* SLP 30-OCT-2007 */
       btviter.picX           = 2 + bnode.level * gTvLevelWidth  /* "2 +" to let a little margin on the left */
       btviter.picY           = 1 + (btviter.iter - 1) * gtviterationHeight
       btviter.labX           = btviter.picX + gTvPicWidth + 1 /* + 1 to let a little gap between the label and the pic */
       btviter.LabY           = btviter.picY
       btviter.labScreenValue = bnode.lab
       btviter.labWidthPixels = bnode.labWidthPixels.

      /* Determine picture file for bnode*/
      IF CAN-FIND(FIRST childNode WHERE childNode.par = bnode.id)
        OR LOOKUP("addOnExpand", bnode.optn, CHR(1)) <> 0
        THEN btviter.picimg = IF bnode.expanded THEN picFileName("expanded", bnode.ico)
                                                ELSE picFileName("collapsed", bnode.ico).
      ELSE btviter.picimg = picFileName("noChild", bnode.ico).

      RUN findNextNodeToShow(bnode.id, NO, OUTPUT nextNodeId).
      IF nextNodeId = 0 THEN LEAVE. /* we are at the bottom of visible  */
      FIND bnode WHERE bnode.id =  nextNodeId.
  END.

  IF lOneNodeHasBeenUpdated THEN RETURN tvRefresh(). /* 08-APR-2007 sla: necessary to take a label update on the fly that could have changed the view port width */

  /*-------------------------------------------------------------------------
  If gGoToNodeAtNextRefresh is set and if no in viewport, then scroll to it
  -------------------------------------------------------------------------*/
  IF gGoToNodeAtNextRefresh <> 0
   AND NOT CAN-FIND(FIRST btviter WHERE btviter.id = gGoToNodeAtNextRefresh)
   THEN DO:
      FIND bnode WHERE bnode.id = gGoToNodeAtNextRefresh.
      ASSIGN
       gRefreshFromNode       = gGoToNodeAtNextRefresh
       gGoToNodeAtNextRefresh = 0. /* ... reset it ... */
      tvRefresh(). /*... otherwize tvRefresh enter a recursive infinite loop */

      selectNode(bnode.ke).
      RETURN YES.
  END.

  /*-------------------------------------------------------------------------
  If some nodes with option InViewPortIfPossible are beyond the bottom of
  the viewport, then scroll up the to bring them into view
  We assume these nodes have been added down
  -------------------------------------------------------------------------*/
  FOR EACH bnode WHERE bnode.optn CONTAINS "InViewPortIfPossible":
      ASSIGN  /* remove this option now */
       bnode.optn = REPLACE(bnode.optn, ",InViewPortIfPossible" ,"")
       bnode.optn = REPLACE(bnode.optn, "InViewPortIfPossible," ,"")
       bnode.optn = REPLACE(bnode.optn, "InViewPortIfPossible"  ,"").
      FIND FIRST btviter WHERE btviter.id = bnode.id NO-ERROR.
      IF NOT AVAIL btviter THEN nScrollDown = nScrollDown + 1.
  END.
  IF nScrollDown <> 0 THEN DO:
      tvScroll(MIN(nScrollDown,gVisibleIterations),NO).
      RETURN YES.
  END.

  /*------------------------------------------------------------------------------
   Try to let the smallest empty space (or even no space at all) at the bottom if
    we can scroll up  (i.e. when gExpChildren >= first hiddenTviter.iter
  -------------------------------------------------------------------------------*/
  FIND FIRST hiddenTviter WHERE hiddenTviter.iter <= gVisibleIterations  /* beware, in some cases, we have invisible iterations at the bottom (horizontal scroll bar, TV has been shrinked down) */
                          AND hiddenTviter.id = ? USE-INDEX iter NO-ERROR.
  IF AVAIL hiddenTviter
   AND gExpChildren >= hiddenTviter.iter THEN DO:
      tvScroll(- MIN(gVisibleIterations - hiddenTviter.iter + 1 /* number of empty iterations */
                    ,gExpChildren - hiddenTviter.iter + 1) /* we cannot scroll more than that... */
              ,YES).
      RETURN YES. /* tvScroll calls tvRefhresh() again */
  END.

  IF glwineMode THEN RUN optimizeTviterWine.
  ELSE RUN  optimizeTviterMSWin.


  /* beware, there could be few rendering widget that would not fit in the
  frame ftv anymore next to a collapse or deleteNode */
  FOR EACH btvpic WHERE btvpic.picX > gFtvVWP - gTvPicWidth:
      ASSIGN
       btvpic.picX = 1
       btvpic.hpic:X = 1.
  END.
  FOR EACH btvLab WHERE btvLab.labX + btvLab.labWidthPixels > gFtvVWP:
      ASSIGN
       btvLab.labX = 1
       btvLab.hlab:X = 1
       btvLab.labWidthPixels = 5 /* 5 is probably small enough ;) */
       btvLab.hlab:WIDTH-PIXELS = 5
       NO-ERROR. /* NO-ERROR required because changing X and WIDTH-PIXELS cannot be done at the *same* time */
  END.

  /*========================= now render the TV =========================*/
  FOR EACH btviter WHERE btviter.id <> ? BY btviter.iter:
      RUN RenderNode (BUFFER btviter).
  END.
END. /*   IF gRefreshFromNode <> 0 THEN DO: */


/* Hide remaining unused image and label in view port*/
FOR EACH btvpic WHERE btvpic.iter = ?
                  AND btvpic.picVisible:
    btvpic.picVisible = NO.
    IF VALID-HANDLE(btvpic.hpic) THEN btvpic.hpic:VISIBLE = NO.

    /* Now, make sure that this stuff to hide still fits the virtual width
      if not then move it to the left */
    IF btvpic.picX + gTvPicWidth > gVWP THEN ASSIGN
     btvpic.picX    = 2 /* = X for a level 1, who knows, we may reuse this value later ...*/
     btvpic.hpic:X  = btvpic.picX.
END.
FOR EACH btvlab WHERE btvlab.iter = ?
                  AND btvlab.labVisible:
    btvlab.labVisible = NO.
    IF VALID-HANDLE(btvlab.hlab) THEN btvlab.hlab:VISIBLE = NO.

    /* Now, make sure that this stuff to hide still fits the virtual width
      if not then move it to the left, and skrink the label */
    IF btvlab.labX + btvlab.labWidthPixels > gVWP THEN ASSIGN
     btvlab.labX    = gTvPicWidth + 2 + 1 /* = X for a level 1, who knows, we may reuse this value later ...*/
     btvlab.hlab:X  = btvlab.labX
     btvlab.labWidthPixels    = 5
     btvlab.hlab:WIDTH-PIXELS = btvlab.labWidthPixels.
END.


/* See above, the reason why it has been disabled and re-enabled
  10-Nov-2004, I prefer to do it before shrinking the VIRTUAL-WIDTH of the
  FRAME so it scrolls back to the left if necessary*/
IF NOT glwineMode THEN DO:
    lockfMain(NO). /* performance is greatly improved with that */
    DEF VAR RetVal AS INTEGER NO-UNDO.
    /* force a paint to refresh */
    RUN SendMessage{&A} IN hppure4gltvApi (FRAME ftv:HWND, {&WM_PAINT}, 0, 0  {&APIRtnParmINT}).
END.


/* adjust virtual width to adjust horiz scrollbar */
IF /*gHorzScrollingActive
 AND */ gVWP + {&VertSBAdjust} < gFtvVWP
 THEN ASSIGN
  gFtvVWP =  MAX(gVWP + {&VertSBAdjust},gFtvWP)
  FRAME ftv:VIRTUAL-WIDTH-PIXELS = gFtvVWP.



/* handle vertical scrollbar */
IF NOT gDoNotRenderVerScrollBar THEN renderVertScrollBar().

/* slacroix 04/04/2006 to solve a chicken & egg story:
Perhaps that we just made the Verticall Scrollbar appear, so we should slightly
shrink the horizontal scrollbar if it was already visible*/
IF gHorzScrollingActive
 /* we just made the vertical scrollbar appear */
 AND NOT lPrevVertScrollingActive
 AND gVertScrollingActive
 THEN RUN ShowScrollBar IN hppure4gltvApi (FRAME ftv:HWND, {&SB_VERT},  1  {&APIRtnParmINT}).

/*-------------- What to do with the previous selected node ? -----------------*/
IF gCurNode <> 0 THEN DO:
    /* If previously selected label has been moved away outside of
    the view port then deselect current selected label (the node remains
    selected, in gCurNode)*/

    FIND btviter WHERE btviter.id = gCurNode NO-ERROR.

    IF NOT AVAIL btviter THEN deselectCurrentNodeLabel().

    /* if = gCurLab, then there is nothing to do.
      This situation can occur when either the selected node has
     not moved at all in the viewport (like if not bellow the
     collapsed node), or it has moved but the selected label has
     moved with it (optimizeScroll did it) */
    ELSE IF btviter.hLab <> gCurhLab THEN selectNodeLabel(btviter.hLab).
END. /* Handle Previous Selected Node*/


/* if gGoToNodeAtNextRefresh then select it and reset it */
IF gGoToNodeAtNextRefresh <> 0 THEN DO:
    FIND bnode WHERE bnode.id = gGoToNodeAtNextRefresh.
    selectNode(bnode.ke).
    gGoToNodeAtNextRefresh = 0. /* ... reset it ... */
END.


RETURN YES.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION tvScroll C-Win
FUNCTION tvScroll RETURNS LOGICAL
  ( ipScrollBy AS INT,
    scrollAsMuchAsPossible AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  Scroll view port by ipScrollBy
                  positive means scroll up    (going down!!!)
                  negative means scroll down  (going up !!!)

    Notes:  Description of the scrollAsMuchAsPossible parameter:
             Scenario: We ask to scroll 6 up but there are only 5 iterations
             above the top so the maximum we can scroll up is actually 5:
              -if scrollAsMuchAsPossible IS set, then we scroll up by 5
              -if scrollAsMuchAsPossible is NOT set, then we do NOT scroll
               at all and we RETURN NO

   Two little exceptions when we return NO for any value of scrollAsMuchAsPossible:
             1) If we are already at the very bottom and try to scroll up
             2) We are already at the very top and we try to scroll down
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iLoop              AS INTEGER    NO-UNDO.
  DEFINE VARIABLE nodeOnTheWay       LIKE node.id  NO-UNDO.
  DEFINE VARIABLE newRefreshFromNode LIKE node.id  NO-UNDO.

  DEF BUFFER bnode FOR node.
  DEF BUFFER btviter FOR tviter.

  /* Scroll up */
  IF ipScrollBy > 0 THEN DO:
      /* Try to make the bottom iteration scroll up by ipScrollBy,
        the point is to work out the actual ipScrollBy if scrollAsMuchAsPossible is not SET
        and especially if it is possible to scroll up (not the case when we are
        already at the bottom) */
      FIND FIRST btviter WHERE btviter.id = ? USE-INDEX iter NO-ERROR.
      IF AVAIL btviter THEN FIND PREV btviter USE-INDEX iter NO-ERROR.
      ELSE FIND btviter WHERE btviter.iter = gVisibleIterations NO-ERROR.

      IF NOT AVAIL btviter THEN RETURN NO. /* Happens when TV is empty*/

      newRefreshFromNode = btviter.id.
      DO WHILE TRUE.
          IF iLoop = ipScrollBy THEN LEAVE. /* We can scroll by ipScrollBy indeed */
          RUN findNextNodeToShow(newRefreshFromNode, NO, OUTPUT nodeOnTheWay).
          IF nodeOnTheWay = 0
           THEN IF scrollAsMuchAsPossible
                 THEN IF iLoop = 0 THEN RETURN NO. /* Already at the very bottom */
                      ELSE LEAVE. /* We will scroll by at bit less than ipScrollBy */
                ELSE RETURN NO.
          newRefreshFromNode = nodeOnTheWay.
          iLoop = iLoop + 1.
      END.

      ipScrollBy = iLoop.
      /* Now find the new node to be at the top so we can set gRefreshFromNode
         and call tvRefresh() */
      FIND btviter WHERE btviter.iter = 1 NO-ERROR.
      newRefreshFromNode =  btviter.id.
      DO iLoop = 1 TO ipScrollBy:
          RUN findNextNodeToShow(newRefreshFromNode, NO, OUTPUT nodeOnTheWay).
          IF nodeOnTheWay = 0
           THEN IF scrollAsMuchAsPossible THEN LEAVE.
           ELSE RETURN NO.
          newRefreshFromNode = nodeOnTheWay.
      END.
  END. /* Scroll up */

  /* Scroll down */
  IF ipScrollBy < 0 THEN DO:
      FIND btviter WHERE btviter.iter = 1 NO-ERROR.
      IF NOT AVAIL btviter THEN RETURN NO. /* Happens when TV is empty*/


      newRefreshFromNode = btviter.id. /* Starting point */
      DO iLoop = -1 TO ipScrollBy BY -1:
          RUN findPrevNodeToShow(newRefreshFromNode, NO, OUTPUT nodeOnTheWay).
          IF nodeOnTheWay = 0
           THEN IF scrollAsMuchAsPossible THEN LEAVE.
           ELSE RETURN NO.
          newRefreshFromNode = nodeOnTheWay.
      END.
  END. /* Scroll down */


  /* OK do it now */
  gRefreshFromNode = newRefreshFromNode.
  tvRefresh().

  RETURN YES.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

