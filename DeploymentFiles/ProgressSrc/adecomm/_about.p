/**************************************************************************
*Copyright (C) 2005,2008 by Progress Software Corporation.                *
*All rights reserved.  Prior versions of this work may contain portions   *
*contributed by participants of Possenet.                                 *
**************************************************************************/
/*
Procedure:    adecomm/_about.p
Author:       R. Ryan
Created:      1/95
Purpose:      re-written by Bob Ryan 1/95 to look better under 3-D
              and provide some Windows environment info
*/

&SCOPED-DEFINE FRAME-NAME  Dialog-1

&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
  &SCOPED-DEFINE StartRow 6
  &SCOPED-DEFINE StartCol 4
  &SCOPED-DEFINE OKBtnRow 8.5
&ELSE
  &SCOPED-DEFINE StartRow 5
  &SCOPED-DEFINE StartCol 11.57
  &SCOPED-DEFINE OKBtnRow 7.5
&ENDIF

{adecomm/adestds.i}

DEFINE INPUT PARAMETER pTitle AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pIcon  AS CHARACTER NO-UNDO.

DEFINE VARIABLE result       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE AboutText1   AS CHARACTER NO-UNDO.
DEFINE VARIABLE AboutText2   AS CHARACTER NO-UNDO.
DEFINE VARIABLE Label1       AS CHARACTER FORMAT "x(50)" VIEW-AS TEXT.
DEFINE VARIABLE Label2       AS CHARACTER FORMAT "x(50)" VIEW-AS TEXT.
DEFINE VARIABLE DBEtestvalue AS CHARACTER NO-UNDO.
DEFINE VARIABLE ProName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE ABLic        AS INTEGER   NO-UNDO.
DEFINE VARIABLE ABTools      AS CHARACTER NO-UNDO.
DEFINE VARIABLE Workshop-only AS LOGICAL  NO-UNDO.

DEFINE VARIABLE majorversion  AS INTEGER   NO-UNDO.
DEFINE VARIABLE minorversion  AS CHARACTER NO-UNDO.
DEFINE VARIABLE dot           AS INTEGER   NO-UNDO.
DEFINE VARIABLE patchlevel    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextFile     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCommercialVer   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLine         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lICF          AS LOGICAL    NO-UNDO.


DEFINE IMAGE AboutImage &IF "{&WINDOW-SYSTEM}" BEGINS "MS-WIN" &THEN
                           SIZE-PIXELS 32 BY 32.
                         &ELSE
                           SIZE-PIXELS 42 BY 42.
                         &ENDIF

DEFINE RECTANGLE TopLine    
  EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL SIZE 55 BY .08.
DEFINE RECTANGLE BottomLine LIKE TopLine.
DEFINE RECTANGLE ContainerRectangle
  EDGE-PIXELS 2 SIZE-PIXELS 40 BY 40 BGCOLOR 8.

DEFINE BUTTON BtnOK AUTO-END-KEY
  LABEL "OK":l SIZE 10 BY 1.

DEFINE FRAME Dialog-1
&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
  ContainerRectangle AT ROW 1.5 COLUMNS 3
  AboutImage AT ROW 1.67 COLUMNS 3.57
  AboutText1 NO-LABELS AT ROW 1.75 COLUMNS {&StartCol}
             VIEW-AS EDITOR SIZE 55 BY 6 NO-BOX
  SKIP
  AboutText2 NO-LABELS AT {&StartCol}
             VIEW-AS EDITOR SIZE 55 BY 5 SCROLLBAR-VERTICAL
  SKIP(1)
  TopLine AT 11 SKIP(0.5)
  Label1 AT {&StartCol} NO-LABELS SKIP(.15)
  Label2 AT {&StartCol} NO-LABELS SKIP(0.5)
  BottomLine AT 11 SKIP(1)
  BtnOK AT 27 SKIP(1)
  WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER
  SIDE-LABELS NO-UNDERLINE SCROLLABLE DEFAULT-BUTTON BtnOK.
&ELSE
  ContainerRectangle AT ROW 1.5 COLUMNS 3
  AboutText1 NO-LABELS AT ROW 1.75 COLUMNS {&StartCol}
             VIEW-AS EDITOR SIZE 50 BY 10
  SKIP
  /* currently empty 
  AboutText2 NO-LABELS AT {&StartCol}
             VIEW-AS EDITOR SIZE 50 BY 5 SCROLLBAR-VERTICAL
  SKIP
  */
  TopLine AT 11     SKIP
  BottomLine AT 11  SKIP(1)
  BtnOK AT 27       SKIP
  WITH VIEW-AS DIALOG-BOX SIDE-LABELS DEFAULT-BUTTON BtnOK.
&ENDIF

 
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ? THEN
  FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} 
  APPLY "END-ERROR":u TO SELF.

/*---------------------------------------------------------------------------*/
main-block:
DO ON ERROR   UNDO main-block, LEAVE main-block
   ON END-KEY UNDO main-block, LEAVE main-block:

  /* general assignments... */
  ASSIGN
    FRAME {&FRAME-NAME}:THREE-D = SESSION:THREE-D
    FRAME {&FRAME-NAME}:TITLE   = "About " + pTitle
    BtnOK:WIDTH                 = IF SESSION:WINDOW-SYSTEM = "TTY":u THEN 
                                    4 ELSE 14
    BtnOK:HEIGHT                = IF SESSION:WINDOW-SYSTEM = "TTY":u THEN 
                                    1 ELSE 1.14
    BtnOK:X                     = (FRAME {&FRAME-NAME}:WIDTH-PIXELS / 2) 
                                - (BtnOK:WIDTH-PIXELS / 2).

  &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
    ASSIGN
      AboutImage:AUTO-RESIZE = TRUE
      result                 = AboutImage:LOAD-IMAGE(pIcon).

    /* Check for WebSpeed Workshop */
    RUN adeshar/_ablic.p (INPUT NO /* ShowMsgs */ , OUTPUT ABLic, OUTPUT ABTools).
    IF ABLic = 2 THEN Workshop-Only = TRUE.
    ELSE Workshop-Only = FALSE.
  &ENDIF 

 RUN GetPatchLevel(OUTPUT patchLevel). /* Read patch level from version file */

 IF Workshop-Only THEN
 DO:
   ASSIGN dot          = INDEX(PROVERSION,".":U)
          majorversion = INT(SUBSTRING(PROVERSION,1,(dot - 1)))
          minorversion = SUBSTRING(PROVERSION,(dot + 1))
          ProName      = "WebSpeed Workshop ":U + 
            STRING(majorversion - 6) + ".":U + minorversion + patchLevel.
 END.
 ELSE
 DO:
 /* Are we running with DBE PROGRESS or not? Test the LENGTH of a value 
   * that is double byte in all 4 languages.  If the RAW LENGTH equals the 
   * character LENGTH, then either the core is not DBE, or the character 
   * set is not one of the 4...
   */
  ASSIGN
    DBEtestvalue = CHR(224) + CHR(164)

    ProName      = IF ( OPSYS <> "WIN32" OR SESSION:DISPLAY-TYPE <> "TTY" OR SESSION:BATCH)
                      AND LENGTH(DBEtestvalue,"CHARACTER":u) <>
                          LENGTH(DBEtestvalue,"RAW":u) THEN
                     "DBE PROGRESS ":u + pTitle
                   ELSE
                     "PROGRESS ":u + pTitle.
  END.
  
  /* Read the commercial version from the "version" text file */
  /* This is going to pick up either Dynamics or DLC version  */
  ASSIGN 
      cTextFile = SEARCH("version":U)
      cCommercialVer = "".
  IF cTextFile <> "" AND ctextFile <> ? THEN DO:
      INPUT FROM VALUE(cTextFile) NO-ECHO.
      REPEAT:
          IMPORT UNFORMATTED cLine.
          ASSIGN cCommercialVer = cCommercialVer + cLine + CHR(10).
      END.
      INPUT CLOSE.
  END.

  /* If this is MS-WINDOWS, go out and make some WIN API calls */
  IF SESSION:WINDOW-SYSTEM BEGINS "MS-WIN":u THEN
    RUN adecomm/_winsys.p (OUTPUT Label1, OUTPUT Label2).
  ELSE
    ASSIGN
      Label1 = "Use the ""showcfg"" utility at the operating " +
               "system prompt for licensing information.".
  
  RUN Realize.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.

HIDE FRAME Dialog-1.

RETURN.

/*---------------------------------------------------------------------------*/
PROCEDURE Realize:
    
DO WITH FRAME {&FRAME-NAME}:
  DEFINE VARIABLE cCopyRight AS CHAR NO-UNDO.
                      
  &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
     cCopyRight = "Copyright �".
  &ELSE
     cCopyRight = "Copyright". /* According to Legal 10/14/2009 */
  &ENDIF

  AboutText1 =
    ProName + CHR(10) +

    (IF Workshop-Only
     THEN pTitle + " (Build: ":U + PROVERSION + patchLevel + ")":U 
     ELSE (IF INTEGER(ENTRY(1,PROVERSION,".")) > 9 THEN "Release ":U ELSE "Version ":U) + PROVERSION + patchLevel
    ) + CHR(10) + CHR(10) +
    (IF cCommercialVer <> "" AND cCommercialVer <> ?
     THEN cCommercialVer + CHR(10) 
     ELSE ""
    ) +
    cCopyright + " 1984-2009 Progress Software Corporation and/or its subsidiaries or affiliates. "
    + "All rights reserved.".

 IF NOT SESSION:WINDOW-SYSTEM BEGINS "TTY":u THEN
  AboutText2 = AboutText2 +
        "OpenEdge includes Infragistics NetAdvantage for .NET v2009 Vol 2. " +  
        cCopyright + " 1996-2009 Infragistics, Inc. " +  
        "All rights reserved." +
        CHR(10) + CHR(10).

  IF NOT SESSION:WINDOW-SYSTEM BEGINS "TTY":u THEN
      AboutText2 = AboutText2 +
         "OpenEdge includes Imaging Technology copyrighted by Snowbound Software Corporation 1993 - 2003." + CHR(10) +
         "www.snowbound.com" +
         CHR(10) + CHR(10).

  ASSIGN AboutText1:READ-ONLY = TRUE
         AboutText1:TAB-STOP  = NO   
         AboutText1:SENSITIVE = SESSION:WINDOW-SYSTEM <> "TTY":U.
  
  &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
      ASSIGN
          AboutText2:READ-ONLY = TRUE
          AboutText2:TAB-STOP  = NO   
          AboutText2:SENSITIVE = SESSION:WINDOW-SYSTEM <> "TTY":U
          NO-ERROR.
  &ENDIF
  
  &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
  DISPLAY
    AboutText1
    AboutText2
    Label1 
    Label2 
    WITH FRAME {&FRAME-NAME}.
  &ELSE
  DISPLAY
    AboutText1
  /*  AboutText2  currently empty */
    WITH FRAME {&FRAME-NAME}.
  &ENDIF

  /* Enable everything so we can test some enabled properties */
  ENABLE btnOK WITH FRAME {&FRAME-NAME}.

  IF FRAME {&FRAME-NAME}:THREE-D THEN
    ASSIGN
      ContainerRectangle:HIDDEN = FALSE
      result                    = ContainerRectangle:MOVE-TO-BOTTOM()
      TopLine:EDGE-PIXELS       = 2
      BottomLine:EDGE-PIXELS    = TopLine:EDGE-PIXELS.
  ELSE
    ASSIGN
      ContainerRectangle:HIDDEN = TRUE
      TopLine:EDGE-PIXELS       = 1
      BottomLine:EDGE-PIXELS    = TopLine:EDGE-PIXELS.
END. /* DO WITH FRAME */

END PROCEDURE.
  
PROCEDURE GetPatchLevel:
  /* Reads the Version file to see if there is a patch level */
  DEFINE OUTPUT PARAMETER patchLevel AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i        AS INTEGER             NO-UNDO.
  DEFINE VARIABLE dlcValue AS CHARACTER           NO-UNDO. /* DLC */
  DEFINE VARIABLE inp      AS CHARACTER           NO-UNDO. /* hold 1st line of version file */
    
  IF OPSYS = "Win32":U THEN /* Get DLC from Registry */
    GET-KEY-VALUE SECTION "Startup":U KEY "DLC":U VALUE dlcValue.

  IF (dlcValue = "" OR dlcValue = ?) THEN DO:
    ASSIGN dlcValue = OS-GETENV("DLC":U). /* Get DLC from environment */
      IF (dlcValue = "" OR dlcValue = ?) THEN DO: /* Still nothing? */
        ASSIGN patchLevel = "".
        RETURN.
      END.
  END.
  FILE-INFO:FILE-NAME = dlcValue + "/version":U.
  IF FILE-INFO:FULL-PATHNAME NE ? THEN DO: /* Read the version file */
    INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME).
      IMPORT UNFORMATTED inp. /* Get the first line */
    INPUT CLOSE.
    /* 
     * As of version 9.1D just append everything from the version file
     * after the version from PROVERSION property
     */
    LEVEL:
    DO i = 2 TO NUM-ENTRIES(inp," ":U):
      IF ENTRY(i,inp," ") BEGINS PROVERSION THEN DO:
        ASSIGN patchLevel = REPLACE(ENTRY(i,inp," "),PROVERSION,"").
        LEAVE LEVEL.
      END.
    END.
  END.         
END PROCEDURE.

&UNDEFINE FRAME-NAME

/* _about.p - end of file */
