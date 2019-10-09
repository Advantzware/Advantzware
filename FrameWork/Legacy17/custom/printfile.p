/* ================================================================= 
   file    : printfile.p
   dd      : august 16, 1998
   purpose : print a text file
   by      : Jurjen Dijkstra
             mailto:jurjen@global-shared.com
             see also http://www.global-shared.com
   usage   : RUN printfile.p ("printfile.p").
   tested  : only on Windows 98 with local printer (HP Laserjet 4)
   ================================================================= */
DEFINE INPUT PARAMETER p-Filename   AS CHAR NO-UNDO.

/* these variables could also be useful as input parameters: */
DEF VAR p-Landscape   AS LOGICAL NO-UNDO INITIAL No.   /* Orientation is Landscape or Portrait */
DEF VAR p-Silent      AS LOGICAL NO-UNDO INITIAL Yes.  /* show no message boxes (on error)     */
DEF VAR p-Devicename  AS CHAR    NO-UNDO.              /* if "" then default printer           */
DEF VAR p-docname     AS CHAR    NO-UNDO.              /* if "" then p-Filename                */
DEF VAR p-Leftmargin  AS DECIMAL NO-UNDO INITIAL 1.0.  /* left margin (inches) from paper edge */
DEF VAR p-fontsize    AS INTEGER NO-UNDO INITIAL 9.    /* pointsize for bodyfont               */

/* ------ status strings. to be checked by calling program ---------- */

  &GLOB status_nodefaultdevice            "error 1: there is no default printer installed on this system"
  &GLOB status_filenotfound    SUBSTITUTE("error 2: file &1 not found", p-Filename)
  &GLOB status_CreateDC        SUBSTITUTE("error 3: can not create context for device '&1'", p-Devicename)
  &GLOB status_StartDoc                   "error 4: can not create print job"
  &GLOB status_ok                         ""

/* ------ jump to main procedure ------------------------------------ */

  RUN Main.
  IF (NOT p-Silent) AND (RETURN-VALUE<>{&status_ok}) THEN
     MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
  RETURN RETURN-VALUE.

/* ------ WIN32 API definitions   ----------------------------------- */

/* moved to includefile for better readability */
{custom/printfile.i}

/* ------ misc variables -------------------------------------------- */

  DEF VAR chStatus         AS CHAR    NO-UNDO.
  DEF VAR ReturnValue      AS INTEGER NO-UNDO.
  DEF VAR hPrinterDC       AS INTEGER NO-UNDO.
  DEF VAR CurrentY         AS INTEGER NO-UNDO.
  DEF VAR LineHeight       AS INTEGER NO-UNDO.
  DEF VAR PageHeight       AS INTEGER NO-UNDO.
  DEF VAR PageWidth        AS INTEGER NO-UNDO.
  DEF VAR HeaderHeight     AS INTEGER NO-UNDO.
  DEF VAR FooterHeight     AS INTEGER NO-UNDO.
  DEF VAR PageNumber       AS INTEGER NO-UNDO.
  DEF VAR textline         AS CHAR    NO-UNDO.
  DEF VAR PageStarted      AS LOGICAL NO-UNDO.
  DEF VAR MarginLeftPix    AS INTEGER NO-UNDO.

  DEF VAR hBodyFont        AS INTEGER NO-UNDO.
  DEF VAR hHeaderFont      AS INTEGER NO-UNDO.
  DEF VAR hStockFont       AS INTEGER NO-UNDO.
  DEF VAR HeaderFontHeight AS INTEGER NO-UNDO.

  DEF STREAM textfile.

/* ------ main procedure -------------------------------------------- */

PROCEDURE Main :

  FILE-INFO:FILE-NAME = p-Filename.
  IF FILE-INFO:FULL-PATHNAME=? THEN
    RETURN {&status_filenotfound}.

  RUN StartDocument.
  IF chStatus<>"" THEN DO:
     RUN EndDocument.
     RETURN chStatus.
  END.
  RUN GetPrinterInfo.

  INPUT STREAM textfile FROM VALUE(p-Filename).
    REPEAT:
       textline = "".
       IMPORT STREAM textfile UNFORMATTED textline.
       RUN PrintTextLine.
    END.
  INPUT STREAM textfile CLOSE.

  RUN EndDocument.
  RETURN {&status_ok}.

END PROCEDURE.

/* ------ private internal procedures ------------------------------- */

PROCEDURE PrintTextLine :

  IF (NOT PageStarted) 
     OR (CurrentY + LineHeight > PageHeight - FooterHeight) THEN
     RUN NewPage.

  RUN TextOutA( hPrinterDC,
                MarginLeftPix, 
                CurrentY, 
                textline, 
                LENGTH(textline), 
                OUTPUT ReturnValue).
  CurrentY = CurrentY + LineHeight.

END PROCEDURE.


PROCEDURE NewPage :

  IF PageStarted THEN DO:
     RUN EndPage (hPrinterDC, OUTPUT ReturnValue).
     PageStarted = NO.
  END.

  IF NOT PageStarted THEN DO:
     RUN StartPage (INPUT hPrinterDC, OUTPUT ReturnValue).
     PageStarted = YES.
     PageNumber  = PageNumber + 1.
     RUN PrintHeader.
     RUN PrintFooter.
     IF hBodyFont<>0 THEN
        RUN SelectObject(hPrinterDC, hBodyFont, OUTPUT ReturnValue).
     RUN SetTextAlign(hPrinterDC, {&TA_TOP} + {&TA_LEFT}, OUTPUT ReturnValue).
     CurrentY = HeaderHeight.
  END.

END PROCEDURE.

PROCEDURE PrintHeader :

  IF hHeaderFont<>0 THEN
     RUN SelectObject(hPrinterDC, hHeaderFont, OUTPUT ReturnValue).

  RUN PrintInformation ( 0,
                         {&TA_TOP},
                         "",
                         "",
                         p-FileName).
  RUN HorizontalLine(HeaderFontHeight + 5).
  HeaderHeight = HeaderFontHeight + 10.

END PROCEDURE.

PROCEDURE PrintFooter :

  IF hHeaderFont<>0 THEN
     RUN SelectObject(hPrinterDC, hHeaderFont, OUTPUT ReturnValue).

  RUN PrintInformation ( PageHeight,
                         {&TA_BOTTOM},
                         'date ' + STRING(TODAY, "99-99-9999"),
                         'time ' + STRING(TIME, "HH:MM:SS"),
                         'page ' + STRING(PageNumber)).
  RUN HorizontalLine(PageHeight - HeaderFontHeight - 7).
  FooterHeight = HeaderFontHeight + 12.

END PROCEDURE.

PROCEDURE PrintInformation :
  DEFINE INPUT PARAMETER Y           AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER valign      AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER Left-Text   AS CHAR    NO-UNDO.
  DEFINE INPUT PARAMETER Center-Text AS CHAR    NO-UNDO.
  DEFINE INPUT PARAMETER Right-Text  AS CHAR    NO-UNDO.

  IF NOT (left-text="" or left-text=?) THEN DO:
     RUN SetTextAlign(hPrinterDC, valign + {&TA_LEFT}, OUTPUT ReturnValue).
     RUN TextOutA( hPrinterDC,
                   MarginLeftPix,
                   Y,
                   left-text,
                   LENGTH(left-text),
                   OUTPUT ReturnValue).
  END.

  IF NOT (center-text="" or center-text=?) THEN DO:
     RUN SetTextAlign(hPrinterDC, valign + {&TA_CENTER}, OUTPUT ReturnValue).
     RUN TextOutA( hPrinterDC,
                   INTEGER((MarginLeftPix + PageWidth) / 2),
                   Y,
                   center-text,
                   LENGTH(center-text),
                   OUTPUT ReturnValue).
  END.

  IF NOT (right-text="" or right-text=?) THEN DO:
     RUN SetTextAlign(hPrinterDC, valign + {&TA_RIGHT}, OUTPUT ReturnValue).
     RUN TextOutA( hPrinterDC,
                   PageWidth,
                   Y,
                   right-text,
                   LENGTH(right-text),
                   OUTPUT ReturnValue).
  END.

END PROCEDURE.

PROCEDURE HorizontalLine :
   DEFINE INPUT PARAMETER Y AS INTEGER NO-UNDO.
   RUN MoveToEx(hPrinterDC, MarginLeftPix, Y, 0, OUTPUT ReturnValue).
   RUN LineTo  (hPrinterDC, PageWidth, Y, OUTPUT ReturnValue).
END PROCEDURE.

PROCEDURE StartDocument :

    DEF VAR devicespecs      AS CHAR    NO-UNDO.
    DEF VAR lpdocname        AS MEMPTR  NO-UNDO.
    DEF VAR lpdocinfo        AS MEMPTR  NO-UNDO.
    DEF VAR JobID            AS INTEGER NO-UNDO.
    DEF VAR lpDefaultDevmode AS MEMPTR  NO-UNDO.
    DEF VAR lpDevmode        AS MEMPTR  NO-UNDO.


    /* no devicename specified? Then use default printer */
    IF p-devicename="" THEN DO:
       devicespecs = FILL("x", 255).
       RUN GetProfileStringA ("windows":U,
                              "device":U,
                              "-unknown-,,":U,
                              OUTPUT devicespecs,
                              LENGTH(devicespecs),
                              OUTPUT ReturnValue).
       p-devicename = ENTRY(1, devicespecs).
    END.

    IF p-devicename="-unknown-":U THEN DO:
       chStatus = {&status_nodefaultdevice}.
       RETURN.
    END.

    IF p-docname="" or p-docname=? THEN
       p-docname=p-Filename.

    SET-SIZE(lpdocname)     = LENGTH(p-docname) + 1.
    PUT-STRING(lpdocname,1) = p-docname.
    SET-SIZE(lpdocinfo)     = 12.
    PUT-LONG(lpdocinfo, 1)  = GET-SIZE(lpdocinfo).
    PUT-LONG(lpdocinfo, 5)  = GET-POINTER-VALUE(lpdocname).
    PUT-LONG(lpdocinfo, 9)  = 0.

    /* ask for the size of the devmode structure: */
    RUN DocumentPropertiesA (0, 0,
                                p-devicename,
                                0,
                                0,
                                0,
                                output ReturnValue).
    SET-SIZE(lpDefaultDevMode) = ReturnValue.
    SET-SIZE(lpDevMode)        = ReturnValue.

    /* get the default contents of the devmode structure: */
    RUN DocumentPropertiesA (0, 0,
                                p-devicename,
                                GET-POINTER-VALUE(lpDefaultDevMode),
                                0,
                                {&DM_OUT_BUFFER},
                                output ReturnValue).

    /* specify what you want to change: */
    PUT-LONG (lpDefaultDevMode, {&CCHDEVICENAME} +  9) = {&DM_ORIENTATION}.
    IF p-Landscape THEN
       PUT-SHORT(lpDefaultDevMode, {&CCHDEVICENAME} + 13) = {&DMORIENT_LANDSCAPE}.
    ELSE
       PUT-SHORT(lpDefaultDevMode, {&CCHDEVICENAME} + 13) = {&DMORIENT_PORTRAIT}.

    /* create a merged devmode: */
    RUN DocumentPropertiesA (0, 0,
                                p-devicename,
                                GET-POINTER-VALUE(lpDevMode),
                                GET-POINTER-VALUE(lpDefaultDevMode),
                                {&DM_IN_BUFFER} + {&DM_OUT_BUFFER},
                                output ReturnValue).
    RUN CreateDCA ("WINSPOOL":U,
                   p-devicename,
                   0,
                   GET-POINTER-VALUE(lpDevMode),
                   OUTPUT hPrinterDC).

    SET-SIZE(lpDefaultDevMode)    = 0.
    SET-SIZE(lpDevMode) = 0.

    IF hPrinterDC = 0 THEN
       chStatus = {&status_CreateDC}.
    ELSE DO:
       RUN StartDocA(INPUT hPrinterDC,
                     GET-POINTER-VALUE(lpdocinfo), 
                     OUTPUT JobID).
       IF JobID<1 THEN
       chStatus = {&status_StartDoc}.
    END.

    PageStarted         = NO.
    PageNumber          = 0.
    SET-SIZE(lpdocinfo) = 0.
    SET-SIZE(lpdocname) = 0.

END PROCEDURE.

PROCEDURE EndDocument :

    IF PageStarted THEN DO:
       RUN EndPage  (hPrinterDC, OUTPUT ReturnValue).
       PageStarted = NO.
    END.
    IF hBodyFont<>0 THEN DO:
       RUN SelectObject(hPrinterDC, hStockFont, OUTPUT ReturnValue).
       RUN DeleteObject(hBodyFont, OUTPUT ReturnValue).
    END.
    IF hHeaderFont<>0 THEN
       RUN DeleteObject(hHeaderFont, OUTPUT ReturnValue).
    RUN EndDoc   (hPrinterDC, OUTPUT ReturnValue).
    RUN DeleteDC (hPrinterDC, OUTPUT ReturnValue).
    hPrinterDC = 0.

END PROCEDURE.

PROCEDURE GetPrinterInfo :

  DEF VAR devcap AS INTEGER NO-UNDO.

  /* create font for body with specified size */
  DEF VAR emheight AS INTEGER NO-UNDO.
  RUN GetDeviceCaps (hPrinterDC, {&LOGPIXELSY}, OUTPUT devcap).
  RUN MulDiv (p-fontsize, devcap, 72, OUTPUT emheight).
  emheight = 0 - emheight.
  RUN CreateFontA ( emheight,
                    0,   /* width (0=default)                       */
                    0,   /* escapement                              */
                    0,   /* orientation                             */
                    400, /* weight: LIGHT=300, NORMAL=400, BOLD=700 */
                    0,   /* italics                                 */
                    0,   /* underline                               */
                    0,   /* strikeout                               */
                    {&ANSI_CHARSET},
                    {&OUT_DEFAULT_PRECIS},
                    {&CLIP_DEFAULT_PRECIS},
                    {&PROOF_QUALITY},
                    {&FIXED_PITCH} + {&FF_DONTCARE},
                    "Courier New":U,
                    OUTPUT hBodyFont).
  IF hBodyFont<>0 THEN
     RUN SelectObject(hPrinterDC, hBodyFont, OUTPUT hStockFont).
  RUN GetLineHeight(OUTPUT LineHeight).

  /* create font for header and footer */
  RUN GetDeviceCaps (hPrinterDC, {&LOGPIXELSY}, OUTPUT devcap).
  RUN MulDiv (7, devcap, 72, OUTPUT emheight).
  emheight = 0 - emheight.
  RUN CreateFontA ( emheight,
                    0,
                    0,
                    0,
                    400,
                    1,
                    0,
                    0,
                    {&ANSI_CHARSET},
                    {&OUT_DEFAULT_PRECIS},
                    {&CLIP_DEFAULT_PRECIS},
                    {&PROOF_QUALITY},
                    {&VARIABLE_PITCH} + {&FF_DONTCARE},
                    "Arial":U,
                    OUTPUT hHeaderFont).
  IF hHeaderFont<>0 THEN DO:
     RUN SelectObject(hPrinterDC, hHeaderFont, OUTPUT ReturnValue).
     RUN GetLineHeight(OUTPUT HeaderFontHeight).
  END.
  ELSE 
     ASSIGN 
        hHeaderFont      = hBodyFont
        HeaderFontHeight = LineHeight.

  /* get the left margin, in pixels */
  RUN GetDeviceCaps (hPrinterDC, {&LOGPIXELSX}, OUTPUT devcap).
  MarginLeftPix = p-Leftmargin * devcap.
  RUN GetDeviceCaps (hPrinterDC, {&PHYSICALOFFSETX}, OUTPUT devcap).
  MarginLeftPix = MAXIMUM(MarginLeftPix - devcap, 0).

  /* get the page height (well, the printable part) */
  RUN GetDeviceCaps (hPrinterDC, {&VERTRES}, OUTPUT PageHeight).

  /* get the page width (the printable part) */
  RUN GetDeviceCaps (hPrinterDC, {&HORZRES}, OUTPUT PageWidth).

END PROCEDURE.


PROCEDURE GetLineHeight :
    DEFINE OUTPUT PARAMETER p-LH AS INTEGER.

    DEF VAR lpTm AS MEMPTR NO-UNDO.
    SET-SIZE(lpTM)= 15 * 4 + 5.
    RUN GetTextMetricsA (INPUT  hPrinterDC,
                         INPUT  GET-POINTER-VALUE(lpTM),
                         OUTPUT ReturnValue).
    p-LH = GET-LONG(lpTM, 1).
    SET-SIZE(lpTM)=0.

END PROCEDURE.

/* eof */
