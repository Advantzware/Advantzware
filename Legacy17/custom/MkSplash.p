/* ===================================================================
   file     : MkSplash.p
   by       : Jurjen Dijkstra, 1997
   language : Progress 8.2A on Windows 95
   purpose  : changes the appearance of a normal Progress window
              into a Splash window, e.g. no caption, no border, 
              centered to screen, stay-on-top.
   params   : hClient    = HWND of a client window
              ThinBorder = YES if a WS_BORDER style is wanted
                           NO creates no border at all
   usage    : during mainblock:
              run MkSplash.p ({&WINDOW-NAME}:HWND, YES).
   =================================================================== */
/*============
Splash is the name for a window that is shown during startup of an application. 
It masks a long loading time (for example during establishing connections to remote databases)
and is often used to show the application title, 'licensed to'-info, author name and often a nice picture.
A Splash has no user interaction; a user must simply wait until the show begins. 
Therefore a Splash should not have a title bar and especially no 'close' button.
It's common to display the Splash in the exact center of the screen and 
it also usually 'stays-on-top'. Most people use third-party 3GL languages to create 
a Splash screen because it seems impossible to create one in Progress. 
The downside of using an external program is that it's hard to determine the proper time to close. 
This would be lots easier if the Splash was created in Progress and then it would also be possible 
to give status information (like "now connecting to system.db").
The next procedure accepts the hWnd of a Progress window (not a Frame or Dialog!)
and makes it look like a Splash by removing the title bar and the thick frame, 
centering it to the screen and making it topmost. 
======*/   
 
DEFINE INPUT PARAMETER hClient AS INTEGER.
DEFINE INPUT PARAMETER ThinBorder AS LOGICAL.
 
  {custom/windows.i}
  {custom/ProExtra.i}
 
  DEF VAR hNonclient AS INTEGER NO-UNDO.
  DEF VAR style AS INTEGER NO-UNDO.
  DEF VAR oldstyle AS INTEGER NO-UNDO.
 
  hNonclient = GetParent(hClient).
 
  /* delete the caption and the thickframe */
  RUN GetWindowLong{&A} IN hpApi(hNonclient, {&GWL_STYLE}, OUTPUT style).
  RUN Bit_Remove IN hpExtra(INPUT-OUTPUT style, {&WS_CAPTION}).
  RUN Bit_Remove IN hpExtra(INPUT-OUTPUT style, {&WS_THICKFRAME}).
  RUN SetWindowLong{&A} IN hpApi(hNonclient, {&GWL_STYLE}, style, OUTPUT oldstyle).
 
  /* the next block creates a thin border around the window. 
     This has to be done in a second SetWindowLong */
  IF ThinBorder THEN DO:
    RUN GetWindowLong{&A} IN hpApi(hNonclient, {&GWL_STYLE}, OUTPUT style).
    RUN Bit_Or IN hpExtra(INPUT-OUTPUT style, {&WS_BORDER}).
    RUN SetWindowLong{&A} IN hpApi(hNonclient, {&GWL_STYLE}, style, OUTPUT oldstyle).
  END.
 
  /* The above changes in window styles are usually done before the window is
     created. Now we are actually too late, windows will not respond with an 
     automatic redraw of the window. We will have to force it. This is done by
     calling SetWindowPos with the SWP_FRAMECHANGED flag. 
     Since we are calling SetWindowPos we might as well ask it to perform 
     some other actions, like:
       make this a TOPMOST window,
       change the coordinates (centered to screen)
  */
 
  DEF VAR lpRect AS MEMPTR NO-UNDO.
  DEF VAR WIDTH AS INTEGER NO-UNDO.
  DEF VAR HEIGHT AS INTEGER NO-UNDO.
  DEF VAR ReturnValue AS INTEGER NO-UNDO.
 
  /* the lpRect structure is defined as LEFT,TOP,RIGHT,BOTTOM. */
  SET-SIZE(lpRect) = 4 * {&INTSIZE}.
 
  /* get the dimensions of the client area: */
  RUN GetWindowRect IN hpApi(hClient, 
                             GET-POINTER-VALUE(lpRect), 
                             OUTPUT ReturnValue).
 
  /* let Windows calculate how large the NonClient area must be
     to fit exactly around the Client area: */
  RUN AdjustWindowRect IN hpApi(GET-POINTER-VALUE(lpRect), style, 0, OUTPUT ReturnValue).
 
  /* so these will be the new dimensions of the Nonclient area: */
  WIDTH  =   get-{&INT}(lpRect, 1 + 2 * {&INTSIZE}) 
           - get-{&INT}(lpRect, 1 + 0 * {&INTSIZE}). 
  HEIGHT =   get-{&INT}(lpRect, 1 + 3 * {&INTSIZE}) 
           - get-{&INT}(lpRect, 1 + 1 * {&INTSIZE}). 
 
  SET-SIZE(lpRect) = 0.
 
  /* Do it. SWP_FRAMECHANGED is the most important flag here */
  RUN SetWindowPos IN hpApi
      (hNonclient, 
       -1, /* = HWND_TOPMOST */
       INTEGER((SESSION:WIDTH-PIXELS - WIDTH) / 2), 
       INTEGER((SESSION:HEIGHT-PIXELS - HEIGHT) / 2), 
       WIDTH, 
       HEIGHT, 
       {&SWP_NOACTIVATE} + {&SWP_FRAMECHANGED},
       OUTPUT ReturnValue
      ).
 
RETURN.
 

