/* get_dir.p */

&IF "{&OPSYS}":U="WIN32":U &THEN
/* 32-bit definitions, Progress 8.2+ */
&Scoped-define BOOL long
&Scoped-define SHELL "shell32"
&ELSE
/* 16-bit definitions, Progress 7 to 8.1 */
&Scoped-define BOOL short
&Scoped-define SHELL "shell.dll"
&ENDIF

DEFINE INPUT-OUTPUT PARAMETER FolderName AS CHARACTER NO-UNDO.

DEFINE VARIABLE DialogTitle AS CHARACTER INITIAL "Select Directory" NO-UNDO.
DEFINE VARIABLE MAX_PATH AS INTEGER INITIAL 260 NO-UNDO.
DEFINE VARIABLE lpbi AS MEMPTR NO-UNDO.  /* pointer to BROWSEINFO structure */
DEFINE VARIABLE pszDisplayName AS MEMPTR NO-UNDO.
DEFINE VARIABLE lpszTitle AS MEMPTR NO-UNDO.
DEFINE VARIABLE lpItemIDList AS INTEGER NO-UNDO.
DEFINE VARIABLE ReturnValue AS INTEGER NO-UNDO.

{methods/defines/hndldefs.i}

SET-SIZE(lpbi)           = 32.
SET-SIZE(pszDisplayName) = MAX_PATH.
SET-SIZE(lpszTitle)      = LENGTH(DialogTitle) + 1.

PUT-STRING(lpszTitle,1)  = DialogTitle.

PUT-LONG(lpbi, 1) = 0.  /* hwnd for parent */
PUT-LONG(lpbi, 5) = 0.
PUT-LONG(lpbi, 9) = GET-POINTER-VALUE(pszDisplayName).
PUT-LONG(lpbi,13) = GET-POINTER-VALUE(lpszTitle).
PUT-LONG(lpbi,17) = 1. /* BIF_RETURNONLYFSDIRS = only accept a file system directory */
PUT-LONG(lpbi,21) = 0. /* lpfn, callback function */
PUT-LONG(lpbi,25) = 0. /* lParam for lpfn */
PUT-LONG(lpbi,29) = 0.

RUN SHBrowseForFolder (GET-POINTER-VALUE(lpbi),OUTPUT lpItemIDList).

/* parse the result: */
IF lpItemIDList = 0 THEN
FolderName = "".
ELSE
DO:
  FolderName = FILL(" ",MAX_PATH).
  RUN SHGetPathFromIDList (lpItemIDList,OUTPUT FolderName,OUTPUT ReturnValue).
  FolderName = trim(FolderName).
END.   

/* free memory: */
SET-SIZE(lpbi)=0.
SET-SIZE(pszDisplayName)=0.
SET-SIZE(lpszTitle)=0.

/* memory leak:
   lpItemIDList still points to something 
   but I don't know how to free it from Progress */

{methods/nowait.i}

PROCEDURE SHBrowseForFolder EXTERNAL {&SHELL} :
  DEFINE INPUT  PARAMETER  lpbi         AS LONG.
  DEFINE RETURN PARAMETER  lpItemIDList AS LONG.
END PROCEDURE.

PROCEDURE SHGetPathFromIDList EXTERNAL {&SHELL} :
  DEFINE INPUT  PARAMETER  lpItemIDList AS LONG.
  DEFINE OUTPUT PARAMETER  pszPath      AS CHAR.
  DEFINE RETURN PARAMETER  ReturnValue  AS {&BOOL}.
END PROCEDURE.
