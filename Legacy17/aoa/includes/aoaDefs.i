/* aoaDefs.i */

&IF "{&test}" NE "YES" &THEN
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
    cocode = gcompany
    locode = gloc
    .

&IF "{&aoaCustListForm}" NE "" &THEN
{sys/inc/CustListForm.i ""{&aoaCustListForm}"" }
&ENDIF

&ELSE
DEFINE VARIABLE g_company AS CHARACTER NO-UNDO INITIAL "001".
DEFINE VARIABLE g_loc     AS CHARACTER NO-UNDO INITIAL "MAIN".
&ENDIF

DEFINE VARIABLE paramStr AS CHARACTER NO-UNDO
    INITIAL "{&aoaID},{&aoaProgramID},{&aoaTitle},{&aoaType},{&aoaColumns},{&aoaCustListForm},".
                                                 
&IF DEFINED(excelOnly) NE 0 &THEN
paramStr = paramStr + "aoaExcelOnly".
&ELSE
paramStr = paramStr + "".
&ENDIF

/* used if testing in AppBuilder */.
&IF "{&test}" EQ "YES" &THEN
paramStr = paramStr + ",aoa/param/{&aoaProgramID}w".
&ENDIF
