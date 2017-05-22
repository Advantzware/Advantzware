{protools/abhack/abhackXml.i}

/* 10-AUG-2007 sla: I thought I would be able to use a dataset:FILL() , but it seems the automatic managmement of relation makes problems when handle fields are used
 as a result, I am going to fill these temp-tables by hand */

/* 14-AUG-2007 sla: now the heditor field will be dumped with an unknown value to make the load easier witout any index conflict */

/* 16-AUG-2007 sla: usage of an extarnal schema file for performances */

DEFINE VARIABLE gcXmlSchemaFile AS CHARACTER   NO-UNDO.
DEFINE VARIABLE glWriteOK       AS LOGICAL     NO-UNDO.

DEFINE BUFFER ttHasBeforeTable FOR tttt.

FIND FIRST ttEdt WHERE ttEdt.hEditor = phEditor.
CREATE tdEdt.
BUFFER-COPY ttEdt TO tdEdt ASSIGN tdEdt.hEditor = ?.

FOR EACH tttt WHERE tttt.hEditor = ttEdt.hEditor AND tttt.cFileName = ttEdt.cFileName:
    /* 04-SEP-2007 sla: don't dump before tables.  We will recreate them at reload */
    FIND FIRST ttHasBeforeTable WHERE
         ttHasBeforeTable.hEditor = tttt.hEditor
     AND ttHasBeforeTable.cBeforeTable = tttt.cttname
     NO-ERROR.
    IF AVAILABLE ttHasBeforeTable THEN NEXT.
    
    
    CREATE tdtt.
    BUFFER-COPY tttt TO tdtt ASSIGN
     tdtt.ittid   = - tttt.ittid /* the point is to avoid a unique index colflict when loading and setting a new real value */
     tdtt.hEditor = ?.
    FOR EACH ttfld WHERE ttfld.ittid = tttt.ittid:
        CREATE tdfld.
        BUFFER-COPY ttfld TO tdfld ASSIGN
         tdfld.ittid = - tttt.ittid. /* the point is to avoid a unique index colflict when loading and setting a new real value */
    END.
    FOR EACH ttIdx WHERE ttIdx.ittid = tttt.ittid:
        CREATE tdIdx.
        BUFFER-COPY ttIdx TO tdIdx ASSIGN tdIdx.ittid = - tttt.ittid. /* the point is to avoid a unique index colflict when loading and setting a new real value */
    END.
END.

FOR EACH ttPreproc WHERE ttPreproc.hEditor = ttEdt.hEditor AND ttPreproc.cFileName = ttEdt.cFileName:
    CREATE tdPreproc.
    BUFFER-COPY ttPreproc TO tdPreproc ASSIGN tdPreproc.hEditor = ?.
END.

FOR EACH ttFunc WHERE ttFunc.hEditor = ttEdt.hEditor AND ttFunc.cFileName = ttEdt.cFileName:
    CREATE tdFunc.
    BUFFER-COPY ttFunc TO tdFunc ASSIGN tdFunc.hEditor = ?.
END.

FOR EACH ttgbuffer WHERE ttgbuffer.hEditor = ttEdt.hEditor AND ttgbuffer.cFileName = ttEdt.cFileName:
    CREATE tdgbuffer.
    BUFFER-COPY ttgbuffer TO tdgbuffer ASSIGN tdgbuffer.hEditor = ?.
END.

FOR EACH ttgVar WHERE ttgVar.hEditor = ttEdt.hEditor AND ttgVar.cFileName = ttEdt.cFileName:
    CREATE tdgVar.
    BUFFER-COPY ttgVar TO tdgVar ASSIGN tdgVar.hEditor = ?.
END.

FOR EACH ttMark WHERE ttMark.hEditor = ttEdt.hEditor:
    CREATE tdMark.
    BUFFER-COPY ttMark TO tdMark ASSIGN tdMark.hEditor = ?.
END.

FOR EACH ttMethod WHERE ttMethod.hEditor = ttEdt.hEditor AND ttMethod.cFileName = ttEdt.cFileName:
    CREATE tdMethod.
    BUFFER-COPY ttMethod TO tdMethod ASSIGN tdMethod.hEditor = ?.
END.

FOR EACH ttProc WHERE ttProc.hEditor = ttEdt.hEditor AND ttProc.cFileName = ttEdt.cFileName:
    CREATE tdProc.
    BUFFER-COPY ttProc TO tdProc ASSIGN tdProc.hEditor = ?.
END.

FOR EACH ttReferedBuffer WHERE ttReferedBuffer.hEditor = ttEdt.hEditor:
    CREATE tdReferedBuffer.
    BUFFER-COPY ttReferedBuffer TO tdReferedBuffer ASSIGN tdReferedBuffer.hEditor = ?.
END.

FOR EACH ttUsing WHERE ttUsing.hEditor = ttEdt.hEditor:
    CREATE tdUsing.
    BUFFER-COPY ttUsing TO tdUsing ASSIGN tdUsing.hEditor = ?. /* 14-AUG-2007 sla: oops, there was a typo here  for the target buffer... (was tdMark) */
END.

FOR EACH ttgLibHandle WHERE ttgLibHandle.hEditor = ttEdt.hEditor:
    CREATE tdgLibHandle.
    BUFFER-COPY ttgLibHandle TO tdgLibHandle ASSIGN tdgLibHandle.hEditor = ?.
END.

gcXmlSchemaFile = SEARCH("{&abhackXmlSchemaPath}/{&abhackXmlSchemaFile}").
IF gcXmlSchemaFile = ? THEN DO:
    FILE-INFO:FILE-NAME = "protools/abhack".
    gcXmlSchemaFile = FILE-INFO:FULL-PATHNAME + "/{&abhackXmlSchemaFile}".

    DATASET dsGlobRsc:WRITE-XMLSCHEMA("file"
                                     ,gcXmlSchemaFile
                                     ,YES /*formatted  no=faster*/).  /* don't use the NO-ERROR option, I prefer it to burst if problem */
END.


DATASET dsGlobRsc:WRITE-XML("file"
                           ,pcDumpFile
                           ,YES /*formatted  no=faster*/
                           ,"" /*encoding ""=utf-8*/
                           ,gcXmlSchemaFile /*schema-location*/
                           ,NO /*write-xmlschema*/ /* required if I want to be able to read it without maintaing another file */
                           ,NO /*min-xmlschema*/
                           ,NO /*write-before-image*/).
