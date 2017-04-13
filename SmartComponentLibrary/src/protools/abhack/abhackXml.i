{protools/abhack/ABHackResourcesTT.i  &SHARED="SHARED"}


DEFINE INPUT  PARAMETER pcDumpFile AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER phEditor   AS HANDLE  NO-UNDO.


/* temp-table to duplicate the data in a dataset  */
DEFINE TEMP-TABLE tdEdt            NO-UNDO LIKE ttEdt          .
DEFINE TEMP-TABLE tdfld            NO-UNDO LIKE ttfld          .
DEFINE TEMP-TABLE tdFunc           NO-UNDO LIKE ttFunc         .
DEFINE TEMP-TABLE tdgbuffer        NO-UNDO LIKE ttgbuffer      .
DEFINE TEMP-TABLE tdgVar           NO-UNDO LIKE ttgVar         .
DEFINE TEMP-TABLE tdIdx            NO-UNDO LIKE ttIdx          .
DEFINE TEMP-TABLE tdMark           NO-UNDO LIKE ttMark         .
DEFINE TEMP-TABLE tdMethod         NO-UNDO LIKE ttMethod       .
DEFINE TEMP-TABLE tdProc           NO-UNDO LIKE ttProc         .
DEFINE TEMP-TABLE tdReferedBuffer  NO-UNDO LIKE ttReferedBuffer.
DEFINE TEMP-TABLE tdtt             NO-UNDO LIKE tttt           .
DEFINE TEMP-TABLE tdUsing          NO-UNDO LIKE ttUsing        .
DEFINE TEMP-TABLE tdgLibHandle     NO-UNDO LIKE ttgLibHandle   .
DEFINE TEMP-TABLE tdPreproc        NO-UNDO LIKE ttPreproc      .

DEFINE DATASET dsGlobRsc
 FOR tdEdt, tdtt, tdfld, tdIdx, tdFunc, tdgbuffer, tdgVar, tdMark, tdMethod, tdProc, tdReferedBuffer, tdUsing, tdgLibHandle, tdPreproc
 DATA-RELATION reltdtt            FOR tdEdt, tdtt            RELATION-FIELDS (hEditor, hEditor, cFileName, cFileName)
 DATA-RELATION reltdfld           FOR tdtt, tdfld            RELATION-FIELDS (ittid, ittid)
 DATA-RELATION reltdIdx           FOR tdtt, tdIdx            RELATION-FIELDS (ittid, ittid)
 DATA-RELATION reltdFunc          FOR tdEdt, tdFunc          RELATION-FIELDS (hEditor, hEditor, cFileName, cFileName)
 DATA-RELATION reltdgbuffer       FOR tdEdt, tdgbuffer       RELATION-FIELDS (hEditor, hEditor, cFileName, cFileName)
 DATA-RELATION reltdgVar          FOR tdEdt, tdgVar          RELATION-FIELDS (hEditor, hEditor, cFileName, cFileName)
 DATA-RELATION reltdMark          FOR tdEdt, tdMark          RELATION-FIELDS (hEditor, hEditor)
 DATA-RELATION reltdMethod        FOR tdEdt, tdMethod        RELATION-FIELDS (hEditor, hEditor, cFileName, cFileName)
 DATA-RELATION reltdProc          FOR tdEdt, tdProc          RELATION-FIELDS (hEditor, hEditor, cFileName, cFileName)
 DATA-RELATION reltdReferedBuffer FOR tdEdt, tdReferedBuffer RELATION-FIELDS (hEditor, hEditor)
 DATA-RELATION reltdUsing         FOR tdEdt, tdUsing         RELATION-FIELDS (hEditor, hEditor)
 DATA-RELATION reltdglibhandle    FOR tdEdt, tdgLibHandle    RELATION-FIELDS (hEditor, hEditor)
 DATA-RELATION reltdPreproc       FOR tdEdt, tdPreproc       RELATION-FIELDS (hEditor, hEditor).
