/* fg/rd-fgexp.w */

DEFINE INPUT PARAMETER ipcItemFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcItemTo   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcItemNameFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcItemNameTo   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcCustPartFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcCustPartTo   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcCustFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcCustTo   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcEstFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcEstTo   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcStyleFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcStyleTo   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcCategoryFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcCategoryTo   AS CHAR NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i} 

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DO TRANSACTION:
   {sys/inc/runAOAVer.i "IF1" }
END.

cAOAFile = SEARCH("AOA/rd-fgexp.p").
IF RunAOAVersion1-log EQ ? AND cAOAFile NE ? THEN RUN AOA/rd-fgexp.p.
ELSE RUN fg/rd-fgexpN.w PERSISTENT
        (ipcItemFrom,
         ipcItemTo,
         ipcItemNameFrom,
         ipcItemNameTo,
         ipcCustPartFrom,
         ipcCustPartTo,
         ipcCustFrom,
         ipcCustTo,
         ipcEstFrom,
         ipcEstTo,
         ipcStyleFrom,
         ipcStyleTo,
         ipcCategoryFrom,
         ipcCategoryTo
        ).
