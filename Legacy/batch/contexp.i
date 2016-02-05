/* ----------------------------------------------- batch/contexp.i   */
/* Download Contact Program for N-R-5                            */
/* -------------------------------------------------------------------------- */

DEFINE VARIABLE olNameSpace     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hOutlook        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE oDLs            AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE oContacts       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE oFolder         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE oEntries        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE oEntry          AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE oDL             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE oItem           AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE oRecipients     AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE iCnt           AS INTEGER NO-UNDO.
DEFINE VARIABLE cDistList      AS CHARACTER NO-UNDO.

CREATE 'Outlook.Application':U hOutlook CONNECT NO-ERROR.
IF NOT VALID-HANDLE(hOutlook)
   THEN MESSAGE "Outlook Application issue."  VIEW-AS ALERT-BOX.
if valid-handle(hOutlook) then do:
   olNameSpace   = hOutlook:GetNameSpace("MAPI":u). 
   IF NOT VALID-HANDLE(olNameSpace)
   THEN MESSAGE "NameSpace issue."  VIEW-AS ALERT-BOX.
   ASSIGN cDistList = list-name:SCREEN-VALUE .
   /* Delete Existing Distribution List Starts */   
   oDLs = olNameSpace:AddressLists.
   oContacts  = oDLs:Item("Contacts").   
   oEntries = oContacts:AddressEntries.   
   oDL = oEntries:Item(cDistList) NO-ERROR.
   IF oDL <> ? THEN           
   oDL:Delete.   
   /* Delete Existing Distribution List Starts */
   /* Create New DL Starts */   
   oFolder = olNameSpace:GetDefaultFolder(10).
   oDL = oFolder:Items:Add(7).
   oDL:DLName = cDistList.
   oDL:Save.
   oItem = hOutlook:CreateItem(0).
   oRecipients = oItem:Recipients.  /*Resolve the current user just added by Email Address*/
   IF NOT VALID-HANDLE(oRecipients)
   THEN MESSAGE "Recipients Issue."  VIEW-AS ALERT-BOX.
   ASSIGN  iCnt = 0.
   FOR EACH ttcontact NO-LOCK:
       ASSIGN iCnt = iCnt + 1.
       IF iCnt > 100 THEN LEAVE.
       oRecipients:Add(ttcontact.email).  /* Add email address as a recipient */
   END.

   oRecipients:ResolveAll.                        
   oDL:AddMembers (oRecipients).  /* if count > 109 - it bombs */
   /* Create new DL Ends */
   
end.  /* if valid-handle(hOutlook) then do: */  
 
RELEASE OBJECT hOutlook   NO-ERROR.

