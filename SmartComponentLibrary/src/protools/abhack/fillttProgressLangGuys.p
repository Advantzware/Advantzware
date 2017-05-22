DEFINE TEMP-TABLE ttProgressLangClass NO-UNDO 
 FIELD cType       AS CHARACTER  
 FIELD cInherits    AS CHARACTER  
 INDEX cType cType.

DEFINE TEMP-TABLE ttProgressLangMember NO-UNDO 
 FIELD cType       AS CHARACTER  
 FIELD cMember     AS CHARACTER  
 FIELD cReturnType AS CHARACTER  
 FIELD cParamsType AS CHARACTER  
 INDEX TypeMember cType cMember.

DEFINE DATASET dsProgressLangMembers FOR ttProgressLangClass, ttProgressLangMember
 DATA-RELATION drType FOR ttProgressLangClass, ttProgressLangMember
   RELATION-FIELDS (ctype, ctype) NESTED.

    
RUN addThisGuy ("Progress.Lang.Class"
               ,""
               ,"Package=CHARACTER,SuperClass=plc,TypeName=CHARACTER,IsFinal()=LOGICAL,IsInterface()=LOGICAL").

RUN addThisGuy ("Progress.Lang.Object"
               ,""
               ,"NEXT-SIBLING=plo,PREV-SIBLING=plo,Clone()=plo,Equals()=LOGICAL,GetClass()=plc,ToString=CHARACTER").

RUN addThisGuy ("Progress.Lang.Error"
               ,""
               ,"CallStack=CHARACTER,NumMessages=INTEGER,Severity=INTEGER,GetMessage()=CHARACTER,GetMessageNum()=INTEGER").

RUN addThisGuy ("Progress.Lang.ProError"
               ,"Progress.Lang.Object"
               ,"CallStack=CHARACTER,NumMessages=INTEGER,Severity=INTEGER,GetMessage()=CHARACTER,GetMessageNum()=INTEGER").

RUN addThisGuy ("Progress.Lang.SysError"
               ,"Progress.Lang.ProError"
               ,"").

RUN addThisGuy ("Progress.Lang.SoapFaultError"
               ,"Progress.Lang.SysError"
               ,"soapFault=HANDLE").

RUN addThisGuy ("Progress.Lang.AppError"
               ,"Progress.Lang.ProError"
               ,"returnValue=CHARACTER,addMessage(),RemoveMessage()").

DATASET dsProgressLangMembers:WRITE-XML("FILE"
                                       ,"C:/v10Tools/protools/abhack/ProgressLangDefs.xml"
                                       ,YES /* formatted */
                                       ,""  /* encoding */
                                       ,""  /* schema location */
                                       ,YES /* write schema in xlm */
                                       ,NO  /* min schema */
                                       ,NO  /* before table */).

PROCEDURE addThisGuy:    
    DEFINE INPUT PARAMETER pcType     AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcInherits AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcMembers  AS CHARACTER   NO-UNDO.
    
    DEFINE VARIABLE cReturnType AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE eMember     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iMember     AS INTEGER     NO-UNDO.
    
    CREATE ttProgressLangClass.
    ASSIGN
     ttProgressLangClass.cType     = pcType
     ttProgressLangClass.cInherits = pcInherits.

    DO iMember = NUM-ENTRIES(pcMembers) TO 1 BY -1:
     
        eMember = ENTRY(iMember, pcMembers).
        IF INDEX(eMember, "=") > 0 THEN ASSIGN
         cReturnType = ENTRY(2, eMember, "=")
         eMember     = ENTRY(1, eMember, "=").
        
        IF cReturnType = "plo" THEN cReturnType = "Progress.Lang.Object".
        IF cReturnType = "plc" THEN cReturnType = "Progress.Lang.Class".

        FIND FIRST ttProgressLangMember WHERE ttProgressLangMember.cType = ttProgressLangClass.cType
                              AND ttProgressLangMember.cMember = eMember NO-ERROR.
        IF AVAILABLE ttProgressLangMember THEN NEXT.
        
        CREATE ttProgressLangMember.
        ASSIGN
         ttProgressLangMember.cType       = ttProgressLangClass.cType
         ttProgressLangMember.cMember     = eMember
         ttProgressLangMember.cReturnType = cReturnType.
    END.

END PROCEDURE.
