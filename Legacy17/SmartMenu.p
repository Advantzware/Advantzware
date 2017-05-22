/* SmartMenu.p - rstark - 3.2.2017 */

/* ***************************  Definitions  ************************** */
USING Consultingwerk.SmartFramework.* FROM PROPATH.

DEFINE VARIABLE oCallParameter  AS RunProcedureCallParameter NO-UNDO .  
DEFINE VARIABLE cRootMenuGuid   AS CHARACTER NO-UNDO .
DEFINE VARIABLE cParentMenuGuid AS CHARACTER NO-UNDO .
DEFINE VARIABLE cFunctionGuid   AS CHARACTER NO-UNDO .
DEFINE VARIABLE cMneumonic      AS CHARACTER NO-UNDO EXTENT 3.
DEFINE VARIABLE iLevel          AS INTEGER   NO-UNDO INITIAL 1.

DEFINE TEMP-TABLE ttblItem NO-UNDO 
    FIELD menuOrder AS INTEGER 
    FIELD menu1     AS CHARACTER 
    FIELD menu2     AS CHARACTER 
    FIELD seq       AS INTEGER 
    FIELD adm       AS LOGICAL
    FIELD menuImage AS CHARACTER
        INDEX ttblItems IS PRIMARY UNIQUE menuOrder menu2
        INDEX menu2 menu2 menuOrder
        .
    
DEFINE TEMP-TABLE ttblMenu NO-UNDO 
    FIELD menuName  AS CHARACTER 
    FIELD menuCount AS INTEGER 
    FIELD menuGuid  AS CHARACTER 
        INDEX ttblMenu IS PRIMARY UNIQUE menuName
        .

/* ***************************  Main Block  *************************** */

/* find the module record */
FIND SmartModule NO-LOCK WHERE SmartModule.ModuleName EQ "legacy" NO-ERROR .
IF NOT AVAILABLE SmartModule THEN RETURN .

/* root menu structrue */
FIND SmartMenu NO-LOCK
     WHERE SmartMenu.ParentMenuGuid EQ ""
       AND SmartMenu.MenuName       EQ "Advantzware"
     NO-ERROR .
IF NOT AVAILABLE SmartMenu THEN RETURN .

ASSIGN cRootMenuGuid = SmartMenu.MenuGuid .

/* clear out smart functions */
RUN deleteSmartFunction .

/* clear out smart menus */
RUN deleteSmartMenu (cRootMenuGuid) .

/* get current ASI menu structure */
RUN asiMenu .

FOR EACH ttblItem:
    FIND FIRST prgrms NO-LOCK WHERE prgrms.prgmname EQ ttblItem.menu1 NO-ERROR .
    IF NOT AVAILABLE prgrms THEN RETURN .

    ASSIGN cFunctionGuid = "" .
    
    IF INDEX (ttblItem.menu1,".") NE 0 THEN 
    RUN asiSmartFunction (BUFFER prgrms,
                          SmartModule.ModuleGuid,
                          ttblItem.adm,
                          ttblItem.menuImage,
                          ttblItem.menu2,
                   OUTPUT cFunctionGuid) .
    
    IF ttblItem.menu2 EQ "file" THEN  
    ASSIGN cParentMenuGuid = cRootMenuGuid .
    ELSE DO:
        FIND FIRST ttblMenu WHERE ttblMenu.menuName EQ ttblItem.menu2 NO-ERROR .
        IF AVAILABLE ttblMenu AND ttblMenu.menuGuid NE "" THEN 
        ASSIGN cParentMenuGuid = ttblMenu.menuGuid .
    END .

    RUN asiSmartMenu (BUFFER prgrms,
                      ttblItem.seq,
                      cFunctionGuid,
                      ttblItem.menuImage,
        INPUT-OUTPUT cParentMenuGuid) .
    
    IF INDEX (ttblItem.menu1,".") EQ 0 THEN DO:
        FIND FIRST ttblMenu WHERE ttblMenu.menuName EQ ttblItem.menu1 NO-ERROR .
        IF AVAILABLE ttblMenu THEN 
        ASSIGN ttblMenu.menuGuid = cParentMenuGuid .
    END .
END.

RUN setExternalID (cRootMenuGuid, iLevel).

/* change name of .dat file once processed */
OS-RENAME VALUE(SEARCH("SmartMenu.dat")) VALUE(REPLACE(SEARCH("SmartMenu.dat"),".dat",".done")) .

PROCEDURE asiMenu:
    DEFINE VARIABLE cPrgrm     AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cMenu      AS CHARACTER NO-UNDO .
    DEFINE VARIABLE lAdm       AS LOGICAL   NO-UNDO .
    DEFINE VARIABLE cType      AS CHARACTER NO-UNDO .
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO .
    DEFINE VARIABLE cImage     AS CHARACTER NO-UNDO .

    INPUT FROM VALUE(SEARCH("SmartMenu.dat")) NO-ECHO .
    REPEAT:
        IMPORT cPrgrm cMenu lAdm cType cImage .
        
        IF CAN-DO ("mainmenu",cMenu) THEN NEXT .
        IF CAN-DO ("rule,skip",cPrgrm) THEN NEXT .
        
        FIND FIRST prgrms NO-LOCK WHERE prgrms.prgmname EQ cPrgrm NO-ERROR .

        FIND FIRST ttblMenu WHERE ttblMenu.menuName EQ cMenu NO-ERROR .

        IF INDEX (cPrgrm,".") EQ 0 THEN DO:
            FIND FIRST ttblMenu WHERE ttblMenu.menuName EQ cPrgrm NO-ERROR .
            IF NOT AVAILABLE ttblMenu THEN DO:         
                CREATE ttblMenu .
                ttblMenu.menuName  = cPrgrm.
            END .
        END .
    
        IF NOT AVAILABLE ttblMenu THEN NEXT .
        
        IF NOT CAN-FIND (FIRST prgrms WHERE prgrms.prgmname EQ cPrgrm) THEN NEXT .

        CREATE ttblItem .
        ASSIGN 
            idx                = idx + 1
            ttblMenu.menuCount = ttblMenu.menuCount + 1
            ttblItem.menuOrder = idx
            ttblItem.menu1     = cPrgrm
            ttblItem.menu2     = cMenu
            ttblItem.adm       = lAdm
            ttblItem.menuImage = cImage
            .
            
    END . /* repeat */
    INPUT CLOSE.
    
    FOR EACH ttblItem USE-INDEX menu2 BREAK BY ttblItem.menu2 :
        IF FIRST-OF (ttblItem.menu2) THEN ASSIGN idx = 0 .
        ASSIGN
            idx          = idx + 1
            ttblItem.seq = idx
            .
    END .
END PROCEDURE.

PROCEDURE asiSmartFunction:
    DEFINE PARAMETER BUFFER prgrms FOR prgrms .
    
    DEFINE INPUT PARAMETER ipcModuleGuid AS CHARACTER NO-UNDO .
    DEFINE INPUT PARAMETER iplAdm        AS LOGICAL   NO-UNDO .
    DEFINE INPUT PARAMETER ipImage       AS CHARACTER NO-UNDO .
    DEFINE INPUT PARAMETER ipMenuGroup   AS CHARACTER NO-UNDO .
    
    DEFINE OUTPUT PARAMETER opcFunctionGuid AS CHARACTER NO-UNDO .
    
    DEFINE VARIABLE dirGroup AS CHARACTER NO-UNDO .

    /* create menu function record */
    ASSIGN 
        dirGroup                     = IF ipMenuGroup BEGINS "AOA" THEN "AOA" ELSE prgrms.dir_group
        oCallParameter               = NEW RunProcedureCallParameter () 
        oCallParameter:AllowMultiple = FALSE 
        oCallParameter:ProcedureName = dirGroup + "/" + prgrms.prgmname + "w"
        oCallParameter:RunPersistent = TRUE
        .
        
    IF iplAdm EQ YES THEN 
    ASSIGN 
        oCallParameter:InitializeInternalProcedure          = "dispatch"
        oCallParameter:InitializeInternalProcedureParameter = "initialize"
        oCallParameter:ReactivateInternalProcedure          = "dispatch"
        oCallParameter:ReactivateInternalProcedureParameter = "view"
        .
    ELSE IF iplAdm EQ ? THEN 
    ASSIGN 
        oCallParameter:InitializeInternalProcedure          = "dispatch"
        oCallParameter:InitializeInternalProcedureParameter = "initialize"
        .
    
    CREATE SmartFunction . 
    ASSIGN
        SmartFunction.FunctionName          = prgrms.prgtitle
        SmartFunction.FunctionDescription   = "Imported Menu Function"
        SmartFunction.FunctionModuleGuid    = ipcModuleGuid
        SmartFunction.FunctionCallParameter = oCallParameter:Serialize()
        SmartFunction.FunctionSmallImage    = "Graphics/16x16/" + ipImage
        SmartFunction.FunctionLargeImage    = "Graphics/32x32/" + ipImage
        opcFunctionGuid                     = SmartFunction.FunctionGuid
        .
    
    FIND CURRENT SmartFunction NO-LOCK .
END PROCEDURE.

PROCEDURE asiSmartMenu:
    DEFINE PARAMETER BUFFER prgrms FOR prgrms .
    
    DEFINE INPUT        PARAMETER ipSeq          AS INTEGER   NO-UNDO .
    DEFINE INPUT        PARAMETER ipFunctionGuid AS CHARACTER NO-UNDO .    
    DEFINE INPUT        PARAMETER ipImage        AS CHARACTER NO-UNDO .
    DEFINE INPUT-OUTPUT PARAMETER iopMenuGuid    AS CHARACTER NO-UNDO .
    
    DEFINE BUFFER SmartMenu FOR SmartMenu .
    
    /* Find or create "master data" menu */
    FIND FIRST SmartMenu  
         WHERE SmartMenu.ParentMenuGuid EQ iopMenuGuid
           AND SmartMenu.MenuName       EQ prgrms.prgtitle
         NO-ERROR .
           
    IF NOT AVAILABLE SmartMenu THEN DO: 
        CREATE SmartMenu .
        ASSIGN
            SmartMenu.ParentMenuGuid    = iopMenuGuid
            SmartMenu.MenuName          = prgrms.prgtitle
            SmartMenu.MenuSmallImage    = "Graphics/16x16/" + ipImage
            SmartMenu.MenuLargeImage    = "Graphics/32x32/" + ipImage
            SmartMenu.MenuSequence      = ipSeq
            SmartMenu.MenuStructureType = (IF ipFunctionGuid NE "" THEN "Item" ELSE "Menu")
            SmartMenu.FunctionGuid      = (IF ipFunctionGuid NE "" THEN ipFunctionGuid ELSE "")
            . 
        FIND CURRENT SmartMenu NO-LOCK . 
    END .           
               
    ASSIGN iopMenuGuid = SmartMenu.MenuGuid .
END PROCEDURE.

PROCEDURE deleteSmartFunction : 
    FOR EACH SmartFunction EXCLUSIVE-LOCK 
        WHERE SmartFunction.FunctionDescription EQ "Imported Menu Function"
        :
        DELETE SmartFunction .
    END.
END PROCEDURE.

PROCEDURE deleteSmartMenu :
    DEFINE INPUT PARAMETER ipcGuid AS CHARACTER NO-UNDO .
    
    DEFINE BUFFER bSmartMenu FOR SmartMenu .
    
    FOR EACH bSmartMenu EXCLUSIVE-LOCK 
        WHERE bSmartMenu.ParentMenuGuid EQ ipcGuid
        :
        IF bSmartMenu.MenuStructureType EQ "menu" THEN 
        RUN deleteSmartMenu (bSmartMenu.MenuGuid).
        DELETE bSmartMenu .
    END.
END PROCEDURE.

PROCEDURE setExternalID :
    DEFINE INPUT PARAMETER ipcGuid  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiLevel AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iCnt        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cExternalID AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bSmartMenu FOR SmartMenu .
    
    FOR EACH bSmartMenu NO-LOCK
        WHERE bSmartMenu.parentmenuguid EQ ipcGuid
        :
        iCnt = iCnt + 1.
        IF ipiLevel EQ 1 THEN
        ASSIGN
            cMneumonic[1] = substr(bSmartMenu.menuname,1,1)
            cMneumonic[2] = ""
            cMneumonic[3] = ""
            .
        IF ipiLevel EQ 2 THEN
        ASSIGN
            cMneumonic[2] = substr(bSmartMenu.menuname,1,1)
            cMneumonic[3] = ""
            .
        IF ipiLevel EQ 3 THEN
        cMneumonic[3] = STRING(iCnt).
        cExternalID = cMneumonic[1] + cMneumonic[2] + cMneumonic[3].
        IF bSmartMenu.menustructuretype EQ "menu" THEN
        RUN setExternalID (bSmartMenu.menuguid, ipiLevel + 1).
        ELSE DO:
            FIND SmartFunction EXCLUSIVE-LOCK
                 WHERE SmartFunction.FunctionGuid EQ bSmartMenu.functionguid
                 NO-ERROR.
            IF AVAILABLE SmartFunction THEN
            SmartFunction.ExternalId = cExternalID.
        END.
    END.
END PROCEDURE.
