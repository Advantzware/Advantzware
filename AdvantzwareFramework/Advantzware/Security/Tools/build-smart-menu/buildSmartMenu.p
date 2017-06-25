/* buildSmartMenu.p */

/* set buildMenu to no if testing */
&SCOPED-DEFINE buildMenu

/* ***************************  Definitions  ************************** */
USING Consultingwerk.SmartFramework.* FROM PROPATH.

DEFINE VARIABLE oCallParameter  AS RunProcedureCallParameter NO-UNDO .  
DEFINE VARIABLE cRootMenuGuid   AS CHARACTER NO-UNDO .
DEFINE VARIABLE cParentMenuGuid AS CHARACTER NO-UNDO .
DEFINE VARIABLE cFunctionGuid   AS CHARACTER NO-UNDO .

DEFINE TEMP-TABLE ttblItem NO-UNDO 
    FIELD menuOrder AS INTEGER 
    FIELD menu1     AS CHARACTER 
    FIELD menu2     AS CHARACTER 
    FIELD seq       AS INTEGER 
    FIELD mneumonic AS CHARACTER 
    FIELD adm       AS LOGICAL
    FIELD menuImage AS CHARACTER
        INDEX ttblItems IS PRIMARY UNIQUE menuOrder menu2
        INDEX menu2 menu2 menuOrder
        .
    
DEFINE TEMP-TABLE ttblMenu NO-UNDO 
    FIELD menuName  AS CHARACTER 
    FIELD menuCount AS INTEGER 
    FIELD menuGuid  AS CHARACTER 
    FIELD mneumonic AS CHARACTER 
        INDEX ttblMenu IS PRIMARY UNIQUE menuName
        .

/* ***************************  Main Block  *************************** */

/* get current ASI menu structure */
RUN asiMenu .

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

&if "{&buildMenu}" EQ "no" &then
OUTPUT TO "c:\tmp\results.txt" .
&endif

FOR EACH ttblItem:
    FIND FIRST prgrms NO-LOCK WHERE prgrms.prgmname EQ ttblItem.menu1 NO-ERROR .
    IF NOT AVAILABLE prgrms THEN RETURN .

    ASSIGN cFunctionGuid = "" .
    
    IF INDEX (ttblItem.menu1,".") NE 0 THEN 
    RUN asiSmartFunction (BUFFER prgrms, SmartModule.ModuleGuid, ttblItem.mneumonic, ttblItem.adm, ttblItem.menuImage, ttblItem.menu2, OUTPUT cFunctionGuid) .
    
    IF ttblItem.menu2 EQ "file" THEN  
    ASSIGN cParentMenuGuid = cRootMenuGuid .
    ELSE DO:
        FIND FIRST ttblMenu WHERE ttblMenu.menuName EQ ttblItem.menu2 NO-ERROR .
        IF AVAILABLE ttblMenu AND ttblMenu.menuGuid NE "" THEN 
        ASSIGN cParentMenuGuid = ttblMenu.menuGuid .
    END .

    RUN asiSmartMenu (BUFFER prgrms, ttblItem.seq, cFunctionGuid, ttblItem.menuImage, INPUT-OUTPUT cParentMenuGuid) .
    
    IF INDEX (ttblItem.menu1,".") EQ 0 THEN DO:
        FIND FIRST ttblMenu WHERE ttblMenu.menuName EQ ttblItem.menu1 NO-ERROR .
        IF AVAILABLE ttblMenu THEN 
        ASSIGN ttblMenu.menuGuid = cParentMenuGuid .
    END .
&if "{&buildMenu}" EQ "no" &then        
    DISPLAY
        ttblItem.seq FORMAT "z9"
        ttblItem.menu1
        ttblItem.menu2
        ttblItem.menuOrder
        prgrms.prgtitle
        prgrms.dir_group
        prgrms.prgmname
        ttblItem.mneumonic
        ttblItem.adm
        ttblItem.menuImage
            WITH WIDTH 200 STREAM-IO .
&endif    
END.

&if "{&buildMenu}" EQ "no" &then
FOR EACH ttblMenu :
    DISPLAY 
        ttblMenu.menuName
        ttblMenu.mneumonic
        ttblMenu.menuCount
            WITH STREAM-IO .
END.
OUTPUT CLOSE .
OS-COMMAND NO-WAIT notepad.exe "c:\tmp\results.txt" .
&endif

PROCEDURE asiMenu:
    DEFINE VARIABLE cPrgrm     AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cMenu      AS CHARACTER NO-UNDO .
    DEFINE VARIABLE lAdm       AS LOGICAL   NO-UNDO .
    DEFINE VARIABLE cType      AS CHARACTER NO-UNDO .
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO .
    DEFINE VARIABLE cMneumonic AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cImage     AS CHARACTER NO-UNDO .

    INPUT FROM VALUE(SEARCH("AdvantzwareFramework\Advantzware\Security\Tools\build-smart-menu\winKitMenu.txt")) NO-ECHO .
    REPEAT:
        IMPORT cPrgrm cMenu lAdm cType cImage .
        
        IF CAN-DO ("mainmenu",cMenu) THEN NEXT .
        IF CAN-DO ("rule,skip",cPrgrm) THEN NEXT .
        
/*        IF cMenu EQ "file" THEN ASSIGN cMenu = cPrgrm .*/
        
        FIND FIRST prgrms NO-LOCK WHERE prgrms.prgmname EQ cPrgrm NO-ERROR .
/*        ASSIGN cMneumonic = SUBSTRING (prgrms.prgtitle,1,1) WHEN AVAILABLE prgrms .*/

        FIND FIRST ttblMenu WHERE ttblMenu.menuName EQ cMenu NO-ERROR .
/*        IF  AVAILABLE ttblMenu THEN                          */
/*        ASSIGN cMneumonic = ttblMenu.mneumonic + cMneumonic .*/

        IF INDEX (cPrgrm,".") EQ 0 THEN DO:
            FIND FIRST ttblMenu WHERE ttblMenu.menuName EQ cPrgrm NO-ERROR .
            IF NOT AVAILABLE ttblMenu THEN DO:         
                CREATE ttblMenu .
                ASSIGN
                    ttblMenu.menuName  = cPrgrm
                    ttblMenu.mneumonic = cMneumonic 
                    .
            END .
        END .
    
        IF NOT AVAILABLE ttblMenu THEN NEXT .
        
        IF NOT CAN-FIND (FIRST prgrms WHERE prgrms.prgmname EQ cPrgrm) THEN NEXT .

/*        IF LENGTH (cMneumonic) EQ 3 THEN                                              */
/*        ASSIGN cMneumonic = SUBSTRING (cMneumonic,1,2) + STRING (ttblMenu.menuCount) .*/
/*                                                                                      */
/*        IF LENGTH (cMneumonic) GT 4 THEN                                              */
/*        cMneumonic = SUBSTRING (cMneumonic,1,4) .                                     */

        CREATE ttblItem .
        ASSIGN 
            idx                = idx + 1
            ttblMenu.menuCount = ttblMenu.menuCount + 1
            ttblItem.menuOrder = idx
            ttblItem.menu1     = cPrgrm
            ttblItem.menu2     = cMenu
            ttblItem.mneumonic = cMneumonic
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
    DEFINE INPUT PARAMETER ipcMneumonic  AS CHARACTER NO-UNDO .
    DEFINE INPUT PARAMETER iplAdm        AS LOGICAL   NO-UNDO .
    DEFINE INPUT PARAMETER ipImage       AS CHARACTER NO-UNDO .
    DEFINE INPUT PARAMETER ipMenuGroup   AS CHARACTER NO-UNDO .
    
    DEFINE OUTPUT PARAMETER opcFunctionGuid AS CHARACTER NO-UNDO .
    
    DEFINE VARIABLE dirGroup AS CHARACTER NO-UNDO .

&if "{&buildMenu}" EQ "no" &then
    IF TRUE THEN RETURN .
&endif
    
    /* create menu function record */
    ASSIGN 
        dirGroup                     = IF ipMenuGroup BEGINS "AOA" THEN "AOA" ELSE prgrms.dir_group
        oCallParameter               = NEW RunProcedureCallParameter () 
        oCallParameter:AllowMultiple = FALSE 
        oCallParameter:ProcedureName = dirGroup + "/" + prgrms.prgmname + "w"
        oCallParameter:RunPersistent = NOT CAN-DO("repstinv.,shipmeth.",prgrms.prgmname)
        .
        
    IF iplAdm THEN 
    ASSIGN 
        oCallParameter:InitializeInternalProcedure          = "dispatch"
        oCallParameter:InitializeInternalProcedureParameter = "initialize"
        oCallParameter:ReactivateInternalProcedure          = "dispatch"
        oCallParameter:ReactivateInternalProcedureParameter = "view"
        .
    
    CREATE SmartFunction . 
    ASSIGN
        SmartFunction.FunctionName          = prgrms.prgtitle
        SmartFunction.FunctionDescription   = "Imported Menu Function"
        SmartFunction.FunctionModuleGuid    = ipcModuleGuid
        SmartFunction.ExternalId            = ipcMneumonic 
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
    
&if "{&buildMenu}" EQ "no" &then
    IF TRUE THEN RETURN .
&endif
    
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
