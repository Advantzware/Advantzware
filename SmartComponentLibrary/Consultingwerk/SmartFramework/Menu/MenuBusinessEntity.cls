/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/ 
/*------------------------------------------------------------------------
    File        : MenuBusinessEntity
    Purpose     : Business Entity for Menu
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 11.10.2012 01:41:25
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                              FROM PROPATH .
USING Consultingwerk.Framework.*                    FROM PROPATH .
USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.SmartFramework.*               FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.*          FROM PROPATH .
USING Consultingwerk.Util.*                         FROM PROPATH . 
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Menu.MenuBusinessEntity 
    INHERITS SmartBusinessEntity: 

    { Consultingwerk/SmartFramework/Menu/dsMenu.i }
    { Consultingwerk/SmartFramework/Menu/dsMenuStyle.i }
    { Consultingwerk/SmartFramework/Authorization/dsMenuGroup.i }
    
    DEFINE VARIABLE oSecurityService                   AS ISecurityService NO-UNDO . 
    DEFINE VARIABLE lPerformMenuStructureSecurityCheck AS LOGICAL          NO-UNDO INIT TRUE .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MenuBusinessEntity class                                                                     
        Notes:   Passes the handle of the dataset instance to the base class and 
                 sets the default DataAccessName                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuBusinessEntity ():
        DEFINE VARIABLE oConfigurationProvider AS IConfigurationProvider NO-UNDO .
        
        SUPER (DATASET dsMenu:HANDLE).
        
        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Menu.MenuDataAccess":U .

        oSecurityService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Authorization.ISecurityService} .

        oConfigurationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider}  .
        
        IF VALID-OBJECT (oConfigurationProvider) THEN 
            lPerformMenuStructureSecurityCheck = DataTypeHelper:ToLogical (oConfigurationProvider:GetValue ("performMenuStructureSecurityCheck":U,
                                                                                                            "true":U)) . 

    END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	    Purpose: Returns Menu Items based on a list of FunctionGuids to the client
	    Notes:
        @param dsMenu The dataset with the Menu Structure
        @param poKeys The CharacterHolder instance with the comma delimited list of Menu Function Keys, CHR(1) delimited
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID GetMenuItems (INPUT-OUTPUT DATASET dsMenu,
	                                 poKeys AS CharacterHolder):
		
        DEFINE VARIABLE oRequest AS FetchDataRequest NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER          NO-UNDO .
        DEFINE VARIABLE cQuery   AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE cValues  AS CHARACTER        NO-UNDO .

        ASSIGN cValues = poKeys:Value
               cQuery  = "FOR EACH eSmartFunction":U . 

        DO i = 1 TO NUM-ENTRIES (cValues, CHR (1)):
            ASSIGN cQuery = cQuery + 
                            (IF i = 1 THEN " WHERE ":U ELSE " OR ":U) +
                            SUBSTITUTE ("eSmartFunction.FunctionGuid = &1":U, QUOTER (ENTRY (i, cValues, CHR (1)))) .
        END.

        oRequest = NEW FetchDataRequest ("eSmartFunction":U,
                                         cQuery,
                                         0) .
		
		THIS-OBJECT:FetchData (oRequest) .
		
		IF lPerformMenuStructureSecurityCheck AND VALID-OBJECT (oSecurityService) THEN DO:
            FOR EACH eSmartFunction ON ERROR UNDO, THROW:
                
                IF oSecurityService:IsRestricted ("SMF":U,
                                                 eSmartFunction.FunctionGuid) THEN 
                    DELETE eSmartFunction  .
            END.
		END.
		
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the eSmartMenu records for a comma delimited list of MenuGuid's  
        Notes:   
        @param dsMenu The dataset with the Menu Structure
        @param poMenuGuids The CharacterHolder instance with the comma delimited list of MenuGuid's
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID GetMenus (INPUT-OUTPUT DATASET dsMenu,
	                             poMenuGuids AS CharacterHolder):
		
        DEFINE VARIABLE oRequest AS FetchDataRequest NO-UNDO . 
        DEFINE VARIABLE cQuery   AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER          NO-UNDO .
        
        DATASET dsMenu:EMPTY-DATASET () . 
        
        IF NUM-ENTRIES (poMenuGuids:Value) = 0 THEN RETURN . 

        ASSIGN cQuery = "FOR EACH eSmartMenu WHERE ":U .
        
        DO i = 1 TO NUM-ENTRIES (poMenuGuids:Value):

            IF i > 1 THEN 
                cQuery = cQuery + " OR ":U .

            ASSIGN cQuery = cQuery + SUBSTITUTE ("eSmartMenu.MenuGuid = &1":U,
                                                 QUOTER (ENTRY (i, poMenuGuids:Value))) .
        END.

        oRequest = NEW FetchDataRequest ("eSmartMenu":U, 
                                         cQuery) .        

        THIS-OBJECT:FetchData (oRequest) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the eSmartMenu records for a comma delimited list of user group  
                 guids
        Notes:   
        @param dsMenu The dataset with the Menu Structure
        @param poGroups The CharacterHolder class that contains the comma delimited list of user groups 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetMenusForGroups (INPUT-OUTPUT DATASET dsMenu,
                                          poGroups AS CharacterHolder):

        DEFINE VARIABLE oMenuGuids AS CharacterHolder NO-UNDO . 

        DATASET dsMenuGroup:EMPTY-DATASET () .      

        /* The the menus of the user groups */
        ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Authorization.MenuGroupBusinessEntity":U,
                                       "GetUserGroupMenus":U,
                                       INPUT-OUTPUT DATASET dsMenuGroup,
                                       poGroups) .
        
        oMenuGuids = NEW CharacterHolder () . 
        
        FOR EACH eSmartMenuGroup ON ERROR UNDO, THROW:
            ASSIGN oMenuGuids:Value = oMenuGuids:Value + 
                                      (IF oMenuGuids:Value > "":U THEN ",":U ELSE "":U) + 
                                      eSmartMenuGroup.MenuGuid  .  
        END.
        
        THIS-OBJECT:GetMenus (INPUT-OUTPUT DATASET dsMenu,
                              oMenuGuids) .
        
        FINALLY:
            DATASET dsMenuGroup:EMPTY-DATASET () . 		
        END FINALLY.

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns a recursive menu structure
        Notes:   
        @param phDataset INPUT-OUTPUT DATASET-HANDLE  
        @param oGetMenuStructureParameter 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID GetMenuStructure (INPUT-OUTPUT DATASET-HANDLE phDataset,
	                                     oGetMenuStructureParameter AS IGetMenuStructureParameter):
		
        DEFINE VARIABLE oRequest                  AS FetchDataRequest NO-UNDO .
        DEFINE VARIABLE hQuery                    AS HANDLE           NO-UNDO .
        DEFINE VARIABLE lSavePerformSecurityCheck AS LOGICAL          NO-UNDO.

        ASSIGN lSavePerformSecurityCheck = lPerformMenuStructureSecurityCheck . 

        IF oGetMenuStructureParameter:SkipAuthorizationCheck = TRUE THEN 
            ASSIGN lPerformMenuStructureSecurityCheck = FALSE . 
		
		CREATE DATASET phDataset .
		phDataset:CREATE-LIKE (DATASET dsMenu:HANDLE) .
		
		IF oGetMenuStructureParameter:IncludeRootMenuEntry THEN DO:
            oRequest = NEW FetchDataRequest ("eSmartMenu":U,
                                             SUBSTITUTE ("FOR EACH eSmartMenu WHERE eSmartMenu.MenuGuid = ~"&1~"":U, oGetMenuStructureParameter:RootMenuKey),
                                             1) .
            
            THIS-OBJECT:FetchData (oRequest) .
    
            FOR EACH eSmartMenu ON ERROR UNDO, THROW:
                IF lPerformMenuStructureSecurityCheck AND 
                   eSmartMenu.FunctionGuid > "":U AND 
                   VALID-OBJECT (oSecurityService) AND oSecurityService:IsRestricted ("SMF":U,
                                                                                      eSmartMenu.FunctionGuid) THEN NEXT .
                
                phDataset::eSmartMenu:BUFFER-CREATE () .
                phDataset::eSmartMenu:BUFFER-COPY (BUFFER eSmartMenu:HANDLE) .
                phDataset::eSmartMenu:BUFFER-RELEASE () .    
            END.
		END .
		
        THIS-OBJECT:GetMenuStructure (phDataset::eSmartMenu,
                                      oGetMenuStructureParameter:RootMenuKey,
                                      oGetMenuStructureParameter:NumberOfLevels,
                                      1) .

        DELETE OBJECT phDataset . 

        DATASET dsMenu:EMPTY-DATASET () . 

        FINALLY:
            ASSIGN lPerformMenuStructureSecurityCheck = lSavePerformSecurityCheck .		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns one level of menu data 
        Notes:   
        @param phBuffer The target buffer (eSmartMenu) 
        @param pcParentMenuKey The current parent menu key
        @param piNumberOfLevels The maximum number of levels to return (limit recursion)
        @param piCurrentLevel The current level (recursion)
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID GetMenuStructure (phBuffer AS HANDLE,
	                                        pcParentMenuKey AS CHARACTER,
	                                        piNumberOfLevels AS INTEGER,
	                                        piCurrentLevel AS INTEGER):

        DEFINE VARIABLE oRequest       AS FetchDataRequest NO-UNDO .
        DEFINE VARIABLE hQuery         AS HANDLE           NO-UNDO .
        DEFINE VARIABLE hBuffer        AS HANDLE           NO-UNDO .
        DEFINE VARIABLE hCanFindBuffer AS HANDLE           NO-UNDO .
        
		DEFINE BUFFER eSmartMenu FOR eSmartMenu . 
		
		CREATE BUFFER hBuffer FOR TABLE phBuffer . 
		
		oRequest = NEW FetchDataRequest ("eSmartMenu":U,
                                         SUBSTITUTE ("FOR EACH eSmartMenu WHERE eSmartMenu.ParentMenuGuid = ~"&1~"":U, pcParentMenuKey),
                                         0) .
		
		THIS-OBJECT:FetchData (oRequest) .

        FOR EACH eSmartMenu ON ERROR UNDO, THROW:
            IF lPerformMenuStructureSecurityCheck AND 
               eSmartMenu.FunctionGuid > "":U AND 
               VALID-OBJECT (oSecurityService) AND oSecurityService:IsRestricted ("SMF":U,
                                                                                  eSmartMenu.FunctionGuid) THEN NEXT .
            
            hBuffer:BUFFER-CREATE () .
            hBuffer:BUFFER-COPY (BUFFER eSmartMenu:HANDLE) .
            hBuffer:BUFFER-RELEASE () .    
        END.

        IF piNumberOfLevels > 0 AND piNumberOfLevels < piCurrentLevel THEN 
            RETURN .

        hQuery = QueryHelper:CreatePreparedQuery (hBuffer, 
                                                  SUBSTITUTE ("FOR EACH eSmartMenu WHERE eSmartMenu.ParentMenuGuid = ~"&1~" AND eSmartMenu.HasChild = TRUE":U, 
                                                              pcParentMenuKey)) .
       
        CREATE BUFFER hCanFindBuffer FOR TABLE hBuffer .
       
        DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:
            GetMenuStructure (phBuffer, 
                              hBuffer::MenuGuid,
                              piNumberOfLevels,
                              piCurrentLevel + 1) .  
                              
            /* Mike Fechner, Consultingwerk Ltd. 20.08.2015
               Delete parents with no childs */
            hCanFindBuffer:FIND-FIRST (SUBSTITUTE ("WHERE ParentMenuGuid = &1":U, QUOTER (hBuffer::MenuGuid))) NO-ERROR .

            IF NOT hCanFindBuffer:AVAILABLE THEN DO:                
                hBuffer:BUFFER-DELETE () .
                ErrorHelper:ResetErrorStatus() .
            END.

            hQuery:GET-NEXT () .
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hCanFindBuffer) .     
            GarbageCollectorHelper:DeleteObject (hQuery) .
            GarbageCollectorHelper:DeleteObject (hBuffer) .		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and 
                 Update operations (i.e. population of aggregated values)                                                                     
        Notes:   Invoked during FetchData () and SaveChanges ()                                                                     
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():
        
        DEFINE VARIABLE oModule AS ModuleDatasetModel NO-UNDO.
        DEFINE VARIABLE cModule AS CHARACTER NO-UNDO.

        ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Menu.MenuStyleBusinessEntity":U,
                                    NEW FetchDataRequest ("eSmartMenuStyle":U),
                                    OUTPUT DATASET dsMenuStyle) .
        
        FOR EACH eSmartMenu ON ERROR UNDO, THROW:
            FIND eSmartMenuStyle WHERE eSmartMenuStyle.MenuStyleGuid = eSmartMenu.MenuStyleGuid
                NO-ERROR . 
                
            IF AVAILABLE eSmartMenuStyle THEN 
                eSmartMenu.MenuStyleCode = eSmartMenuStyle.MenuStyleCode .
            ELSE  
                ASSIGN eSmartMenu.MenuStyleCode = "":U
                       /* reset error status */ 
                       ERROR-STATUS:ERROR       = FALSE NO-ERROR . 
        END.
        
        FOR EACH eSmartFunction BREAK BY eSmartFunction.FunctionModuleGuid:
    
            IF FIRST-OF (eSmartFunction.FunctionModuleGuid) THEN DO:
                oModule = NEW ModuleDatasetModel (eSmartFunction.FunctionModuleGuid) .
                IF oModule:SmartModule:Available THEN 
                    ASSIGN cModule = oModule:SmartModule:ModuleName .
                ELSE 
                    ASSIGN cModule = ? .    
            END.
    
            ASSIGN eSmartFunction.ModuleName = cModule . 
    
        END.    

        FINALLY:
            DATASET dsMenuStyle:EMPTY-DATASET () .		
        END FINALLY.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook for high level data validation before Update 
                 operations                                                                     
        Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
                 is set, the Update operation will be cancelled before writing back
                 the data to the database using the DataAccess object                                                                      
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ValidateData ():
        
    END METHOD.

END CLASS.
