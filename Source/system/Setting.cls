 
 /*------------------------------------------------------------------------
    File        : Setting
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Wed Jul 14 05:35:51 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS system.Setting: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
    
    {system/ttSetting.i}

	CONSTRUCTOR PUBLIC Setting (  ):
		SUPER ().
		
		RUN spGetSessionParam (INPUT "Company", OUTPUT cCompany).
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC CHARACTER Delete( ipiSettingID AS INTEGER ):
        DEFINE BUFFER bf-setting FOR setting.
		
		FIND FIRST bf-setting EXCLUSIVE-LOCK
		     WHERE bf-setting.settingID EQ ipiSettingID
		     NO-ERROR.
		IF NOT AVAILABLE bf-setting THEN
		    RETURN "Setting is invalid or locked".
		
		DELETE bf-setting.
		
		RETURN "".
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC CHARACTER Get( ipcSettingName AS CHARACTER ):
		DEFINE BUFFER bf-setting FOR setting.
		DEFINE BUFFER bf-settingType FOR settingType.
		DEFINE BUFFER bf-scope FOR scope.

        FIND FIRST bf-scope NO-LOCK
             WHERE bf-scope.scopeTable  EQ "Company"
               AND bf-scope.scopeField1 EQ cCompany
             NO-ERROR.
		     
	    IF AVAILABLE bf-scope THEN DO:
            FIND FIRST bf-setting NO-LOCK
                 WHERE bf-setting.settingName EQ ipcSettingName
                   AND bf-setting.settingUser EQ USERID("ASI")
                   AND bf-setting.scopeID     EQ bf-scope.scopeID
                   AND bf-setting.inactive    EQ FALSE
                 NO-ERROR.

            IF NOT AVAILABLE bf-setting THEN
                FIND FIRST bf-setting NO-LOCK
                     WHERE bf-setting.settingName EQ ipcSettingName
                       AND bf-setting.scopeID     EQ bf-scope.scopeID
                       AND bf-setting.inactive    EQ FALSE
                     NO-ERROR.
        END.
        
        IF AVAILABLE bf-setting THEN
            RETURN bf-setting.settingValue.
            
        FIND FIRST bf-settingType NO-LOCK
             WHERE bf-settingType.settingName EQ ipcSettingName
             NO-ERROR.
        
        IF AVAILABLE bf-settingType THEN
            RETURN bf-settingType.defaultValue.    		
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID pGetAll( ipcCategory AS CHARACTER, OUTPUT TABLE ttSetting ):
        DEFINE BUFFER bf-setting FOR setting.
        DEFINE BUFFER bf-settingType FOR settingType.
		DEFINE BUFFER bf-scope FOR scope.
		DEFINE BUFFER bf-ttSetting FOR ttSetting.		
		
		FOR EACH bf-settingType NO-LOCK:
		    IF ipcCategory NE "" AND bf-settingType.hasContext EQ FALSE AND LOOKUP(ipcCategory, bf-settingType.categoryTags) EQ 0 THEN
                NEXT.
		    CREATE ttSetting.
		    BUFFER-COPY bf-settingType TO ttSetting.     
		END.

        FOR EACH ttSetting:
            FOR EACH bf-setting NO-LOCK
                WHERE bf-setting.settingTypeID EQ ttSetting.settingTypeID:
                CREATE bf-ttSetting.
                BUFFER-COPY ttSetting TO bf-ttSetting.
                
                BUFFER-COPY bf-setting TO bf-ttSetting.
                
                FIND FIRST bf-scope NO-LOCK
                     WHERE bf-scope.scopeID EQ bf-setting.scopeID
                     NO-ERROR.
                IF AVAILABLE bf-scope THEN
                    BUFFER-COPY bf-scope TO ttSetting.
            END.
        END.
	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID pGetByUser ( ipcUser AS CHARACTER, OUTPUT TABLE ttSetting ):
        DEFINE BUFFER bf-setting FOR setting.
        DEFINE BUFFER bf-settingType FOR settingType.
        DEFINE BUFFER bf-scope FOR scope.
        DEFINE BUFFER bf-ttSetting FOR ttSetting.       

        FOR EACH bf-setting NO-LOCK
            WHERE bf-setting.settingUser EQ ipcUser:                
            CREATE bf-ttSetting.
            BUFFER-COPY bf-setting TO bf-ttSetting.

            FIND FIRST bf-settingType NO-LOCK
                 WHERE bf-settingType.settingTypeID EQ bf-setting.settingTypeID
                 NO-ERROR.
            IF AVAILABLE bf-settingType THEN
                BUFFER-COPY bf-settingType TO ttSetting.
                            
            FIND FIRST bf-scope NO-LOCK
                 WHERE bf-scope.scopeID EQ bf-setting.scopeID
                 NO-ERROR.
            IF AVAILABLE bf-scope THEN
                BUFFER-COPY bf-scope TO ttSetting.
        END.
    END METHOD.
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID GetByCategory(ipcCategory AS CHARACTER, OUTPUT TABLE ttSetting ):
        pGetAll(ipcCategory, OUTPUT TABLE ttSetting).
	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID GetAll(OUTPUT TABLE ttSetting ):
        pGetAll("", OUTPUT TABLE ttSetting).
    END METHOD.
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE INT64 pCreateScope(
	    ipcScopeTable  AS CHARACTER, 
        ipcScopeField1 AS CHARACTER, 
        ipcScopeField2 AS CHARACTER, 
        ipcScopeField3 AS CHARACTER):
        
        DEFINE BUFFER bf-scope FOR scope.

        FIND FIRST bf-scope NO-LOCK
             WHERE bf-scope.scopeTable  EQ ipcScopeTable
               AND bf-scope.scopeField1 EQ ipcScopeField1
               AND bf-scope.scopeField2 EQ ipcScopeField2
               AND bf-scope.scopeField3 EQ ipcScopeField3
             NO-ERROR.
        IF NOT AVAILABLE bf-scope THEN DO:
            CREATE bf-scope.
            ASSIGN
                bf-scope.scopeTable  = ipcScopeTable
                bf-scope.scopeField1 = ipcScopeField1
                bf-scope.scopeField2 = ipcScopeField2
                bf-scope.scopeField3 = ipcScopeField3
                .
        END.
            		
		RETURN bf-scope.scopeID.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC CHARACTER Update( 
        ipiSettingTypeID AS INTEGER, 
        ipiSettingID     AS INTEGER,
        ipcSettingName   AS CHARACTER, 
        ipcSettingValue  AS CHARACTER, 
        ipcSettingDesc   AS CHARACTER, 
        ipcSettingUser   AS CHARACTER, 
        ipcProgramID     AS CHARACTER,
        iplInactive      AS LOGICAL, 
        ipcScopeTable    AS CHARACTER, 
        ipcScopeField1   AS CHARACTER, 
        ipcScopeField2   AS CHARACTER, 
        ipcScopeField3   AS CHARACTER,
        OUTPUT opiSettingID AS INTEGER):
        
        DEFINE VARIABLE iScopeID AS INTEGER NO-UNDO.   
         
        DEFINE BUFFER bf-setting FOR setting.
        DEFINE BUFFER bf-settingType FOR settingType.
		
        FIND FIRST bf-settingType NO-LOCK
             WHERE bf-settingType.settingTypeID EQ ipiSettingTypeID
             NO-ERROR.
        IF NOT AVAILABLE bf-settingType THEN
            RETURN "SettingType ID is not valid".
                    
        FIND FIRST bf-setting EXCLUSIVE-LOCK
             WHERE bf-setting.settingID EQ ipiSettingID
             NO-ERROR.
        IF NOT AVAILABLE bf-setting THEN DO:
            CREATE bf-setting.
            ASSIGN
                bf-setting.settingTypeID      = bf-settingType.settingTypeID
                bf-setting.settingValue       = ipcSettingValue
                bf-setting.Description        = ipcSettingDesc
                bf-setting.programID          = ipcProgramID
                bf-setting.settingUser        = ipcSettingUser                
                .
        END.
        assign
          bf-setting.Description        = ipcSettingDesc
          bf-setting.settingName        = ipcSettingName
          bf-setting.inactive           = iplInactive          
          opiSettingID                  = bf-setting.settingID   .
        
        IF bf-settingType.hasContext AND ipcScopeTable NE "" THEN DO:
            iScopeID = pCreateScope (ipcScopeTable, ipcScopeField1, ipcScopeField2, ipcScopeField3).
            
            bf-setting.scopeID = iScopeID.
        END.
        
        RETURN "".
        
	END METHOD.
    
END CLASS.