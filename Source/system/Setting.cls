 
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
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.ObjectModelParser.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS system.Setting: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCompany                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUser                        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iUserSecurityLevel           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cScopeTypeWithContextList    AS CHARACTER NO-UNDO INITIAL "System,Company,Customer,ShipTo,Vendor,Location".
    DEFINE VARIABLE cScopeTypeWithoutContextList AS CHARACTER NO-UNDO INITIAL "System,Company".
    DEFINE VARIABLE cSearchFieldListSettingType  AS CHARACTER NO-UNDO INITIAL "settingName,settingTypeDesc,defaultValue".
    DEFINE VARIABLE cSearchFieldListSetting      AS CHARACTER NO-UNDO INITIAL "scopeTable,scopeField1,scopeField2,scopeField3,settingUser,inactive".
    DEFINE VARIABLE cCategoryTags                AS CHARACTER NO-UNDO.
                       
    {system/ttSetting.i}
 
	DEFINE PUBLIC PROPERTY CurrentProgram AS CHARACTER NO-UNDO 
	GET.
	SET.

    DEFINE PUBLIC PROPERTY CurrentProgramHotkey AS CHARACTER NO-UNDO 
    GET.
    SET.
    
	CONSTRUCTOR PUBLIC Setting (  ):
		SUPER ().
		
		pSetContext (PROGRAM-NAME(2), "").
	END CONSTRUCTOR.

    CONSTRUCTOR PUBLIC Setting ( ipcProgramName AS CHARACTER ):
        SUPER ().

        pSetContext (ipcProgramName, "").        
    END CONSTRUCTOR.

    CONSTRUCTOR PUBLIC Setting ( ipcProgramName AS CHARACTER, ipcCategoryTags AS CHARACTER ):
        SUPER ().

        pSetContext (ipcProgramName, ipcCategoryTags).        
    END CONSTRUCTOR.
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC CHARACTER Delete( ipiSettingID AS INT64, iplSaveLocally AS LOGICAL ):
        DEFINE BUFFER bf-setting FOR setting.
		
		IF iplSaveLocally THEN DO:
            FIND FIRST ttSetting EXCLUSIVE-LOCK
                 WHERE ttSetting.settingID EQ ipiSettingID
                 NO-ERROR.
            IF NOT AVAILABLE ttSetting THEN
                RETURN "Setting is invalid or locked".
            
            DELETE ttSetting.
            
            RETURN "".
	    END.
	    
		FIND FIRST bf-setting EXCLUSIVE-LOCK
		     WHERE bf-setting.settingID EQ ipiSettingID
		     NO-ERROR.
		IF NOT AVAILABLE bf-setting THEN
		    RETURN "Setting is invalid or locked".
		
		DELETE bf-setting.
		
        FIND FIRST ttSetting EXCLUSIVE-LOCK
             WHERE ttSetting.settingID EQ ipiSettingID
             NO-ERROR.
        IF AVAILABLE ttSetting THEN
            DELETE ttSetting.
            		
		RETURN "".
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC INTEGER GetSecurityLevel(  ):
		
		RETURN iUserSecurityLevel.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LOGICAL HasProgramContext( INPUT ipcSettingName AS CHARACTER ):
		
		DEFINE VARIABLE lHasProgramContext AS LOGICAL NO-UNDO.
        
        DEFINE BUFFER bf-setting FOR setting.
        
		FIND FIRST bf-setting NO-LOCK
		     WHERE bf-setting.settingName EQ ipcSettingName
		       AND bf-setting.programID   NE ""
		       AND bf-setting.inActive    EQ FALSE
		     NO-ERROR.
		lHasProgramContext = AVAILABLE bf-setting. 

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE VOID pConcatRecordData( BUFFER ipbf-ttSetting FOR ttSetting):
		DEFINE VARIABLE cAllData AS CHARACTER NO-UNDO.
		DEFINE VARIABLE hdBuffer AS HANDLE    NO-UNDO.
		DEFINE VARIABLE hdColumn AS HANDLE    NO-UNDO.
        DEFINE VARIABLE iIndex   AS INTEGER   NO-UNDO.
        
        DEFINE VARIABLE cFieldList AS CHARACTER NO-UNDO.
        
        IF NOT AVAILABLE ipbf-ttSetting THEN
            RETURN.
            
        hdBuffer = BUFFER ipbf-ttSetting:HANDLE.
		
		IF ipbf-ttSetting.recordSource EQ "SettingType" THEN
		    cFieldList = cSearchFieldListSettingType.
		ELSE
		    cFieldList = cSearchFieldListSetting.
		    
		DO iIndex = 1 TO NUM-ENTRIES(cFieldList):
		    hdColumn = hdBuffer:BUFFER-FIELD (ENTRY(iIndex, cFieldList)):HANDLE NO-ERROR.
		    
		    IF VALID-HANDLE (hdColumn) AND hdColumn:BUFFER-VALUE NE ? THEN
		        cAllData = cAllData + "|" + STRING(hdColumn:BUFFER-VALUE, hdColumn:FORMAT).
		END.

        ipbf-ttSetting.allData = cAllData.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE INTEGER pGetPriorityID( ipcScope AS CHARACTER ):
		
		CASE ipcScope:
            WHEN "Location" THEN
                RETURN 1000.
            WHEN "ShipTo" THEN
                RETURN 1000.
            WHEN "Customer" THEN
                RETURN 500.
            WHEN "Vendor" THEN
                RETURN 500.
            WHEN "Company" THEN
                RETURN 200.
            WHEN "System" THEN
                RETURN 0.
            OTHERWISE
                RETURN 0.
        END CASE.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: Returns the scope table and fields for a given scope id
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC CHARACTER pGetScope( 
	    INPUT  ipiScopeID     AS INT64,
	    OUTPUT opcScopeTable  AS CHARACTER,
	    OUTPUT opcScopeField1 AS CHARACTER,
	    OUTPUT opcScopeField2 AS CHARACTER,
	    OUTPUT opcScopeField3 AS CHARACTER):
		
		DEFINE BUFFER bf-scope FOR scope.

        FIND FIRST bf-scope NO-LOCK
             WHERE bf-scope.scopeID EQ ipiScopeID
             NO-ERROR.
        IF AVAILABLE bf-scope THEN DO:
            ASSIGN
                opcScopeTable  = bf-scope.scopeTable
                opcScopeField1 = bf-scope.scopeField1
                opcScopeField2 = bf-scope.scopeField2
                opcScopeField3 = bf-scope.scopeField3
                .
            RETURN "".
        END.
        ELSE
            RETURN "Scope ID '" + STRING (ipiScopeID) + "' does not exist".
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID pSetContext( ipcProgramName AS CHARACTER, ipcCategoryTags AS CHARACTER ):
		
        RUN spGetSessionParam (INPUT "Company", OUTPUT cCompany).
        RUN spGetSessionParam (INPUT "UserID", OUTPUT cUser).
        
        pSetCurrentProgram(ipcProgramName).
        pSetUserSecurityLevel().
        
        cCategoryTags = ipcCategoryTags.
    END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID pSetCurrentProgram( ipcProgramName AS CHARACTER ):
        DEFINE BUFFER bf-prgrms FOR prgrms.

        ASSIGN
            CurrentProgram = ipcProgramName
            CurrentProgram = SUBSTRING(CurrentProgram,1,INDEX(CurrentProgram,"."))
            CurrentProgram = ENTRY(NUM-ENTRIES(CurrentProgram," "),CurrentProgram," ")
            CurrentProgram = SUBSTR(CurrentProgram, R-INDEX(CurrentProgram, "/") + 1)
            CurrentProgram = SUBSTR(CurrentProgram, R-INDEX(CurrentProgram, "\") + 1)
            .
        
        FIND FIRST bf-prgrms NO-LOCK 
             WHERE bf-prgrms.prgmname EQ CurrentProgram 
             NO-ERROR.
        IF AVAILABLE bf-prgrms THEN
            CurrentProgramHotkey = bf-prgrms.mnemonic.
        ELSE
            CurrentProgram = ipcProgramName.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID pSetUserSecurityLevel(  ):
        DEFINE BUFFER bf-users  FOR users.

        FIND FIRST bf-users NO-LOCK
             WHERE bf-users.user_id EQ cUser
             NO-ERROR.
        IF AVAILABLE bf-users THEN
            iUserSecurityLevel = bf-users.securityLevel.            
	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    METHOD PUBLIC VOID Refresh( BUFFER iopttSetting FOR ttSetting  ):
        DEFINE BUFFER bf-setting     FOR setting.
        DEFINE BUFFER bf-scope       FOR scope.
        DEFINE BUFFER bf-settingType FOR settingType.
        
        FIND FIRST bf-settingType NO-LOCK
             WHERE bf-settingType.settingTypeID EQ iopttSetting.settingTypeID
                OR bf-settingType.settingName   EQ iopttSetting.settingName
             NO-ERROR.
        IF AVAILABLE bf-settingType THEN DO:
            BUFFER-COPY bf-settingType TO iopttSetting.
            ASSIGN
                iopttSetting.settingTypeDesc = bf-settingType.description
                iopttSetting.description     = bf-settingType.description
                iopttSetting.settingValue    = bf-settingType.defaultValue
                iopttSetting.recordSource    = "SettingType"
                .
        END.
        
        FIND FIRST bf-setting NO-LOCK
             WHERE bf-setting.settingID EQ iopttSetting.settingID
             NO-ERROR.
        IF AVAILABLE bf-setting THEN DO:
            BUFFER-COPY bf-setting TO iopttSetting.
            iopttSetting.recordSource = "Setting".
            
            FIND FIRST bf-scope NO-LOCK
                 WHERE bf-scope.scopeID EQ bf-setting.scopeID
                 NO-ERROR.
            IF AVAILABLE bf-scope THEN
                BUFFER-COPY bf-scope EXCEPT rec_key TO iopttSetting.
        END.
        
        iopttSetting.priorityID = pGetPriorityID(iopttSetting.scopeTable).
        
        pConcatRecordData(BUFFER iopttSetting).
    END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: Get the value of a setting by name
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC CHARACTER GetByName( ipcSettingName AS CHARACTER ):
		
		RETURN pGet( 
		   ipcSettingName,
		   "Company",    /* Scope Table */
		   cCompany,     /* Scope Field 1 */
		   "",           /* Scope Field 2 */
		   ""            /* Scope Field 3 */
		   ). 

	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Get the value of a setting by name and customer
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER GetByNameAndCustomer ( ipcSettingName AS CHARACTER, ipcCustomerID AS CHARACTER ):
        
        RETURN pGet( 
           ipcSettingName,
           "Customer",     /* Scope Table */
           cCompany,       /* Scope Field 1 */
           ipcCustomerID,  /* Scope Field 2 */
           ""              /* Scope Field 3 */
           ). 

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Get the value of a setting by name and vendor
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER GetByNameAndVendor ( ipcSettingName AS CHARACTER, ipcVendorID AS CHARACTER ):
        
        RETURN pGet( 
           ipcSettingName,
           "Vendor",     /* Scope Table */
           cCompany,     /* Scope Field 1 */
           ipcVendorID,  /* Scope Field 2 */
           ""            /* Scope Field 3 */
           ). 

    END METHOD.
    
    /*------------------------------------------------------------------------------
     Purpose: Get the value of a setting by name and shipto
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER GetByNameAndShipTo ( ipcSettingName AS CHARACTER, ipcCustomerID AS CHARACTER, ipcShipToID AS CHARACTER ):
        
        RETURN pGet( 
           ipcSettingName,
           "ShipTo",       /* Scope Table */
           cCompany,       /* Scope Field 1 */
           ipcCustomerID,  /* Scope Field 2 */
           ipcShipToID     /* Scope Field 3 */
           ).
    END METHOD.   
     
    
    /*------------------------------------------------------------------------------
     Purpose: Get the value of a setting by name and Location
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER GetByNameAndLocation ( ipcSettingName AS CHARACTER, ipcCustomerID AS CHARACTER, ipcLocation AS CHARACTER ):
        
        RETURN pGet( 
           ipcSettingName,
           "Location",       /* Scope Table */
           cCompany,       /* Scope Field 1 */
           ipcCustomerID,  /* Scope Field 2 */
           ipcLocation     /* Scope Field 3 */
           ).
    END METHOD.    
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC CHARACTER GetCategoryTagsList(  ):
		
		DEFINE BUFFER bf-settingType FOR settingType.
		
		FIND FIRST bf-settingType NO-LOCK
		     WHERE bf-settingType.settingName EQ "CategoryTags"
		     NO-ERROR.
		IF AVAILABLE bf-settingType THEN
		    RETURN bf-settingType.validValues.
		ELSE
		    RETURN "".

	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER GetScopeList( iplHasContext AS LOGICAL ):
        
        IF iplHasContext THEN
            RETURN cScopeTypeWithContextList.
        ELSE
            RETURN cScopeTypeWithoutContextList. 
        
    END METHOD.
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE CHARACTER pGet( ipcSettingName AS CHARACTER, ipcScopeTable AS CHARACTER, ipcScopeField1 AS CHARACTER, ipcScopeField2 AS CHARACTER, ipcScopeField3 AS CHARACTER ):
		
		DEFINE BUFFER bf-setting     FOR setting.
		DEFINE BUFFER bf-settingType FOR settingType.
		DEFINE BUFFER bf-scope       FOR scope.

        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lFound AS LOGICAL   NO-UNDO.
        
        lFound = pGetByScope(ipcSettingName, ipcScopeTable, ipcScopeField1, ipcScopeField2, ipcScopeField3, OUTPUT cValue).    
        
        IF NOT lFound AND (ipcScopeTable EQ "ShipTo" OR ipcScopeTable EQ "Location") THEN
            lFound = pGetByScope(ipcSettingName, "Customer", ipcScopeField1, ipcScopeField2, "", OUTPUT cValue).
            
        IF NOT lFound THEN
            lFound = pGetByScope(ipcSettingName, "Company", ipcScopeField1, "", "", OUTPUT cValue).

        IF NOT lFound THEN
            lFound = pGetByScope(ipcSettingName, "System", "", "", "", OUTPUT cValue).

        IF lFound THEN
            RETURN cValue.
        		    
        FIND FIRST bf-settingType NO-LOCK
             WHERE bf-settingType.settingName EQ ipcSettingName
             NO-ERROR.
        
        IF AVAILABLE bf-settingType THEN
            RETURN bf-settingType.defaultValue. 
        
        FINALLY:
            /* Meant to be created in pGetByScope if a setting is available */
            IF NOT AVAILABLE ttSetting AND AVAILABLE bf-settingType THEN DO:
                CREATE ttSetting.
                ASSIGN
                    ttSetting.settingTypeID = bf-settingType.settingTypeID
                    ttSetting.settingName   = bf-settingType.settingName
                    .
                
                Refresh(BUFFER ttSetting).
                
            END.            		
            
            RUN spCreateSettingUsageRecord (TEMP-TABLE ttSetting:DEFAULT-BUFFER-HANDLE).
                        
            RELEASE ttSetting.            
        END FINALLY.
	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PRIVATE LOGICAL pGetByScope( ipcSettingName AS CHARACTER, ipcScopeTable AS CHARACTER, ipcScopeField1 AS CHARACTER, ipcScopeField2 AS CHARACTER, ipcScopeField3 AS CHARACTER, OUTPUT opcValue AS CHARACTER):
        
        DEFINE BUFFER bf-setting FOR setting.
        DEFINE BUFFER bf-settingType FOR settingType.
        DEFINE BUFFER bf-scope FOR scope.

        FOR EACH ttSetting NO-LOCK
            WHERE ttSetting.settingName  EQ ipcSettingName
              AND ttSetting.scopeTable   EQ ipcScopeTable
              AND ttSetting.scopeField1  EQ ipcScopeField1
              AND ttSetting.scopeField2  EQ ipcScopeField2
              AND ttSetting.scopeField3  EQ ipcScopeField3
              AND ttSetting.inactive     EQ FALSE
              AND (ttSetting.programID   EQ CurrentProgramHotkey OR ttSetting.programID EQ "")
              AND (ttSetting.settingUser EQ cUser OR ttSetting.settingUser EQ "") 
            BY ttSetting.recordSource
            BY ttSetting.programID DESCENDING
            BY ttSetting.settingUser DESCENDING:
            LEAVE.
        END.

        IF AVAILABLE ttSetting THEN DO:
            FIND FIRST bf-setting NO-LOCK
                 WHERE bf-setting.settingID EQ ttSetting.settingID
                 NO-ERROR.
            /* If any of the scopeID, setting value or inactive are modified in between then refresh the ttSetting record and fetch record from setting table instead */
            IF AVAILABLE bf-setting AND (ttSetting.scopeID NE bf-setting.scopeID OR ttSetting.settingValue NE bf-setting.settingValue OR ttSetting.inActive NE ttSetting.inactive) THEN
                Refresh(BUFFER ttSetting).
            ELSE IF NOT AVAILABLE bf-setting THEN DO:
                /* Delete ttSetting record as database record is deleted */
                DELETE ttSetting.
            END. 
            ELSE DO:
                opcValue = ttSetting.settingValue.
                RETURN TRUE.
            END.
        END.
                                             
        IF ipcScopeTable NE "" THEN
            FIND FIRST bf-scope NO-LOCK
                 WHERE bf-scope.scopeTable  EQ ipcScopeTable
                   AND bf-scope.scopeField1 EQ ipcScopeField1
                   AND bf-scope.scopeField2 EQ ipcScopeField2
                   AND bf-scope.scopeField3 EQ ipcScopeField3
                 NO-ERROR.        

        IF AVAILABLE bf-scope THEN DO:
            FOR EACH bf-setting NO-LOCK
                WHERE bf-setting.settingName  EQ ipcSettingName
                  AND bf-setting.scopeID      EQ bf-scope.scopeID
                  AND bf-setting.inactive     EQ FALSE
                  AND (bf-setting.programID   EQ CurrentProgramHotkey OR bf-setting.programID EQ "")
                  AND (bf-setting.settingUser EQ cUser OR bf-setting.settingUser EQ "")
                BY bf-setting.programID DESCENDING
                BY bf-setting.settingUser DESCENDING:
                LEAVE. 
            END.
        END.
        
        IF AVAILABLE bf-setting THEN DO:
            opcValue = bf-setting.settingValue.
            RETURN TRUE.
        END.
        
        RETURN FALSE.
        
        FINALLY:
            IF NOT AVAILABLE ttSetting AND AVAILABLE bf-setting THEN DO:
                CREATE ttSetting.
                ASSIGN
                    ttSetting.settingID     = bf-setting.settingID
                    ttSetting.settingTypeID = bf-setting.settingTypeID
                    ttSetting.settingName   = bf-setting.settingName
                    .
                
                Refresh(BUFFER ttSetting).
            END.
        END FINALLY.
    END METHOD.
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID GetBySearch(
	    ipiSettingTypeID      AS INTEGER,
	    ipcSettingName        AS CHARACTER, 
        iplInactive           AS LOGICAL,
        ipcCategory           AS CHARACTER,
        ipcScopeTable         AS CHARACTER,
        ipcScopeField1        AS CHARACTER,
        ipcScopeField2        AS CHARACTER,
        ipcScopeField3        AS CHARACTER,
        ipcUser               AS CHARACTER, 
        ipcProgram            AS CHARACTER,
        ipcRecordSource       AS CHARACTER,
        OUTPUT TABLE ttSetting ):
		
		pGetAll (
		   ipiSettingTypeID,
		   ipcSettingName,
           iplInactive,
           ipcCategory,
           ipcScopeTable,
           ipcScopeField1,
           ipcScopeField2,
           ipcScopeField3,
           ipcUser,
           ipcProgram,
           FALSE, /* Include Empty program settings */
           ipcRecordSource,
           OUTPUT TABLE ttSetting
           ).
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE VOID pGetAll( 
	    ipiSettingTypeID       AS INTEGER,
	    ipcSettingName         AS CHARACTER, 
	    iplInactive            AS LOGICAL,
	    ipcCategory            AS CHARACTER,
	    ipcScopeTable          AS CHARACTER,
	    ipcScopeField1         AS CHARACTER,
	    ipcScopeField2         AS CHARACTER,
	    ipcScopeField3         AS CHARACTER,
        ipcUser                AS CHARACTER, 
        ipcProgram             AS CHARACTER,
        iplIncludeEmptyProgram AS LOGICAL,
        ipcRecordSource        AS CHARACTER,
	    OUTPUT TABLE ttSetting):

        EMPTY TEMP-TABLE ttSetting.
        
        DEFINE VARIABLE hdSettingTypeQuery  AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cSettingTypeQuery   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hdSettingQuery      AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cSettingQuery       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lHasSettingOverride AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE iIndex              AS INTEGER   NO-UNDO.
        
        DEFINE BUFFER bf-setting     FOR setting.
        DEFINE BUFFER bf-settingType FOR settingType.
		DEFINE BUFFER bf-scope       FOR scope.
		DEFINE BUFFER bf-ttSetting   FOR ttSetting.		
		
		IF ((ipcRecordSource EQ "" OR ipcRecordSource EQ "SettingType") AND iplInactive NE TRUE AND (ipcScopeTable EQ "" OR ipcScopeTable EQ "System"))
		   OR ipcCategory NE "" THEN DO:
		    FIND FIRST bf-scope NO-LOCK
		         WHERE bf-scope.scopeTable EQ "System"
		         NO-ERROR.

    		CREATE QUERY hdSettingTypeQuery.
    		hdSettingTypeQuery:ADD-BUFFER(BUFFER bf-settingType:HANDLE).
    		
    		cSettingTypeQuery = ' FOR EACH bf-settingType NO-LOCK '.
    		
    		IF ipcSettingName NE "" OR ipcCategory NE "" THEN
    		    cSettingTypeQuery = cSettingTypeQuery + ' WHERE TRUE '.
            
            IF ipiSettingTypeID NE 0 THEN
                cSettingTypeQuery = cSettingTypeQuery + ' AND bf-settingType.settingTypeID EQ ' + STRING(ipiSettingTypeID).
                                		    
    		IF ipcSettingName NE "" THEN
    		    cSettingTypeQuery = cSettingTypeQuery + ' AND bf-settingType.settingName BEGINS ' + QUOTER(ipcSettingname).
            
            cSettingTypeQuery = cSettingTypeQuery 
                              + (IF INDEX(cSettingTypeQuery, "WHERE") GT 0 THEN ' AND ' ELSE ' WHERE ')
                              + ' (bf-settingType.securityLevel LE ' + STRING(iUserSecurityLevel) + ' OR bf-settingType.securityLevel EQ 0)'.

    		IF ipcCategory NE "" AND ipcCategory NE ? THEN DO:
    		    IF NUM-ENTRIES(ipcCategory) EQ 1 THEN
    		        cSettingTypeQuery = cSettingTypeQuery + ' AND bf-settingType.categoryTags CONTAINS ' + QUOTER(ipcCategory) + ' AND LOOKUP(' + QUOTER(ipcCategory) + ', bf-settingType.categoryTags) GT 0 '. 
    		    ELSE DO:
                    cSettingTypeQuery = cSettingTypeQuery + ' AND ('.

        		    DO iIndex = 1 TO NUM-ENTRIES(ipcCategory):
        		        cSettingTypeQuery = cSettingTypeQuery + ' (bf-settingType.categoryTags CONTAINS ' + QUOTER(ENTRY(iIndex, ipcCategory)) + ' AND LOOKUP(' + QUOTER(ENTRY(iIndex, ipcCategory)) + ', bf-settingType.categoryTags) GT 0) '.
        		        
        		        IF iIndex NE NUM-ENTRIES(ipcCategory) THEN
        		            cSettingTypeQuery = cSettingTypeQuery + " OR ".
        		    END.
        		    
        		    cSettingTypeQuery = cSettingTypeQuery + ' )'.
    		    END.
    		END.
    		
    		hdSettingTypeQuery:QUERY-PREPARE (cSettingTypeQuery).

    	    hdSettingTypeQuery:QUERY-OPEN().
    
    	    hdSettingTypeQuery:GET-NEXT ().
    
    	    DO WHILE NOT hdSettingTypeQuery:QUERY-OFF-END:
                CREATE ttSetting.
                BUFFER-COPY bf-settingType TO ttSetting. 
    
                ASSIGN
                    ttSetting.settingValue    = bf-settingType.defaultValue
                    ttSetting.settingTypeDesc = bf-settingType.description
                    ttSetting.inactive        = FALSE
                    ttSetting.recordSource    = "SettingType"
                    .	            

                IF AVAILABLE bf-scope THEN
                    BUFFER-COPY bf-scope EXCEPT rec_key TO ttSetting.
                
                ttSetting.priorityID = pGetPriorityID(ttSetting.scopeTable).
                
                pConcatRecordData(BUFFER ttSetting).
                
    	        hdSettingTypeQuery:GET-NEXT ().
    	    END.
    	    
    	    hdSettingTypeQuery:QUERY-CLOSE ().
    
    	    DELETE OBJECT hdSettingTypeQuery.
	    END.	    
        
        /* This is important. AS sometimes the temp-table record is not committed and can't get found in next queries */
        RELEASE ttSetting.
        
        IF ipcRecordSource EQ "" OR ipcRecordSource EQ "Setting" THEN DO:
            CREATE QUERY hdSettingQuery.
                            
            IF ipcCategory NE "" THEN DO:
                hdSettingQuery:ADD-BUFFER(BUFFER bf-ttSetting:HANDLE).
                hdSettingQuery:ADD-BUFFER (BUFFER bf-setting:HANDLE).
                
                cSettingQuery = cSettingQuery + " FOR EACH bf-ttSetting WHERE bf-ttSetting.recordSource EQ 'SettingType' ".
                cSettingQuery = cSettingQuery + " , EACH bf-setting NO-LOCK WHERE bf-setting.settingTypeID EQ bf-ttSetting.settingTypeID ".
            END.
            ELSE DO:
                hdSettingQuery:ADD-BUFFER (BUFFER bf-setting:HANDLE).
                
                cSettingQuery = " FOR EACH bf-setting NO-LOCK ".           
            END.
    
            IF ipiSettingTypeID NE 0 OR ipcSettingName NE "" OR iplInactive NE ? OR ipcUser NE "" OR ipcProgram NE "" OR 
               ipcScopeTable NE "" OR ipcScopeField1 NE "" OR ipcScopeField2 NE "" OR ipcScopeField3 NE "" THEN
                cSettingQuery = cSettingQuery + IF INDEX(cSettingQuery, "WHERE") EQ 0 THEN " WHERE TRUE " ELSE "".

            IF ipiSettingTypeID NE 0 THEN
                cSettingQuery = cSettingQuery + ' AND bf-setting.settingTypeID EQ ' + STRING(ipiSettingTypeID).
                            
            IF ipcSettingName NE "" THEN
                cSettingQuery = cSettingQuery + ' AND bf-setting.settingName BEGINS ' + QUOTER(ipcSettingname).                   
    
            IF iplInactive NE ? THEN
                cSettingQuery = cSettingQuery + ' AND bf-setting.inactive EQ ' + STRING(iplInactive).
            
            IF ipcUser NE "" THEN
                cSettingQuery = cSettingQuery + ' AND bf-setting.settingUser EQ ' + QUOTER(ipcUser).
    
            IF ipcProgram NE "" THEN DO:
                IF NOT iplIncludeEmptyProgram THEN
                    cSettingQuery = cSettingQuery + ' AND bf-setting.programID EQ ' + QUOTER(ipcProgram).
                ELSE
                    cSettingQuery = cSettingQuery + ' AND (bf-setting.programID EQ ' + QUOTER(ipcProgram) + "OR bf-setting.programID EQ '')".
            END.

            IF ipcScopeTable NE "" THEN DO:
                IF ipcScopeTable EQ "System" THEN
                    FIND FIRST bf-scope NO-LOCK 
                         WHERE bf-scope.scopeTable  EQ ipcScopeTable
                           AND bf-scope.scopeField1 EQ ""
                           AND bf-scope.scopeField2 EQ ""
                           AND bf-scope.scopeField3 EQ ""
                         NO-ERROR.                
                ELSE IF ipcScopeTable EQ "Company" AND ipcScopeField1 NE "" THEN
                    FIND FIRST bf-scope NO-LOCK 
                         WHERE bf-scope.scopeTable  EQ ipcScopeTable
                           AND bf-scope.scopeField1 EQ ipcScopeField1
                           AND bf-scope.scopeField2 EQ ""
                           AND bf-scope.scopeField3 EQ ""                           
                         NO-ERROR.
                ELSE IF ipcScopeTable EQ "Customer" AND ipcScopeField1 NE "" AND ipcScopeField2 NE "" THEN
                    FIND FIRST bf-scope NO-LOCK 
                         WHERE bf-scope.scopeTable  EQ ipcScopeTable
                           AND bf-scope.scopeField1 EQ ipcScopeField1
                           AND bf-scope.scopeField2 EQ ipcScopeField2
                           AND bf-scope.scopeField3 EQ ""                           
                         NO-ERROR.
                ELSE IF ipcScopeTable EQ "Vendor" AND ipcScopeField1 NE "" AND ipcScopeField2 NE "" THEN
                    FIND FIRST bf-scope NO-LOCK 
                         WHERE bf-scope.scopeTable  EQ ipcScopeTable
                           AND bf-scope.scopeField1 EQ ipcScopeField1
                           AND bf-scope.scopeField2 EQ ipcScopeField2
                           AND bf-scope.scopeField3 EQ ""                           
                         NO-ERROR.                         
                ELSE IF (ipcScopeTable EQ "ShipTo" OR ipcScopeTable EQ "Location") AND ipcScopeField1 NE "" AND ipcScopeField2 NE "" AND ipcScopeField3 NE "" THEN
                    FIND FIRST bf-scope NO-LOCK 
                         WHERE bf-scope.scopeTable  EQ ipcScopeTable
                           AND bf-scope.scopeField1 EQ ipcScopeField1
                           AND bf-scope.scopeField2 EQ ipcScopeField2
                           AND bf-scope.scopeField3 EQ ipcScopeField3                        
                         NO-ERROR.      
                
                IF AVAILABLE bf-scope THEN
                    cSettingQuery = cSettingQuery + ' AND bf-setting.scopeID EQ ' + STRING(bf-scope.scopeID).                   
            END.

            hdSettingQuery:QUERY-PREPARE (cSettingQuery).

            hdSettingQuery:QUERY-OPEN().

            hdSettingQuery:GET-NEXT().

            DO WHILE NOT hdSettingQuery:QUERY-OFF-END:
                FIND FIRST bf-scope NO-LOCK
                     WHERE bf-scope.scopeID EQ bf-setting.scopeID
                     NO-ERROR.
                         
                IF NOT AVAILABLE bf-scope THEN DO:
                    hdSettingQuery:GET-NEXT().
                    
                    NEXT.
                END.
                
                IF (ipcScopeTable  NE "" AND bf-scope.scopeTable  NE ipcScopeTable) OR
                   (ipcScopeField1 NE "" AND bf-scope.scopeField1 NE ipcScopeField1) OR
                   (ipcScopeField2 NE "" AND bf-scope.scopeField2 NE ipcScopeField2) OR
                   (ipcScopeField3 NE "" AND bf-scope.scopeField3 NE ipcScopeField3) THEN DO:
                    hdSettingQuery:GET-NEXT().

                    NEXT.
                END.
                
                CREATE ttSetting.

                IF NOT AVAILABLE bf-ttSetting THEN
                    FIND FIRST bf-ttSetting NO-LOCK
                         WHERE bf-ttSetting.settingTypeID EQ bf-setting.settingTypeID
                         NO-ERROR.

                IF AVAILABLE bf-ttSetting THEN DO:
                    BUFFER-COPY bf-ttSetting TO ttSetting.
                END.
                ELSE DO:
                    FIND FIRST bf-settingType NO-LOCK
                         WHERE bf-settingType.settingTypeID EQ bf-setting.settingTypeID
                         NO-ERROR.
                    IF AVAILABLE bf-settingType THEN DO:
                        BUFFER-COPY bf-settingType TO ttSetting. 

                        ttSetting.settingTypeDesc = bf-settingType.description.
                    END.
                END.
                
                BUFFER-COPY bf-setting TO ttSetting.
                ttSetting.recordSource = "Setting".
                
                BUFFER-COPY bf-scope EXCEPT rec_key TO ttSetting.
                
                ttSetting.priorityID = pGetPriorityID(ttSetting.scopeTable).
                
                pConcatRecordData(BUFFER ttSetting).
                
                hdSettingQuery:GET-NEXT().                
            END.
            
            hdSettingQuery:QUERY-CLOSE().
            
            DELETE OBJECT hdSettingQuery.	
        END.

        /* All the records that are required from settingType which are not part of the filters are deleted here */
        FOR EACH ttSetting
            WHERE (ipcRecordSource EQ "Setting" AND ttSetting.recordSource EQ "SettingType")
               OR (ipcUser NE "" AND NOT ttSetting.settingUser BEGINS ipcUser)
               OR (ipcProgram NE "" AND NOT iplIncludeEmptyProgram AND NOT ttSetting.programID BEGINS ipcProgram)
               OR (ipcScopeTable NE "" AND ttSetting.scopeTable NE ipcScopetable)
               OR (iplInactive NE ? AND ttSetting.inactive NE iplInactive):  
            DELETE ttSetting.
        END.
            
	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID LoadByUser ( ipcUser AS CHARACTER ):
        pGetAll(
           0,
           "",    /* SettingName  */
           ?,     /* Inactive     */
           "",    /* Category     */
           "",    /* ScopeTable   */
           "",    /* ScopeField1  */
           "",    /* ScopeField2  */
           "",    /* ScopeField3  */
           ipcUser,    /* User         */
           "",    /* Program      */  
           FALSE, /* Include Empty program settings */
           "",    /* Record Source */
           OUTPUT TABLE ttSetting
           ).        
    END METHOD.
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID LoadByCategory(ipcCategory AS CHARACTER ):
        pGetAll(
           0,
           "",    /* SettingName  */
           ?,     /* Inactive     */
           ipcCategory,    /* Category     */
           "",    /* ScopeTable   */
           "",    /* ScopeField1  */
           "",    /* ScopeField2  */
           "",    /* ScopeField3  */
           "",    /* User         */
           "",    /* Program      */ 
           FALSE, /* Include Empty program settings */ 
           "",    /* Record Source */
           OUTPUT TABLE ttSetting
           ).

        cCategoryTags = ipcCategory.
	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID LoadByCategoryAndProgram(ipcCategory AS CHARACTER):
        pGetAll(
           0,
           "",    /* SettingName  */
           ?,     /* Inactive     */
           ipcCategory,    /* Category     */
           "",    /* ScopeTable   */
           "",    /* ScopeField1  */
           "",    /* ScopeField2  */
           "",    /* ScopeField3  */
           "",    /* User         */
           CurrentProgramHotkey,    /* Program      */ 
           TRUE,  /* Include Empty program settings */ 
           "",    /* Record Source */
           OUTPUT TABLE ttSetting
           ).

        cCategoryTags = ipcCategory.
                   
        RUN spCreateSettingUsage (TABLE ttSetting).
    END METHOD.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID GetAll( OUTPUT TABLE ttSetting ):
        pGetAll(
           0,
           "",    /* SettingName  */
           ?,     /* Inactive     */
           "",    /* Category     */
           "",    /* ScopeTable   */
           "",    /* ScopeField1  */
           "",    /* ScopeField2  */
           "",    /* ScopeField3  */
           "",    /* User         */
           "",    /* Program      */
           FALSE, /* Include Empty program settings */ 
           "",    /* Record Source */
           OUTPUT TABLE ttSetting).
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Send the current settings in ttSetting
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID GetCurrentSetting( OUTPUT TABLE ttSetting):

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
        
        IF ipcScopeTable EQ "System" THEN
            ASSIGN
                ipcScopeField1 = ""
                ipcScopeField2 = ""
                ipcScopeField3 = ""
                .
        ELSE IF ipcScopeTable EQ "Company" THEN
            ASSIGN
                ipcScopeField2 = ""
                ipcScopeField3 = ""
                .
        ELSE IF ipcScopeTable EQ "Customer" THEN
            ASSIGN
                ipcScopeField3 = ""
                .
        ELSE IF ipcScopeTable EQ "Vendor" THEN
            ASSIGN
                ipcScopeField3 = ""
                .

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

	METHOD PUBLIC VOID Reload(  ):
		IF cCategoryTags NE "" THEN
		    LoadByCategoryAndProgram(cCategoryTags).

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID SetCompany( ipcCompany AS CHARACTER ):
		cCompany = ipcCompany.
	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Updates the value of a Company/System scope setting
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER Update(ipcSettingName AS CHARACTER, ipcSettingValue AS CHARACTER):

        DEFINE VARIABLE iScopeID       AS INT64     NO-UNDO.
        DEFINE VARIABLE iSettingTypeID AS INT64     NO-UNDO.
        DEFINE VARIABLE iSettingID     AS INT64     NO-UNDO.
        DEFINE VARIABLE cSettingDesc   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cScopeTable    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cScopeField1   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cScopeField2   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cScopeField3   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSettingUser   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cProgramHotkey AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lInactive      AS LOGICAL   NO-UNDO.
                        
        DEFINE BUFFER bf-setting     FOR setting.
        DEFINE BUFFER bf-settingType FOR settingType.
        
        FIND FIRST bf-settingType NO-LOCK
             WHERE bf-settingType.settingName EQ ipcSettingName
             NO-ERROR.
        IF NOT AVAILABLE bf-settingType THEN
            RETURN "SettingType Name is not valid".

        ASSIGN
            iSettingTypeID = bf-settingType.settingTypeID
            cSettingDesc   = bf-settingType.description
            .
                    
        iScopeID = pCreateScope ("Company", cCompany, "", ""). 

        FOR EACH bf-setting NO-LOCK
            WHERE bf-setting.settingName  EQ ipcSettingName
              AND bf-setting.scopeID      EQ iScopeID
              AND (bf-setting.programID   EQ CurrentProgramHotkey OR bf-setting.programID EQ "")
              AND (bf-setting.settingUser EQ cUser OR bf-setting.settingUser EQ "")
            BY bf-setting.programID DESCENDING
            BY bf-setting.settingUser DESCENDING:
            LEAVE. 
        END.
                           
        IF NOT AVAILABLE bf-setting THEN DO:
            iScopeID = pCreateScope ("System", "", "", "").
    
            FOR EACH bf-setting NO-LOCK
                WHERE bf-setting.settingName  EQ ipcSettingName
                  AND bf-setting.scopeID      EQ iScopeID
                  AND (bf-setting.programID   EQ CurrentProgramHotkey OR bf-setting.programID EQ "")
                  AND (bf-setting.settingUser EQ cUser OR bf-setting.settingUser EQ "")
                BY bf-setting.programID DESCENDING
                BY bf-setting.settingUser DESCENDING:
                LEAVE. 
            END.        
        END.

        IF AVAILABLE bf-setting THEN
            ASSIGN
                iSettingID     = bf-setting.settingID
                cSettingDesc   = bf-setting.description
                iScopeID       = bf-setting.scopeID
                cSettingUser   = bf-setting.settingUser
                cProgramHotkey = bf-setting.programID
                lInactive      = bf-setting.inActive
                .
                
        pGetScope (
            INPUT  iScopeID,
            OUTPUT cScopeTable,
            OUTPUT cScopeField1,
            OUTPUT cScopeField2,
            OUTPUT cScopeField3            
            ).        

        RETURN THIS-OBJECT:Update(
            iSettingTypeID, 
            iSettingID,
            ipcSettingValue, 
            cSettingDesc, 
            cSettingUser, 
            cProgramHotKey,
            lInactive, 
            cScopeTable, 
            cScopeField1, 
            cScopeField2, 
            cScopeField3,
            FALSE,                  /* Save locally */
            OUTPUT iSettingID
            ).
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Updates the value of a given scope setting
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER Update(ipcSettingName AS CHARACTER, ipcScopeTable AS CHARACTER, ipcScopeField1 AS CHARACTER, ipcScopeField2 AS CHARACTER, ipcScopeField3 AS CHARACTER, ipcSettingValue  AS CHARACTER):

        DEFINE VARIABLE iScopeID       AS INT64     NO-UNDO.
        DEFINE VARIABLE iSettingTypeID AS INT64     NO-UNDO.
        DEFINE VARIABLE iSettingID     AS INT64     NO-UNDO.
        DEFINE VARIABLE cSettingDesc   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSettingUser   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cProgramHotkey AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lInactive      AS LOGICAL   NO-UNDO.
                
        DEFINE BUFFER bf-setting     FOR setting.
        DEFINE BUFFER bf-settingType FOR settingType.
        
        FIND FIRST bf-settingType NO-LOCK
             WHERE bf-settingType.settingName EQ ipcSettingName
             NO-ERROR.
        IF NOT AVAILABLE bf-settingType THEN
            RETURN "SettingType Name is not valid".

        ASSIGN
            iSettingTypeID = bf-settingType.settingTypeID
            cSettingDesc   = bf-settingType.description
            .
                    
        iScopeID = pCreateScope (ipcScopeTable, ipcScopeField1, ipcScopeField2, ipcScopeField3).
        
        FOR EACH bf-setting NO-LOCK
            WHERE bf-setting.settingName  EQ ipcSettingName
              AND bf-setting.scopeID      EQ iScopeID
              AND (bf-setting.programID   EQ CurrentProgramHotkey OR bf-setting.programID EQ "")
              AND (bf-setting.settingUser EQ cUser OR bf-setting.settingUser EQ "")
            BY bf-setting.programID DESCENDING
            BY bf-setting.settingUser DESCENDING:
            LEAVE. 
        END.
        
        IF AVAILABLE bf-setting THEN
            ASSIGN
                iSettingID     = bf-setting.settingID
                cSettingDesc   = bf-setting.description
                cSettingUser   = bf-setting.settingUser
                cProgramHotkey = bf-setting.programID
                lInactive      = bf-setting.inActive
                .
                
        RETURN THIS-OBJECT:Update(
            iSettingTypeID, 
            iSettingID,
            ipcSettingValue, 
            cSettingDesc, 
            cSettingUser, 
            cProgramHotKey,
            lInactive, 
            ipcScopeTable, 
            ipcScopeField1, 
            ipcScopeField2, 
            ipcScopeField3,
            FALSE,                  /* Save locally */
            OUTPUT iSettingID
            ).
    END METHOD.
        
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC CHARACTER Update( 
        ipiSettingTypeID AS INT64, 
        ipiSettingID     AS INT64,
        ipcSettingValue  AS CHARACTER, 
        ipcSettingDesc   AS CHARACTER, 
        ipcSettingUser   AS CHARACTER, 
        ipcProgramID     AS CHARACTER,
        iplInactive      AS LOGICAL, 
        ipcScopeTable    AS CHARACTER, 
        ipcScopeField1   AS CHARACTER, 
        ipcScopeField2   AS CHARACTER, 
        ipcScopeField3   AS CHARACTER,
        iplSaveLocally   AS LOGICAL,
        OUTPUT opiSettingID AS INTEGER):
        
        DEFINE VARIABLE iScopeID   AS INTEGER   NO-UNDO.   
        DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iSettingID AS INT64     NO-UNDO.
         
        DEFINE BUFFER bf-setting     FOR setting.
        DEFINE BUFFER bf-settingType FOR settingType.
        DEFINE BUFFER bf-ttSetting   FOR ttSetting.
        
        cMessage = Validate(
            ipiSettingTypeID,
            ipiSettingID,
            ipcSettingValue,
            ipcSettingDesc,
            ipcSettingUser,
            ipcProgramID,
            iplInactive,
            ipcScopeTable,
            ipcScopeField1,
            ipcScopeField2,
            ipcScopeField3
            ).
        IF cMessage NE "" THEN
            RETURN cMessage.

        FIND FIRST bf-settingType NO-LOCK
             WHERE bf-settingType.settingTypeID EQ ipiSettingTypeID
             NO-ERROR.
        IF NOT AVAILABLE bf-settingType THEN
            RETURN "SettingType ID is not valid".

        /* If settingID is 0, then the setting is newly created */
        FIND FIRST bf-ttSetting
             WHERE bf-ttSetting.settingID EQ ipiSettingID
               AND bf-ttSetting.settingID NE 0
             NO-ERROR.
        IF NOT AVAILABLE bf-ttSetting THEN DO:
            FIND LAST ttSetting USE-INDEX settingID NO-ERROR.
            IF AVAILABLE ttSetting THEN
                iSettingID = ttSetting.settingID + 1.
            ELSE
                iSettingID = iSettingID + 1.

            CREATE bf-ttSetting.
            ASSIGN
                bf-ttSetting.settingID     = iSettingID
                bf-ttSetting.settingName   = bf-settingType.settingName
                bf-ttSetting.settingTypeID = bf-settingType.settingTypeID
                .                
        END.
        
        ASSIGN
            bf-ttSetting.settingValue = ipcSettingValue
            bf-ttSetting.description  = ipcSettingDesc
            bf-ttSetting.programID    = ipcProgramID
            bf-ttSetting.settingUser  = ipcSettingUser                
            bf-ttSetting.inactive     = iplInactive  
            bf-ttSetting.scopeTable   = ipcScopeTable
            bf-ttSetting.scopeField1  = ipcScopeField1
            bf-ttSetting.scopeField2  = ipcScopeField2
            bf-ttSetting.scopeField3  = ipcScopeField3        
            opiSettingID              = bf-ttSetting.settingID
            .
        
        IF iplSaveLocally THEN
            RETURN "".
                    
        FIND FIRST bf-setting EXCLUSIVE-LOCK
             WHERE bf-setting.settingID EQ ipiSettingID
             NO-ERROR.
        IF NOT AVAILABLE bf-setting THEN DO:
            CREATE bf-setting.
            ASSIGN
                bf-setting.settingName   = bf-settingType.settingName
                bf-setting.settingTypeID = bf-settingType.settingTypeID
                .
        END.
        
        ASSIGN
            bf-setting.settingValue = ipcSettingValue
            bf-setting.description  = ipcSettingDesc
            bf-setting.programID    = ipcProgramID
            bf-setting.settingUser  = ipcSettingUser                
            bf-setting.inactive     = iplInactive          
            opiSettingID            = bf-setting.settingID
            .
        
        iScopeID = pCreateScope (ipcScopeTable, ipcScopeField1, ipcScopeField2, ipcScopeField3).
            
        bf-setting.scopeID = iScopeID.
        
        /* Save ttSetting record as well */
        ASSIGN
            bf-ttSetting.settingID = bf-setting.settingID
            bf-ttSetting.scopeID   = bf-setting.scopeID
            .
        
        Refresh(BUFFER bf-ttSetting).
                
        RETURN "".
        
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC CHARACTER Validate( 
	    ipiSettingTypeID AS INT64, 
        ipiSettingID     AS INT64,
        ipcSettingValue  AS CHARACTER, 
        ipcSettingDesc   AS CHARACTER, 
        ipcSettingUser   AS CHARACTER, 
        ipcProgramID     AS CHARACTER,
        iplInactive      AS LOGICAL, 
        ipcScopeTable    AS CHARACTER, 
        ipcScopeField1   AS CHARACTER, 
        ipcScopeField2   AS CHARACTER, 
        ipcScopeField3   AS CHARACTER):

        DEFINE VARIABLE oJsonParser AS ObjectModelParser NO-UNDO.
        DEFINE VARIABLE oJsonObject AS JsonObject        NO-UNDO.
		         
        DEFINE BUFFER bf-setting     FOR setting.
        DEFINE BUFFER bf-settingType FOR settingType.
        DEFINE BUFFER bf-scope       FOR scope.
        DEFINE BUFFER bf-company     FOR company.
        DEFINE BUFFER bf-cust        FOR cust.
        DEFINE BUFFER bf-shipto      FOR shipto.
        DEFINE BUFFER bf-vend        FOR vend.
        DEFINE BUFFER bf-loc         FOR loc.
        
        FIND FIRST bf-settingType NO-LOCK
             WHERE bf-settingType.settingTypeID EQ ipiSettingTypeID
             NO-ERROR.
        IF NOT AVAILABLE bf-settingType THEN
            RETURN "SettingType ID is not valid".
        
        IF NOT bf-settingType.hasContext AND 
           (ipcScopeTable NE "System" AND ipcScopeTable NE "Company") THEN
            RETURN "This setting cannot have a '" + ipcScopeTable + "' scope".

        IF bf-settingType.validValues NE "" AND LOOKUP(ipcSettingValue, bf-settingType.validValues) EQ 0 THEN
            RETURN "'" + ipcSettingValue + "' is not a valid value for '" + bf-settingType.settingName + "'. Available values are '" + bf-settingType.validValues + "'".
        
        IF bf-settingType.dataType EQ "Integer" THEN
            INTEGER(ipcSettingValue) NO-ERROR.
        ELSE IF bf-settingType.dataType EQ "Decimal" THEN
            DECIMAL(ipcSettingValue) NO-ERROR.
        ELSE IF bf-settingType.dataType EQ "Date" THEN
            DATE(ipcSettingValue) NO-ERROR.
        ELSE IF bf-settingType.dataType EQ "DateTime" THEN
            DATETIME(ipcSettingValue) NO-ERROR.
        ELSE IF bf-settingType.dataType EQ "DateTime-TZ" THEN
            DATETIME-TZ(ipcSettingValue) NO-ERROR.
        ELSE IF bf-settingType.dataType EQ "Logical" THEN
            LOGICAL(ipcSettingValue) NO-ERROR.
        ELSE IF bf-settingType.dataType EQ "Json" THEN DO:
            IF ipcSettingValue NE "" THEN DO:
                IF LENGTH(ipcSettingValue) GT 31995 THEN
                    RETURN "Maximum character value reached. Cannot save data more than 31995 characters". 
                oJsonParser = NEW ObjectModelParser().
                oJsonObject = CAST(oJsonParser:Parse(INPUT ipcSettingValue), JsonObject) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    RETURN "Error parsing Json " + ERROR-STATUS:GET-MESSAGE(1).  
            END.            
        END.
                                                        
        IF ERROR-STATUS:ERROR THEN
             RETURN "'" + ipcSettingValue + "' is not a valid value for '" + bf-settingType.settingName + "'. Value has to be " + STRING(bf-settingType.dataType EQ "Integer", "an/a") + "'" + bf-settingType.dataType + "'".
        
        IF bf-settingType.dataType EQ "Integer" OR bf-settingType.dataType EQ "Decimal" THEN DO:
            IF bf-settingType.validValueMin NE "" THEN DO:
                INTEGER(bf-settingType.validValueMin) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR AND INTEGER(ipcSettingValue) LT INTEGER(bf-settingType.validValueMin) THEN
                    RETURN "Setting Value '" + ipcSettingValue + "' has to be more than '" + bf-settingType.validValueMin + "'".
            END.

            IF bf-settingType.validValueMax NE "" THEN DO:
                INTEGER(bf-settingType.validValueMax) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR AND INTEGER(ipcSettingValue) GT INTEGER(bf-settingType.validValueMin) THEN
                    RETURN "Setting Value '" + ipcSettingValue + "' has to be less than '" + bf-settingType.validValueMax + "'".
            END.
        END.
             
        IF ipcScopeTable EQ "Company" THEN DO:
            FIND FIRST bf-company NO-LOCK
                 WHERE bf-company.company EQ ipcScopeField1
                 NO-ERROR.
            IF NOT AVAILABLE bf-company THEN
                RETURN "Invalid company '" + ipcScopeField1 + "'".
        END.
        ELSE IF ipcScopeTable EQ "Customer" THEN DO:
            FIND FIRST bf-cust NO-LOCK
                 WHERE bf-cust.company EQ ipcScopeField1
                   AND bf-cust.cust-no EQ ipcScopeField2
                 NO-ERROR.
            IF NOT AVAILABLE bf-cust THEN
                RETURN "Invalid Company/Customer '" + ipcScopeField1 + "/" + ipcScopeField2 + "'".
        END.
        ELSE IF ipcScopeTable EQ "ShipTo" THEN DO:
            FIND FIRST bf-shipto NO-LOCK
                 WHERE bf-shipto.company EQ ipcScopeField1
                   AND bf-shipto.cust-no EQ ipcScopeField2
                   AND bf-shipto.ship-id EQ ipcScopeField3
                 NO-ERROR.
            IF NOT AVAILABLE bf-shipto THEN     
                RETURN "Invalid Company/Customer/Shipto '" + ipcScopeField1 + "/" + ipcScopeField2 + "/" + ipcScopeField3 + "'".
        END.
        ELSE IF ipcScopeTable EQ "Location" THEN DO:
            FIND FIRST bf-cust NO-LOCK
                 WHERE bf-cust.company EQ ipcScopeField1
                   AND bf-cust.cust-no EQ ipcScopeField2
                 NO-ERROR.
            IF NOT AVAILABLE bf-cust THEN
                RETURN "Invalid Company/Customer '" + ipcScopeField1 + "/" + ipcScopeField2 + "'".
            IF AVAILABLE bf-cust THEN
            FIND FIRST bf-loc NO-LOCK
                 WHERE bf-loc.company EQ ipcScopeField1
                   AND bf-loc.loc     EQ ipcScopeField3
                 NO-ERROR.
            IF NOT AVAILABLE bf-loc THEN     
                RETURN "Invalid Company/Customer/Location '" + ipcScopeField1 + "/" + ipcScopeField2 + "/" + ipcScopeField3 + "'".
        END.
        ELSE IF ipcScopeTable EQ "Vendor" THEN DO:
            FIND FIRST bf-vend NO-LOCK
                 WHERE bf-vend.company EQ ipcScopeField1
                   AND bf-vend.vend-no EQ ipcScopeField2
                 NO-ERROR.
            IF NOT AVAILABLE bf-vend THEN
                RETURN "Invalid Company/Vendor '" + ipcScopeField1 + "/" + ipcScopeField2 + "'".        
        END.
        ELSE IF ipcScopeTable EQ "" OR ipcScopeTable EQ ? THEN DO:
            RETURN "Scope Type cannot be empty".
        END.

        IF ipcScopeTable EQ "System" THEN
            ASSIGN
                ipcScopeField1 = ""
                ipcScopeField2 = ""
                ipcScopeField3 = ""
                .
        ELSE IF ipcScopeTable EQ "Company" THEN
            ASSIGN
                ipcScopeField2 = ""
                ipcScopeField3 = ""
                .
        ELSE IF ipcScopeTable EQ "Vendor" OR ipcScopeTable EQ "Customer" THEN
            ipcScopeField3 = "".

        FIND FIRST bf-scope NO-LOCK
             WHERE bf-scope.scopeTable  EQ ipcScopeTable
               AND bf-scope.scopeField1 EQ ipcScopeField1
               AND bf-scope.scopeField2 EQ ipcScopeField2
               AND bf-scope.scopeField3 EQ ipcScopeField3
             NO-ERROR.
        IF AVAILABLE bf-scope THEN DO:
            FIND FIRST bf-setting NO-LOCK
                 WHERE bf-setting.settingTypeID EQ ipiSettingTypeID
                   AND bf-setting.scopeID       EQ bf-scope.scopeID
                   AND bf-setting.settingUser   EQ ipcSettingUser
                   AND bf-setting.programID     EQ ipcProgramID
                 NO-ERROR.
            IF AVAILABLE bf-setting AND bf-setting.settingID NE ipiSettingID THEN
                RETURN "Setting already exists. Please enter a different scope/user/program or override existing setting".
        END.
		
        RETURN "".
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC Setting ( ):
        EMPTY TEMP-TABLE ttSetting.
	END DESTRUCTOR.
    
END CLASS.
