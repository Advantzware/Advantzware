 
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
    DEFINE VARIABLE cCompany                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUser                        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScopeTypeWithContextList    AS CHARACTER NO-UNDO INITIAL "System,Company,Customer,ShipTo,Vendor".
    DEFINE VARIABLE cScopeTypeWithoutContextList AS CHARACTER NO-UNDO INITIAL "System,Company".
    DEFINE VARIABLE cSearchFieldListSettingType  AS CHARACTER NO-UNDO INITIAL "settingName,settingTypeDesc,defaultValue".
    DEFINE VARIABLE cSearchFieldListSetting      AS CHARACTER NO-UNDO INITIAL "scopeTable,scopeField1,scopeField2,scopeField3,settingUser,inactive".

                       
    {system/ttSetting.i}
 
	DEFINE PUBLIC PROPERTY CurrentProgram AS CHARACTER NO-UNDO 
	GET.
	SET.

    DEFINE PUBLIC PROPERTY CurrentProgramHotkey AS CHARACTER NO-UNDO 
    GET.
    SET.
    
	CONSTRUCTOR PUBLIC Setting (  ):
		SUPER ().
		
		DEFINE BUFFER bf-prgrms FOR prgrms.

		RUN spGetSessionParam (INPUT "Company", OUTPUT cCompany).
		RUN spGetSessionParam (INPUT "UserID", OUTPUT cUser).

        ASSIGN
            CurrentProgram = PROGRAM-NAME(2)
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
        
        IF NOT lFound AND ipcScopeTable EQ "ShipTo" THEN
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
            opcValue = ttSetting.settingValue.
            RETURN TRUE.
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
           ipcRecordSource,
           OUTPUT TABLE ttSetting
           ).
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE VOID pGetAll( 
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
    
            IF ipcProgram NE "" THEN
                cSettingQuery = cSettingQuery + ' AND bf-setting.programID EQ ' + QUOTER(ipcProgram).
            
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
                ELSE IF ipcScopeTable EQ "ShipTo" AND ipcScopeField1 NE "" AND ipcScopeField2 NE "" AND ipcScopeField3 NE "" THEN
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
               OR (ipcProgram NE "" AND NOT ttSetting.programID BEGINS ipcProgram)
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
           "",    /* Record Source */
           OUTPUT TABLE ttSetting
           ).
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
        
        bf-ttSetting.priorityID = pGetPriorityID(bf-ttSetting.scopeTable).
        
        pConcatRecordData(BUFFER bf-ttSetting).
        
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
		         
        DEFINE BUFFER bf-setting     FOR setting.
        DEFINE BUFFER bf-settingType FOR settingType.
        DEFINE BUFFER bf-scope       FOR scope.
        DEFINE BUFFER bf-company     FOR company.
        DEFINE BUFFER bf-cust        FOR cust.
        DEFINE BUFFER bf-shipto      FOR shipto.
        DEFINE BUFFER bf-vend        FOR vend.
        
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