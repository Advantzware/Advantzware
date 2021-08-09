 
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
    DEFINE VARIABLE cCompany        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScopeTypeWithContextList    AS CHARACTER NO-UNDO INITIAL "System,Company,Customer,ShipTo,Vendor".
    DEFINE VARIABLE cScopeTypeWithoutContextList AS CHARACTER NO-UNDO INITIAL "System,Company".
            
    {system/ttSetting.i}

	CONSTRUCTOR PUBLIC Setting (  ):
		SUPER ().
		
		RUN spGetSessionParam (INPUT "Company", OUTPUT cCompany).
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC CHARACTER Delete( ipiSettingID AS INT64 ):
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
                BUFFER-COPY bf-scope TO iopttSetting.
        END.

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
		
		DEFINE BUFFER bf-setting FOR setting.
		DEFINE BUFFER bf-settingType FOR settingType.
		DEFINE BUFFER bf-scope FOR scope.

        FIND FIRST ttSetting NO-LOCK
             WHERE ttSetting.settingName EQ ipcSettingName
               AND ttSetting.scopeTable  EQ ipcScopeTable
               AND ttSetting.scopeField1 EQ ipcScopeField1
               AND ttSetting.scopeField2 EQ ipcScopeField2
               AND ttSetting.scopeField3 EQ ipcScopeField3
               AND ttSetting.inactive    EQ FALSE
             NO-ERROR.
        IF NOT AVAILABLE ttSetting THEN
            FIND FIRST ttSetting NO-LOCK
                 WHERE ttSetting.settingName EQ ipcSettingName
                   AND ttSetting.scopeTable  EQ "Company"
                   AND ttSetting.scopeField1 EQ cCompany
                   AND ttSetting.inactive    EQ FALSE
                 NO-ERROR.
        IF NOT AVAILABLE ttSetting THEN
            FIND FIRST ttSetting NO-LOCK
                 WHERE ttSetting.settingName EQ ipcSettingName
                   AND ttSetting.scopeTable  EQ "System"
                   AND ttSetting.inactive    EQ FALSE
                 NO-ERROR.

        IF AVAILABLE ttSetting THEN
            RETURN ttSetting.settingValue.
                                     
        IF ipcScopeTable NE "" THEN
            FIND FIRST bf-scope NO-LOCK
                 WHERE bf-scope.scopeTable  EQ ipcScopeTable
                   AND bf-scope.scopeField1 EQ ipcScopeField1
                   AND bf-scope.scopeField2 EQ ipcScopeField2
                   AND bf-scope.scopeField3 EQ ipcScopeField3
                 NO-ERROR.        
        ELSE
            FIND FIRST bf-scope NO-LOCK
                 WHERE bf-scope.scopeTable  EQ "Company"
                   AND bf-scope.scopeField1 EQ cCompany
                 NO-ERROR.

        IF NOT AVAILABLE bf-scope THEN
            FIND FIRST bf-scope NO-LOCK
                 WHERE bf-scope.scopeTable  EQ "System"
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
                       AND bf-setting.settingUser EQ ""
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

	METHOD PUBLIC VOID GetBySearch(
	    ipcSettingName        AS CHARACTER, 
        iplInactive           AS LOGICAL,
        ipcCategory           AS CHARACTER,
        ipcScopeTable         AS CHARACTER,
        ipcScopeField1        AS CHARACTER,
        ipcScopeField2        AS CHARACTER,
        ipcScopeField3        AS CHARACTER,
        ipcUser               AS CHARACTER, 
        ipcProgram            AS CHARACTER,
        iplDefaultsOnly       AS LOGICAL,
        OUTPUT TABLE ttSetting  ):
		
		pGetAll (
		   ipcSettingName,
           iplInactive,
           ipcCategory,
           ipcScopeTable,
           ipcScopeField1,
           ipcScopeField2,
           ipcScopeField3,
           ipcUser,
           ipcProgram,
           iplDefaultsOnly,
           OUTPUT TABLE ttSetting
           ).
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE VOID pGetAll( 
	    ipcSettingName        AS CHARACTER, 
	    iplInactive           AS LOGICAL,
	    ipcCategory           AS CHARACTER,
	    ipcScopeTable         AS CHARACTER,
	    ipcScopeField1        AS CHARACTER,
	    ipcScopeField2        AS CHARACTER,
	    ipcScopeField3        AS CHARACTER,
        ipcUser               AS CHARACTER, 
        ipcProgram            AS CHARACTER,
        iplDefaultsOnly       AS LOGICAL,
	    OUTPUT TABLE ttSetting):

        EMPTY TEMP-TABLE ttSetting.
        
        DEFINE VARIABLE hdSettingTypeQuery  AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cSettingTypeQuery   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hdSettingQuery      AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cSettingQuery       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lHasSettingOverride AS LOGICAL   NO-UNDO.
        
        DEFINE BUFFER bf-setting     FOR setting.
        DEFINE BUFFER bf-settingType FOR settingType.
		DEFINE BUFFER bf-scope       FOR scope.
		DEFINE BUFFER bf-ttSetting   FOR ttSetting.		
		
		IF iplInactive NE TRUE AND (ipcScopeTable EQ "" OR ipcScopeTable EQ "System") THEN DO:
		    FIND FIRST bf-scope NO-LOCK
		         WHERE bf-scope.scopeTable EQ "System"
		         NO-ERROR.

    		CREATE QUERY hdSettingTypeQuery.
    		hdSettingTypeQuery:ADD-BUFFER(BUFFER bf-settingType:HANDLE).
    		
    		cSettingTypeQuery = ' FOR EACH bf-settingType NO-LOCK '.
    		
    		IF ipcSettingName NE "" OR ipcCategory NE "" THEN
    		    cSettingTypeQuery = cSettingTypeQuery + ' WHERE TRUE '.
    		    
    		IF ipcSettingName NE "" THEN
    		    cSettingTypeQuery = cSettingTypeQuery + ' AND bf-settingType.settingName BEGINS ' + QUOTER(ipcSettingname).
    
    		IF ipcCategory NE "" THEN
    		    cSettingTypeQuery = cSettingTypeQuery + ' AND bf-settingType.categoryTags CONTAINS ' + QUOTER(ipcCategory) + ' AND LOOKUP(' + QUOTER(ipcCategory) + ', bf-settingType.categoryTags) GT 0 '.
    		
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
                    BUFFER-COPY bf-scope TO ttSetting.
                    
    	        hdSettingTypeQuery:GET-NEXT ().
    	    END.
    	    
    	    hdSettingTypeQuery:QUERY-CLOSE ().
    
    	    DELETE OBJECT hdSettingTypeQuery.
	    END.	    

        IF iplDefaultsOnly THEN
            RETURN.

        CREATE QUERY hdSettingQuery.
                
        IF ipcCategory NE "" AND iplInactive NE TRUE THEN DO:
            hdSettingQuery:ADD-BUFFER(BUFFER bf-ttSetting:HANDLE).
            hdSettingQuery:ADD-BUFFER (BUFFER bf-setting:HANDLE).
            
            cSettingQuery = cSettingQuery + " FOR EACH bf-ttSetting ".
            cSettingQuery = cSettingQuery + " , EACH bf-setting NO-LOCK ".
        END.
        ELSE DO:
            hdSettingQuery:ADD-BUFFER (BUFFER bf-setting:HANDLE).

            cSettingQuery = " FOR EACH bf-setting NO-LOCK ".           
        END.

        IF ipcSettingName NE "" OR iplInactive NE ? OR ipcUser NE "" OR ipcProgram NE "" OR 
           ipcScopeTable NE "" OR ipcScopeField1 NE "" OR ipcScopeField2 NE "" OR ipcScopeField3 NE "" THEN
            cSettingQuery = cSettingQuery + " WHERE TRUE ".
        
        IF ipcSettingName NE "" THEN
            cSettingQuery = cSettingQuery + ' AND bf-setting.settingName BEGINS ' + QUOTER(ipcSettingname).                   

        IF iplInactive NE ? THEN
            cSettingQuery = cSettingQuery + ' AND bf-setting.inactive EQ ' + STRING(iplInactive).
        
        IF ipcUser NE "" THEN
            cSettingQuery = cSettingQuery + ' AND bf-setting.settingUser EQ ' + QUOTER(ipcUser).

        IF ipcProgram NE "" THEN
            cSettingQuery = cSettingQuery + ' AND bf-setting.programID EQ ' + QUOTER(ipcProgram).

        hdSettingQuery:QUERY-PREPARE (cSettingQuery).
        
        hdSettingQuery:QUERY-OPEN().

        hdSettingQuery:GET-NEXT().
        
        DO WHILE NOT hdSettingQuery:QUERY-OFF-END:        
            FIND FIRST bf-scope NO-LOCK
                 WHERE bf-scope.scopeID EQ bf-setting.scopeID
                 NO-ERROR.
            
            IF AVAILABLE bf-scope THEN DO:            
                IF (ipcScopeTable  NE "" AND bf-scope.scopeTable  NE ipcScopeTable) OR
                   (ipcScopeField1 NE "" AND bf-scope.scopeField1 NE ipcScopeField1) OR
                   (ipcScopeField2 NE "" AND bf-scope.scopeField2 NE ipcScopeField2) OR
                   (ipcScopeField3 NE "" AND bf-scope.scopeField3 NE ipcScopeField3) THEN DO:
                    hdSettingQuery:GET-NEXT().

                    NEXT.
                END.
                
                FIND FIRST ttSetting NO-LOCK
                     WHERE ttSetting.settingTypeID EQ bf-setting.settingTypeID
                       AND ttSetting.scopeID       EQ bf-setting.scopeID
                       AND ttSetting.settingUser   EQ bf-setting.settingUser
                       AND ttSetting.programID     EQ bf-setting.programID
                       AND ttSetting.inactive      EQ bf-setting.inactive
                     NO-ERROR.
                IF NOT AVAILABLE ttSetting THEN DO:
                    CREATE ttSetting.
                    
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
                
                BUFFER-COPY bf-scope TO ttSetting.
            END.
            
            hdSettingQuery:GET-NEXT().                
        END.
        
        hdSettingQuery:QUERY-CLOSE().
        
        FOR EACH ttSetting:           
            IF ipcUser NE "" AND NOT ttSetting.settingUser BEGINS ipcUser THEN DO:
                DELETE ttSetting.
                NEXT.
            END.  

            IF ipcProgram NE "" AND NOT ttSetting.programID BEGINS ipcProgram THEN DO:
                DELETE ttSetting.
                NEXT.
            END.  
        END.

        DELETE OBJECT hdSettingQuery.	    
	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID GetByUser ( ipcUser AS CHARACTER, OUTPUT TABLE ttSetting ):
        pGetAll(
           "",    /* SettingName  */
           ?,     /* Inactive     */
           "",    /* Category     */
           "",    /* ScopeTable   */
           "",    /* ScopeField1  */
           "",    /* ScopeField2  */
           "",    /* ScopeField3  */
           ipcUser,    /* User         */
           "",    /* Program      */  
           FALSE,  /* Defaults Only */
           OUTPUT TABLE ttSetting
           ).        
    END METHOD.
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID GetByCategory(ipcCategory AS CHARACTER, OUTPUT TABLE ttSetting ):
        pGetAll(
           "",    /* SettingName  */
           ?,     /* Inactive     */
           ipcCategory,    /* Category     */
           "",    /* ScopeTable   */
           "",    /* ScopeField1  */
           "",    /* ScopeField2  */
           "",    /* ScopeField3  */
           "",    /* User         */
           "",    /* Program      */  
           FALSE, /* Defaults Only */
           OUTPUT TABLE ttSetting
           ).
	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID GetAll( OUTPUT TABLE ttSetting ):
        pGetAll(
           "",    /* SettingName  */
           ?,     /* Inactive     */
           "",    /* Category     */
           "",    /* ScopeTable   */
           "",    /* ScopeField1  */
           "",    /* ScopeField2  */
           "",    /* ScopeField3  */
           "",    /* User         */
           "",    /* Program      */ 
           FALSE, /* Defaults Only */  
           OUTPUT TABLE ttSetting).
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
        OUTPUT opiSettingID AS INTEGER):
        
        DEFINE VARIABLE iScopeID AS INTEGER NO-UNDO.   
         
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
                RETURN "Setting already exists. Please enter a different scope/user/program".
        END.
        
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