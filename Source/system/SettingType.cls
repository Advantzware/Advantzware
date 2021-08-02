 
 /*------------------------------------------------------------------------
    File        : SettingType
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Thu Jul 29 05:35:51 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS system.SettingType: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
        
	CONSTRUCTOR PUBLIC SettingType (  ):
		SUPER ().
		
		RUN spGetSessionParam (INPUT "Company", OUTPUT cCompany).
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC CHARACTER Delete( ipiSettingID AS INTEGER ):
        DEFINE BUFFER bf-settingType FOR settingType.
		
		FIND FIRST bf-settingType EXCLUSIVE-LOCK
		     WHERE bf-settingType.settingTypeID EQ ipiSettingID
		     NO-ERROR.
		IF NOT AVAILABLE bf-settingType THEN
		    RETURN "Setting is invalid or locked".
		
		DELETE bf-settingType.
		
		RETURN "".
	END METHOD.
	
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    

	METHOD PUBLIC CHARACTER Update( 
        ipiSettingTypeID AS INTEGER, 
        ipcSettingName   AS CHARACTER,
        ipcDescription   AS CHARACTER, 
        ipcDataType      AS CHARACTER, 
        ipcVaildValue    AS CHARACTER, 
        ipcDefaultValue  AS CHARACTER, 
        ipcValidValueMin AS CHARACTER,
        ipValidValueMax  AS CHARACTER, 
        iplIsPassword    AS LOGICAL, 
        iplHasContext    AS LOGICAL, 
        ipiSecurityLevel AS INTEGER, 
        ipcCategoryTags  AS CHARACTER,
        OUTPUT opiSettingTypeID AS INTEGER):
                
                
        DEFINE BUFFER bf-settingType FOR settingType.	
        
                            
        FIND FIRST bf-settingType EXCLUSIVE-LOCK
             WHERE bf-settingType.settingTypeID EQ ipiSettingTypeID
             NO-ERROR.
        IF NOT AVAILABLE bf-settingType THEN DO:
            CREATE bf-settingType.
            
        END.
        ASSIGN                
         bf-settingType.settingName   = ipcSettingName
         bf-settingType.description   = ipcDescription
         bf-settingType.dataType      = ipcDataType
         bf-settingType.validValues   = ipcVaildValue                          
         bf-settingType.defaultValue  = ipcDefaultValue
         bf-settingType.validValueMin = ipcValidValueMin                          
         bf-settingType.validValueMax = ipValidValueMax
         bf-settingType.isPassword    = iplIsPassword                          
         bf-settingType.hasContext    = iplHasContext
         bf-settingType.securityLevel = ipiSecurityLevel                          
         bf-settingType.categoryTags  = ipcCategoryTags.                                                   

         opiSettingTypeID = INTEGER(bf-settingType.settingTypeID).
        RETURN "".
        
	END METHOD.
    
END CLASS.