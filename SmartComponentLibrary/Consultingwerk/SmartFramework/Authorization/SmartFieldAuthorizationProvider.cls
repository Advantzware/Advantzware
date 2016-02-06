/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartFieldAuthorizationProvider
    Purpose     : SmartDB driven field authorization provider implementation 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Mar 31 21:19:51 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                    FROM PROPATH  .
USING Consultingwerk.Framework.Authorization.*      FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH.
USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.Util.*                         FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .
USING Consultingwerk.Assertion.* FROM PROPATH.

{Consultingwerk/products.i} 

CLASS Consultingwerk.SmartFramework.Authorization.SmartFieldAuthorizationProvider 
    IMPLEMENTS IFieldAuthorizationProvider: 

    { Consultingwerk/SmartFramework/Authorization/dsFieldSecurityItem.i }

	/*------------------------------------------------------------------------------
	    Purpose: Constructor for the SmartFieldAuthorizationProvider class
	    Notes:
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SmartFieldAuthorizationProvider ():
		SUPER ().
		
		THIS-OBJECT:RefreshFieldSecurityItemCache () .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Provides authorization for fields 
        Notes:   
        @param poRequestor The reference to the requesting object
        @param pcFieldName The field name to secure
        @param pcSecurityKey The security key of the field's container (Viewer, Browser)
        @return The FieldRestrictionEnum value  
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC FieldRestrictionEnum IsFieldRestricted (poRequestor AS Progress.Lang.Object, 
	                                                      pcFieldName AS CHARACTER, 
	                                                      pcSecurityKey AS CHARACTER):
		
        DEFINE VARIABLE cObjectName           AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE oSecurityService      AS ISecurityService     NO-UNDO . 
        DEFINE VARIABLE oRestriction          AS FieldRestrictionEnum NO-UNDO .  
        DEFINE VARIABLE cAbbreviatedFieldName AS CHARACTER            NO-UNDO .

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcFieldName "'pcFieldName':U"} .
        Assert:Equals (NUM-ENTRIES (pcFieldName, ".":U), 2) .        

        oSecurityService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Authorization.ISecurityService} .        

        IF NOT VALID-OBJECT (oSecurityService) THEN 
            RETURN FieldRestrictionEnum:None .
			
				
		IF VALID-OBJECT (poRequestor) THEN DO:		
/* Mike Fechner, Consultingwerk Ltd. 15.04.2015
   SCL-755 : Workaround for poRequestor:GetClass():TypeName crash on 
             10.2B */
&IF PROVERSION BEGINS "10" AND DEFINED (DotNetAccessible) NE 0 &THEN        
            ASSIGN cObjectName = CAST (poRequestor, System.Object):GetType():ToString() 
&ELSE    
	        ASSIGN cObjectName = poRequestor:GetClass():TypeName 
&ENDIF        
        
	               cAbbreviatedFieldName         = pcFieldName
		           ENTRY (1, pcFieldName, ".":U) = "*":U .
        END.

		oRestriction = THIS-OBJECT:SecurityCheck (oSecurityService,
		                                          cObjectName,
		                                          pcFieldName,
		                                          pcSecurityKey,
		                                          "SFV":U,
		                                          FieldRestrictionEnum:Hidden) . 
		
		IF VALID-OBJECT (oRestriction) THEN 
		    RETURN oRestriction .

        oRestriction = THIS-OBJECT:SecurityCheck (oSecurityService,
                                                  cObjectName,
                                                  cAbbreviatedFieldName,
                                                  pcSecurityKey,
                                                  "SFV":U,
                                                  FieldRestrictionEnum:Hidden) . 
        
        IF VALID-OBJECT (oRestriction) THEN 
            RETURN oRestriction .
		
        oRestriction = THIS-OBJECT:SecurityCheck (oSecurityService,
                                                  cObjectName,
                                                  pcFieldName,
                                                  pcSecurityKey,
                                                  "SFS":U,
                                                  FieldRestrictionEnum:Disabled) . 
        
        IF VALID-OBJECT (oRestriction) THEN 
            RETURN oRestriction .

        oRestriction = THIS-OBJECT:SecurityCheck (oSecurityService,
                                                  cObjectName,
                                                  cAbbreviatedFieldName,
                                                  pcSecurityKey,
                                                  "SFS":U,
                                                  FieldRestrictionEnum:Disabled) . 
        
        IF VALID-OBJECT (oRestriction) THEN 
            RETURN oRestriction .
		
		RETURN FieldRestrictionEnum:None .
		
        FINALLY:
            ErrorHelper:ResetErrorStatus() .		
        END FINALLY.

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Refreshes the cache of field security items
	    Notes:
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID RefreshFieldSecurityItemCache ():
		
		DEFINE VARIABLE oRequest AS FetchDataRequest NO-UNDO . 
		
		oRequest = NEW FetchDataRequest ("eSmartFieldSecurityItem":U,
		                                 0) .
 		
		IF CONNECTED ("SmartDB":U) THEN 
		    ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Authorization.FieldSecurityItemBusinessEntity":U,
		                                oRequest,
		                                OUTPUT DATASET dsFieldSecurityItem) .
		ELSE 
            FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                           "Consultingwerk.SmartFramework.Authorization.FieldSecurityItemBusinessEntity":U,
                                                           oRequest,
                                                           OUTPUT DATASET dsFieldSecurityItem) .

	END METHOD.

	/*------------------------------------------------------------------------------
        Purpose: Performs field security check for a single Realm (Visible/Enabled) 
        Notes:   
        @param poSecurityService The reference to the ISecurityService implementation
        @param pcObjectName The name of the requesting object
        @param pcFieldName The field name to secure
        @param pcSecurityKey The security key of the field's container (Viewer, Browser)
        @param pcSecurityRealm The field security realm value to check
        @param poFieldRestrictionEnum The FieldRestrictionEnum value to return when restricted
        @return The FieldRestrictionEnum value  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED FieldRestrictionEnum SecurityCheck (poSecurityService AS ISecurityService,
	                                                     pcObjectName AS CHARACTER, 
                                                         pcFieldName AS CHARACTER, 
                                                         pcSecurityKey AS CHARACTER,
                                                         pcSecurityRealm AS CHARACTER,
                                                         poFieldRestrictionEnum AS FieldRestrictionEnum):
		
        /* Exact match on field name */ 
        IF pcSecurityKey > "":U THEN DO:
            FIND FIRST eSmartFieldSecurityItem WHERE eSmartFieldSecurityItem.FieldName   = pcFieldName
                                                 AND eSmartFieldSecurityItem.SecurityKey = pcSecurityKey
                                                 AND eSmartFieldSecurityItem.ObjectName  = pcObjectName NO-ERROR . 
        
            IF AVAILABLE eSmartFieldSecurityItem AND 
               poSecurityService:IsRestricted (pcSecurityRealm, eSmartFieldSecurityItem.FieldSecurityItemGuid) THEN 
                RETURN poFieldRestrictionEnum  . 
        END.
        
        FIND FIRST eSmartFieldSecurityItem WHERE eSmartFieldSecurityItem.FieldName   = pcFieldName
                                             AND eSmartFieldSecurityItem.SecurityKey = "":U
                                             AND eSmartFieldSecurityItem.ObjectName  = pcObjectName NO-ERROR . 
    
        IF AVAILABLE eSmartFieldSecurityItem AND 
           poSecurityService:IsRestricted (pcSecurityRealm, eSmartFieldSecurityItem.FieldSecurityItemGuid) THEN 
            RETURN poFieldRestrictionEnum . 

        FIND FIRST eSmartFieldSecurityItem WHERE eSmartFieldSecurityItem.FieldName   = pcFieldName
                                             AND eSmartFieldSecurityItem.SecurityKey = "":U
                                             AND eSmartFieldSecurityItem.ObjectName  = "":U NO-ERROR . 
    
        IF AVAILABLE eSmartFieldSecurityItem AND 
           poSecurityService:IsRestricted (pcSecurityRealm, eSmartFieldSecurityItem.FieldSecurityItemGuid) THEN 
            RETURN poFieldRestrictionEnum . 
		
		RETURN ? .

	END METHOD.

END CLASS.
