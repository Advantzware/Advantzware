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
    File        : SmartBusinessEntityConfigurationProvider
    Purpose     : Default ISmartBusinessEntityConfiguration service
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 03 19:28:56 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Assertion.*             FROM PROPATH . 
USING Consultingwerk.Framework.*             FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Consultingwerk.OERA.*                  FROM PROPATH . 
USING Progress.Lang.*                        FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH.

CLASS Consultingwerk.SmartFramework.System.SmartBusinessEntityConfigurationProvider IMPLEMENTS ISmartBusinessEntityConfigurationProvider: 
 
    {Consultingwerk/SmartFramework/System/dsSmartBusinessEntity.i}
 
    DEFINE VARIABLE lInitialized AS LOGICAL NO-UNDO INIT FALSE . 
 
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityConfigurationProvider class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SmartBusinessEntityConfigurationProvider ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the ISmartBusinessEntityConfiguration instance for the a 
                 Business Entity
        Notes:   Returns ? when no configuration exists
        @param pcBusinessEntityName The full class name (including package) of the SmartBusinessEntity
        @return The ISmartBusinessEntityConfiguration instance for the requested Business Entity
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC ISmartBusinessEntityConfiguration GetSmartBusinessEntityConfiguration (pcBusinessEntityName AS CHARACTER):
		
        DEFINE VARIABLE oClassName      AS ClassName                             NO-UNDO .
        DEFINE VARIABLE oBusinessEntity AS SmartBusinessEntityConfiguration      NO-UNDO .
        DEFINE VARIABLE oTable          AS SmartBusinessEntityTableConfiguration NO-UNDO .  
		
		IF NOT lInitialized THEN 
		    THIS-OBJECT:ResetCache () .
		
		Assert:NotNullOrEmpty (pcBusinessEntityName, "pcBusinessEntityName":U) .

        oClassName = ClassHelper:SplitClassName (pcBusinessEntityName) .        

        IF NOT CAN-FIND (eSmartBusinessEntity WHERE eSmartBusinessEntity.BusinessEntityPackage = oClassName:PackageName
                                                AND eSmartBusinessEntity.BusinessEntityName    = oClassName:ClassName) THEN 
            RETURN ? .                                                     

        FIND eSmartBusinessEntity WHERE eSmartBusinessEntity.BusinessEntityPackage = oClassName:PackageName
                                    AND eSmartBusinessEntity.BusinessEntityName    = oClassName:ClassName . 

        oBusinessEntity = NEW SmartBusinessEntityConfiguration () . 

        ASSIGN oBusinessEntity:BusinessEntityPackage = eSmartBusinessEntity.BusinessEntityPackage
               oBusinessEntity:BusinessEntityName    = eSmartBusinessEntity.BusinessEntityName   
               oBusinessEntity:BusinessEntityPurpose = eSmartBusinessEntity.BusinessEntityPurpose
               oBusinessEntity:DBRequired            = eSmartBusinessEntity.DBRequired           
               oBusinessEntity:TrackDeletions        = eSmartBusinessEntity.TrackDeletions       
               .

        FOR EACH eSmartBusinessEntityTable OF eSmartBusinessEntity ON ERROR UNDO, THROW:
           
            ASSIGN oTable = NEW SmartBusinessEntityTableConfiguration () . 
            
            ASSIGN oTable:TableName          = eSmartBusinessEntityTable.TableName         
                   oTable:TableOrder         = eSmartBusinessEntityTable.TableOrder                      
                   oTable:TableLabelSingular = eSmartBusinessEntityTable.TableLabelSingular
                   oTable:TableLabelPlural   = eSmartBusinessEntityTable.TableLabelPlural  
                   oTable:BatchSize          = eSmartBusinessEntityTable.BatchSize         
                   oTable:ShouldUseBatching  = eSmartBusinessEntityTable.ShouldUseBatching 
                   oTable:AllowCreate        = eSmartBusinessEntityTable.AllowCreate       
                   oTable:AllowDelete        = eSmartBusinessEntityTable.AllowDelete       
                   oTable:AllowUpdate        = eSmartBusinessEntityTable.AllowUpdate       
                   .
                   
            oBusinessEntity:Tables:Add (oTable) .                  
        END. 

        RETURN oBusinessEntity .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Resets the services cache (if present) 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ResetCache ():

        DEFINE VARIABLE oRequest AS FetchDataRequest NO-UNDO . 

        DATASET dsSmartBusinessEntity:EMPTY-DATASET () .

        ASSIGN oRequest     = NEW FetchDataRequest ("*":U, 0) 
               lInitialized = TRUE .

        IF CONNECTED ("SmartDB":U) THEN 
            ServiceInterface:FetchData ("Consultingwerk.SmartFramework.System.SmartBusinessEntityBusinessEntity":U,
                                        oRequest, 
                                        OUTPUT DATASET dsSmartBusinessEntity) .
        ELSE 
            FrameworkSettings:ServiceAdapter:RetrieveData("":U,
                                                          "Consultingwerk.SmartFramework.System.SmartBusinessEntityBusinessEntity":U,
                                                          oRequest, 
                                                          OUTPUT DATASET dsSmartBusinessEntity) .
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartBusinessEntityConfigurationProvider class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartBusinessEntityConfigurationProvider ():

        DATASET dsSmartBusinessEntity:EMPTY-DATASET () .

    END DESTRUCTOR .

END CLASS.
