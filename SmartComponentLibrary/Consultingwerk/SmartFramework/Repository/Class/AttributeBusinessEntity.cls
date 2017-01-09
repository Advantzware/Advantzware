/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : AttributeBusinessEntity
    Purpose     : Business Entity for Attribute
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 21.05.2014 12:50:30
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .
USING Consultingwerk.OERA.*                            FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH . 
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.Class.AttributeBusinessEntity 
    INHERITS SmartBusinessEntity: 

    { Consultingwerk/SmartFramework/Repository/Class/dsAttribute.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the AttributeBusinessEntity class                                                                     
        Notes:   Passes the handle of the dataset instance to the base class and 
                 sets the default DataAccessName                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttributeBusinessEntity ():
        SUPER (DATASET dsAttribute:HANDLE).
        
        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Repository.Class.AttributeDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and 
                 Update operations (i.e. population of aggregated values)                                                                     
        Notes:   Invoked during FetchData () and SaveChanges ()                                                                     
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook for high level data validation before Update 
                 operations                                                                     
        Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
                 is set, the Update operation will be cancelled before writing back
                 the data to the database using the DataAccess object                                                                      
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ValidateData ():
        
        FOR EACH eSmartAttribute ON ERROR UNDO, THROW:
            
            /* Attribute Group Guid */
            Consultingwerk.OERA.Validate:CanFind (BUFFER eSmartAttribute:HANDLE, "AttributeGroupGuid":U, 
                                                  "Consultingwerk.SmartFramework.Repository.Class.AttributeGroupBusinessEntity":U,
                                                  "eSmartAttributeGroup":U, 
                                                  SUBSTITUTE ("FOR EACH eSmartAttributeGroup WHERE eSmartAttributeGroup.AttributeGroupGuid = &1":U, QUOTER (eSmartAttribute.AttributeGroupGuid)),
                                                  MessageFormatter:GetMessage ("VALMSG":U, 2, "Attribute Group":U, eSmartAttribute.AttributeGroupGuid)).
                                
            /* Attribute Label */
            Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartAttribute:HANDLE, "AttributeLabel":U, 
                                                           MessageFormatter:GetMessage ("VALMSG":U, 1, "Attribute Label":U)).
            
            /* Technical Name */
            IF eSmartAttribute.TechnicalName EQ "":U THEN 
                ASSIGN eSmartAttribute.TechnicalName = eSmartAttribute.AttributeLabel.
                
            /* Constant Level */
            Consultingwerk.OERA.Validate:IsInList (BUFFER eSmartAttribute:HANDLE, "ConstantLevel":U, 
                                                   ",Class,Master":U, 
                                                   MessageFormatter:GetMessage ("VALMSG":U, 2, "Constant Level":U, eSmartAttribute.ConstantLevel)).
            
            /* Lookup Type */
            Consultingwerk.OERA.Validate:IsInList (BUFFER eSmartAttribute:HANDLE, "LookupType":U, 
                                                   ",LIST,DIALOG,DIALOG-R":U, 
                                                   MessageFormatter:GetMessage ("VALMSG":U, 2, "Lookup Type":U, eSmartAttribute.LookupType)).
            
            /* Property Type */
            Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartAttribute:HANDLE, "PropertyType":U, 
                                                           MessageFormatter:GetMessage ("VALMSG":U, 1, "Property Type":U)).
            
            /* Repository Type */
            Consultingwerk.OERA.Validate:IsInList (BUFFER eSmartAttribute:HANDLE, "RepositoryType":U, 
                                                   "CHARACTER,INTEGER,INT64,DATE,DATETIME,DATETIME-TZ,DECIMAL,LOGICAL,RAW":U, 
                                                   MessageFormatter:GetMessage ("VALMSG":U, 2, "Repository Type":U, eSmartAttribute.RepositoryType)).
        END.
        
    END METHOD.

END CLASS.
