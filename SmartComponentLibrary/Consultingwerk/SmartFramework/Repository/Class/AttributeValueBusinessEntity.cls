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
    File        : AttributeValueBusinessEntity
    Purpose     : Business Entity for SmartAttributeValue
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 20.10.2015 20:22:29
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                       FROM PROPATH .
USING Consultingwerk.SmartFramework.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .
USING Consultingwerk.OERA.*                            FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH . 
USING Progress.Lang.*                                  FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity", type="BusinessEntity") .

CLASS Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity 
    INHERITS SmartBusinessEntity
    USE-WIDGET-POOL: 

    { Consultingwerk/SmartFramework/Repository/Class/dsAttributeValue.i }
    { Consultingwerk/SmartFramework/Repository/Class/dsDesignAttributeValue.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the AttributeValueBusinessEntity class                                                                     
        Notes:   Passes the handle of the dataset instance to the base class and 
                 sets the default DataAccessName                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttributeValueBusinessEntity ():
        SUPER (DATASET dsAttributeValue:HANDLE).
        
        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Repository.Class.AttributeValueDataAccess":U .

    END CONSTRUCTOR.

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Repository.Class.GetObjectTypeDesignAttributesParameter", datasetInput="false", datasetOutput="true") . 
    /*------------------------------------------------------------------------------
        Purpose: Returns the Design Time Attributes for the given SmartObjectType 
        Notes:   
        @param dsDesignAttributeValue INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectTypeDesignAttributes (INPUT-OUTPUT DATASET dsDesignAttributeValue, 
                                                      poParameter AS GetObjectTypeDesignAttributesParameter):
  
        DEFINE VARIABLE oRequest         AS FetchDataRequest          NO-UNDO . 
        DEFINE VARIABLE oParentClasses   AS GetParentClassesParameter NO-UNDO . 
        DEFINE VARIABLE oObjectTypeModel AS ObjectTypeDatasetModel    NO-UNDO . 
        DEFINE VARIABLE i                AS INTEGER                   NO-UNDO .
        DEFINE VARIABLE iEntries         AS INTEGER                   NO-UNDO .
        DEFINE VARIABLE cParentClassGuid AS CHARACTER                 NO-UNDO .

        DATASET dsDesignAttributeValue:EMPTY-DATASET () .
         
        oRequest = NEW FetchDataRequest ("*":U,
                                         SUBSTITUTE ('for each eSmartAttributeValue where eSmartAttributeValue.ObjectTypeGuid             = &1':U + 
                                                                                   '  and eSmartAttributeValue.ObjectMasterGuid           = ""':U +      
                                                                                   '  and eSmartAttributeValue.ContainerObjectMasterGuid  = ""':U +                
                                                                                   '  and eSmartAttributeValue.ObjectInstanceGuid         = ""':U,
                                                     QUOTER (poParameter:ObjectTypeGuid)),
                                         0) .          
        
        THIS-OBJECT:FetchData (oRequest) .
        
        FOR EACH eSmartAttributeValue ON ERROR UNDO, THROW:
  
            CREATE eDesignAttributeValue.
            
            BUFFER-COPY eSmartAttributeValue TO eDesignAttributeValue .
        
            FIND FIRST eSmartAttribute OF eSmartAttributeValue NO-ERROR . 
            IF AVAILABLE eSmartAttribute THEN 
                BUFFER-COPY eSmartAttribute TO eDesignAttributeValue .
            
            ASSIGN eDesignAttributeValue.IsInherited = FALSE . 
            
            FINALLY:
                ErrorHelper:ResetErrorStatus() .		
            END FINALLY.        
        END.
  
        oParentClasses = NEW GetParentClassesParameter (poParameter:ObjectTypeGuid) .
        
        oObjectTypeModel = NEW ObjectTypeDatasetModel () .
        oObjectTypeModel:GetParentClasses(oParentClasses) .
        
        ASSIGN iEntries = NUM-ENTRIES (oParentClasses:ParentObjectTypeGuids, CHR(1)) .
        
        DO i = 1 TO iEntries:
            oRequest = NEW FetchDataRequest ("*":U,
                                             SUBSTITUTE ('for each eSmartAttributeValue where eSmartAttributeValue.ObjectTypeGuid             = &1':U + 
                                                                                       '  and eSmartAttributeValue.ObjectMasterGuid           = ""':U +      
                                                                                       '  and eSmartAttributeValue.ContainerObjectMasterGuid  = ""':U +                
                                                                                       '  and eSmartAttributeValue.ObjectInstanceGuid         = ""':U,
                                                         QUOTER (ENTRY (i, oParentClasses:ParentObjectTypeGuids, CHR(1)))),
                                             0) .          
            
            THIS-OBJECT:FetchData (oRequest) .
            
            FOR EACH eSmartAttributeValue ON ERROR UNDO, THROW:
      
                FIND eDesignAttributeValue WHERE eDesignAttributeValue.ObjectTypeGuid     = poParameter:ObjectTypeGuid
                                             AND eDesignAttributeValue.ObjectMasterGuid   = "":U
                                             AND eDesignAttributeValue.ObjectInstanceGuid = "":U
                                             AND eDesignAttributeValue.AttributeLabel     = eSmartAttributeValue.AttributeLabel NO-ERROR .
      
                /* Value not (yet) overridden by child class */
                IF NOT AVAILABLE eDesignAttributeValue THEN DO:
                    CREATE eDesignAttributeValue.
                    
                    BUFFER-COPY eSmartAttributeValue TO eDesignAttributeValue 
                        ASSIGN eDesignAttributeValue.ObjectTypeGuid = poParameter:ObjectTypeGuid.
                
                    FIND FIRST eSmartAttribute OF eSmartAttributeValue NO-ERROR . 
                    IF AVAILABLE eSmartAttribute THEN 
                        BUFFER-COPY eSmartAttribute TO eDesignAttributeValue .
                    
                    ASSIGN eDesignAttributeValue.IsInherited   = TRUE 
                           eDesignAttributeValue.InheritedFrom = ENTRY (LOOKUP (eSmartAttributeValue.ObjectTypeGuid, 
                                                                                oParentClasses:ParentObjectTypeGuids,
                                                                                CHR (1)), 
                                                                        oParentClasses:ParentObjectTypeNames) . 
                END.
                ELSE DO: 
                    IF eDesignAttributeValue.InheritedFrom = "":U THEN
                        ASSIGN eDesignAttributeValue.InheritedFrom = ENTRY (LOOKUP (eSmartAttributeValue.ObjectTypeGuid, 
                                                                                    oParentClasses:ParentObjectTypeGuids,
                                                                                    CHR (1)), 
                                                                            oParentClasses:ParentObjectTypeNames) .
                    /* When not yet Constant Value, apply Constant value 
                       setting from parent class */
                    IF eDesignAttributeValue.ConstantValue = FALSE THEN 
                        eDesignAttributeValue.ConstantValue = eSmartAttributeValue.ConstantValue .  
                END.
                
                FINALLY:
                    ErrorHelper:ResetErrorStatus() .        
                END FINALLY.        
            END.
        END.
        
        FINALLY:
            DATASET dsAttributeValue:EMPTY-DATASET ().		
        END FINALLY.
        
    END METHOD .

    @InvokeMethod (template="invoke-receive-dataset", parameterClassName="Consultingwerk.SmartFramework.Repository.Class.GetObjectTypeDesignAttributeParameter", datasetInput="false", datasetOutput="true") . 
    /*------------------------------------------------------------------------------
        Purpose: Returns a single SmartAttributeValue record to the client
        Notes:   
        @param dsDesignAttributeValue INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectTypeDesignAttribute (INPUT-OUTPUT DATASET dsDesignAttributeValue, 
                                                     poParameter AS Consultingwerk.SmartFramework.Repository.Class.GetObjectTypeDesignAttributeParameter):

        DEFINE VARIABLE oRequest         AS FetchDataRequest          NO-UNDO . 
        DEFINE VARIABLE oObjectTypeModel AS ObjectTypeDatasetModel    NO-UNDO . 

        oObjectTypeModel = NEW ObjectTypeDatasetModel () .
        oObjectTypeModel:SmartObjectType:Filter:ObjectTypeName:EQ (poParameter:ObjectTypeName):Run () .

        BufferAssert:IsAvailable (oObjectTypeModel:SmartObjectType:QueryHandle:GET-BUFFER-HANDLE (1)) .

        DATASET dsDesignAttributeValue:EMPTY-DATASET () .
         
        oRequest = NEW FetchDataRequest ("*":U,
                                         SUBSTITUTE ('for each eSmartAttributeValue where eSmartAttributeValue.ObjectTypeGuid             = &1':U + 
                                                                                   '  and eSmartAttributeValue.ObjectMasterGuid           = ""':U +      
                                                                                   '  and eSmartAttributeValue.ContainerObjectMasterGuid  = ""':U +                
                                                                                   '  and eSmartAttributeValue.ObjectInstanceGuid         = ""':U +
                                                                                   '  and eSmartAttributeValue.AttributeLabel             = &2':U,
                                                     QUOTER (oObjectTypeModel:SmartObjectType:ObjectTypeGuid),
                                                     QUOTER (poParameter:AttributeLabel)),
                                         0) .          

        THIS-OBJECT:FetchData (oRequest) .
        
        FOR EACH eSmartAttributeValue ON ERROR UNDO, THROW:

            CREATE eDesignAttributeValue.
            
            BUFFER-COPY eSmartAttributeValue TO eDesignAttributeValue .
        
            FIND FIRST eSmartAttribute OF eSmartAttributeValue NO-ERROR . 
            IF AVAILABLE eSmartAttribute THEN 
                BUFFER-COPY eSmartAttribute TO eDesignAttributeValue .
            
            ASSIGN eDesignAttributeValue.IsInherited = FALSE . 
            
            FINALLY:
                ErrorHelper:ResetErrorStatus() .        
            END FINALLY.        
        END.

        FINALLY:
            DATASET dsAttributeValue:EMPTY-DATASET ().      
        END FINALLY.
        
    END METHOD .

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
        
    END METHOD.
    
END CLASS.
