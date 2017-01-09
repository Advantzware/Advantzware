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
    File        : RelationBusinessEntity
    Purpose     : Business Entity for Relation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.04.2013 21:42:53
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.*        FROM PROPATH . 
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH . 
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.RelationBusinessEntity 
    INHERITS SmartBusinessEntity: 

    { Consultingwerk/SmartFramework/System/dsRelation.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RelationBusinessEntity class                                                                     
        Notes:   Passes the handle of the dataset instance to the base class and 
                 sets the default DataAccessName                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RelationBusinessEntity ():
        SUPER (DATASET dsRelation:HANDLE).
        
        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.System.RelationDataAccess":U .

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
        
        FOR EACH eSmartRelation ON ERROR UNDO, THROW:
            
            Consultingwerk.OERA.Validate:IsInList (BUFFER eSmartRelation:HANDLE,
                                                   "ParentCardinality":U,
                                                   "1,01,01n,1n":U,
                                                   MessageFormatter:GetMessage ("VALMSG":U, 2, "Parent Cardinality":U, eSmartRelation.ParentCardinality)) .

            Consultingwerk.OERA.Validate:IsInList (BUFFER eSmartRelation:HANDLE,
                                                   "ChildCardinality":U,
                                                   "01n,1,01,1n":U,
                                                   MessageFormatter:GetMessage ("VALMSG":U, 2, "Child Cardinality":U, eSmartRelation.ChildCardinality)) .
            
        END . 
        
    END METHOD.

END CLASS.
