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
    File        : KeyFieldAssignmentTypeBusinessEntity
    Purpose     : Business Entity for SmartKeyFieldAssignmentType
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.10.2015 10:27:21
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.SmartFramework.* FROM PROPATH .
USING Consultingwerk.OERA.*           FROM PROPATH .
USING Consultingwerk.Util.*           FROM PROPATH . 
USING Progress.Lang.*                 FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.KeyFieldAssignmentTypeBusinessEntity", type="BusinessEntity") .

CLASS Consultingwerk.SmartFramework.System.KeyFieldAssignmentTypeBusinessEntity 
    INHERITS SmartBusinessEntity
    USE-WIDGET-POOL: 

    { Consultingwerk/SmartFramework/System/dsKeyFieldAssignmentType.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the KeyFieldAssignmentTypeBusinessEntity class                                                                     
        Notes:   Passes the handle of the dataset instance to the base class and 
                 sets the default DataAccessName                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC KeyFieldAssignmentTypeBusinessEntity ():
        SUPER (DATASET dsKeyFieldAssignmentType:HANDLE).
        
        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.System.KeyFieldAssignmentTypeDataAccess":U .

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
        
    END METHOD.

END CLASS.
