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
    File        : ValueListBusinessEntity
    Purpose     : Business Entity for ValueList
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 08.03.2013 15:44:37
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                       FROM PROPATH . 
USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH . 
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.ValueListBusinessEntity 
    INHERITS SmartBusinessEntity: 

    { Consultingwerk/SmartFramework/System/dsValueList.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ValueListBusinessEntity class                                                                     
        Notes:   Passes the handle of the dataset instance to the base class and 
                 sets the default DataAccessName                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ValueListBusinessEntity ():
        SUPER (DATASET dsValueList:HANDLE).
        
        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.System.ValueListDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the eSmartValueListEntry row for the given GUID 
        Notes:   
        @param dsValueList INPUT-OUTPUT Dataset that will contain the eSmartValueListEntry row
        @param poValueListEntryGuid A CharacterHolder that contains the GUID for which to return the eSmartValueListEntry row
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID EntryForGuid (INPUT-OUTPUT DATASET dsValueList,
	                                 poValueListEntryGuid AS CharacterHolder):
		
        DEFINE VARIABLE cGUID    AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE oRequest AS FetchDataRequest NO-UNDO . 

        Consultingwerk.Assertion.ObjectAssert:IsValid (poValueListEntryGuid, 
                                                       "ValueListEntryGuid":U) .

        ASSIGN cGUID    = poValueListEntryGuid:Value 
               oRequest = NEW FetchDataRequest ("eSmartValueListEntry":U,
                                                SUBSTITUTE ("FOR EACH eSmartValueListEntry WHERE eSmartValueListEntry.ValueListEntryGuid = &1":U,
                                                            QUOTER (cGUID))).

        THIS-OBJECT:FetchData (oRequest) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the eSmartValueListEntry row for the given ValueListKey and ValueListEntryKey
        Notes:   
        @param dsValueList INPUT-OUTPUT Dataset that will contain the eSmartValueListEntry row
        @param poEntryByKeyParameter A CharacterHolder that contains the GUID for which to return the eSmartValueListEntry row
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EntryForKey (INPUT-OUTPUT DATASET dsValueList,
                                    poEntryByKeyParameter AS GetValueListEntryByKeyParameter):
        
        DEFINE VARIABLE cValueListKey      AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE cValueListEntryKey AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE oRequest           AS FetchDataRequest NO-UNDO . 

        Consultingwerk.Assertion.ObjectAssert:IsValid (poEntryByKeyParameter, 
                                                       "EntryByKeyParameter":U) .

        ASSIGN cValueListKey      = poEntryByKeyParameter:ValueListKey
               cValueListEntryKey = poEntryByKeyParameter:ValueListEntryKey
               
               oRequest = NEW FetchDataRequest ("eSmartValueList,eSmartValueListEntry":U,
                                                SUBSTITUTE ("FOR EACH eSmartValueList WHERE eSmartValueList.ValueListKey = &1":U,
                                                            QUOTER (cValueListKey)) + CHR (1) +
                                                SUBSTITUTE ("FOR EACH eSmartValueListEntry WHERE eSmartValueListEntry.ValueListEntryKey = &1":U,
                                                            QUOTER (cValueListEntryKey))).

        THIS-OBJECT:FetchData (oRequest) .

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
