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
    File        : ValueListProvider
    Purpose     : Service class to provide an Infragistics ValueList by
                  a list key
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 15 20:42:34 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*         FROM PROPATH . 
USING Consultingwerk.OERA.*              FROM PROPATH . 
USING Consultingwerk.Util.*              FROM PROPATH . 
USING Consultingwerk.Windows.Framework.* FROM PROPATH .  
USING Progress.Lang.*                    FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.ValueListProvider 
    IMPLEMENTS IValueListProvider: 

    { Consultingwerk/SmartFramework/System/dsValueList.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the ValueList for the given key
        Notes:   
        @param pcValueListKey The key to return a value list for 
        @return The ValueList for the given key
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC Infragistics.Win.ValueList ValueListFromKey (pcValueListKey AS CHARACTER):
		
	    DEFINE VARIABLE oRequest AS FetchDataRequest NO-UNDO . 	
    		
		oRequest = NEW FetchDataRequest ("*":U,
		                                 SUBSTITUTE ("FOR EACH eSmartValueList WHERE eSmartValueList.ValueListKey = &1":U,
                                                     QUOTER (pcValueListKey)),
		                                 1) .

        FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                       "Consultingwerk.SmartFramework.System.ValueListBusinessEntity":U,
                                                       oRequest,
                                                       OUTPUT DATASET dsValueList) .        		

        IF CAN-FIND (FIRST eSmartValueListEntry WHERE eSmartValueListEntry.ValueListEntryImageKey > "":U) THEN 
            RETURN ValueListHelper:FromTempTableAndImageProvider (TEMP-TABLE eSmartValueListEntry:HANDLE,
                                                                  "ValueListEntryKey":U,
                                                                  "ValueListEntryDescription":U,
                                                                  "&1":U,
                                                                  "ValueListEntryImageKey":U,
                                                                  "":U,
                                                                  "":U) .            
        ELSE 
            RETURN ValueListHelper:FromTempTable (TEMP-TABLE eSmartValueListEntry:HANDLE,
                                                  "ValueListEntryKey":U,
                                                  "ValueListEntryDescription":U,
                                                  "&1":U) .

        FINALLY:
            DATASET dsValueList:EMPTY-DATASET () .		
        END FINALLY.

	END METHOD .

END CLASS.
