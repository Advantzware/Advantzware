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
    File        : SortHelper
    Purpose     : Helper class that allows to sort a delimited character 
                  list using a temp-table 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jul 14 10:24:41 CEST 2015
    Notes       : Used by Consultingwerk.Util.ListHelper:SortList()
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*         FROM PROPATH .  
USING Consultingwerk.Util.Support.* FROM PROPATH .  
USING Progress.Lang.*               FROM PROPATH .

CLASS Consultingwerk.Util.Support.SortHelper: 

    DEFINE TEMP-TABLE ttList NO-UNDO 
        FIELD ListEntry AS CHARACTER 
        INDEX ListEntry ListEntry . 

    /*------------------------------------------------------------------------------
        Purpose: Returns a delimited CHARACTER list sorted
        Notes:   Empty list entries will be removed
        @param pcValues The delimited list to sort
        @param pcDelimiter The list delimiter
        @return The sorted list 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER SortList (pcValues AS CHARACTER,
	                                  pcDelimiter AS CHARACTER):
		
        DEFINE VARIABLE i       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
		
		EMPTY TEMP-TABLE ttList . 
		
		DO i = 1 TO NUM-ENTRIES (pcValues, pcDelimiter):
		    CREATE ttList . 
		    ASSIGN ttList.ListEntry = ENTRY (i, pcValues, pcDelimiter) .
		END.
		
		FOR EACH ttList ON ERROR UNDO, THROW:
		    cReturn = cReturn + ttList.ListEntry + pcDelimiter .
		END.

        RETURN TRIM (cReturn, pcDelimiter) .
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a delimited LONGCHAR list sorted
        Notes:   Empty list entries will be removed
        @param pcValues The delimited list to sort
        @param pcDelimiter The list delimiter
        @return The sorted list 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR SortList (pcValues AS LONGCHAR,
                                     pcDelimiter AS CHARACTER):
        
        DEFINE VARIABLE i       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
        
        EMPTY TEMP-TABLE ttList . 
        
        DO i = 1 TO NUM-ENTRIES (pcValues, pcDelimiter):
            CREATE ttList . 
            ASSIGN ttList.ListEntry = ENTRY (i, pcValues, pcDelimiter) .
        END.
        
        FOR EACH ttList ON ERROR UNDO, THROW:
            cReturn = cReturn + ttList.ListEntry + pcDelimiter .
        END.

        RETURN TRIM (cReturn, pcDelimiter) .
        
    END METHOD .

END CLASS.
