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
    File        : TableIOStateEnum
    Purpose     : Enumeration for possible TableIOState values
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 20 19:50:58 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Util.* FROM PROPATH . 
USING Progress.Lang.*       FROM PROPATH  .

CLASS Consultingwerk.SmartComponents.Enum.TableIOStateEnum:
        
    /*------------------------------------------------------------------------------
        Purpose: When no record is available, only Add and Copy will make sense                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY AddCopyAllowed AS CHARACTER NO-UNDO INIT "AddCopyAllowed":U 
    GET .
         
    /*------------------------------------------------------------------------------
        Purpose: The Fields of the Viewer/DataGrid have been disabled, allow Edit, 
                 Add, Copy and Delete                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY FieldsDisabled AS CHARACTER NO-UNDO INIT "FieldsDisabled":U 
    GET .
         
    /*------------------------------------------------------------------------------
        Purpose: The Fields of the Viewer/DataGrid have been enabled but no change 
                 has been made so far, allow Edit, Add, Copy and Delete                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY FieldsEnabled AS CHARACTER NO-UNDO INIT "FieldsEnabled":U 
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Changing Data, only cancel and save are enabled                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ModifyingData AS CHARACTER NO-UNDO INIT "ModifyingData":U 
    GET .

    /*------------------------------------------------------------------------------
        Purpose: No datasource available                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY NoDataSource AS CHARACTER NO-UNDO INIT "NoDataSource":U 
    GET .

    /*------------------------------------------------------------------------------
        Purpose: No parent record is available                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY NoParentRecordAvailable AS CHARACTER NO-UNDO INIT "NoParentRecordAvailable":U 
    GET .
    
    /*------------------------------------------------------------------------------
        Purpose: No record is available                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY NoRecordAvailable AS CHARACTER NO-UNDO INIT "NoRecordAvailable":U 
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TableIOStateEnum class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PRIVATE TableIOStateEnum ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Validates a character against the list of valid TableIOStateEnum
                 values                                                                       
        Notes:      
        @param pcDataSourceState Character value of a data source state 
        @return Logical value indicating if the Character value stands for a valid enum member                                                                              
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC LOGICAL IsValid (pcDataSourceState AS CHARACTER):
        
        RETURN ListHelper:EntryIsInList (pcDataSourceState, "AddCopyAllowed,FieldsDisabled,FieldsEnabled,ModifyingData,NoDataSource,NoRecordAvailable,NoParentRecordAvailable":U) .      
                        
    END METHOD.

END CLASS.