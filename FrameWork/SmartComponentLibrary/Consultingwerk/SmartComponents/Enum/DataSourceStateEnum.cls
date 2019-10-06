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
    File        : DataSourceStateEnum
    Purpose     : Enumeration for possible record position values
    Syntax      : 
    Description : Used by the SmartDataAdapter:SmartDataSourceState property
                  (ISmartDataSource Interface)
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 18 15:21:48 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Util.* FROM PROPATH . 
USING Progress.Lang.*       FROM PROPATH  .

CLASS Consultingwerk.SmartComponents.Enum.DataSourceStateEnum: 
    
    /*------------------------------------------------------------------------------
        Purpose: The DataAdapter is currently Navigatable and not in Update mode                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC STATIC PROPERTY Navigatable AS CHARACTER NO-UNDO INIT "Navigatable":U 
    GET .

    /*------------------------------------------------------------------------------
        Purpose: The DataAdapter is currently Updating Data and thus not navigatable 
                 (Toolbar/Panel or Browser)                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC STATIC PROPERTY Updating AS CHARACTER NO-UNDO INIT "Updating":U 
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DataSourceStateEnum class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE DataSourceStateEnum ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Validates a character against the list of valid DataSourceStateEnum
                 values                                                                       
        Notes:      
        @param pcDataSourceState Character value of a data source state, i.e. Navigatable or Updating
        @return Logical value indicating if the Character value stands for a valid enum member
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC LOGICAL IsValid (pcDataSourceState AS CHARACTER):
        
        RETURN ListHelper:EntryIsInList (pcDataSourceState, "Navigatable,Updating":U) .      

    END METHOD.

END CLASS.
