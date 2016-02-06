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
    File        : SmartBusinessEntityLookupSerializer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Wed Oct 26 21:33:16 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Framework.*      FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Framework.SmartBusinessEntityLookupSerializer IMPLEMENTS ISmartBusinessEntityLookupSerializer: 

    /*------------------------------------------------------------------------------
        Purpose: Serializes the component to the buffer                                                                       
        Notes:                 
        @param phBuffer The handle of the Buffer to deserialize from                                                                      
        @param poComponent The reference of the object to deserialize to 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Deserialize (phBuffer AS HANDLE, 
                                    poComponent AS Progress.Lang.Object):
        
        DEFINE VARIABLE oLookup AS SmartBusinessEntityLookup NO-UNDO . 

        oLookup = CAST (poComponent, SmartBusinessEntityLookup) .
        
        ASSIGN oLookup:AppServerPartition          = phBuffer::AppServerPartition         
               oLookup:LookupBrowserFields         = phBuffer::LookupBrowserFields        
               oLookup:LookupControls              = phBuffer::LookupControls             
               oLookup:LookupDialogClassName       = phBuffer::LookupDialogClassName      
               oLookup:LookupDialogFilterFields    = phBuffer::LookupDialogFilterFields   
               oLookup:LookupDialogFilterOperators = phBuffer::LookupDialogFilterOperators
               oLookup:LookupDialogQuerySort       = phBuffer::LookupDialogQuerySort      
               oLookup:LookupDialogQueryString     = phBuffer::LookupDialogQueryString    
               oLookup:LookupDialogTitle           = phBuffer::LookupDialogTitle          
               oLookup:LookupEntityName            = phBuffer::LookupEntityName           
               oLookup:LookupEntityTable           = phBuffer::LookupEntityTable          
               oLookup:LookupEntityView            = phBuffer::LookupEntityView           
               oLookup:LookupFields                = phBuffer::LookupFields               
               oLookup:LookupKeyField              = phBuffer::LookupKeyField             
               oLookup:LookupKeyValueColumn        = phBuffer::LookupKeyValueColumn       
               oLookup:LookupQuerySort             = phBuffer::LookupQuerySort              
               oLookup:LookupQueryString           = phBuffer::LookupQueryString          
               oLookup:LookupTimerDelay            = phBuffer::LookupTimerDelay                             
               . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Serializes the component from the buffer                                                                       
        Notes: 
        @param poComponent The reference of the object to serialize to the Buffer
        @param phBuffer The handle of the Buffer to serialize to                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Serialize (poComponent AS Progress.Lang.Object, 
                                  phBuffer AS HANDLE):
        
        DEFINE VARIABLE oLookup AS SmartBusinessEntityLookup NO-UNDO . 

        oLookup = CAST (poComponent, SmartBusinessEntityLookup) .
        
        ASSIGN phBuffer::AppServerPartition          = oLookup:AppServerPartition
               phBuffer::LookupBrowserFields         = oLookup:LookupBrowserFields
               phBuffer::LookupControls              = oLookup:LookupControls
               phBuffer::LookupDialogClassName       = oLookup:LookupDialogClassName
               phBuffer::LookupDialogFilterFields    = oLookup:LookupDialogFilterFields
               phBuffer::LookupDialogFilterOperators = oLookup:LookupDialogFilterOperators
               phBuffer::LookupDialogQuerySort       = oLookup:LookupDialogQuerySort
               phBuffer::LookupDialogQueryString     = oLookup:LookupDialogQueryString
               phBuffer::LookupDialogTitle           = oLookup:LookupDialogTitle
               phBuffer::LookupEntityName            = oLookup:LookupEntityName
               phBuffer::LookupEntityTable           = oLookup:LookupEntityTable
               phBuffer::LookupEntityView            = oLookup:LookupEntityView
               phBuffer::LookupFields                = oLookup:LookupFields
               phBuffer::LookupKeyField              = oLookup:LookupKeyField
               phBuffer::LookupKeyValueColumn        = oLookup:LookupKeyValueColumn
               phBuffer::LookupQuerySort             = oLookup:LookupQuerySort
               phBuffer::LookupQueryString           = oLookup:LookupQueryString
               phBuffer::LookupTimerDelay            = oLookup:LookupTimerDelay
/*               phBuffer::RenderingClassName          = oLookup:GetClass ():TypeName*/
               . 

    END METHOD.

END CLASS.
