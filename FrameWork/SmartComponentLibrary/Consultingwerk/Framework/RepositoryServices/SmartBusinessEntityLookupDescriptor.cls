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
    File        : SmartBusinessEntityLookupDescriptor
    Purpose     : Holder class for a SmartBusinessEntityLookup Definition
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 28 16:54:39 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                FROM PROPATH .
USING Consultingwerk.SmartComponents.Framework.*      FROM PROPATH .  
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Framework.RepositoryServices.SmartBusinessEntityLookupDescriptor 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
    :
        
    {Consultingwerk/JsonSerializableProperty.i AppServerPartition CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupBrowserFields CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupControls CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupDialogClassName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupDialogFilterFields CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupDialogFilterOperators CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupDialogQuerySort CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupDialogQueryString CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupDialogTitle CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupEntityName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupEntityTable CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupEntityView CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupFields CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupKeyField CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupKeyValueColumn CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupQuerySort CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LookupQueryString CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityLookupDescriptor class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityLookupDescriptor ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

&IF DEFINED (SmartComponentLibrary) NE 0 &THEN 
    /*------------------------------------------------------------------------------
        Purpose: Assigns the values from the SmartBusinessEntityLookup instance
        Notes:   
        @param poLookup The reference of the SmartBusinessEntityLookup instance to assign the values from
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignFromSmartBusinessEntityLookup (poLookup AS SmartBusinessEntityLookup):
        
        DEFINE VARIABLE cProperty AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i         AS INTEGER   NO-UNDO.

&IF NOT PROVERSION BEGINS "10.2":U &THEN
        DO i = 1 TO NUM-ENTRIES ("{&SerializableProperties}":U) BY 2:
            ASSIGN cProperty = ENTRY (i, "{&SerializableProperties}":U) .
            
            DYNAMIC-PROPERTY (THIS-OBJECT, cProperty) = DYNAMIC-PROPERTY (poLookup, cProperty) .
        END.     
&ELSE
        ASSIGN THIS-OBJECT:AppServerPartition          = poLookup:AppServerPartition          
               THIS-OBJECT:LookupBrowserFields         = poLookup:LookupBrowserFields        
               THIS-OBJECT:LookupControls              = poLookup:LookupControls             
               THIS-OBJECT:LookupDialogClassName       = poLookup:LookupDialogClassName      
               THIS-OBJECT:LookupDialogFilterFields    = poLookup:LookupDialogFilterFields   
               THIS-OBJECT:LookupDialogFilterOperators = poLookup:LookupDialogFilterOperators
               THIS-OBJECT:LookupDialogQuerySort       = poLookup:LookupDialogQuerySort      
               THIS-OBJECT:LookupDialogQueryString     = poLookup:LookupDialogQueryString    
               THIS-OBJECT:LookupDialogTitle           = poLookup:LookupDialogTitle          
               THIS-OBJECT:LookupEntityName            = poLookup:LookupEntityName           
               THIS-OBJECT:LookupEntityTable           = poLookup:LookupEntityTable          
               THIS-OBJECT:LookupEntityView            = poLookup:LookupEntityView           
               THIS-OBJECT:LookupFields                = poLookup:LookupFields               
               THIS-OBJECT:LookupKeyField              = poLookup:LookupKeyField             
               THIS-OBJECT:LookupKeyValueColumn        = poLookup:LookupKeyValueColumn       
               THIS-OBJECT:LookupQuerySort             = poLookup:LookupQuerySort            
               THIS-OBJECT:LookupQueryString           = poLookup:LookupQueryString            
            .
&ENDIF

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Assigns the values to the SmartBusinessEntityLookup instance
        Notes:   
        @param poLookup The reference of the SmartBusinessEntityLookup instance to assign the values to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignToSmartBusinessEntityLookup (poLookup AS SmartBusinessEntityLookup):
        
        DEFINE VARIABLE cProperty AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i         AS INTEGER   NO-UNDO.

&IF NOT PROVERSION BEGINS "10.2":U &THEN
        DO i = 1 TO NUM-ENTRIES ("{&SerializableProperties}":U) BY 2:
            ASSIGN cProperty = ENTRY (i, "{&SerializableProperties}":U) .
            
            DYNAMIC-PROPERTY (poLookup, cProperty) = DYNAMIC-PROPERTY (THIS-OBJECT, cProperty) .
        END.     
&ELSE
        ASSIGN poLookup:AppServerPartition          = THIS-OBJECT:AppServerPartition          
               poLookup:LookupBrowserFields         = THIS-OBJECT:LookupBrowserFields        
               poLookup:LookupControls              = THIS-OBJECT:LookupControls             
               poLookup:LookupDialogClassName       = THIS-OBJECT:LookupDialogClassName      
               poLookup:LookupDialogFilterFields    = THIS-OBJECT:LookupDialogFilterFields   
               poLookup:LookupDialogFilterOperators = THIS-OBJECT:LookupDialogFilterOperators
               poLookup:LookupDialogQuerySort       = THIS-OBJECT:LookupDialogQuerySort      
               poLookup:LookupDialogQueryString     = THIS-OBJECT:LookupDialogQueryString    
               poLookup:LookupDialogTitle           = THIS-OBJECT:LookupDialogTitle          
               poLookup:LookupEntityName            = THIS-OBJECT:LookupEntityName           
               poLookup:LookupEntityTable           = THIS-OBJECT:LookupEntityTable          
               poLookup:LookupEntityView            = THIS-OBJECT:LookupEntityView           
               poLookup:LookupFields                = THIS-OBJECT:LookupFields               
               poLookup:LookupKeyField              = THIS-OBJECT:LookupKeyField             
               poLookup:LookupKeyValueColumn        = THIS-OBJECT:LookupKeyValueColumn       
               poLookup:LookupQuerySort             = THIS-OBJECT:LookupQuerySort            
               poLookup:LookupQueryString           = THIS-OBJECT:LookupQueryString            
            .
&ENDIF

    END METHOD .
&ENDIF
END CLASS.
