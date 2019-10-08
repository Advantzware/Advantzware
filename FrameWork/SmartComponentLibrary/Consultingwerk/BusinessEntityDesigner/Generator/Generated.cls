/*********************************************************************
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
    File        : Generated
    Purpose     : Contains information about the most recently built code
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Feb 26 07:05:42 CET 2016
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.* FROM PROPATH .  
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Generated 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i BusinessEntityFileName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i DataAccessFileName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i DatasetControllerFileName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i DatasetFileName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i TempTablesFileName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i XsdFileName CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Generated class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Generated ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Generated class
        Notes:
        @param pcBusinessEntityFileName The value for the BusinessEntityFileName property    
        @param pcDataAccessFileName The value for the DataAccessFileName property
        @param pcDatasetControllerFileName The value for the DatasetControllerFileName property
        @param pcDatasetFileName The value for the DatasetFileName property          
        @param pcTempTablesFileName The value for the TempTablesFileName property       
        @param pcXsdFileName The value for the XsdFileName property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Generated (pcBusinessEntityFileName AS CHARACTER,
                                  pcDataAccessFileName AS CHARACTER,  
                                  pcDatasetControllerFileName AS CHARACTER,
                                  pcDatasetFileName AS CHARACTER,
                                  pcTempTablesFileName AS CHARACTER,    
                                  pcXsdFileName AS CHARACTER):
        THIS-OBJECT  ().

        ASSIGN THIS-OBJECT:BusinessEntityFileName    = pcBusinessEntityFileName    
               THIS-OBJECT:DataAccessFileName        = pcDataAccessFileName       
               THIS-OBJECT:DatasetControllerFileName = pcDatasetControllerFileName
               THIS-OBJECT:DatasetFileName           = pcDatasetFileName          
               THIS-OBJECT:TempTablesFileName        = pcTempTablesFileName       
               THIS-OBJECT:XsdFileName               = pcXsdFileName .          

    END CONSTRUCTOR. 

END CLASS.
