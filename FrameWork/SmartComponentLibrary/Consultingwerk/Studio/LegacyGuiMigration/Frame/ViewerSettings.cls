/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ViewerSettings
    Purpose     : Value class for the Viewer and Binding Source Settings
                  in the AblFrameMigrationForm 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 10 12:59:06 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                 FROM PROPATH .
USING Consultingwerk.Studio.LegacyGuiMigration.Frame.* FROM PROPATH .  
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.Studio.LegacyGuiMigration.Frame.ViewerSettings 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    :

    {Consultingwerk/JsonSerializableProperty.i CreateBindingSource LOGICAL} .
    {Consultingwerk/JsonSerializableProperty.i EntityName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i EntityTables CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ViewerPackage CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ViewerName CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ViewerSettings class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ViewerSettings ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

END CLASS.
