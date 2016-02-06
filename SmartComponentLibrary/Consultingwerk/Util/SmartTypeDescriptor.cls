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
    File        : SmartTypeDescriptor
    Purpose     : Standard Type Descriptor implementation for the 
                  SmartComponent Library 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Feb 22 14:55:28 CET 2013
    Notes       : As this class is very central to the SmartComponent Library
                  it's located in a top level package to avoid compilation
                  issues
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (SmartComponentLibrary) NE 0 &THEN 
USING Consultingwerk.SmartComponents.TypeDescriptor.* FROM PROPATH .
&ENDIF
USING Consultingwerk.Util.*                           FROM PROPATH .   
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Util.SmartTypeDescriptor: 

    {Consultingwerk/Util/TempTables/ttClassNames.i &ACCESS="STATIC"}

&IF DEFINED (SmartComponentLibrary) NE 0 &THEN 
    DEFINE STATIC VARIABLE oClassDictionary AS PropertyDescriptorCollectionCollection NO-UNDO . 
    /*------------------------------------------------------------------------------
        Purpose: Static Constructor for the SmartTypeDescriptor class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR STATIC SmartTypeDescriptor ():

        oClassDictionary = NEW PropertyDescriptorCollectionCollection () . 

		SmartTypeDescriptor:LoadPropertiesFrom ("Consultingwerk/SmartComponents/TypeDescriptor/Components/components.xml":U) .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Attributes for a Class' Property
        Notes:   
        @param poClass The reference to the class object 
        @param pcPropertyName The name of the Property
        @return The List of Attributes for the Class' Property
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC "System.Collections.Generic.List<System.Attribute>":U GetClassPropertyAttributes (poClass AS Progress.Lang.Class,
                                                                                                           pcPropertyName AS CHARACTER):
                                                                                                        
        DEFINE VARIABLE oPropertyDescriptorCollection AS PropertyDescriptorCollection NO-UNDO . 
        DEFINE VARIABLE oPropertyDescriptor           AS IPropertyDescriptor          NO-UNDO . 

        DEFINE VARIABLE cClassName AS CHARACTER NO-UNDO.
        
        ASSIGN cClassName = poClass:TypeName .
        
        IF NOT SmartTypeDescriptor:HasClassDescription (cClassName) THEN DO: 
            IF VALID-OBJECT (poClass:SuperClass) AND NOT poClass:TypeName BEGINS "System.":U THEN 
                RETURN SmartTypeDescriptor:GetClassPropertyAttributes (poClass:SuperClass,
                                                               pcPropertyName) .
            ELSE             
                RETURN ? .
        END.            
            
        oPropertyDescriptorCollection = SmartTypeDescriptor:GetClassPropertyDescriptorCollection (cClassName) .

        IF NOT oPropertyDescriptorCollection:ContainsKey (pcPropertyName) THEN DO:
            IF VALID-OBJECT (poClass:SuperClass) AND NOT poClass:TypeName BEGINS "System.":U THEN 
                RETURN SmartTypeDescriptor:GetClassPropertyAttributes (poClass:SuperClass,
                                                               pcPropertyName) .
            ELSE             
                RETURN ? .
        END.            
           
        oPropertyDescriptor = oPropertyDescriptorCollection:GetItem (pcPropertyName) .
                      
        RETURN oPropertyDescriptor:Attributes .             

    END.                                                                                                        

    /*------------------------------------------------------------------------------
        Purpose: Returns the PropertyDescriptorCollection for a class 
        Notes:   
        @param pcClassName The name of the class to return the PropertyDescrioptorCollection (Charcater to simplify cross-usage between Progress.Lang.Class and System.Type)
        @return The PropertyDescriptorCollection for the named class
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC PropertyDescriptorCollection GetClassPropertyDescriptorCollection (pcClassName AS CHARACTER):
                        
        RETURN oClassDictionary:GetItem (pcClassName) .
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Determines if the Class Description is available in the ISmartTypeDescriptor
        Notes:   
        @param pcClassName The name of the class to return the PropertyDescrioptorCollection (Charcater to simplify cross-usage between Progress.Lang.Class and System.Type)
        @return Logical value indicating if the description for that class is present  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL HasClassDescription (pcClassName AS CHARACTER):

        RETURN oClassDictionary:ContainsKey (pcClassName) .
        
    END METHOD .        

    /*------------------------------------------------------------------------------
        Purpose: Loads all PropertyDescriptorCollection classes that are references in the 
                 given xml file 
        Notes:   
        @param pcFile The name of the xml file to load the properties from 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID LoadPropertiesFrom (pcFile AS CHARACTER):

        DEFINE VARIABLE oInstance AS PropertyDescriptorCollection NO-UNDO . 

        /* Mike Fechner, Consultingwerk Ltd. 21.03.2013
           Verify XML file name */
        FILE-INFO:FILE-NAME = pcFile . 
        
        IF FILE-INFO:FULL-PATHNAME > "":U 
            THEN .
        ELSE 
            RETURN . 

        TEMP-TABLE ttClassNames:READ-XML ("FILE":U,
                                          FILE-INFO:FULL-PATHNAME,
                                          "EMPTY":U,
                                          ?,
                                          ?) . 

        FOR EACH ttClassNames ON ERROR UNDO, THROW:
            &IF DEFINED (DEBUG) NE 0 AND DEFINED (DebugCustomTypeDescriptor) NE 0 &THEN
            Consultingwerk.Debug:Log ("LogProperties":U, 
                                      SUBSTITUTE ("<Loading>: &1":U,
                                                  ttClassNames.ClassName)) .                    
            &ENDIF               

            oInstance = DYNAMIC-NEW (ttClassNames.ClassName) () .

            oClassDictionary:Add (oInstance:TypeName,
                                  oInstance) .            
        
            CATCH err AS Progress.Lang.Error:
            	ErrorHelper:ShowErrorMessage (err, SUBSTITUTE ("Error while loading &1":U, ttClassNames.ClassName)) .	
            END CATCH.
        END.

	END METHOD .
&ENDIF

END CLASS.
