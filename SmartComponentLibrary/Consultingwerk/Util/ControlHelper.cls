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
    File        : ClassHelper
    Purpose     : Class contains generic supporting routines to work 
                  with classes
    Syntax      : Static methods only, private constructor to disallow 
                  instance creation
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 12 20:31:09 CEST 2010
    Notes       : This file is intended for GUI for .NET only
                  The result temp-table description is in Consultingwerk/Util/TempTables/ttClassNames.i
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Assertion.* FROM PROPATH .
USING Consultingwerk.Util.*      FROM PROPATH .  
USING Progress.Lang.*            FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING System.Collections.*       FROM ASSEMBLY .
USING System.Windows.Forms.*     FROM ASSEMBLY .
&ENDIF 

CLASS Consultingwerk.Util.ControlHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ControlHelper class
        Notes:   Protected default constructor                   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED ControlHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
	/*------------------------------------------------------------------------------
	    Purpose: Returns if the given list contains an object with the given name
	    Notes:   Some Telerik lists do not provide an Exists method. This method here
	             may be used to test if a list contains an object with the provided
	             Name
	    @param pcName The name to test for
	    @param poCollection The list to test in
	    @return Logical property indicating if the list contains an object with the given name 
	------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC LOGICAL ExistsByName (pcName AS CHARACTER,
	                                           poCollection AS IEnumerable):
		
		{Consultingwerk/foreach.i System.Object oObject in poCollection}

            IF ReflectionHelper:HasProperty (oObject, "Name":U) AND 
               UNBOX (ReflectionHelper:GetPropertyValue (oObject, "Name":U)) = pcName THEN 
                RETURN TRUE . 
		END.
		
		RETURN FALSE .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the given list contains an object with the given name
        Notes:   Some Telerik lists do not provide an Exists method. This method here
                 may be used to test if a list contains an object with the provided
                 Name
        @param poCollection The list to test in
        @param pcName The name to test for
        @return Logical property indicating if the list contains an object with the given name 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL ExistsByName (poCollection AS IEnumerable,
                                               pcName AS CHARACTER):
        
        {Consultingwerk/foreach.i System.Object oObject in poCollection}

            IF ReflectionHelper:HasProperty (oObject, "Name":U) AND 
               UNBOX (ReflectionHelper:GetPropertyValue (oObject, "Name":U)) = pcName THEN 
                RETURN TRUE . 
        END.
        
        RETURN FALSE .

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns if the given list contains an object with the given Tag
        Notes:   Some Telerik lists do not provide an Exists method. This method here
                 may be used to test if a list contains an object with the provided
                 Name
        @param pcTag The Tag value to test for
        @param poCollection The list to test in
        @return Logical property indicating if the list contains an object with the given Tag 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL ExistsByTag (pcTag AS CHARACTER,
                                              poCollection AS IEnumerable):
        
        DEFINE VARIABLE oTag AS System.Object NO-UNDO . 
        
        {Consultingwerk/foreach.i System.Object oObject in poCollection}

            IF ReflectionHelper:HasProperty (oObject, "Tag":U) THEN DO:
                oTag = ReflectionHelper:GetPropertyValue (oObject, "Tag":U) . 
                
                IF VALID-OBJECT (oTag) AND TYPE-OF (oTag, System.String) AND 
                   UNBOX (oTag) = pcTag THEN 
                    
                    RETURN TRUE . 
            END.
        END.
        
        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the given list contains an object with the given Tag
        Notes:   Some Telerik lists do not provide an Exists method. This method here
                 may be used to test if a list contains an object with the provided
                 Name
        @param poCollection The list to test in
        @param pcTag The Tag value to test for
        @return Logical property indicating if the list contains an object with the given Tag 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL ExistsByTag (poCollection AS IEnumerable,
                                              pcTag AS CHARACTER):
        
        DEFINE VARIABLE oTag AS System.Object NO-UNDO . 
        
        {Consultingwerk/foreach.i System.Object oObject in poCollection}

            IF ReflectionHelper:HasProperty (oObject, "Tag":U) THEN DO:
                oTag = ReflectionHelper:GetPropertyValue (oObject, "Tag":U) . 
                
                IF VALID-OBJECT (oTag) AND TYPE-OF (oTag, System.String) AND 
                   UNBOX (oTag) = pcTag THEN 
                    
                    RETURN TRUE . 
            END.
        END.
        
        RETURN FALSE .

    END METHOD.
        
    /*------------------------------------------------------------------------------
        Purpose: Returns the first object from the list with the given Tag
        Notes:   Returns ? when the Tag cannot be found
        @param pcTag The Tag value to search for
        @param poCollection The list to search in
        @return The reference to the object with the given Tag 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Object FindByTag (pcTag AS CHARACTER,
                                                  poCollection AS IEnumerable):
        
        DEFINE VARIABLE oTag AS System.Object NO-UNDO . 
        
        {Consultingwerk/foreach.i System.Object oObject in poCollection}

            IF ReflectionHelper:HasProperty (oObject, "Tag":U) THEN DO:
                oTag = ReflectionHelper:GetPropertyValue (oObject, "Tag":U) . 
                
                IF VALID-OBJECT (oTag) AND TYPE-OF (oTag, System.String) AND 
                   UNBOX (oTag) = pcTag THEN 
                    
                    RETURN oObject . 
            END.
        END.
        
        RETURN ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the first object from the list with the given Tag
        Notes:   Returns ? when the Tag cannot be found
        @param poCollection The list to search in
        @param pcTag The Tag value to search for
        @return The reference to the object with the given Tag 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Object FindByTag (poCollection AS IEnumerable,
                                                  pcTag AS CHARACTER):
        
        DEFINE VARIABLE oTag AS System.Object NO-UNDO . 
        
        {Consultingwerk/foreach.i System.Object oObject in poCollection}

            IF ReflectionHelper:HasProperty (oObject, "Tag":U) THEN DO:
                oTag = ReflectionHelper:GetPropertyValue (oObject, "Tag":U) . 
                
                IF VALID-OBJECT (oTag) AND TYPE-OF (oTag, System.String) AND 
                   UNBOX (oTag) = pcTag THEN 
                    
                    RETURN oObject . 
            END.
        END.
        
        RETURN ? .

    END METHOD.
                
    /*------------------------------------------------------------------------------
        Purpose: Returns a List of all Controls in the Form that are of the given type
        Notes:   
        @param poContainerControl The reference to the parent control
        @param poType The reference to the System.Type 
        @return The List of all Controls of the given type
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC "System.Collections.Generic.List<System.Windows.Forms.Control>":U FindAllControlsOfType (poContainerControl AS Control,
                                                                                                                  poType AS System.Type):
        
        DEFINE VARIABLE oList AS "System.Collections.Generic.List<System.Windows.Forms.Control>":U NO-UNDO . 
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poType """Type"":U"} .
        
        oList = NEW "System.Collections.Generic.List<System.Windows.Forms.Control>" () .

        ControlHelper:FindAllControlsOfType (poContainerControl, 
                                             oList,
                                             poType) .

        RETURN oList . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a List of all Controls in the Form that are of the given type
        Notes:   
        @param poParentControl The current parent Control reference
        @param poList The List of Control to add the matching Controls to
        @param poType The reference to the System.Type 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID FindAllControlsOfType (poParentControl AS System.Windows.Forms.Control,
                                                        poList AS "System.Collections.Generic.List<System.Windows.Forms.Control>":U,                          
                                                        poType AS System.Type):

        {Consultingwerk/foreach.i System.Windows.Forms.Control oControl in poParentControl:Controls}
        
            IF poType:IsInstanceOfType (oControl) THEN 
                poList:Add (oControl) .
            
            IF oControl:Controls:Count > 0 THEN 
                ControlHelper:FindAllControlsOfType (oControl, 
                                                     poList,
                                                     poType) .
                                                     
            CATCH err AS Progress.Lang.Error:
            	IF err:GetMessageNum (1) = 15580 OR err:GetMessage(1) MATCHES "* (15580)*":U THEN 
            	   /* ignore */ .
            	ELSE 
            	    UNDO, THROW err . 	
            END CATCH.                                                     
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Walks up the Parent-Chain of a .NET Control and returns the first 
                 UserControl
        Notes:   When no user control is found this method returns ?    
                 When used on a UserControl, the method will still be seeking for a 
                 UserControl in the Parent-Chain. 
        @param poControl The Control to search the next parent UserControl for
        @return The reference to the parent UserControl
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC System.Windows.Forms.UserControl FindParentUserControl (poControl AS System.Windows.Forms.Control):
		
		DEFINE VARIABLE oParent AS System.Windows.Forms.Control NO-UNDO .
		
		{Consultingwerk/Assertion/ObjectAssert/IsValid.i poControl "'poControl':U"} .
		
		oParent = poControl:Parent .
		
		DO WHILE VALID-OBJECT (oParent):
		    
		    IF TYPE-OF (oParent, System.Windows.Forms.UserControl) THEN 
		        RETURN CAST (oParent, System.Windows.Forms.UserControl) .
    
	        oParent = oParent:Parent . 	    
		END. 
		
		RETURN ? .

	END METHOD .

	/*------------------------------------------------------------------------------
	   Purpose: Returns the Control with the given ChildIndex in the ControlsCollection
	   Notes:
	   @param poControlCollection The ControlsCollection to locate the ChildIndex in
	   @param piIndex The Child Index to locate the Control at
	   @return The located Control or ? when no Control was found
	------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC Control GetControlByChildIndex (poControlCollection AS "Control+ControlCollection",
	                                                     piIndex AS INTEGER):
		
		{Consultingwerk/foreach.i Control oControl in poControlCollection}
		
		    IF poControlCollection:GetChildIndex (oControl) = piIndex THEN  
		        RETURN oControl .
		END.

        RETURN ? . 
        
	END METHOD.
&ENDIF    

END CLASS.
