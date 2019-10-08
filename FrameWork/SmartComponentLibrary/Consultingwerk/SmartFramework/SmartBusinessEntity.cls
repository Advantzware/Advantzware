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
    File        : SmartBusinessEntity
    Purpose     : Enhanced base class for Business Entities that provide
                  SmartFramework functionality like attachments and comments
    Syntax      : Abstract base class
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 13 10:58:26 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartFramework.SmartBusinessEntity
    INHERITS BusinessEntity ABSTRACT:

    /**
     * Purpose: Constructor for the SmartBusinessEntity class
     * Notes:
     * @param phDataset The handle of the Business Entity Dataset (passed from the actual Business Entity)
     */
    CONSTRUCTOR PUBLIC SmartBusinessEntity (phDataset AS HANDLE):
        SUPER (INPUT phDataset).

    END CONSTRUCTOR.

    /**
     * Purpose: Initializes the DataAcess Object
     * Notes:   Override to Business Entity method, verifies that the DataAccessObject
     *          of a SmartBusinessEntity is a SmartDataAccess instance
     */
    METHOD OVERRIDE PROTECTED VOID InitializeDataAccessObject ():

&IF DEFINED (SmartFramework) NE 0 &THEN
        DEFINE VARIABLE oConfigurationProvider AS ISmartBusinessEntityConfigurationProvider NO-UNDO .
        DEFINE VARIABLE oConfiguration         AS ISmartBusinessEntityConfiguration         NO-UNDO .
&ENDIF

        SUPER:InitializeDataAccessObject().

        IF NOT TYPE-OF (THIS-OBJECT:DataAccessObject, SmartDataAccess) THEN
            UNDO, THROW NEW AppError ("A SmartBusinessEntity can only use a SmartDataAccess Object":U) .

        /* Mike Fechner, Consultingwerk Ltd. 03.09.2015
           SCL-997: Ability to Control TrackDeletions based on SmartDB.SmartBusinessEntity
           settings */
&IF DEFINED (SmartFramework) NE 0 &THEN
        ASSIGN oConfigurationProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.System.ISmartBusinessEntityConfigurationProvider
                                                                      "NEW Consultingwerk.SmartFramework.System.SmartBusinessEntityConfigurationProvider ()"} .

        IF VALID-OBJECT (oConfigurationProvider) THEN DO:

           oConfiguration = oConfigurationProvider:GetSmartBusinessEntityConfiguration (THIS-OBJECT:GetClass():TypeName) .

           IF VALID-OBJECT (oConfiguration) AND oConfiguration:TrackDeletions <> ? THEN
                CAST (THIS-OBJECT:DataAccessObject, SmartDataAccess):TrackDeletions = oConfiguration:TrackDeletions .
        END.
&ENDIF

    END METHOD .

    /**
     * Purpose: Returns the name of the source (database) table for a ProDataset
     *          table name
     * Notes:
     * @param pcTempTableName The temp-table buffer name to return the source table for
     * @return The database table name of the source table
     */
    METHOD PUBLIC CHARACTER SourceTable (pcTempTableName AS CHARACTER):

        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN
            THIS-OBJECT:InitializeDataAccessObject () .

        RETURN CAST (THIS-OBJECT:DataAccessObject,
                     SmartDataAccess):SourceTable (pcTempTableName) .

    END METHOD .

END CLASS.
