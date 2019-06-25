/*------------------------------------------------------------------------
  File:         webOrderHold.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 6.25.2019
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

DEFINE TEMP-TABLE ttTempTable NO-UNDO LIKE oe-ord
    FIELD createDT    LIKE tag.createDT
    FIELD createUser  LIKE tag.createUser
    FIELD description LIKE tag.description
    FIELD groupCode   LIKE tag.groupCode
    FIELD linkRecKey  LIKE tag.linkRecKey
    FIELD linkTable   LIKE tag.linkTable
    FIELD Note1       LIKE tag.Note[1]
    FIELD Note2       LIKE tag.Note[2]
    FIELD Note3       LIKE tag.Note[3]
    FIELD Note4       LIKE tag.Note[4]
    FIELD Note5       LIKE tag.Note[5]
    FIELD ownerUser   LIKE tag.ownerUser
    FIELD statusCode  LIKE tag.statusCode
    FIELD tagType     LIKE tag.tagType
    FIELD updateDT    LIKE tag.updateDT
    FIELD updateUser  LIKE tag.updateUser
        INDEX ttTempTable IS PRIMARY
            company
            ord-no
            csrUser_id
            .
/* Local Variable Definitions ---                                       */

&Scoped-define subjectID 40
{AOA/includes/subjectID{&subjectID}Defs.i}

/* subject business logic */
/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE BUFFER bTag FOR tag.
    
    FOR EACH tag NO-LOCK
        WHERE tag.tagType EQ "HOLD"
          AND CAN-FIND(FIRST bTag
                       WHERE bTag.tagType EQ "RELEASE") EQ NO,
        FIRST oe-ord NO-LOCK
        WHERE oe-ord.company EQ cCompany
          AND oe-ord.rec_key EQ tag.linkRecKey
        :
        CREATE ttTempTable.
        BUFFER-COPY oe-ord TO ttTempTable.
        ASSIGN
            ttTempTable.createDT    = tag.createDT
            ttTempTable.createUser  = tag.createUser
            ttTempTable.description = tag.description
            ttTempTable.groupCode   = tag.groupCode
            ttTempTable.linkRecKey  = tag.linkRecKey
            ttTempTable.linkTable   = tag.linkTable
            ttTempTable.Note1       = tag.Note[1]
            ttTempTable.Note2       = tag.Note[2]
            ttTempTable.Note3       = tag.Note[3]
            ttTempTable.Note4       = tag.Note[4]
            ttTempTable.Note5       = tag.Note[5]
            ttTempTable.ownerUser   = tag.ownerUser
            ttTempTable.statusCode  = tag.statusCode
            ttTempTable.tagType     = tag.tagType
            ttTempTable.updateDT    = tag.updateDT
            ttTempTable.updateUser  = tag.updateUser
            .
    END. /* each tag */
END PROCEDURE.
