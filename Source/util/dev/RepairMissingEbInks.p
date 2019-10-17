/*------------------------------------------------------------------------
    File        : fixMissingEbColors.p
    Purpose     : 
    Syntax      :
    Description : 
    Author(s)   : 
    Created     : Wed Oct 16 11:31:51 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEF BUFFER b-eb FOR eb.
DEF BUFFER bf-itemfg-ink FOR itemfg-ink.

DEF VAR ip-rowid AS ROWID NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR iPassCtr AS INT NO-UNDO.
DEF VAR iInkCtr AS INT NO-UNDO.
DEF VAR iCoatCtr AS INT NO-UNDO.
DEF VAR iCoatPCtr AS INT NO-UNDO.
DEF VAR iInkPCtr AS INT NO-UNDO.
DEF VAR iColorCtr AS INT NO-UNDO.

DEF TEMP-TABLE ttInk NO-UNDO LIKE itemfg-ink
    FIELD mat-type LIKE item.mat-type
    FIELD seq-no   AS   INT.
DISABLE TRIGGERS FOR LOAD OF eb.

/* This will be a full table scan, but can't be helped */
FOR EACH eb NO-LOCK WHERE 
    eb.company eq '001' and 
    eb.i-code[1] EQ "":

    /* Before we do ANY work, throw out the eb records that don't require any mods */
    IF NOT CAN-FIND(FIRST itemfg-ink WHERE
        itemfg-ink.company EQ eb.company AND
        itemfg-ink.i-no EQ eb.stock-no) THEN
        NEXT.

    ASSIGN 
        iInkCtr = 0
        iCoatCtr = 0
        iColorCtr = 0
        .
    EMPTY TEMP-TABLE ttInk.

    /* We know there's at least one itemfg-ink, so build the temp-table, but ONLY if supporting records exist */
    FOR EACH itemfg-ink
        WHERE itemfg-ink.company EQ eb.company
        AND itemfg-ink.i-no    EQ eb.stock-no
        NO-LOCK:

        FIND FIRST est-op NO-LOCK WHERE 
            est-op.company EQ eb.company AND 
            est-op.est-no  EQ eb.est-no AND 
            est-op.s-num   EQ eb.form-no AND 
            est-op.op-pass EQ itemfg-ink.pass AND 
            est-op.line    LE 500 AND 
            (est-op.b-num  EQ eb.blank-no OR eb.est-type NE 3) AND 
            (est-op.dept   EQ "PR" OR est-op.dept EQ "CT")
            NO-ERROR.
        IF NOT AVAIL est-op THEN NEXT.

        FIND FIRST mach NO-LOCK WHERE 
            mach.company    EQ est-op.company AND 
            mach.m-code     EQ est-op.m-code
            NO-ERROR.
        IF NOT AVAIL mach THEN NEXT.

        FIND FIRST ITEM NO-LOCK WHERE
            item.company    EQ eb.company AND
            item.i-no       EQ itemfg-ink.rm-i-no 
            AND item.press-type EQ mach.pr-type
            NO-ERROR.
        IF NOT AVAIL ITEM THEN NEXT.
        ASSIGN 
            iColorCtr = iColorCtr + 1
            iInkCtr = iInkCtr + (IF item.mat-type EQ "I" THEN 1 ELSE 0)
            iCoatCtr = iCoatCtr + (IF item.mat-type EQ "V" THEN 1 ELSE 0)
            .
        CREATE ttInk.
        BUFFER-COPY itemfg-ink TO ttInk.
        ASSIGN 
            ttInk.seq-no = iColorCtr
            ttInk.mat-type = item.mat-type.
    END.

    /* Now we have one TT record per ink, so load the eb buffer and populate inks array with the TT values */
    DO TRANSACTION:
        FIND b-eb EXCLUSIVE WHERE 
            ROWID(b-eb) EQ ROWID(eb)
            NO-ERROR.
        ASSIGN 
            iCtr = 0
            iPassCtr = 0
            iCoatPCtr = 0.
        FOR EACH ttInk NO-LOCK 
            BREAK BY ttInk.pass
            BY ttInk.rm-i-no
            BY ttInk.seq-no:

            ASSIGN 
                iCtr = iCtr + 1
                iCoatPCtr = iCoatPCtr + (IF ttInk.mat-type EQ "V" THEN 1 ELSE 0)
                iInkPCtr = iInkPCtr + (IF ttInk.mat-type EQ "I" THEN 1 ELSE 0).
/* display eb.stock-no format "x(30)" eb.est-no format "x(10)". */
            IF eb.est-type GT 4 THEN DO:
                ASSIGN
                    b-eb.i-ps[iCtr]   = ttInk.pass
                    b-eb.i-code[iCtr] = ttInk.rm-i-no
                    b-eb.i-dscr[iCtr] = ttInk.dscr
                    b-eb.i-%[iCtr]    = ttInk.cover%
                    b-eb.side[iCtr]   = IF ttInk.in-out THEN " " ELSE "F".
            END.
            ELSE DO:
                ASSIGN
                    b-eb.i-ps2[iCtr]     = ttInk.pass
                    b-eb.i-code2[iCtr]   = ttInk.rm-i-no
                    b-eb.i-dscr2[iCtr]   = ttInk.dscr
                    b-eb.i-%2[iCtr]      = ttInk.cover%
                    b-eb.side[iCtr]      = IF ttInk.in-out THEN " " ELSE "F".
            END.

            /* Assign pass-level field values */
            IF LAST-OF(ttInk.pass) THEN ASSIGN
                b-eb.i-pass   = b-eb.i-pass + (IF iInkPCtr GT 0 THEN 1 ELSE 0)
                b-eb.i-coat-p = b-eb.i-coat-p + (IF iCoatPCtr GT 0 THEN 1 ELSE 0)
                iCoatPCtr   = 0
                iInkPCtr = 0.
        END.
        /* Finally, assign the total color count */
        ASSIGN
            b-eb.i-Col    = iInkCtr
            b-eb.i-coat =  iCoatCtr.
    END. /*Transaction*/
/* leave.  One record at a time for testing */
END. /*eb*/
message "I'm done!" view-as alert-box.
