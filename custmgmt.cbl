       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMGMT.
      ****************************************************
      * This program manages a customer database.
      * It allows:
      * - Adition of new records
      * - Deletion of existing records
      * - Maintenance of existing records
      * - Inquiry of existing records
      ***************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *    COPY "CUSTFC.CBL".
           SELECT CUSTOMER-FILE
              ASSIGN TO "customer.dat"
              ORGANIZATION IS INDEXED
              RECORD KEY IS CUSTOMER-NUMBER
              ACCESS MODE IS DYNAMIC.

       DATA DIVISION.
       FILE SECTION.

      *    COPY "CUSTFC.CBL"
       FD  CUSTOMER-FILE
           LABEL RECORDS ARE STANDARD.
       01  CUSTOMER-RECORD.
           03  CUSTOMER-NUMBER                 PIC 9(05).
           03  CUSTOMER-NAME                   PIC X(50).
           03  CUSTOMER-ADDRESS-1              PIC X(50).
           03  CUSTOMER-ADDRESS-2              PIC X(50).
           03  CUSTOMER-CITY                   PIC X(30).
           03  CUSTOMER-STATE                  PIC X(30).
           03  CUSTOMER-POSTCODE               PIC X(05).
           03  CUSTOMER-EMAIL                  PIC X(50).
           03  CUSTOMER-PHONE                  PIC X(15).

       WORKING-STORAGE SECTION.

       77  MENU-PICK                           PIC 9(01).
           88  MENU-PICK-IS-VALID              VALUES 0 THRU 4.

       77  THE-MODE                            PIC X(07).
       77  WHICH-FIELD                         PIC 9(01).
       77  OK-TO-DELETE                        PIC X(01).
       77  RECORD-FOUND                        PIC X(01).
       77  CUSTOMER-NUMBER-FIELD               PIC X(05).

      * CBL_CHECK_FILE_EXIST
       01  FILE-INFO.
           05  FILE-SIZE-IN-BYTES              PIC 9(18) COMP.
           05  MOD-DD                          PIC 9(02) COMP.
           05  MOD-MO                          PIC 9(02) COMP.
           05  MOD-YYYY                        PIC 9(04) COMP.
           05  MOD-HH                          PIC 9(02) COMP.
           05  MOD-MM                          PIC 9(02) COMP.
           05  MOD-SS                          PIC 9(02) COMP.
           05  FILLER                          PIC 9(02) COMP.
       77  INPUT-FILE                          PIC X(20).
       77  RETURN-STATUS                       PIC 9(03).

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           PERFORM CHECK-FILE-EXIST.
           PERFORM OPENING-PROCEDURE.
           PERFORM MAIN-PROCESS.
           PERFORM CLOSING-PROCEDURE.

       PROGRAM-DONE.
           STOP RUN.

       CHECK-FILE-EXIST.
           MOVE "customer.dat" TO INPUT-FILE.
           MOVE ZEROES TO RETURN-CODE.
           CALL "CBL_CHECK_FILE_EXIST"
              USING INPUT-FILE
                    FILE-INFO
              RETURNING RETURN-STATUS.
      * File not exists
           IF RETURN-STATUS NOT = 0
              PERFORM CREATE-DATA-FILE
           END-IF.

       CREATE-DATA-FILE.
           OPEN OUTPUT CUSTOMER-FILE.
           CLOSE CUSTOMER-FILE.

       OPENING-PROCEDURE.
           OPEN I-O CUSTOMER-FILE.

       CLOSING-PROCEDURE.
           CLOSE CUSTOMER-FILE.

       MAIN-PROCESS.
           PERFORM GET-MENU-PICK.
           PERFORM MAINTAIN-THE-FILE
              UNTIL MENU-PICK = 0.

      ****************************************************
      * Menu
      ****************************************************
       GET-MENU-PICK.
           PERFORM DISPLAY-THE-MENU.
           PERFORM ACCEPT-MENU-PICK.
           PERFORM RETRY-MENU-PICK
              UNTIL MENU-PICK-IS-VALID.

       DISPLAY-THE-MENU.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Customer Management Menu".
           DISPLAY " ".
           DISPLAY "1. Add Customer Records".
           DISPLAY "2. Delete a Customer Record".
           DISPLAY "3. Change a Customer Record".
           DISPLAY "4. Inquire a Customer Record".
           DISPLAY " ".
           DISPLAY "0. Exit".
           PERFORM SCROLL-LINE 8 TIMES.

       ACCEPT-MENU-PICK.
      *    DISPLAY "Your choice (0-4)?".
           DISPLAY "Your choice (0-4)? " WITH NO ADVANCING.
           ACCEPT MENU-PICK.

       RETRY-MENU-PICK.
           DISPLAY "Invalid selection - Please retry.".
           PERFORM ACCEPT-MENU-PICK.

       CLEAR-SCREEN.
           PERFORM SCROLL-LINE 25 TIMES.

       SCROLL-LINE.
           DISPLAY " ".

       MAINTAIN-THE-FILE.
           PERFORM DO-THE-PICK.
           PERFORM GET-MENU-PICK.

       DO-THE-PICK.
           EVALUATE MENU-PICK
              WHEN 1
                 PERFORM ADD-MODE
              WHEN 2
                 PERFORM DELETE-MODE
              WHEN 3
                 PERFORM CHANGE-MODE
              WHEN 4
                 PERFORM INQUIRE-MODE
           END-EVALUATE.

      ****************************************************
      * Add
      ****************************************************
       ADD-MODE.
           MOVE "add" TO THE-MODE.
           PERFORM GET-NEW-CUSTOMER-NUMBER.
           PERFORM ADD-RECORDS
              UNTIL CUSTOMER-NUMBER = ZEROES.

       GET-NEW-CUSTOMER-NUMBER.
           PERFORM INIT-CUSTOMER-RECORD.
           PERFORM ENTER-CUSTOMER-NUMBER.
           MOVE "Y" TO RECORD-FOUND.
           PERFORM FIND-NEW-CUSTOMER-RECORD
              UNTIL RECORD-FOUND = "N"
              OR    CUSTOMER-NUMBER = ZEROES.

       FIND-NEW-CUSTOMER-RECORD.
           PERFORM READ-CUSTOMER-RECORD.
           IF RECORD-FOUND = "Y"
              DISPLAY "Record already on file"
              PERFORM ENTER-CUSTOMER-NUMBER
           END-IF.

       ADD-RECORDS.
           PERFORM ENTER-REMAINING-FIELDS.
           PERFORM WRITE-CUSTOMER-RECORD.
           PERFORM GET-NEW-CUSTOMER-NUMBER.

       ENTER-REMAINING-FIELDS.
           PERFORM ENTER-CUSTOMER-NAME.
           PERFORM ENTER-CUSTOMER-ADDRESS-1.
           PERFORM ENTER-CUSTOMER-ADDRESS-2.
           PERFORM ENTER-CUSTOMER-CITY.
           PERFORM ENTER-CUSTOMER-STATE.
           PERFORM ENTER-CUSTOMER-POSTCODE.
           PERFORM ENTER-CUSTOMER-EMAIL.
           PERFORM ENTER-CUSTOMER-PHONE.

      ****************************************************
      * Change
      ****************************************************
       CHANGE-MODE.
           MOVE "change" TO THE-MODE.
           PERFORM GET-CUSTOMER-RECORD.
           PERFORM CHANGE-RECORDS
              UNTIL CUSTOMER-NUMBER = ZEROES.

       CHANGE-RECORDS.
           PERFORM GET-FIELD-TO-CHANGE.
           PERFORM CHANGE-ONE-FIELD
              UNTIL WHICH-FIELD = ZERO.
           PERFORM GET-CUSTOMER-RECORD.

       GET-FIELD-TO-CHANGE.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM ASK-WHICH-FIELD.

       ASK-WHICH-FIELD.
           DISPLAY "Enter the number of the field.".
           DISPLAY "To change (1-8) or 0 to exit.".
           DISPLAY "Field number: " WITH NO ADVANCING.
           ACCEPT WHICH-FIELD.
           IF WHICH-FIELD > 8
              DISPLAY "Invalid entry"
           END-IF.

       CHANGE-ONE-FIELD.
           PERFORM CHANGE-THIS-FIELD.
           PERFORM GET-FIELD-TO-CHANGE.

       CHANGE-THIS-FIELD.
           EVALUATE WHICH-FIELD
              WHEN 1
                 PERFORM ENTER-CUSTOMER-NAME
              WHEN 2
                 PERFORM ENTER-CUSTOMER-ADDRESS-1
              WHEN 3
                 PERFORM ENTER-CUSTOMER-ADDRESS-2
              WHEN 4
                 PERFORM ENTER-CUSTOMER-CITY
              WHEN 5
                 PERFORM ENTER-CUSTOMER-STATE
              WHEN 6
                 PERFORM ENTER-CUSTOMER-POSTCODE
              WHEN 7
                 PERFORM ENTER-CUSTOMER-EMAIL
              WHEN 8
                 PERFORM ENTER-CUSTOMER-PHONE
           END-EVALUATE.

           PERFORM REWRITE-CUSTOMER-RECORD.

      ****************************************************
      * Delete
      ****************************************************
       DELETE-MODE.
           MOVE "delete" TO THE-MODE.
           PERFORM GET-CUSTOMER-RECORD.
           PERFORM DELETE-RECORDS
              UNTIL CUSTOMER-NUMBER = ZEROES.

        DELETE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.
           MOVE "X" TO OK-TO-DELETE.

           PERFORM ASK-TO-DELETE
              UNTIL OK-TO-DELETE = "Y" OR "N".

           IF OK-TO-DELETE = "Y"
              PERFORM DELETE-CUSTOMER-RECORD
           END-IF.

           PERFORM GET-CUSTOMER-RECORD.

       ASK-TO-DELETE.
           DISPLAY "Delete this record (Y/N)? " WITH NO ADVANCING.
           ACCEPT OK-TO-DELETE.
           EVALUATE OK-TO-DELETE
              WHEN "y"
                 MOVE "Y" TO OK-TO-DELETE
              WHEN "n"
                 MOVE "N" TO OK-TO-DELETE
             WHEN OTHER
                 DISPLAY "You must enter Y or N"
           END-EVALUATE.

      ****************************************************
      * Inquire
      ****************************************************
       INQUIRE-MODE.
           MOVE "display" TO THE-MODE.
           PERFORM GET-CUSTOMER-RECORD.
           PERFORM INQUIRE-RECORDS
              UNTIL CUSTOMER-NUMBER = ZEROES.

       INQUIRE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM GET-CUSTOMER-RECORD.

      ****************************************************
      * Routines shared by all modes
      ****************************************************
       INIT-CUSTOMER-RECORD.
           INITIALIZE CUSTOMER-RECORD.
           MOVE ZEROES TO CUSTOMER-NUMBER.

       ENTER-CUSTOMER-NUMBER.
           DISPLAY " ".
           DISPLAY "Enter Customer Number of the Customer".
           DISPLAY "to " THE-MODE " (1-99999)."
           DISPLAY "Enter 0 to stop entry."
           DISPLAY "Customer Number: " WITH NO ADVANCING.
           ACCEPT CUSTOMER-NUMBER-FIELD.
      * OR ACCEPT CUSTOMER-NUMBER-FIELD WITH CONVERSION.
           MOVE CUSTOMER-NUMBER-FIELD TO CUSTOMER-NUMBER.

       GET-CUSTOMER-RECORD.
           PERFORM INIT-CUSTOMER-RECORD.
           PERFORM ENTER-CUSTOMER-NUMBER.
           MOVE "N" TO RECORD-FOUND.
           PERFORM FIND-CUSTOMER-RECORD
              UNTIL RECORD-FOUND = "Y"
              OR    CUSTOMER-NUMBER = ZEROES.

      ****************************************************
      * Routines shared by Add and Change
      ****************************************************
       FIND-CUSTOMER-RECORD.
           PERFORM READ-CUSTOMER-RECORD.
           IF RECORD-FOUND = "N"
              DISPLAY "Record not found"
              PERFORM ENTER-CUSTOMER-NUMBER
           END-IF.

       ENTER-CUSTOMER-NAME.
           DISPLAY "Customer Name:   " WITH NO ADVANCING.
           ACCEPT CUSTOMER-NAME.

       ENTER-CUSTOMER-ADDRESS-1.
           DISPLAY "Address 1:       " WITH NO ADVANCING.
           ACCEPT CUSTOMER-ADDRESS-1.

       ENTER-CUSTOMER-ADDRESS-2.
           DISPLAY "Address 2:       " WITH NO ADVANCING.
           ACCEPT CUSTOMER-ADDRESS-2.

       ENTER-CUSTOMER-CITY.
           DISPLAY "City:            " WITH NO ADVANCING.
           ACCEPT CUSTOMER-CITY.

       ENTER-CUSTOMER-STATE.
           DISPLAY "State:           " WITH NO ADVANCING.
           ACCEPT CUSTOMER-STATE.

       ENTER-CUSTOMER-POSTCODE.
           DISPLAY "Postcode:        " WITH NO ADVANCING.
           ACCEPT CUSTOMER-POSTCODE.

       ENTER-CUSTOMER-EMAIL.
           DISPLAY "Email:           " WITH NO ADVANCING.
           ACCEPT CUSTOMER-EMAIL.

       ENTER-CUSTOMER-PHONE.
           DISPLAY "Phone Number:    " WITH NO ADVANCING.
           ACCEPT CUSTOMER-PHONE.

      ****************************************************
      * Routines shared by Change, Inquire and Delete
      ****************************************************
       DISPLAY-ALL-FIELDS.
           DISPLAY " ".
           PERFORM DISPLAY-CUSTOMER-NUMBER.
           PERFORM DISPLAY-CUSTOMER-NAME.
           PERFORM DISPLAY-CUSTOMER-ADDRESS-1.
           PERFORM DISPLAY-CUSTOMER-ADDRESS-2.
           PERFORM DISPLAY-CUSTOMER-CITY.
           PERFORM DISPLAY-CUSTOMER-STATE.
           PERFORM DISPLAY-CUSTOMER-POSTCODE.
           PERFORM DISPLAY-CUSTOMER-EMAIL.
           PERFORM DISPLAY-CUSTOMER-PHONE.

       DISPLAY-CUSTOMER-NUMBER.
           DISPLAY "   Customer Number: " CUSTOMER-NUMBER.

       DISPLAY-CUSTOMER-NAME.
           DISPLAY "1. Customer Name:   " CUSTOMER-NAME.

       DISPLAY-CUSTOMER-ADDRESS-1.
           DISPLAY "2. Address 1:       " CUSTOMER-ADDRESS-1.

       DISPLAY-CUSTOMER-ADDRESS-2.
           DISPLAY "3. Address 2:       " CUSTOMER-ADDRESS-2.

       DISPLAY-CUSTOMER-CITY.
           DISPLAY "4. City:            " CUSTOMER-CITY.

       DISPLAY-CUSTOMER-STATE.
           DISPLAY "5. State:           " CUSTOMER-STATE.

       DISPLAY-CUSTOMER-POSTCODE.
           DISPLAY "6. Postcode:        " CUSTOMER-POSTCODE.

       DISPLAY-CUSTOMER-EMAIL.
           DISPLAY "7. Email:           " CUSTOMER-EMAIL.

       DISPLAY-CUSTOMER-PHONE.
           DISPLAY "8. Phone Number:    " CUSTOMER-PHONE.

      ****************************************************
      * File I-O routines
      ****************************************************
       READ-CUSTOMER-RECORD.
           MOVE "Y" TO RECORD-FOUND.
           READ CUSTOMER-FILE RECORD
      *    READ CUSTOMER-FILE RECORD WITH LOCK
      *    READ CUSTOMER-FILE RECORD WITH HOLD
              INVALID KEY
                 MOVE "N" TO RECORD-FOUND.

       WRITE-CUSTOMER-RECORD.
           WRITE CUSTOMER-RECORD
              INVALID KEY
                 DISPLAY "Record already on file".

       REWRITE-CUSTOMER-RECORD.
           REWRITE CUSTOMER-RECORD
              INVALID KEY
                 DISPLAY "Error rewriting Customer record".

       DELETE-CUSTOMER-RECORD.
           DELETE CUSTOMER-FILE RECORD
              INVALID KEY
                 DISPLAY "Error deleting Customer record".
