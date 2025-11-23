INCLUDE Irvine32.inc
    .DATA
    welcomeMsg BYTE "                                          WELCOME TO LIBRARY MANAGEMENT SYSTEM  ",0
    msg1 BYTE 0AH

    BYTE "1. Register a Librarian", 0dh, 0ah
    BYTE "2. View Librarians", 0dh, 0ah
    BYTE "3. Add Book", 0dh, 0ah
    BYTE "4. View Books", 0dh, 0ah
    BYTE "5. Remove Book", 0dh, 0ah
    BYTE "6. Exit ", 0dh, 0ah
    BYTE "Enter Choice: ", 0
    REG_MSG BYTE " Enter Librarian's Name to register: ", 0
    VIEW_LIBRARIANS_MSG BYTE 0Ah," Viewing Registered Librarians: ",0AH, 0DH, 0
    ADD_MSG BYTE " Enter Book Name & Author Name to Add: ", 0dh, 0ah,
    "Separated By Comma:",0
    VIEW_BOOKS_MSG BYTE 0Ah, " Viewing Books in Library: ",  0dh, 0ah, 0
    MEMBERS_EMPTY BYTE "No Librarians in the Library",0
    REMOVE_BOOK_MSG BYTE 0AH, " Enter Book Name to Remove: ", 0
    BOOKS_EMPTY byte "No books in the library",0
    BOOK_REMOVED_PROMPT BYTE "Book removed successfully",0
    EXIT_MSG   BYTE 0AH
    BYTE "Thank You!",0dh, 0ah
    BYTE "Project Members:",0dh, 0ah
    BYTE "Umer Siddiqui(23K-0644)",0dh, 0ah
    BYTE "Ahmed Iltaf(22K-4384)",0dh, 0ah
    BYTE "Sajjad Ahmed(23K-0754)",0
   
    ; New messages
    EMP_REGISTER_SUCCESS BYTE "Librarian registered successfully!",0
    BOOK_ADD_SUCCESS BYTE "Book added successfully!",0
    MAX_EMP_MSG BYTE "Maximum number of Librarians reached!",0
    MAX_BOOK_MSG BYTE "Maximum number of books reached!",0

    LIBRARIANS_COUNT DWORD 0
    SUCCESS_MSG BYTE "Book deleted",0
    BOOKS_COUNT     DWORD 0
    INVALID_CHOICE BYTE "Invalid choice,choose only from 1 to 6",0
    REGISTER DWORD 1
    VIEW_LIBRARIANS DWORD 2
    ADD_BOOK DWORD 3
    VIEW_BOOKS DWORD 4
    REMOVE_BOOK   DWORD 5
    EXITP DWORD 6

    Librarian_SIZE = 30
    MAX_LIBRARIANS = 6
    Librarian1 BYTE Librarian_SIZE DUP (?)
    Librarian2 BYTE Librarian_SIZE DUP (?)
    Librarian3 BYTE Librarian_SIZE DUP (?)
    Librarian4 BYTE Librarian_SIZE DUP (?)
    Librarian5 BYTE Librarian_SIZE DUP (?)
    Librarian6 BYTE Librarian_SIZE DUP (?)
    Librarians DWORD Librarian1, Librarian2, Librarian3, Librarian4, Librarian5, Librarian6, 0AH, 0DH, 0

    BOOK_SIZE = 50
    MAX_BOOKS = 7
    BOOK1 BYTE BOOK_SIZE DUP (?)
    BOOK2 BYTE BOOK_SIZE DUP (?)
    BOOK3 BYTE BOOK_SIZE DUP (?)
    BOOK4 BYTE BOOK_SIZE DUP (?)
    BOOK5 BYTE BOOK_SIZE DUP (?)
    BOOK6 BYTE BOOK_SIZE DUP (?)
    BOOK7 BYTE BOOK_SIZE DUP (?)

    BOOKS DWORD BOOK1, BOOK2, BOOK3, BOOK4, BOOK5, BOOK6, BOOK7, 0AH, 0DH, 0
    TEMP_BOOKS BYTE BOOK_SIZE * 7 DUP (?) ; Temporary array to hold books

    BUFFER_SIZE = 50
    BUFFER BYTE BUFFER_SIZE DUP (0)  
    .CODE                                       ; code from here

    ; Improved Book Comparison Procedure
    COMPARE_BOOKS PROC
        ; Compare two null-terminated strings: [ESI] = book, [ECX] = input string (BUFFER)
        PUSH ESI
        PUSH ECX

    COMPARE_LOOP:
        MOV AL, [ESI]
        MOV BL, [ECX]
   
        ; If both are null, it's a match
        CMP AL, 0
        JE CHECK_ECX_NULL
   
        ; Compare characters
        CMP AL, BL
        JNE NO_MATCH

        ; Move to next character
        INC ESI
        INC ECX
        JMP COMPARE_LOOP

    CHECK_ECX_NULL:
        CMP BYTE PTR [ECX], 0
        JE MATCH_FOUND

    NO_MATCH:
        POP ECX
        POP ESI
        MOV EAX, 0
        RET

    MATCH_FOUND:
        POP ECX
        POP ESI
        MOV EAX, 1
        RET
    COMPARE_BOOKS ENDP

    ; Helper Procedure to Display Messages
    MSG_DISPLAY PROC USES EDX, VAR:ptr dword
        mov eax, GREEN
        call settextcolor
        MOV EDX, VAR
        CALL WRITESTRING
        RET
    MSG_DISPLAY ENDP

    ; Helper Procedure for String Input
    STRING_INPUT PROC USES EDX ECX, var: ptr dword
        MOV EDX, VAR
        MOV ECX, 50
        CALL READSTRING
        RET
    STRING_INPUT ENDP

    main PROC
        ; Display welcome message
        mov eax,yellow
        call settextcolor
        lea edx,welcomeMsg
        call writestring
        CALL CRLF

        ; Display menu
    START:
        mov eax,green
        call settextcolor
   
        MOV EDX, OFFSET msg1
        CALL WRITESTRING

        CALL READINT
   
        ; Compare and jump to appropriate section
        CMP EAX, REGISTER
        JE REGISTER_SECTION
   
        CMP EAX, VIEW_LIBRARIANS
        JE VIEW_LIBRARIANS_SECTION
   
        CMP EAX, ADD_BOOK
        JE ADD_BOOK_SECTION
   
        CMP EAX, VIEW_BOOKS
        JE VIEW_BOOKS_SECTION
   
        CMP EAX, REMOVE_BOOK
        JE REMOVE_B
   
        CMP EAX, EXITP
        JE EXIT_MENU
   
        ; Invalid choice
        MOV EAX, RED
        CALL SETTEXTCOLOR
        MOV EDX, OFFSET INVALID_CHOICE
        CALL WRITESTRING
        CALL CRLF
        JMP START

    REGISTER_SECTION:
        ; Check if maximum Librarians reached
        MOV EAX, LIBRARIANS_COUNT
        CMP EAX, MAX_LIBRARIANS
        JE MAX_LIBRARIANS_REACHED

        ; Prompt for Librarian name
        INVOKE MSG_DISPLAY, ADDR REG_MSG
   
        ; Clear buffer
        MOV EDI, OFFSET BUFFER
        MOV ECX, BUFFER_SIZE
        MOV AL, 0
        REP STOSB

        ; Read Librarian name
        MOV EDX, OFFSET BUFFER
        MOV ECX, BUFFER_SIZE
        CALL READSTRING

        ; Calculate offset for new Librarian
        MOV EAX, LIBRARIANS_COUNT
        MOV EBX, Librarian_SIZE
        MUL EBX
   
        ; Copy name to Librarian array
        MOV ESI, OFFSET BUFFER
        MOV EDI, OFFSET Librarians
        ADD EDI, EAX
   
        ; Copy name
        MOV ECX, BUFFER_SIZE
        REP MOVSB

        ; Increment Librarians count
        INC LIBRARIANS_COUNT

        ; Display success message
        MOV EAX, GREEN
        CALL SETTEXTCOLOR
        MOV EDX, OFFSET EMP_REGISTER_SUCCESS
        CALL WRITESTRING
        CALL CRLF
        JMP START

    MAX_LIBRARIANS_REACHED:
        MOV EAX, RED
        CALL SETTEXTCOLOR
        MOV EDX, OFFSET MAX_EMP_MSG
        CALL WRITESTRING
        CALL CRLF
        JMP START

    VIEW_LIBRARIANS_SECTION:
        ; Check if any Librarians registered
        MOV ECX, LIBRARIANS_COUNT
        CMP ECX, 0
        JE NO_LIBRARIANS_REGISTERED

        ; Display Librarians header
        INVOKE MSG_DISPLAY, ADDR VIEW_LIBRARIANS_MSG
   
        ; Initialize loop variables
        MOV EBX, 0

        VIEW_LIBRARIANS_LOOP:
        ; Calculate Librarian offset
        MOV EAX, EBX
        MOV EDX, Librarian_SIZE                 ;names byte name1,name2,name3....
                                                ;100h
        MUL EDX

        ; Display Librarian name
        MOV EDX, OFFSET Librarians
        ADD EDX, EAX
        CALL WRITESTRING
        CALL CRLF

        ; Move to next Librarian
        INC EBX
        LOOP VIEW_LIBRARIANS_LOOP

        JMP START

    NO_LIBRARIANS_REGISTERED:
        MOV EAX, RED
        CALL SETTEXTCOLOR
        MOV EDX, OFFSET MEMBERS_EMPTY
        CALL WRITESTRING
        CALL CRLF
        JMP START

    ADD_BOOK_SECTION:
        ; Check if maximum books reached
        MOV EAX, BOOKS_COUNT
        CMP EAX, MAX_BOOKS
        JE MAX_BOOKS_REACHED

        ; Prompt for book details
        INVOKE MSG_DISPLAY, ADDR ADD_MSG
   
        ; Clear buffer
        MOV EDI, OFFSET BUFFER
        MOV ECX, BUFFER_SIZE                ;BS = 50
        MOV AL, 0
        REP STOSB

        ; Read book details
        MOV EDX, OFFSET BUFFER
        MOV ECX, BUFFER_SIZE
        CALL READSTRING

        ; Calculate offset for new book
        MOV EAX, BOOKS_COUNT
        MOV EBX, BOOK_SIZE
        MUL EBX
   
        ; Copy book details to books array
        MOV ESI, OFFSET BUFFER
        MOV EDI, OFFSET BOOKS
        ADD EDI, EAX                        ;next loc
   
        ; Copy book details
        MOV ECX, BUFFER_SIZE
        REP MOVSB

        ; Increment books count
        INC BOOKS_COUNT

        ; Display success message
        MOV EAX, cyan
        CALL SETTEXTCOLOR
        MOV EDX, OFFSET BOOK_ADD_SUCCESS
        CALL WRITESTRING
        CALL CRLF
        JMP START

    MAX_BOOKS_REACHED:
        MOV EAX, RED
        CALL SETTEXTCOLOR
        MOV EDX, OFFSET MAX_BOOK_MSG
        CALL WRITESTRING
        CALL CRLF
        JMP START

    ; View Books Section
    VIEW_BOOKS_SECTION:
        MOV ECX, BOOKS_COUNT          ; Load the count of books into ECX
        CMP ECX, 0                    ; Check if there are any books
        JE NO_BOOKS_IN_LIBRARY        ; If no books, jump to the no-books handler

        ; Display message for viewing books
        INVOKE MSG_DISPLAY, ADDR VIEW_BOOKS_MSG
        MOV EBX, 0                    ; bk = 0

    OUTPUT_BOOKS:
        MOV ESI, OFFSET BOOKS        
        MOV EAX, BOOK_SIZE     ; Load book size into EAX
        MUL EBX                           ; current book ka address(offset)
        ADD ESI, EAX              ; Add offset to ESI
        MOV EDX, ESI                  ; Copy ESI to EDX
   
        PUSH ECX                   ; Save ECX (loop counter)
        MOV ECX, BOOK_SIZE            ; Set size of the book to display
        CALL WRITESTRING              ; Call function to display the book
        POP ECX                       ; Restore ECX
   
        INC EBX                      
        CALL CRLF                      
        LOOP OUTPUT_BOOKS             ; Loop until all books are displayed
        JMP START                     ; Jump back to start of the program

    NO_BOOKS_IN_LIBRARY:
        MOV EDX, OFFSET BOOKS_EMPTY   ; Load no-books message
        MOV EAX, RED                  ; Set text color to red
        CALL SETTEXTCOLOR
        CALL WRITESTRING              ; Display the message
        CALL CRLF                      
        JMP START                     ; Jump back to start of the program

    ; Remove Book Section
    REMOVE_B:
        ; Check if there are books in the library
        MOV ECX, BOOKS_COUNT
        CMP ECX, 0
        JE NO_BOOKS                   ; If no books, jump to the no-books handler

        ; input nayi book
        MOV EDX, OFFSET REMOVE_BOOK_MSG
        CALL WRITESTRING

          ;buffer khaali krna
        MOV EDI, OFFSET BUFFER
        MOV ECX, BUFFER_SIZE
        MOV AL, 0
        REP STOSB                     ; Fill the buffer with zeros

        ; inpput book name to remove
        MOV EDX, OFFSET BUFFER
        MOV ECX, BUFFER_SIZE
        CALL READSTRING

        ; Initialize variables for book removal
        MOV ESI, OFFSET BOOKS                   ; Point ESI to the books array
        MOV EDI, OFFSET TEMP_BOOKS    ; Point EDI to the temp books array
        MOV EBX, 0                    
        MOV ECX, BOOKS_COUNT          
        MOV EDX,0                
    REMOVE_BOOK_LOOP:
        PUSH EBX                      ; Save the current book index
        PUSH ECX                      ; Save the loop counter

        ; Calculate the offset for the current book
        MOV EAX, BOOK_SIZE
        MUL EBX
        LEA ESI, [BOOKS + EAX]        ; ESI points to the current book

        MOV ECX, OFFSET BUFFER        ; ECX points to the user input
        CALL COMPARE_BOOKS            

        ; If no match is found, copy the book to TEMP_BOOKS
        CMP EAX, 0
        JE COPY_BOOK                  ; If not matched, jump to copy book logic
        JMP CONTINUE_LOOP             ; If matched, skip copying

    COPY_BOOK:
        ; Calculate the destination offset in TEMP_BOOKS
        MOV EAX, EDX
        IMUL EAX, EAX, BOOK_SIZE
        LEA EDI, [TEMP_BOOKS + EAX]   ; Destination offset in TEMP_BOOKS

        ; Copy the book from BOOKS to TEMP_BOOKS
        MOV ESI, OFFSET BOOKS
        MOV ECX, BOOK_SIZE
        IMUL ECX, EBX
        ADD ESI, ECX
        PUSH EDI                      ; Save destination address
        MOV ECX, BOOK_SIZE            ; Size of the book to copy
        CLD                           ; direction flag set to 0(forward direction)
        REP MOVSB                     ; Copy the book
        POP EDI                       ; Restore destination address
        INC EDX                       ; Increment TEMP_BOOKS counter

    CONTINUE_LOOP:
        POP ECX                       ; Restore loop counter
        POP EBX                       ; Restore book index
        INC EBX                       ; Move to the next book
        DEC ECX                      
        JNZ REMOVE_BOOK_LOOP          ; Repeat if there are more books

        ; Update the books array and count
        MOV ESI, OFFSET TEMP_BOOKS    
        MOV EDI, OFFSET BOOKS        
        MOV ECX, EDX                  ; books countn to copy
        IMUL ECX, BOOK_SIZE           ; Calculate total bytes
        CLD                          
        REP MOVSB                     ; Copy books back to BOOKS

        ; Update the book count
        MOV BOOKS_COUNT, EDX        
        ;if book removed then print bk removed
        MOV EAX, cyan
        CALL SETTEXTCOLOR
        MOV EDX, OFFSET BOOK_REMOVED_PROMPT
        CALL WRITESTRING
        CALL CRLF
        JMP START

    NO_BOOKS:
        MOV EAX, RED
        CALL SETTEXTCOLOR
        MOV EDX, OFFSET BOOKS_EMPTY
        CALL WRITESTRING
        CALL CRLF
        JMP START

    ; Exit Menu Section
    EXIT_MENU:
        mov eax,cyan
        call settextcolor
        lea edx,EXIT_MSG
        call writestring
        call crlf
        call crlf
        mov eax,white
        call setTextcolor
        INVOKE ExitProcess, 0
    main ENDP
    END main
