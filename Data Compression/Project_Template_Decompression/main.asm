INCLUDE Irvine32.inc
INCLUDE macros.inc

Get_frequencies PROTO,
    pString:PTR BYTE,   ; points to string
    pTable:PTR DWORD    ; points to frequency table

	
BUFFER_SIZE = 5000

freqarray STRUCT
character byte ?
frequency Dword 0
freqarray ENDS

.data
Frequency_Arr freqarray 100 dup(<>)


count_unique_chars dword 0
buffer BYTE BUFFER_SIZE DUP(?)

filename BYTE "compressed_Text.txt",0
fileHandle HANDLE ?
freqTable DWORD 256 DUP(0)


.code
ReadFrequency PROC
; Open the file for input.
mov edx,OFFSET filename
call OpenInputFile
mov fileHandle,eax
; Check for errors.
cmp eax,INVALID_HANDLE_VALUE ; error opening file?
jne file_ok ; no: skip
mWrite <"Cannot open file",0dh,0ah>
jmp quit ; and quit
file_ok:
; Read the file into a buffer.
mov edx,OFFSET buffer
mov ecx,BUFFER_SIZE
call ReadFromFile
jnc check_buffer_size ; error reading?
mWrite "Error reading file. " ; yes: show error message
call WriteWindowsMsg
jmp close_file
check_buffer_size:
cmp eax,BUFFER_SIZE ; buffer large enough?
jb buf_size_ok ; yes
mWrite <"Error: Buffer too small/ for the file",0dh,0ah>
jmp quit ; and quit
buf_size_ok:
mov buffer[eax],0 ; insert null terminator
mWrite "File size: "
call WriteDec ; display file size
;call Crlf
; Display the buffer.
mWrite <"Buffer:",0dh,0ah,0dh,0ah>
mov edx,OFFSET buffer ; display the buffer
;call WriteString
;call Crlf
close_file:
mov eax,fileHandle
call CloseFile
quit:
call Clrscr
    mov  ecx,SIZEOF buffer - 1
    mov  edx,OFFSET buffer

    INVOKE Get_frequencies, ADDR buffer, ADDR freqTable
    call DisplayTable
ret
ReadFrequency ENDP

;-------------------------------------------------------------

 Get_frequencies PROC,
    pString:PTR BYTE,   ; points to string
    pTable:PTR DWORD    ; points to frequencey table

;
; Constructs a character frequency table. Each array position
; is indexed by its corresponding ASCII code.
;
; Returns: Each entry in the table contains a count of how
; many times that character occurred in the string.
;-------------------------------------------------------------

mov esi,pString
mov edi,pTable
cld     ; clear Direction flag (forward)

L1: mov eax,0       ; clear upper bits of EAX
   lodsb        ; AL = [ESI], inc ESI
   cmp al,0     ; end of string?
   je  Exit_proc        ; yes: exit
   shl eax,2        ; multiply by 4
   inc DWORD PTR [edi + eax]    ; inc table[AL]
   jmp L1       ; repeat loop

 Exit_proc:
    ret
 Get_frequencies ENDP
  ;-------------------------------------------------------------

 DisplayTable PROC

  ;
  ; Display the non-empty entries of the frequency table.
  ; This procedure was not required, but it makes it easier
  ; to demonstrate that Get_frequencies works.
  ;-------------------------------------------------------------


  mov ecx,LENGTHOF freqTable    ; entries to show
  mov esi,OFFSET freqTable
  mov edx,OFFSET  Frequency_Arr

  mov ebx,0 ; index counter

 L1:    mov eax,[esi]   ; get frequency count
        cmp eax,0   ; count = 0?

        jna L2  ; if so, skip to next entry
				add count_unique_chars,1

         mov al,bl    ; display the index
		 mov (freqarray ptr[edx]).character,al
         call WriteChar
         mov eax,[esi]  ; show frequency count
		 mov (freqarray ptr[edx]).frequency, eax
		 add edx,SIZEOF freqarray  ; point to next table entry
         call WriteDec
        call Crlf
  L2:   add esi,TYPE freqTable  ; point to next table entry
		
        inc ebx ; increment index
        loop L1
        mov edx, offset Frequency_Arr
        ;call Crlf
		;mov edx,count_unique_chars
		;mov num_unique_char , edx
        ;mov num_unique_char_temp ,edx
        ;mov num_unique_char_temp2, edx
        ret
      DisplayTable ENDP
.code
main PROC
 call ReadFrequency
	exit
main ENDP

END main