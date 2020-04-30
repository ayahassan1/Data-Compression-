
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

Tree STRUCT
left dword 0
right dword 0
value dword 0
character byte ?
Tree ENDS

Code STRUCT
character byte ?
code_str byte 20 dup(?)
code_str_size dword 0
Code ENDS

.data


count_unique_chars dword 0
buffer BYTE BUFFER_SIZE DUP(?)

filename BYTE "output.txt",0
fileHandle HANDLE ?
freqTable DWORD 256 DUP(0)


num_unique_char dword 0
num_unique_char_temp dword 0
num_unique_char_temp2 dword 0
Frequency_Arr freqarray 100 dup(<>)
tree_Arr Tree 100 dup(<>)
Code_Arr Code 100 dup(<>)
minimum_1 dword 999999999
minimum_2 dword 999999999
size_Frequency_Arr dword 0
index dword 0
index_freq_arr1 dword 0
index_freq_arr2 dword 0
counter dword 0    ;save index 2 indecies of 2 selected minimums
sum_node dword 0
num_element_tree dword 0
counter_get_code dword 0
search_index_in_tree dword 1
search_index_in_tree_temp dword 1
search_counter_in_tree dword 2
code_str_size_temp dword ?
num dword 0
tree_size dword 0
file_tree_arr_indecies dword 100 dup(-1)
;file_tree_arr_chars byte 30 dup(?)
;file_tree_arr_values dword 100 dup(-1)
var_type_tree dword 0
file_tree_arr_chars_addr dword 0
Code_Arr_codestr_addr dword 0
Code_Arr_codestr_addr_temp dword 0
to_write_codes_file byte 9999 dup(?)
save_ecx dword 0
code_str_size_file_temp byte 0


endl Byte 0Dh,0Ah
Strf BYTE ?
spaceStr BYTE " ",0

file_tree_arr_values dword 100 dup(-1)
file_tree_arr_chars byte 30 dup(?)
    numberstring byte 16 DUP (0)
    numberChar Dword 0
    fmt byte "%d",0
fileNameOutW byte 'number.txt',0
FileHandleW Dword ?
numberBytesW Dword ?
numberW Dword ?


numberstringC byte 16 DUP (0)
numberCharC Dword 0
fmtC byte "%d",0
fileNameOutC byte 'code.txt',0
FileHandleC Dword ?
numberBytesC Dword ?
numberC Dword ?

.code

WriteToFileP proc
    ;create file
    push NULL
    push FILE_ATTRIBUTE_NORMAL
    push CREATE_ALWAYS
    push NULL
    push 0
    push GENERIC_WRITE
    push offset fileNameOutW
    call CreateFileA
    mov FileHandleW,eax

	mov ecx , lengthof file_tree_arr_values
	mov esi , offset file_tree_arr_values
	l:
	mov ebx , ecx
	mov edi , [esi]

	 cmp edi,-1	
	 jz end_tree

	call numF
	mov eax,FileHandleW
	mov edx,OFFSET spaceStr
	mov ecx,lengthof spaceStr
	call WriteToFile
	add esi,4
	mov ecx,ebx
   loop l
    end_tree:

   	mov eax,FileHandleW
	mov edx, offset endl
	mov ecx,lengthof endl
	call WriteToFile

		

   mov ecx , lengthof file_tree_arr_chars
	mov edi, offset file_tree_arr_chars
	mov ebx , ecx

	   	mov eax,FileHandleW
			l1:
	mov eax , [edi]
	cmp eax,0
	jz breakfinally
	mov strf ,al
	mov ebx , ecx
	mov eax,FileHandleW
	mov edx, offset strf
	mov ecx,lengthof strf
	call WriteToFile

		mov eax,FileHandleW
	mov edx,OFFSET spaceStr
	mov ecx,lengthof spaceStr
	call WriteToFile
	add edi,1
	mov ecx,ebx
   loop l1
   breakfinally:
  mov eax,fileHandleW
  call CloseFile 
	ret
WriteToFileP ENDP

numF proc
mov numberW , edi
    ;convert number to string
    push numberW                     ; Argument for format string
    push offset fmt                 ; Pointer to format string ("%d")
    push offset numberstring        ; Pointer to buffer for output
    call wsprintf                   ; Irvine32.inc / SmallWin.inc / User32.lib / User32.dll
    mov numberChar, eax             ; Length of the stored string
    add esp, (3*4)                  ; CCALL calling function! Adjust the stack.

    ;write
    push NULL
    push offset numberBytesW
    push numberChar
    push offset numberstring
    push FileHandleW
    call WriteFile


    ret
numF ENDP

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
mWrite <"Error: Buffer too small for the file",0dh,0ah>
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
         ;call WriteChar
         mov eax,[esi]  ; show frequency count
		 mov (freqarray ptr[edx]).frequency, eax
		 add edx,SIZEOF freqarray  ; point to next table entry
         ;call WriteDec
        ;call Crlf
  L2:   add esi,TYPE freqTable  ; point to next table entry
		
        inc ebx ; increment index
        loop L1
        mov edx, offset Frequency_Arr
        ;call Crlf
		mov edx,count_unique_chars
		mov num_unique_char , edx
        mov num_unique_char_temp ,edx
        mov num_unique_char_temp2, edx
        ret
      DisplayTable ENDP

initialize_tree_func PROC
mov ecx,num_unique_char
mov edi, offset Frequency_Arr
mov edx,offset tree_Arr 
initialize_tree:
mov esi,index
mov tree_Arr[esi].left,0
mov tree_Arr[esi].right,0
mov eax,(freqarray ptr [edi]).frequency
mov (tree_Arr[esi].value),eax
mov al,(freqarray ptr [edi]).character
mov (tree_Arr[esi].character),al
add edi,sizeof freqarray
add index,type Tree
add num_element_tree ,1
add tree_size,1
loop initialize_tree
   ret 
initialize_tree_func ENDP

get_2_minimums_func PROC 

get_2_minimums:
mov eax,(freqarray ptr [edi]).frequency
cmp eax,0
je continue
cmp eax,-1
je check2
cmp eax,minimum_1
jnl check
  xchg eax,minimum_1
  mov minimum_2,eax
  mov edx,index_freq_arr1
  mov index_freq_arr2,edx
  mov edx,counter
  add edx,1
  mov index_freq_arr1,edx
  jmp check2
check:
     cmp eax,minimum_2
  jnl check2
     xchg eax,minimum_2  
     mov edx,counter
	 add edx,1
     mov index_freq_arr2,edx
check2:
add edi,sizeof freqarray
add counter,1
loop get_2_minimums
continue:
call Complete_Tree_func_cont
   ret
get_2_minimums_func ENDP

Complete_Tree_func_cont PROC
mov ebx,offset tree_Arr
dec num_unique_char_temp
mov eax,minimum_1
add eax,minimum_2
mov sum_node,eax
mov edi,index
mov edx,index_freq_arr1
mov  tree_Arr[edi].left,edx
add tree_size,1
mov edx,index_freq_arr2
mov tree_Arr[edi].right,edx
mov tree_Arr[edi].value,eax 
mov esi,offset Frequency_Arr
mov eax,type freqarray
mul num_unique_char_temp2
mov ebx,sum_node
mov (freqarray PTR[esi+eax]).frequency,ebx
mov eax,type freqarray
dec index_freq_arr1
mul index_freq_arr1
mov (freqarray PTR[esi+eax]).frequency,-1
mov eax,type freqarray
dec index_freq_arr2
mul index_freq_arr2
mov (freqarray PTR[esi+eax]).frequency,-1
add index,type Tree
mov ecx,100
add num_unique_char_temp2,1
add num_element_tree,1
ret
Complete_Tree_func_cont   ENDP 

Complete_Tree_func PROC
mov ecx,1000
Complete_Tree:
mov minimum_1,999999
mov minimum_2,999999
cmp num_unique_char_temp,1       ;hyb2a fe wa7d hwa a5r wa7d et3mlo append 
je next_step
mov edi, offset Frequency_Arr
mov counter,0
 call get_2_minimums_func
loop Complete_Tree
next_step:
 ret
Complete_Tree_func ENDP

get_code_func PROC
mov ecx,num_unique_char
mov edi,offset tree_Arr
mov edx,offset Frequency_Arr
mov edx,0
mov eax,offset Code_Arr
mov Code_Arr_codestr_addr,eax
mov Code_Arr_codestr_addr_temp,eax
get_code:
mov eax,(Tree ptr [edi]).value
cmp num,eax
jz nextstep2
mov al,(Tree ptr [edi]).character
mov (Code_Arr[edx].character),al
mov esi,edi
add esi,sizeof Tree
mov counter_get_code,ecx
mov ecx,99999
mov ebx,search_index_in_tree
mov search_index_in_tree_temp,ebx
mov code_str_size_temp,0

call search_in_Tree_fun

mov eax,code_str_size_temp
mov (Code_Arr[edx].code_str_size),eax
mov eax,Code_Arr_codestr_addr
add eax,sizeof Code
mov Code_Arr_codestr_addr,eax
mov Code_Arr_codestr_addr_temp,eax
mov ecx,counter_get_code
add edi,sizeof Tree
add edx,type Code
add search_index_in_tree,1
mov eax,search_index_in_tree
inc eax
mov search_counter_in_tree,eax
loop get_code
nextstep2:
ret
get_code_func ENDP

code_found_left_func PROC
mov eax,Code_Arr_codestr_addr_temp
add eax,1
mov Code_Arr_codestr_addr_temp,eax
mov ebx,'0'
mov [eax],ebx
mov ebx,search_counter_in_tree
mov search_index_in_tree_temp,ebx
inc code_str_size_temp
ret
code_found_left_func ENDP

code_found_right_func PROC
mov eax,Code_Arr_codestr_addr_temp
add eax,1
mov Code_Arr_codestr_addr_temp,eax
mov ebx,'1'
mov [eax],ebx
mov ebx,search_counter_in_tree
mov search_index_in_tree_temp,ebx
inc code_str_size_temp
ret
code_found_right_func ENDP

search_in_Tree_fun PROC
 search_in_Tree:
mov eax,(Tree ptr [esi]).value
cmp num,eax
jz next_iteration
mov eax,(Tree ptr [esi]).left
cmp search_index_in_tree_temp,eax
jz nextstep_left
mov eax,(Tree ptr [esi]).right
cmp search_index_in_tree_temp,eax
jz nextstep_right
jmp coutinue_loop
nextstep_left:

call code_found_left_func

jmp coutinue_loop
nextstep_right:

call code_found_right_func

coutinue_loop:
mov eax,search_counter_in_tree
add eax,1
mov search_counter_in_tree,eax
add esi,sizeof Tree
loop search_in_Tree

next_iteration:
ret
search_in_Tree_fun ENDP

out_file_tree_index_func PROC
mov ecx,999999
out_file_tree_index:

mov edi,[ebx]
cmp edi,-1
jz nextstep3
mov edi,[ebx]
cmp edi,num
jz root
mov eax,[ebx]
dec eax
mul var_type_tree
mov edi,Tree_Arr[eax].left
mov [esi],edi
add esi,4
mov edi,Tree_Arr[eax].right
mov [esi],edi
add esi,4
cmp edi,num
jnz root
mov edi,file_tree_arr_chars_addr
mov al,Tree_Arr[eax].character
mov [edi],al
add file_tree_arr_chars_addr,1
root:
add ebx,4
loop out_file_tree_index
nextstep3:
ret
out_file_tree_index_func ENDP

get_index_file_tree_func PROC
mov ebx,offset file_tree_arr_indecies
mov file_tree_arr_chars_addr,offset file_tree_arr_chars
dec tree_size
mov eax,tree_size
mov var_type_tree,type tree
mul var_type_tree
inc tree_size
mov edx,tree_size
mov [ebx],edx


add ebx,4
mov edx,Tree_Arr[eax].left
mov [ebx],edx

add ebx,4
mov edx,Tree_Arr[eax].right
mov [ebx],edx
mov esi,ebx
add esi,4
mov ecx,99999
sub ebx,4
call out_file_tree_index_func
ret 
get_index_file_tree_func ENDP

out_file_tree_value_func PROC
mov ecx,99999
mov ebx,offset file_tree_arr_values
mov edi,offset file_tree_arr_indecies
out_file_tree_value:
mov eax,[edi]
cmp eax,-1
jz nextstep4
mov eax,[edi]
cmp eax,num
jz No_index
dec eax
mul var_type_tree
mov esi,Tree_Arr[eax].value
mov [ebx],esi
jmp continue_values
No_index:
mov esi,0
mov [ebx],esi
continue_values:
add edi,4
add ebx,4
loop out_file_tree_value
nextstep4:
ret 
out_file_tree_value_func ENDP


Code_Original_Text PROC
mov ecx, 999999
mov eax,offset buffer
mov ebx,offset to_write_codes_file
mov esi,offset Code_Arr
file_code_Original_Text:
mov dl,[eax]
cmp dl,0
je break_loop
mov dl,[eax]
add ecx,1
mov esi,offset Code_Arr
search_char:
cmp (Code ptr [esi]).character,dl
jne continue_search_char_in_codearr 
mov save_ecx,ecx
mov ecx, (Code ptr [esi]).code_str_size
mov edi,esi
add edi,ecx
get_char_code:
mov dl,byte ptr[edi]
mov byte ptr[ebx],dl
dec edi
inc ebx
loop get_char_code
mov ecx,save_ecx
jmp next_char
continue_search_char_in_codearr:
add esi,sizeof Code
loop search_char
next_char:
dec ecx
inc eax
loop file_code_Original_Text
break_loop:
ret 
Code_Original_Text	ENDP
Compression PROC

call ReadFrequency
call initialize_tree_func
call Complete_Tree_func
call get_code_func
call get_index_file_tree_func
call out_file_tree_value_func
call Code_Original_Text
call WriteToFileP

ret
Compression ENDP


WriteCodeToFile proc
mov edx,OFFSET fileNameOutC
call CreateOutputFile
mov fileHandleC,eax
mov eax,fileHandleC
mov edx,OFFSET to_write_codes_file
mov ecx,lengthof to_write_codes_file
call WriteToFile
 mov eax,fileHandleC
  call CloseFile 
   ret
WriteCodeToFile ENDP

;Write your functions here
;Do not forget to modify main.def file accordingly 

; DllMain is required for any DLL
DllMain PROC hInstance:DWORD, fdwReason:DWORD, lpReserved:DWORD 

mov eax, 1		; Return true to caller. 
ret 				
DllMain ENDP

END DllMain
