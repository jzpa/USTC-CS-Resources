DATAS SEGMENT
    ADDRESS_I  DB  'Input1.txt',0
    ADDRESS_O  DB  'Output1.txt$',0
	BUFFER    DB  '>>11222233334444111122223333444411112222333344441111222233334444',0
	 
DATAS  ENDS

CODES  SEGMENT
     ASSUME    CS:CODES,DS:DATAS
     
START:
	MOV  AX,DATAS
    MOV  DS,AX
	 
	;Create a file called "Input1.txt" 
	LEA  DX,ADDRESS_I
	MOV  CX,00H
	CALL CreateFile
	
	;Load string from keyboard into buffer
	LEA  AX,BUFFER
	CALL ScanfString 
	
	;Open file called "Input1.txt"
    LEA  DX,ADDRESS_I
	MOV  AL,1
    CALL OpenFile
	 ;CALL printCRLF
	
	;Write in the Buffer into the opened file
	LEA  BX,BUFFER
	ADD  BX,1
	MOV  CL,[BX]
	MOV  BX,AX
	LEA  DX,BUFFER
	ADD  DX,2
	MOV  AX,DX
	CALL WriteFile
	
	;Close the opened file
	CALL CloseFile
	
    ;Reopen the file
    LEA  DX,ADDRESS_I
    MOV  AL,0H
    CALL OpenFile

    ;Read the file into buffer
    LEA  DX,BUFFER
    MOV  BX,AX
    CALL ReadFile

    ;Transfer the String in the Buffer,,依赖于前面提供的CX尚为字符串长度值
	LEA BX,BUFFER
	MOV SI,0H
    MOV AX,CX
    CALL printCRLF
INLOOP:
	CMP SI,CX
	JAE  GETOUT
	MOV AX,[BX+SI]
	CALL AlphaTrans
	MOV [BX+SI],AX
	ADD SI,1H
	JMP INLOOP
GETOUT:	
	LEA AX,BUFFER
	CALL PrintString

    ;Create the output file Output1.txt
    PUSH CX
    LEA  DX,ADDRESS_O
	MOV  CX,00H
	CALL CreateFile
    POP  CX

    ;Open the output file
    LEA  DX,ADDRESS_O
	MOV  AL,1
    CALL OpenFile

    ;Write in the buffer into the output file
	MOV  BX,AX
	LEA  DX,BUFFER
	CALL WriteFile

CODEEND:    
    MOV  AH,4CH
    INT  21H

;Transmit the lower case alpha into hign 
AlphaTrans proc
	cmp AL,61H
	JB NOTCASE1
	cmp AL,7AH
	JA NOTCASE2
	SUB AL,20H
	ret
NOTCASE1:
    ;CALL printC
	ret
NOTCASE2:
    ;CALL printB
    ret
AlphaTrans endp	 

;create file in  routine %dx ,type %cx	,return in %ax 
CreateFile proc            
	mov  ah,3CH
	int 21H
	ret
CreateFile endp	

;Open file in  routine %dx ,type %al	,return in %ax 
OpenFile proc            
	mov  ah,3DH
	int 21H
	ret
OpenFile endp	

;Close file in  FP %bx ,return in %ax 
CloseFile proc            
	mov  ah,3EH
	int 21H
	ret
CloseFile endp

;Read file in  routine %dx ,FILE %bx ,size %cx , return in %ax 
ReadFile proc            
	mov  ah,3FH
	int 21H
	ret
ReadFile endp

;Write file in  routine %dx ,FILE %bx ,size %cx , return in %ax 
WriteFile proc            
	mov  ah,40H
	int 21H
	ret
WriteFile endp

;Delete file in  routine %dx ,return in %ax 
DeleteFile proc            
	mov  ah,41H
	int 21H
	ret
DeleteFile endp


; print %al
printAl proc            
    push    ax
	push    dx
    mov     ah, 2
    mov     dl, al
    int     21h
	pop     dx
    pop     ax
    ret
printAl endp

; print '\n'
printCRLF proc
    push ax
    mov     al, 10
    call    printAl
    mov     al, 13
    call    printAl
    pop  ax
    ret
printCRLF endp

; print c
printC proc            
    push    ax
	push    dx
    mov     dl, 'c'
	mov     ah,  02H
    int     21h
	pop     dx
    pop     ax
    ret
printC endp

; print b
printB proc            
    push    ax
	push    dx
    mov     dl, 'b'
	mov     ah,  02H
    int     21h
	pop     dx
    pop     ax
    ret
printB endp

; printf string in %ax
PrintString proc
	push ax
	push dx
	mov  dx,ax
	mov  ah,09H
	int  21H
	pop  dx
	pop  ax
	ret
PrintString endp

; scanf string into %ax from the keyboard, return the true number in %ax
ScanfString proc
	push ax
	push dx
	push bx
	mov  dx,ax
	mov  ah,0AH
	int  21H
	add  dx,1
	mov  bx,dx
	mov  ax,[bx]
	pop  bx
	pop  dx
	pop  ax
	ret
ScanfString endp

CODES  ENDS
END   START
