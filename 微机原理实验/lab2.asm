DATAS SEGMENT
    DOUBLEARRAY DB 100 DUP(0)
DATAS  ENDS

CODES  SEGMENT
     ASSUME    CS:CODES,DS:DATAS
     
START:

MOV  AX,DATAS
MOV  DS,AX

CALL scanfVal
;把n存在AL里面
MOV  BX,0H
MOV  BL,AL
;把得到的n存到DX里面

;下面开始循环，根据得到的整数n把需要的值存进数组
ARRAYINIT:
    MOV AX,BX
    MUL BL
    MOV CX,AX

ARRAY_LOAD_LOOP:
    CMP CX,0H
    JBE ARRAY_LOAD_END
    MOV AX,CX
    SUB AX,1H
    ;AX=CX-1
    SUB AH,AH
    DIV BL
    ;获取数组坐标
    CALL SaveValue
    ;数据存好了
    SUB CX,1H
    JMP ARRAY_LOAD_LOOP

ARRAY_LOAD_END:
    CALL printCRLF

;下面开始二轮循环，依次打印出二维数组
DISPLAY_INIT1:
    MOV CX,1H
    ;CX标定行，DX标定列

DISPLAY_LOOP1:
    CMP CX,BX
    JA  DISPLAY_END1

    DISPLAY_INIT2:
        MOV DX,1H
        ;CX标定行，DX标定列

    DISPLAY_LOOP2:
        CMP DX,CX
        JA DISPLAY_END2
        
        MOV AH,DL
        MOV AL,CL 
        SUB AX,101H
        PUSH CX
        CALL LoadValue
        MOV AX,CX
        POP CX
        CALL printVal
        CALL printSp

        ADD DX,1H
        JMP DISPLAY_LOOP2

    DISPLAY_END2:

    CALL printCRLF
    ADD CX,1H
    JMP DISPLAY_LOOP1

DISPLAY_END1:
    





MOV  AH,4CH
INT  21H

; Save a value %cx into the doublearray[%al,%ah]
SaveValue proc
    PUSH AX
    PUSH BX
    PUSH SI
    PUSH DX

    ;MOV DX,BX
    LEA BX,DOUBLEARRAY
    MOV SI,0H
    MOV DX,0H
    MOV DL,AH
    ADD SI,DX
    ;列数据已加入
    MOV DX,5H
    ADD DX,5H
    MUL DL
    MOV DX,0H
    MOV DL,AL
    ADD SI,DX
    ;导入行数据，得到需要的SI值
    MOV [BX+SI],CL

    POP DX
    POP SI
    POP BX
    POP AX

    ret
SaveValue endp

; Load a value in %cx from the doublearray[%al,%ah],
LoadValue proc
    PUSH AX
    PUSH BX
    PUSH SI
    PUSH DX

    ;MOV DX,BX
    LEA BX,DOUBLEARRAY
    MOV SI,0H
    MOV DX,0H
    MOV DL,AH
    ADD SI,DX
    ;列数据已加入
    MOV DX,5H
    ADD DX,5H
    MUL DL
    MOV DX,0H
    MOV DL,AL
    ADD SI,DX
    ;导入行数据，得到需要的SI值
    MOV CX,[BX+SI]

    POP DX
    POP SI
    POP BX
    POP AX

    ret
LoadValue endp


; scanf a value into %al
scanfVal proc
    mov ah,01H
    int 21h
    sub ax,30H
    ret
scanfVal endp


; print %al in Decixal
printVal proc
DETECT:
    push bx
    push cx
    push dx
INIT1:
    mov  cx,0H
    mov  bx,0H
    add  bx,5H
    add  bx,5H
LOOP1:
    cmp  al,bl
    jb  INIT2
    sub  ah,ah
    div  bl
    push ax
    add  cx,1H
    jmp LOOP1
Init2:
    mov  ah,al
    push ax
    add  cx,1H
LOOP2:
    cmp  cx,0H
    jbe  RELOAD
    sub  cx,1H
    pop  ax
    mov  al,ah
    add  al,30H
    call printAl
    jmp LOOP2
RELOAD:
    pop  dx
    pop  cx
    pop  bx
    ret
printVal endp

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

; print space
printSp proc            
    push    ax
	push    dx
    mov     dl, ' '
	mov     ah,  02H
    int     21h
	pop     dx
    pop     ax
    ret
printSp endp

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