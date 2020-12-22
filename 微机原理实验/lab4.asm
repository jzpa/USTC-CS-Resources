DATAS SEGMENT
    ADDRESS DB 'Input_lab3.txt',0
    TESTARRAY DB '23321'
    ARRAY DB 100 DUP(0)
DATAS  ENDS

CODES  SEGMENT
     ASSUME    CS:CODES,DS:DATAS
     
START:
    MOV  AX,DATAS
    MOV  DS,AX

    LEA  BX,TESTARRAY
    MOV  [BX],2
    ADD  BX,1
    MOV  [BX],3
    ADD  BX,2
    MOV  CX,5    
    ;MOV  AL,2 
    LEA  BX,TESTARRAY
    CALL PrintValArray

    MOV  AH,4CH
    INT  21H

;假设一个无符号长整数被逆序存在数组里，数组是双字节，数组的地址是bx%,数组的长度是cx%,那么打印这个长整数
PrintValArray proc
    push ax
    push bx
    push cx
    
    add bx,cx
    sub bx,1
    
    PRINTARRAY_LOOP_BEGIN:
        cmp cx,0H
        je PRINTARRAY_LOOP_END
        mov ax,[bx]
        CALL PrintVal
        sub bx,1H
        sub cx,1H
        jmp PRINTARRAY_LOOP_BEGIN
    PRINTARRAY_LOOP_END:

    pop cx
    pop bx 
    pop ax
    ret

PrintValArray endp

;假设一个十进制数字被存在al%里，那么打印它
PrintVAl proc            
    push    ax
	push    dx
    mov     ah, 2
    mov     dl, al
    add     dl,30H
    int     21h
	pop     dx
    pop     ax
    ret
printVAl endp




; 从键盘输入一个两位的十进制无符号整数，该整数的真实值被存在al%里面
ScanfInt proc
    push cx
    push ax
    push bx

    mov bl,10
    call scanfVal
    mul bl
    mov cl,al
    call scanfVal
    add cl,al

    pop bx
    pop ax

    mov al,cl

    pop cx
    ret
ScanfInt endp

; scanf a value into %al
scanfVal proc
    mov ah,01H
    int 21h
    sub ax,30H
    ret
scanfVal endp




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

; print -
printSUB proc            
    push    ax
	push    dx
    mov     dl, '-'
	mov     ah,  02H
    int     21h
	pop     dx
    pop     ax
    ret
printSUB endp

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


CODES  ENDS
END   START