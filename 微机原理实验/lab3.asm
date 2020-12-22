DATAS SEGMENT
    ADDRESS DB 'Input_lab3.txt',0
    BUFFER DB 5 DUP(0)
    ARRAY DB 100 DUP(0)
DATAS  ENDS

CODES  SEGMENT
     ASSUME    CS:CODES,DS:DATAS
     
START:
    MOV  AX,DATAS
    MOV  DS,AX

    ;将文件内的数字读入数组中，每两个字节为一个数
    LEA  AX,ADDRESS
    LEA  BX,BUFFER
    LEA  CX,ARRAY
    CALL ReadArray

    ;使用冒泡排序
    LEA AX,ARRAY
    CALL OrderArray

    ;打印输出数组
    LEA ax,ARRAY
    CALL PrintArray
    ;mov ax,-1AH
    ;CALL PrintSignedVal

    MOV  AH,4CH
    INT  21H

;assume that %ax save the array address,%si sace the array length,print it
    
PrintArray proc
    push bx
    push cx
    mov bx,ax
    mov cx,si
    PRINTARRAY_LOOP_BEGIN:
        cmp cx,0H
        je PRINTARRAY_LOOP_END
        mov ax,[bx]
        CALL PrintSignedVal
        CALL printCRLF
        add bx,2H
        sub cx,1H
        jmp PRINTARRAY_LOOP_BEGIN
    PRINTARRAY_LOOP_END:

    pop cx
    pop bx
    ret

PrintArray endp


; assume that %ax save the array address, %si save the array length,then order it 
OrderArray proc
    mov bx,ax

    mov ax,si
    sub ax,1H
    sub ah,ah
    push si
    mov si,2H
    ORDER_LOOP1_BEGIN:
        cmp al,0H
        jle ORDER_LOOP1_END

        push bx
        sub ah,ah
        ORDER_LOOP2_BEGIN:
            cmp ah,al
            jge ORDER_LOOP2_END

            mov cx,[bx]
            mov dx,[bx+si]
            cmp cx,dx
            jle ORDER_BRANCH_END
            ORDER_BRANCH_IF:
                push cx
                push dx
                pop cx
                mov [bx],cx
                pop cx
                mov [bx+si],cx
            ORDER_BRANCH_END:

            add bx,2H

            add ah,1H
            jmp ORDER_LOOP2_BEGIN
        ORDER_LOOP2_END:
        pop bx

        sub al,1H
        jmp ORDER_LOOP1_BEGIN
    ORDER_LOOP1_END:
    pop si
    ret
OrderArray endp

;假设输入串的首地址是%bx，所读到的偏移量是%si
GetToken_Value proc
    
    push dx
        mov dx,0
        push cx
            mov cx,100
            push ax
                mov al,ah
                sub al,30H
                sub ah,ah
                mul cl
                add dx,ax
            pop ax

            mov cx,10
            push ax
                sub al,30H
                sub ah,ah
                mul cl
                mul cl
                mul cl
                add dx,ax
            pop ax

            mov cx,1
            push bx
                mov al,bh
                sub al,30H
                sub ah,ah
                mul cl
                add dx,ax
            pop bx

            mov cx,10
            push bx
                mov al,bl
                sub al,30H
                sub ah,ah
                mul cl
                add dx,ax
            pop bx    
        pop cx
        cmp cx,2DH
        je CASE1
        cmp cx,2BH
        je CASE2
        jmp CONDITIONEND
        ;负数情况
        CASE1:
        mov ax,dx
        push bx
            mov bx,-1H
            imul bx
        pop bx
        jmp CONDITIONEND
        CASE2:
        mov ax,dx
        jmp CONDITIONEND
        CONDITIONEND:
    pop dx

    ret
AtoI endp

; Read in an array from %ax to the buffer called %bx,and the put them in the array in %cx,save the length of the array in %si
ReadArray proc
    push dx
        mov si,0H
        mov dx,ax
        mov al,00H
        call OpenFile ;now the FILE saved in ax,dx remain the same%
        mov dx,bx
        mov bx,ax
    
        ;now dx remain the BUFFER,bx remain the FILE,cx remain the size
        LOOP_ReadArray_Begin:
        ;CALL printC
    
        push cx
            mov cx,5H
            call ReadFile
            cmp ax,cx
        pop  cx    
            jb LOOP_ReadArray_Out
        
        add si,1H
        push dx
            push ax
                push bx
                    push cx
                        mov bx,dx
                        mov cl,[bx]
                        add bx,1H
                        mov ax,[bx]
                        add bx,2H
                        mov bx,[bx]
                        call AtoI
                    pop cx
                    mov bx,cx
                    mov [bx],ax
                    add cx,2H
                pop  bx
            pop  ax
        pop  dx
    
    
        jmp LOOP_ReadArray_Begin
        LOOP_ReadArray_Out:
        call CloseFile

    pop dx


    ret
ReadArray endp


; scanf a value into %al
scanfVal proc
    mov ah,01H
    int 21h
    sub ax,30H
    ret
scanfVal endp


; print %ax in Signed Decixal
PrintSignedVal proc
DETECT:
    push bx
    push cx
    push dx

    cmp ax,0H
    jge PRINT_BRANCH_END
PRINT_BRANCH_IF:
    call printSUB
    mov bx,-1
    imul bx
PRINT_BRANCH_END:

    mov cx,0H
    mov bx,10
PRINT_LOOP1_BEGIN:
    sub dx,dx
    div bx
    push dx
    add cx,1H
    cmp ax,0
    je PRINT_LOOP1_END
    jmp PRINT_LOOP1_BEGIN
PRINT_LOOP1_END:

PRINT_LOOP2_BEGIN:
    cmp cx,0
    je PRINT_LOOP2_END
    pop ax
    add ax,30H
    CALL printAl
    sub cx,1H    
    jmp PRINT_LOOP2_BEGIN
PRINT_LOOP2_END:

RELOAD:
    pop  dx
    pop  cx
    pop  bx
    ret
PrintSignedVal endp

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
    ;CALL printB
	mov  ah,3FH
    ;CALL printB
	int 21H
    ;CALL printC
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

CODES  ENDS
END   START