DATAS SEGMENT
    ADDRESS DB 'Input_lab3.txt',0
    TESTARRAY DB 'ss-3+(8+(12-(5)))$',0
    ARRAY DB 100 DUP(0)
DATAS  ENDS

CODES  SEGMENT
     ASSUME    CS:CODES,DS:DATAS
     
START:
    MOV  AX,DATAS
    MOV  DS,AX

    LEA  DX,TESTARRAY
    MOV  AH,0AH
    INT 21H
    LEA  BX,TESTARRAY
    CALL Calculate

    CALL printCRLF
    MOV bl,1
    IMUL bl
    CALL PrintSignedVal

    MOV  AH,4CH
    INT  21H


; 假设表达式串的首地址存在%bx，那么计算出它的值存在%ax
Calculate proc
    push bx
    push cx
    push si


    mov si,2;

    ; 将‘(’压栈
    mov ah,'('
    sub al,al
    push ax
    CALCULATE_LOOP_BEGIN:
        ; 若读到的是串结束符就跳出循环
        cmp [bx+si],'$'
        je CALCULATE_LOOP_END
        
        ; 根据栈顶进行判断分支
        pop ax;
        push ax;
        cmp ah,'+'
        je CALCULATE_SWITCH1_CASE1
        cmp ah,'-'
        je CALCULATE_SWITCH1_CASE2
        cmp ah,'('
        je CALCULATE_SWITCH1_CASE3
        cmp ah,1
        je CALCULATE_SWITCH1_CASE4
        jmp CALCULATE_SWITCH1_END

        CALCULATE_SWITCH1_CASE1:
            
            cmp [bx+si],'('
            je CALCULATE_SWITCH1_CASE1_ELSE
            CALCULATE_SWITCH1_CASE1_IF:
                CALL GetToken_Value
                mov cx,ax
                pop ax
                pop ax
                add al,cl
                push ax
                jmp CALCULATE_SWITCH1_CASE1_END
            CALCULATE_SWITCH1_CASE1_ELSE:
                mov ah,[bx+si]
                sub al,al
                push ax
                add si,1
                jmp CALCULATE_SWITCH1_CASE1_END
            CALCULATE_SWITCH1_CASE1_END:

            jmp CALCULATE_SWITCH1_END
        CALCULATE_SWITCH1_CASE2:
            
            cmp [bx+si],'('
            je CALCULATE_SWITCH1_CASE2_ELSE
            CALCULATE_SWITCH1_CASE2_IF:
                CALL GetToken_Value
                mov cx,ax
                pop ax
                pop ax
                sub al,cl
                push ax
                jmp CALCULATE_SWITCH1_CASE2_END
            CALCULATE_SWITCH1_CASE2_ELSE:
                mov ah,[bx+si]
                sub al,al
                push ax
                add si,1
                jmp CALCULATE_SWITCH1_CASE2_END
            CALCULATE_SWITCH1_CASE2_END:
            
            jmp CALCULATE_SWITCH1_END
        CALCULATE_SWITCH1_CASE3:

            cmp [bx+si],'('
            je CALCULATE_SWITCH1_CASE3_ELSE
            CALCULATE_SWITCH1_CASE3_IF:
                CALL GetToken_Value
                push ax
                jmp CALCULATE_SWITCH1_CASE3_END
            CALCULATE_SWITCH1_CASE3_ELSE:
                mov ah,[bx+si]
                sub al,al
                push ax
                add si,1
                jmp CALCULATE_SWITCH1_CASE3_END
            CALCULATE_SWITCH1_CASE3_END:

            jmp CALCULATE_SWITCH1_END
        CALCULATE_SWITCH1_CASE4:

            cmp [bx+si],')'
            je CALCULATE_SWITCH1_CASE4_ELSE
            CALCULATE_SWITCH1_CASE4_IF:
                mov ah,[bx+si]
                sub al,al
                push ax
                add si,1
                jmp CALCULATE_SWITCH1_CASE4_END
            CALCULATE_SWITCH1_CASE4_ELSE:
                ;弹两次栈，把value存在cx，同时把栈顶存在ax
                pop cx
                pop ax
                pop ax
                push ax

                ;分支判断
                cmp ah,'+'
                je CALCULATE_SWITCH1_CASE4_ELSE_IF1
                cmp ah,'-'
                je CALCULATE_SWITCH1_CASE4_ELSE_IF2
                jmp CALCULATE_SWITCH1_CASE4_ELSE_ELSE
                CALCULATE_SWITCH1_CASE4_ELSE_IF1:
                    pop ax
                    pop ax
                    add al,cl
                    push ax
                    jmp CALCULATE_SWITCH1_CASE4_ELSE_END
                CALCULATE_SWITCH1_CASE4_ELSE_IF2:
                    pop ax
                    pop ax
                    sub al,cl
                    push ax
                    jmp CALCULATE_SWITCH1_CASE4_ELSE_END
                CALCULATE_SWITCH1_CASE4_ELSE_ELSE:
                    push cx
                    jmp CALCULATE_SWITCH1_CASE4_ELSE_END
                CALCULATE_SWITCH1_CASE4_ELSE_END:

                add si,1
                jmp CALCULATE_SWITCH1_CASE4_END
            CALCULATE_SWITCH1_CASE4_END:

            jmp CALCULATE_SWITCH1_END
        CALCULATE_SWITCH1_END:


        jmp CALCULATE_LOOP_BEGIN
    CALCULATE_LOOP_END:
    pop ax
    pop cx

    pop si
    pop cx
    pop bx
    ret
Calculate endp



; 假设表达式串的首地址存在%bx，而读指针的偏移量是%si，那么我将以此为基点，读取一个有符号数，这个数被存在%al，且%ah置1,并且将%si更新到下一个读取点
GetToken_Value proc
    push bx
    push cx
    push dx

    ;若首字符是数字，置%cl为1，如果是负号，则置%cl为-1，读指针右移
    cmp [bx+si] , '-'
    je GETTOKEN_VALUE_BRANCH_ELSE
    GETTOKEN_VALUE_BRANCH_IF:;首字符是数字
        mov cl,1
        jmp GETTOKEN_VALUE_BRANCH_END
    GETTOKEN_VALUE_BRANCH_ELSE:;首字符是负号
        mov cl,-1
        add si,1
        jmp GETTOKEN_VALUE_BRANCH_END
    GETTOKEN_VALUE_BRANCH_END:

    ;首先不管发生了啥，先读一个字符，这个字符肯定是数字，将其与%ax相乘作为第一笔启动资金
    mov al,0
    GETTOKEN_VALUE_LOOP_BEGIN:
        ;若读到的字符已经不是数字，那么果断跳出循环
        cmp [bx+si] , '0'
        jb GETTOKEN_VALUE_LOOP_END
        cmp [bx+si] , '9'
        ja GETTOKEN_VALUE_LOOP_END

        ;因为读到的字符仍然是数字，所以将读到的字符转换成数字，并且%ax原值自乘10后加上这个新读到的数字，随后si自增
        mov dl,10
        mul dl
        mov dl,[bx+si]
        sub dl,48
        add al,dl
        add si,1

        jmp GETTOKEN_VALUE_LOOP_BEGIN
    GETTOKEN_VALUE_LOOP_END:

    mul cl
    mov ah,1

    pop dx
    pop cx
    pop bx

    ret
GetToken_Value endp

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