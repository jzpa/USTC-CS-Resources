
DATAS SEGMENT
    ADDRESS DB 'Error: x<0!$',0
    TESTARRAY DB 'ss-3+(8+(12-(5)))$',0
    ARRAYAONE DB '00001000010000100001',0
    ARRAYATWO DB '00001000010000100001',0
    ARRAYATHREE DB '00001000010000100001',0
    ARRAYX DB '00001000010000100001',0
    VAR1 DB 'value',0
    VAR2 DB 'value',0
    VAR3 DB 'value',0
    VAR4 DB 'value',0
    VAR5 DB 'value',0
    VAR6 DB 'value',0
    VAR7 DB 'value',0
    VAR8 DB 'value',0
    VAR9 DB 'value',0
    VARa DB 'value',0
    VARb DB 'value',0
    VARc DB 'value',0
    DOUBLEONE DD  -100.25
    DOUBLETWO DD  -100.25
    DOUBLETHREE DD  -100.25
    DOUBLEX DD  -100.25
    DOUBLES DD 1.0
    DOUBLEX1 DD -100.25
    DOUBLEX2 DD -100.25
    DOUBLEX3 DD -100.25
    ARRAY DB 100 DUP(0)
DATAS  ENDS

CODES  SEGMENT
     ASSUME    CS:CODES,DS:DATAS
     
START:
    MOV  AX,DATAS
    MOV  DS,AX

    
    FINIT
    ;FLD DOUBLEONE
    ;FLD DOUBLETWO
    ;FADD
    ;FST DOUBLEONE
    LEA AX,ARRAYX
    CALL ScanfString
    
    LEA DX,DOUBLEX
    LEA CX,ARRAYX 
    CALL DtoB

    CALL printCRLF

    LEA AX,ARRAYAONE
    CALL ScanfString
    
    LEA DX,DOUBLEONE
    LEA CX,ARRAYAONE
    CALL DtoB

    CALL printCRLF
    
    LEA AX,ARRAYATWO
    CALL ScanfString
    
    LEA DX,DOUBLETWO
    LEA CX,ARRAYATWO
    CALL DtoB

    CALL printCRLF

    LEA AX,ARRAYATHREE
    CALL ScanfString
    
    LEA DX,DOUBLETHREE
    LEA CX,ARRAYATHREE
    CALL DtoB



    
    
    ;LOAD FINISHED!!
    CALL printCRLF
    lea bx,DOUBLEONE
    CALL printFloat_in_bit
    CALL printCRLF
    LEA bx,DOUBLETWO
    CALL printFloat_in_bit
    CALL printCRLF
    LEA bx,DOUBLETHREE
    CALL printFloat_in_bit
    CALL printCRLF
    LEA bx,DOUBLEX
    CALL printFloat_in_bit

    ;CALL printCRLF
    ;LEA bx,DOUBLETHREE
    ;CALL BtoD


    LEA BX,ARRAYX
    ADD BX,2
    cmp byte ptr [BX],'-'
    je BRANCH_PENDP_IF
    jmp BRANCH_PENDP_ELSE
    BRANCH_PENDP_IF:
        lea ax,ADDRESS
        call PrintString
        jmp BRANCH_PENDP_END
    BRANCH_PENDP_ELSE:
        jmp BRANCH_PENDP_END
    BRANCH_PENDP_END:
    ;
    FLD DOUBLEONE
    FLD DOUBLEX
    FSQRT
    ;;;;;;;;;;;;;;;;;
    FST DOUBLEX1
    CALL printCRLF
    LEA bx,DOUBLEX1
    CALL BtoD
    ;;;;;;;;;;;;;;;;;;
    FMUL
    ;;;;;;;;;;;;;;;;;
    FST DOUBLEX1
    CALL printCRLF
    LEA bx,DOUBLEX1
    CALL BtoD
    ;;;;;;;;;;;;;;;;;;
    FLD DOUBLETWO
    FLD DOUBLEX
    FYL2X
    
    FADD
    ;;;;;;;;;;;;;;;;;
    FST DOUBLEX1
    CALL printCRLF
    LEA bx,DOUBLEX1
    CALL BtoD
    ;;;;;;;;;;;;;;;;;;
    FLD DOUBLETHREE
    FLD DOUBLEX
    DB 0D9H,0FEH
    
    FMUL
    FADD
    

    FST DOUBLEX1

    CALL printCRLF
    LEA bx,DOUBLEX1
    CALL BtoD

    CALL printCRLF
    lea bx,DOUBLEX1
    CALL printFloat_in_bit
    ;CALL printCRLF
    ;LEA bx,DOUBLETWO
    ;CALL BtoD

    ;CALL printCRLF
    ;LEA bx,DOUBLETHREE
    ;CALL BtoD

    MOV  AH,4CH
    INT  21H

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

; print %al in Decixal
printVal proc
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

    pop  dx
    pop  cx
    pop  bx
    ret
printVal endp

; print %ax in Signed Decixal
PrintSignedVal proc
DETECT:
    push bx
    push cx
    push dx


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


; assume that bx% save the float ,print it 
BtoD proc
    push ax
    push bx
    push cx
    push dx

    ; 保存源地址
    mov ax,bx
    lea bx,VAR6
    mov [bx],ax
    
    mov bx,ax
    add bx,3
    mov al,[bx]
    and al,080H

    ;存储正负值
    cmp al,080H
    je BRANCH4_IF
    jmp BRANCH4_ELSE
    BRANCH4_IF:
        lea bx,VAR2
        mov byte ptr [bx],-1
        jmp BRANCH4_END
    BRANCH4_ELSE:
        lea bx,VAR2
        mov byte ptr [bx],1
        jmp BRANCH4_END
    BRANCH4_END:

    ;开始分析整数性质，首先得到偏移数
    lea bx,VAR6
    mov bx,[bx]
    add bx,2
    mov al,[bx]
    add bx,1
    mov ah,[bx]
    ;高16位已经存在ax%
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    sub al,07FH
    lea bx,VARa
    mov [bx],al
    mov cl,al
    ;浮点位置已经存在了VARa,现在被转移到cl
    lea bx,VAR6
    mov bx,[bx]
    add bx,2
    mov al,[bx]
    add bx,1
    mov ah,[bx]
    ;高16位存于ax%
    or al,080H
    ; 首位置1
    sub bx,2
    mov dh,[bx]
    sub bx,1
    mov dl,[bx]

    ; 无代价地置cx的值为需要移位的数
    push ax
    push bx

    lea bx,VARa
    mov al,[bx]
    mov cl,7
    sub cl,al

    pop bx
    pop ax
    ; 此时，dx里面存满了高位

    ; 循环移位
    LOOP4_BEGIN:
        cmp cl,0
        je LOOP4_END

        shr dx,1
        shr al,1
        jc BRANCH5_IF
        jmp BRANCH5_ELSE
        BRANCH5_IF:
            or dx,08000H
            jmp BRANCH5_END
        BRANCH5_ELSE:
            jmp BRANCH5_END
        BRANCH5_END:
        sub cl,1
        jmp LOOP4_BEGIN
    LOOP4_END:

    ; 此刻al内存的恰为需要的整数部分的二进制值
    ; 此刻dx内存的是需要的小数部分的二进制值
    ; 整数部分二进制值存入VAR3
    lea bx,VAR3
    mov [bx],ax

    ; 准备计算浮点部分值，bx用于计数，ax是乘法元，cx是循环变量
    mov cx,16
    mov bx,0
    mov ax,10000
    LOOP5_BEGIN:
        cmp cx,0
        je LOOP5_END

        push dx
        push bx
        mov bx,2
        sub dx,dx
        div bx
        pop bx
        pop dx
        shl dx,1
        jc BRANCH6_IF
        jmp BRANCH6_ELSE
        BRANCH6_IF:
            add bx,ax
            jmp BRANCH6_END
        BRANCH6_ELSE:
            jmp BRANCH6_END
        BRANCH6_END:

        sub cx,1
        jmp LOOP5_BEGIN
    LOOP5_END:

    ;至此，bx中存着小数部分的值
    mov ax,bx
    lea bx,VAR4
    mov [bx],ax

    ; 开始打印
    lea bx,VAR2
    mov bx,[bx]
    cmp bx,1
    je BRANCHOUT
    jmp BRANCHOUTEND
    BRANCHOUT:
        call printSUB
    BRANCHOUTEND:

    lea bx,VAR3
    mov ax,[bx]
    call printVal
    call printPoint
    lea bx,VAR4
    mov ax,[bx]
    call PrintSignedVal
    
    pop dx
    pop cx
    pop bx
    pop ax
    ret
BtoD endp

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

; assume that bx% save the float, print it 
printFloat_in_bit proc
    push ax
    push bx
    mov ax,[bx]
    CALL printbit_8
    add bx,1
    mov ax,[bx]
    CALL printbit_8
    add bx,1
    mov ax,[bx]
    CALL printbit_8
    add bx,1
    mov ax,[bx]
    CALL printbit_8
    pop bx
    pop ax
printFloat_in_bit endp


; assume that cx% save the address of the float number in Dex，then convert it to float in address dx%
DtoB proc
    push ax
    push bx
    push cx
    push dx



    ;存储源、目标地址
    lea bx,VAR5
    mov [bx],cx
    lea bx,VAR6
    mov [bx],dx
    
    ;取出源地址存于bx
    lea bx,VAR5
    mov bx,[bx]
    
    ;定位到数组长度部分，长度存于var1
    add bx,1
    mov ax,[bx]
    lea bx,VAR1
    mov [bx],ax

    ;取出源地址存于bx
    lea bx,VAR5
    mov bx,[bx]

    ;定位到第一位，存正负信息于var2
    add bx,2
    mov ax,[bx]
    lea bx,VAR2
    cmp al,'-'
    je POSITIVE_PENDING_BRANCH_IF
    jmp POSITIVE_PENDING_BRANCH_ELSE
    POSITIVE_PENDING_BRANCH_IF:
        mov byte ptr [bx],-1
        lea bx,VAR5
        mov bx,[bx]
        add bx,3
        jmp POSITIVE_PENDING_BRANCH_END
    POSITIVE_PENDING_BRANCH_ELSE:
        mov byte ptr [bx],1
        lea bx,VAR5
        mov bx,[bx]
        add bx,2
        jmp POSITIVE_PENDING_BRANCH_END
    POSITIVE_PENDING_BRANCH_END:

    mov ax,0
    ;开始读入整数部分,内容存于VAR3
    READ1_LOOP_BEGIN:
        cmp byte ptr [bx],'.'
        je READ1_LOOP_END
        cmp byte ptr [bx],'$'
        je READ1_LOOP_END

        mov cx, 10
        mul cx
        mov cx, [bx]
        sub cx,30H
        sub ch,ch
        add ax,cx

        add bx,1        
        jmp READ1_LOOP_BEGIN
    READ1_LOOP_END:

    cmp byte ptr [bx],'$'
    push bx
    LEA bx,VAR3
    mov [bx],ax
    pop bx
    je DORF_PENDING_BRANCH_IF
    jmp DORF_PENDING_BRANCH_ELSE
    DORF_PENDING_BRANCH_IF:
        ;是整数，直接写好小数部分，小数位数和整数、浮点标记
        lea bx,VAR7
        mov byte ptr [bx],0
        lea bx,VAR4
        mov word ptr [bx],0
        lea bx,VAR8
        mov word ptr [bx],0
        jmp DORF_PENDING_BRANCH_END
    DORF_PENDING_BRANCH_ELSE:
        ;若是浮点数，那么先填写
        push bx
        lea bx,VAR7
        mov byte ptr [bx],1
        pop bx
        add bx,1
        ;现在bx定位到了小数的第一位

        ;开始读入小数部分,内容存于VAR3，小数十进制位数存于VAR8
    mov ax,0
    mov dx,0
    READ2_LOOP_BEGIN:
        cmp byte ptr [bx],'$'
        je READ2_LOOP_END

        add dx,1
        push dx
        mov cx, 10
        mul cx
        pop dx
        mov cx, [bx]
        sub cx,30H
        sub ch,ch
        add ax,cx

        add bx,1        
        jmp READ2_LOOP_BEGIN
    READ2_LOOP_END:
    
    lea bx,VAR4
    mov [bx],ax
    lea bx,VAR8
    mov [bx],dx

        jmp DORF_PENDING_BRANCH_END
    DORF_PENDING_BRANCH_END:
        
    ;至此，已经完成8个变量的全部填写，接下来由这8个变量生成需要的浮点值
    ;首先分析整数部分的二进制位数
    lea bx,VAR3
    mov ax,[bx]
    mov cx,0

    PART1_CAL_LOOP_BEGIN:
        cmp ax,0
        je PART1_CAL_LOOP_END
        shr ax,1
        add cx,1
        jmp PART1_CAL_LOOP_BEGIN
    PART1_CAL_LOOP_END:
    ;此时cx中存着整数二进制位数
    ;分析整数，如果整个浮点数是个纯小数，为了方便起见先把它加个1
    lea bx,VAR3
    cmp word ptr [bx],0
    je BRANCH1_IF
    jmp BRANCH1_ELSE
    BRANCH1_IF:
        mov word ptr [bx],1
        lea bx,VARb
        mov word ptr [bx],1
        mov cx,1
        jmp BRANCH1_END
    BRANCH1_ELSE:
        lea bx,VARb
        mov word ptr [bx],0
        jmp BRANCH1_END
    BRANCH1_END:
    lea bx,VAR9
    mov [bx],cx
    sub cx,1
    add cl,07fH
    lea bx,VARa
    mov [bx],cl

    ;开始分析小数部分，总是假设整数部分的值小于100
    lea bx,VAR8
    mov cx,[bx]
    mov bx,10
    mov ax,1
    LOOP1_BEGIN:
        cmp cx,0
        je LOOP1_END
        
        mul bl
        sub cx,1

        jmp LOOP1_BEGIN
    LOOP1_END:
    mov bx,ax
    push bx
    lea bx,VAR4
    mov ax,[bx]
    pop bx

    ;不管别的先算出小数点后16位再说
    mov dx,0
    mov cx,16
    LOOP2_BEGIN:
        cmp cx,0
        je LOOP2_END
        shl ax,1
        push cx
        push dx
        sub dx,dx
        div bx
        mov cx,ax
        mov ax,dx
        pop dx
        shl dx,1
        add dx,cx
        pop cx
        sub cx,1
        jmp LOOP2_BEGIN
    LOOP2_END:
    ;现在dx存着的就是小数点后16位的值
    ;然后要根据顶层的需要决定再算多少位
    ;首先在无代价情况下置cx为7-整数的二进制位数
    ;并将存在var3中的整数值复制到varc中
    push ax
    push bx
    push dx
    lea bx,VAR9
    mov ax,[bx]
    mov cx,8
    sub cx,ax
    lea bx,VAR3
    mov ax,[bx]
    lea bx,VARc
    mov [bx],ax
    pop dx
    pop bx
    pop ax

    LOOP3_BEGIN:
        cmp cx,0
        je LOOP3_END
        push cx

        shl ax,1
        push dx
        sub dx,dx
        div bx
        mov cx,ax
        mov ax,dx
        pop dx
        shl dx,1
        ;额外增加处理整数部分的内容
        jc BRANCH2_IF
        jmp BRANCH2_ELSE
        BRANCH2_IF:
            push ax
            push bx
            push cx
            push dx

            lea bx,VARc
            mov ax,[bx]
            shl ax,1
            add ax,1
            lea bx,VARc
            mov [bx],ax

            pop dx
            pop cx
            pop bx
            pop ax
            jmp BRANCH2_END
        BRANCH2_ELSE:
            push ax
            push bx
            push cx
            push dx

            lea bx,VARc
            mov ax,[bx]
            shl ax,1
            lea bx,VARc
            mov [bx],ax

            pop dx
            pop cx
            pop bx
            pop ax
            jmp BRANCH2_END
        BRANCH2_END:

        add dx,cx
        pop cx
        sub cx,1
        jmp LOOP3_BEGIN
    LOOP3_END:

    ;现在dx中存着低16位的数据，那么
    lea bx,VAR6
    mov bx,[bx]
    mov [bx],dl
    add bx,1
    mov [bx],dh
    lea bx,VAR2
    mov ax,[bx]
    cmp al,1
    mov cx,0
    je BRANCH3_IF
    jmp BRANCH3_ELSE
    BRANCH3_IF:
        shl cx,1
        add cx,0
        jmp BRANCH3_END
    BRANCH3_ELSE:
        shl cx,1
        add cx,1
        jmp BRANCH3_END
    BRANCH3_END:
    lea bx,VARa
    mov ax,[bx]
    and ax,00FFH
    shl cx,1
    shl cx,1
    shl cx,1
    shl cx,1
    shl cx,1
    shl cx,1
    shl cx,1
    shl cx,1
    add cx,ax

    lea bx,VARc
    mov ax,[bx]
    and ax,007FH
    shl cx,1
    shl cx,1
    shl cx,1
    shl cx,1
    shl cx,1
    shl cx,1
    shl cx,1
    add cx,ax

    lea bx,VAR6
    mov bx,[bx]
    add bx,2
    mov [bx],cl
    add bx,1
    mov [bx],ch


    pop dx
    pop cx
    pop bx
    pop ax

    ret

DtoB endp


; print %al
print1 proc            
    push    ax
	push    dx
    mov     ah, 2
    mov     dl, 31H
    int     21h
	pop     dx
    pop     ax
    ret
print1 endp
print0 proc            
    push    ax
	push    dx
    mov     ah, 2
    mov     dl, 30H
    int     21h
	pop     dx
    pop     ax
    ret
print0 endp
printbit_8 proc
    push dx
    push cx
    push bx 
    mov bx,ax
    mov cx,8
    mov dx,1
PLOOP1_BEGIN:
    cmp cx,0
    je PLOOP1_END
    sub cx,1
    mov al,bl
    and al,dl
    sub ah,ah
    div dl
    cmp al,1
    je BRANCH_IF
    jmp BRANCH_ELSE
    BRANCH_IF:
        call print1
        jmp BRANCH_END
    BRANCH_ELSE:
        call print0
        jmp BRANCH_END
    BRANCH_END:
    mov al,dl
    sub ah,ah
    mov dl,2
    mul dl
    mov dl,al
    jmp PLOOP1_BEGIN
PLOOP1_END:
    pop bx
    pop cx
    pop dx
    ret

printbit_8 endp

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

; print '\n'
printPoint proc
    push ax
    mov     al, '.'
    call    printAl
    pop  ax
    ret
printPoint endp

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