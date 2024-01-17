[org 0x0100]
jmp start
livesRemaining: db 3
string: db'Time Remaining: '
str2: db'Lives:'
initialMins:db 78
initialSecs: db 59
MinsRemaining: db 03
timeOver: dw 0
tickCount:db 0
winner: db 'YOU WON!'
gameOver: db 'GAME OVER!'
points: db 'Score:'
score: dw 0
seconds: db 0
tickCount2: db 0
speed: db 5
flag: db 0;indicates no poison
welcome: db'LETS PLAY!!!!'
snake: times 50 dw 0;
asterisk:db '*'
space: db ' '
head:db '>'
left: db 0
right:db 0
up: db 0
down: db 0
fruit: dw 0
food:db 'O','*','O'
poison:db 'P'
array: dw 0,0,0,0
foodlocation: dw 0
length:dw 6
lives: db 3
random: db 4
boundary:db '='
oldkbisr:dd 0


print:	push bp
	mov bp,sp
	push bx
	push cx
	push dx
	push es
	push ax
	mov ah, 0x13 ; service 13 - print string
	mov al, 1 ; subservice 01 update cursor
	mov bh, 0 ; output on page 0
	mov bl, [bp+8] ; normal attrib
	mov dx,[bp+6]; row 10 column 3
	mov cx, 1 ; length of string
	push cs
	pop es ; segment of string
	mov bp, [bp+4]; offset of string
	int 0x10 ; call BIOS video service
	pop ax
	pop es
	pop dx
	pop cx
	pop bx
	pop bp
	ret 6

	clrscr:	push dx
	push cx
	push bx
	push ax
	mov cx,2000;clears screen using BIOS service
	mov dx,0x7
	
	mov al,00
	mov ah,00
l1:	push dx
	push ax
	mov bx,space
	push bx
	call print
	cmp al,79
	je clear
	inc al
	jmp next
clear:  mov al,0
	inc ah
next:   loop l1
	pop ax
	pop bx
	pop cx
	pop dx
	ret


displayPoison:
    ;;;pushing all th registers 
    push cx
	push dx
	push si
	push ax
	mov bx,snake
	mov di,0

	mov ax,[array+di]
	mov cx,[length]

loop_p:
	cmp ax,[bx]
	jne continue
	add di,2
	mov ax,[array+di]  ;;;dislpaying all the poisons at the random locations 
	mov bx,snake
	cmp di,3
	jg s


continue:
    add bx,2
	loop loop_p
	mov dx,1h
	push dx
	push ax
	mov bx,poison
	push bx
	call print
	mov byte[flag],1    ;;;;;indiactes poison is there if the flag is 1 

	

s:
    pop cx
	pop dx
	pop si
	pop ax
	ret

	increaselength:
	push ax
	push bx
	push cx
	mov cx,4
	mov dx,2
shifting:mov ah,[snake+1];row number
	mov al,[snake];column number
	cmp ah,[snake+2+1]
	jne vertical
	cmp al,[snake+2]
	jl decreasingCol
	inc al
	jmp save
decreasingCol:dec al
	jmp save
vertical:cmp ah,[snake+2]
	jl decreasingRow
	inc ah
	jmp save
decreasingRow:dec ah
save:	push ax;location of new character saved
	mov dx,[length]
	shl dx,1
	mov si,0
check:	cmp ax,[snake+si]
	jne noTouch
	call temp
	pop ax
	jmp pop30
noTouch:add si,2
	cmp si,dx
	jnz check

	mov bx,[length]
	shl bx,1
	sub bx,2
l7:	mov ax,[snake+bx]
	mov [snake+bx+2],ax
	sub bx,2
	cmp bx,0
	jge l7
	pop ax
	mov [snake],ax
	inc word[length]
	mov dx,240
	cmp word[length],dx
	jne nvm
	call victory
nvm:	call printSnake
	loop shifting
	mov ax,0x7
	push ax
	push word[fruit]
	
	mov bx,space
	push bx
	call print
pop30:	pop cx
	pop bx
	pop ax
	ret


setPoison:push ax
	push bx
	xor bx,bx
	mov ah,04
	mov al,07
	mov [array+bx],ax
	mov ah,10
	add bx,2
	mov [array+bx],ax
	mov ah,17
	mov al,56
	add bx,2
	mov [array+bx],ax
	mov ah,20
	mov al,75
	add bx,2
	mov [array+bx],ax
	pop bx
	pop ax
	ret

	printSnake:
	push bx
	push cx
	push di
	push dx
	mov cx,0xA
	push cx
	push word[snake]
	mov bx,head
	push bx
	call print;prints head
	mov di,2
	mov cx,[length]
	sub cx,2;1 for head and 1 for space at the end
body:	
    mov dx,0x2
	push dx
	push word[snake+di];location of character to be printed pushed
	mov bx,asterisk
	push bx
	call print
	add di,2
	loop body;characters of the remaining body of snake printed
	mov dx,0x7
	push dx
	push word[snake+di];clearing the previous location of last character of snake's body  
	mov bx,space
	push bx
	call print
	pop dx
	pop di
	pop cx
	pop bx
	ret

	border:
    push ax ; border of the game 
	push es
	push dx
	push cx
	mov dx,0
	mov cx,79
	mov ah,0
	mov al,0x95
f1:
    push ax
	push dx
	push boundary
	call print
	inc dl
	loop f1
	inc dh;move to next row
	mov dl,0
	mov cx,21
f2:	
    push ax
	push dx
	push boundary
	call print
	add dl,78
	push ax
	push dx
	push boundary
	call print
	inc dh
	mov dl,0
	loop f2
	mov cx,79
	mov dl,0
f3:
    push ax
	push dx
	push boundary
	call print
	inc dl
	loop f3
	mov cx,6
	mov ax,0xb800
	mov es,ax
	mov bx,str2
	mov di,3840
f4:
    mov al,[bx]
	mov ah,0x7
	mov word[es:di],ax
	add di,2
	inc bx
	loop f4
	call displayLives
	call displayScore
	mov di,3952
	mov bx,string
	mov cx,15
t1:
    mov al,[bx]
	mov ah,0x7
	mov word[es:di],ax
	add di,2
	inc bx
	loop t1
exitBorder:
    pop cx
	pop dx
	pop es
	pop ax
	ret


printNum: push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov ax, [bp+4] ; load number in ax
	mov bx, 10 ; use base 10 for division
	mov cx, 0 ; initialize count of digits
	nextdigit: mov dx, 0 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30 ; convert digit into ascii value
	push dx ; save ascii value on stack
	inc cx ; increment count of values
	cmp ax, 0 ; is the quotient zero
	jnz nextdigit ; if no divide it again
	mov di, 3872 ;
	nextpos: pop dx ; remove a digit from the stack
	mov dh, 0x07 ; use normal attribute
	mov [es:di], dx ; print char on screen
	add di, 2 ; move to next screen location
	loop nextpos ; repeat for all digits on stack
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2



	time_cal:push ax ; time calculations
	push es
	push di
	push bx
	push cx
	
	mov ax,0xb800
	mov es,ax
	mov di,3982;
	
hours:
    mov al,0;remaining hour's always gonna be 0
	add al,0x30
	mov ah,0x7
	mov word[es:di],ax;displaying higher digit of hour
	add di,2
	mov word[es:di],ax
	add di,2
	call colon
	add di,2



mins:
    mov dl,[cs:MinsRemaining]
	add dl,0x30
	mov dh,0x7
	mov al,0
	add al,0x30
	mov ah,0x7
	mov word[es:di],ax;displaying higher digit
	add di,2
	mov word[es:di],dx;displaying lower digit
	add di,2
	call colon
	add di,2
secs:	cmp byte[cs:initialSecs],0
	jne decrement
	mov byte[cs:initialSecs],60
	dec byte[cs:MinsRemaining]
decrement:dec byte[cs:initialSecs]
	mov dl,[cs:initialSecs]
noChange:mov al,dl
	mov ah,0
	mov dl,10
	div dl;divides remaining secs by 10
	mov dl,al;dl contains quotient
	mov dh,0x7
	add dl,0x30
	mov [es:di],dx
	add di,2
	mov al,ah;al contains remainder
	add al,0x30
	mov ah,0x7
	mov [es:di],ax
	add di,2
	jmp endDisplay
colon: 
    push ax
	push di
	mov al,':'
	mov ah,0x7
	mov word[es:di],ax
	add di,2
	pop di
	pop ax
	ret
 

timer:	
	
	push ax
	mov ax,[cs:speed]
	cmp [cs:tickCount],ax
	jne alreadySet
	call printing
alreadySet:cmp byte[cs:tickCount],18
	jne endTimer
	call printing
	inc byte[cs:seconds]
	mov byte[cs:tickCount],0
	call time_cal
	cmp byte[cs:initialSecs],0
	jne endTimer
	cmp byte[cs:MinsRemaining],0
	jne l9
	call quit
l9:	cmp byte[cs:seconds],20
	jne endTimer
	mov byte[cs:seconds],0
	sub byte[cs:speed],2
endTimer:inc byte[cs:tickCount]	
	mov al,0x20
	out 0x20,al
	
	pop ax
	iret

	
victory:
    call clrscr
	mov ax,0xb800
	mov es,ax
	mov cx,8
	mov di,2000
	mov bx,victory


msg:	

    mov al,[bx]
	mov ah,00000011b
	mov word[es:di],ax
	add bx,1
	add di,2
	loop msg
	mov ax,0x4c00
	int 0x21
quit:
 
    push ax
	push es
	push cx
	push di
	push bx
	
	cmp word[cs:length],240
	jne loser
	call victory

loser:	
    cmp byte[livesRemaining],0
	je over
	dec byte[livesRemaining]
	call displayLives
	jmp nextChance

over:
    call clrscr
	mov ax,0xb800
	mov es,ax
	mov cx,23
	mov di,1828
	mov bx,gameOver

msg2:	
    mov al,[bx]
	mov ah,0x7
	mov word[es:di],ax
	add bx,1
	add di,2
	loop msg2
	mov ax,0x4c00
	int 0x21
nextChance:
    mov byte[cs:MinsRemaining],00
	mov byte[cs:initialSecs],59
	pop bx
	pop di
	pop cx
	pop es
	pop ax
	ret
endDisplay:	pop cx
	pop bx
	pop di
	pop es
	pop ax
	ret

	headstarting:push si
	push cx
	push ax 
	mov ax,0x0A0A;location of snake's head
	mov si,0
	mov cx,[length]
set:	mov [snake+si],ax
	add al ,1;column number incremented
	add si,2
	loop set
	mov byte[left],0
	mov byte[right],0
	mov byte[up],0
	mov byte[down],0
	pop ax
	pop cx
	pop si
	ret
clrPoison:push ax
	push word[array+bx]
	push space
	call print
	ret

	appleGen:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push si
 	push di

	
	mov cx,[length]
	shl cx,1
	mov bx,0
	
	mov al,1
	mov ah,1
	
reset:
	add byte[random],2
	jmp repeat
it:
	mov byte[random],8

	
repeat:	
	add al,[random]
	add al,4
	cmp al,79
	ja it
	je reset
	add ah,[random]
	sub ah,2
	cmp ah,23
	ja it
	je reset

	cmp al,0
	je repeat
	
	cmp ah,0
	je repeat
	
l5:
	cmp ax, [snake+bx]
	je repeat
	add bx,2
	cmp bx,cx
	jne l5

	mov dx,0x4
	push dx
	push ax
	mov [fruit],ax
	mov bx,food
	cmp word[foodlocation],3
	jl goAhead
	mov word[foodlocation],0
goAhead:
    add bx,[foodlocation]
	inc word[foodlocation]
	push bx
	call print
	

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret


	displayScore:
	
	push di
	push ax
	push es
	push bx
	mov bx,points
	mov di,3860 
	;;;;; printing the score at almost the corner of the corner 

	mov ax,0xb800
	mov es,ax
	mov cx,6

printScore:

	mov al,[bx]
	mov ah,00000011b
	mov word[es:di],ax
	add bx,1
	add di,2
	loop printScore
	push word[score]
	call printNum

	;;;;;;popping all the re
	pop bx
	pop es
	pop ax
	pop di
	ret



displayLives:
	push di
	push ax
	push es
	mov di,3852
	mov ax,0xb800
	mov es,ax
	mov al,[livesRemaining]	
	add al,0x30
	mov ah,0x4
	mov word[es:di],ax
	pop es
	pop ax
	pop di
	ret



keymovement:push si
	push cx
	push ax
	push di
skip:	mov si,[length];si set to last index of snake
	shl si,1
	sub si,2
	mov cx,[length]
	dec cx
l3:	mov ax,[snake+si-2];
	mov [snake+si],ax
	sub si,2
	loop l3
	pop ax
	pop di
	pop cx
	pop si
	ret
temp:push dx
	push cx
	push bx
	push ax
	
	dec byte[livesRemaining]
	mov bx,snake
	mov cx,[length]
remove2:	mov ax,0x7
	push ax
	push word[bx]
	add bx,2
	mov dx,space
	push dx
	call print
	loop remove2
	mov word[length],21
	call headstarting
	call displayLives
	
	cmp byte[livesRemaining],0
	jne popTemp
	call quit
popTemp:pop ax
	pop bx
	pop cx
	pop dx
	ret

	printing:
	push ax
	cmp byte[cs:right],0
	je checkl
	mov al,[cs:snake];column number of snake's head
	cmp al,79
	je exit;snake needs to remain there at the last index if end of screen reached
	inc al;column number incremented
	mov ah,[cs:snake+1];previous row of snake retained
	push ax;new location of snake's head pushed
	call coordinates
	call printSnake
	jmp exit
checkl:
	cmp byte[cs:left],0
	je checkU
	mov al,[cs:snake];column number of snake's head
	cmp al,0
	je exit
	dec al;column number decremented
	mov ah,[cs:snake+1];previous row of snake retained
	push ax;new location of snake's head pushed
	call coordinates
	call printSnake
	jmp exit
checkU:
    cmp byte[cs:up],0
	je checkD
	mov ah,[cs:snake+1];ah contains row number of head
	cmp ah,0
	je exit
	dec ah
	mov al,[cs:snake];previous column of snake retained
		
	push ax;new location of snake's head pushed
	call coordinates
	call printSnake
	jmp exit
checkD:
   cmp byte[cs:down],0
	je exit
	mov ah,[cs:snake+1];ah contains row number of head
	cmp ah,23
	je exit
	inc ah;row number incremented
	mov al,[cs:snake]
	push ax
	call coordinates
	call printSnake
	jmp exit
exit:	
    pop ax
	ret




coordinates:push bp
	mov bp,sp;takes coordinates of head as parameter
	push si
	push cx
	push ax
	push bx
	mov ax,[bp+4];new location of snake's head
	cmp ah,00
	je borderTouched
	cmp ah,22
	je borderTouched
	cmp al,00
	je borderTouched
	cmp al,78
	je borderTouched	
	mov bx,snake
	mov cx,[length]
	dec cx
checkHead:	cmp ax,[bx];checks whether the snake touches itself
	je borderTouched
	add bx,2
	loop checkHead
	call keymovement
	mov ax,[bp+4];setting head to the argument passed
	mov cx,4
	mov bx,0
	cmp byte[flag],1
	jne noCheckForPoison
p:	cmp ax,[array+bx];compares new location of head with all possible locations of poison
	jne next2
	mov byte[flag],0;poison removed
	mov ax,0x7
	call clrPoison
	jmp borderTouched
next2:	add bx,2
	loop p
noCheckForPoison:
	mov [snake],ax	
	mov si,ax;location of head
	mov bx,[fruit];location of fruit
	cmp si,bx
	jne popAll
	call increaselength
	call appleGen
	add byte[score],4
	mov al,[score]
	mov dl,16
	mov ah,0
	div dl
	cmp ah,0
	jne noPoison
	call displayPoison
noPoison:	call displayScore
	jmp popAll
	
borderTouched:
	
	dec byte[livesRemaining]
	mov bx,snake
	mov cx,[length]
remove:	mov ax,0x7
	push ax
	push word[bx]
	add bx,2
	mov dx,space
	push dx
	call print
	loop remove 
	call headstarting
	call displayLives
	cmp byte[livesRemaining],0
	jne popAll
	call quit
popAll:	pop bx
	pop ax
	pop cx
	pop si
	pop bp
	ret 2


kbisr:
    push ax
	in al,0x60
	cmp al,75
	jne NOT_A
	cmp byte[left],1
	je Terminate
	mov byte[left],1
	mov byte[right],0
	mov byte[up],0
	mov byte[down],0
	jmp Terminate



NOT_A:  	

    cmp al,77
	jne NOT_AD
	cmp byte[right],1
	je Terminate
	mov byte[right],1
	mov byte[left],0
	mov byte[up],0
	mov byte[down],0
	jmp Terminate

NOT_AD:  

    cmp al,72
	jne NOT_D
	cmp byte[up],1
	je Terminate
	mov byte[up],1
	mov byte[left],0
	mov byte[right],0
	mov byte[down],0
	jmp Terminate

NOT_D:	

    cmp al,80
	jne Terminate
	cmp byte[down],1
	je Terminate
	mov byte[down],1
	mov byte[left],0
	mov byte[right],0
	mov byte[up],0

Terminate:	

    pop ax
	jmp far[cs:oldkbisr]
	
start: 

   call clrscr
	mov cx,11
	mov ax,0xb800
	mov es,ax
	mov ah,0x5
	mov bx,welcome
	mov di,1828

loop1:
	mov al,[bx]
	add bx,1
	mov word[es:di],ax
	add  di,2
	loop loop1

	mov ah,0
	int 0x16    ; calling the interrupt 16 for keyboard
	call clrscr
	call border

	;;calling the int 9 n=9
	xor ax,ax
	mov es,ax
	mov ax,[es:9*4]
	mov [oldkbisr],ax
	mov ax,[es:9*4+2]
	mov [oldkbisr+2],ax
	cli

	mov word[es:9*4],kbisr
	mov word[es:9*4+2],cs
	sti

	mov byte[cs:MinsRemaining],3

	xor ax, ax
	mov es, ax 
	; point es to IVT base
	cli 
	; disable interrupts

	mov word [es:8*4], timer; store offset at n*4
	mov [es:8*4+2], cs ; store segment at n*4+2

	sti ; enable interrupts
	call headstarting
	call setPoison
	call printSnake
	call appleGen

delay: jmp delay   ;;delaying for the last page 

finalend:
mov ax,0x4c00
int 0x21
