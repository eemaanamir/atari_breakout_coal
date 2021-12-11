[org 0x0100]
jmp start

;for myRand
fab: dw  0, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765		;20 in total 
fabIndex: dw 0																						;which fabonaicii number to send next

underlineW1: db '/\/\/\/\/\/\/\/\/\/\/\/\/' , 0						
w1: db 'WELCOME TO ATARI BREAKOUT', 0								;welcome screen messages
w2: db '-> Use arrow keys to move the bar',0
w3: db '-> You must not let the ball fall or you loose a life',0
w4: db '-> You have five lives',0
w5: db '-> You must break all blocks in the game to win',0
w6: db 'Press any key to play',0
w7: db 'Press escape to exit',0
border: db '*',0

e1: db 'THANKYOU FOR PLAYING ATARI BREAKOUT',0						;exit screen messages

;left most x cordinate of the bar 
barPos: dw 30			;bar length is 20 words

;ball data
;bools to determine if to add or substract 1 from the x and y cordinates to calculate next position
boolX: db 0				;0=+1 1=-1 
boolY: db 1

;ball cordinates
ballX: dw 40			;col
ballY: dw 22			;row

;to store old keyboard old isr
oldisr: dd 0

score: dw 0
scoreText: db 'SCORE: ',0

lives: db '5'
livesText: db 'LIVES: ',0

quit: db 'YOU QUIT!',0
lost: db 'GAME OVER YOU LOSE!',0
win: db 'WELL PLAYED YOU WIN!',0

;total number of bricks in the game to comapre with to see if player has won
totalBricks: dw 0

myName: db 'DEVELOPED BY EEMAAN',0
;----------------------------------------------------------------------------------
;function to clear the screen by printing black spaces
clrscr:		
			push es
			push ax
			push di

			mov ax, 0xb800
			mov es, ax					; point es to video base
			mov di, 0					; point di to top left column

nextloc:	mov word [es:di], 0x0720	; clear next char on screen
			add di, 2					; move to next screen location
			cmp di, 4000				; has the whole screen cleared
			jne nextloc					; if no clear next position

			pop di
			pop ax
			pop es
			ret
;---------------------------------------------------------------------------------
;function to calculate random number with system time and send back in dx
rand: 		
			;mov dx,8
			
			push ax
			push bx
			push cx
			push es
			push bp
			push di
			push si
	
			mov bx,dx		; saving prev dx
		
randloop:	MOV AH, 00h  	; interrupts to get system time        
			INT 1Ah      	; CX:DX now hold number of clock ticks since midnight      
			
			mov  ax, dx
			xor  dx, dx
			mov  cx, 9    
			div  cx       	; here dx contains the remainder of the division - from 0 to 9

			add dx, 10		;range: 10 to 15 
			
			cmp dx,bx		; comaring new dx with old that is stored in bx
			je randloop		; find rand again if same so consective blocks have different lengths 
			
			pop si
			pop di
			pop bp
			pop es
			pop cx
			pop bx
			pop ax
			
			ret
;--------------------------------------------------------------------------------
;function to make cx even if its odd 
makeCXeven: 
			push bx
			
			mov bx,cx			;saving value of cx
			shr cx,1
			jnc evenCX
			add bx,1			;making value of cx even if was odd

evenCX: 	mov cx,bx			;restoring value of cx
			
			pop bx
			ret
			
;---------------------------------------------------------------------------------
;before every brick a rand must be called so add 1 to brick count then call rand
countAndRand:

			add word[totalBricks],1
			call rand
			ret
			
;---------------------------------------------------------------------------------
;function that recives row number and color of blocks and prints them in diffreent sizes in the whole row
print_blocks:
			push bp
			mov bp,sp
			push ax
			;push dx
			push cx
			push es
			push di
			push bx
			
			mov ax, 0xb800 					
			mov es, ax 	

			mov al, 80				; load al with columns per row
			mul byte [bp+6]			; 80 x r
			shl ax, 1				; byte no ((80xr)x2)

			mov di, ax				; point di to row start
		
			mov bx,ax
			add bx,160				;indicates end of row
			
			mov ah,byte[bp+4]		;loads attribute in ah
			mov al,0x20				;space ascii
			mov cx,di				; cx to start of row
			call countAndRand
			add cx,dx				;this controls length of 1st rectangle
			call makeCXeven
			
nextchar: 	mov word [es:di], ax 
here:		add di, 2 
			cmp di,bx				;bx has end of row
			jae goback				;stop printing and return if end of row is reached
			cmp di,cx 					
			jna nextchar
			call countAndRand
			add cx,dx				;this controls length of next rectangle
			call makeCXeven
			mov ah, 0x00			;attributes of black space (diffrent from other black spaces so that ball can bounce back from here)
			mov word [es:di], ax	; print black space
			mov ah,[bp+4]			;restoring coloured atribute to print next block
			jmp here
			
goback:		pop bx
			pop di
			pop es
			pop cx
			;pop dx 				;dx is not saved and restored in this subroutine because we need the value of dx in next calls
			pop ax
			pop bp
			ret	4
;------------------------------------------------------------------------------------
;function that recives x y cordinates of ball and prints ball there
print_ball:
			push ax
			push es
			push di
			
			mov ax, 0xb800 					
			mov es, ax 	

			mov ax, 80				; load al with columns per row
			mul word [ballY]		; 80 x r
			add ax, word[ballX]		; ((80xr)+c)
			shl ax, 1				; byte no (((80xr)+c)x2)
			mov di, ax
			
			
			mov ah, 0x0F			;white
			mov al, 'o'				;o ascii
			
			mov word [es:di], ax 
			
			
			
			pop di
			pop es
			pop ax
			ret 
;------------------------------------------------------------------------------------
;function that recives x cordinate of left side of bar and prints bar of length 20 in 23rd row
print_bar:
			push ax
			push es
			push di
			push cx
			
			mov ax,0xb800
			mov es,ax
			
			mov al, 80
			mov cl, 23				;row
			mul cl
			add ax, word[barPos]	;column
			shl ax,1
			mov di,ax
			
			mov ah, 0x18			;blue
			mov al, 0x20			;space
			
			mov cx,20
			
l1:			cmp cx,11				;differnt color to indicate middle
			je changeBar
			cmp cx,10
			je changeBar
barChanged:	mov word [es:di], ax	
			add di, 2	
			mov ah,0x18				;restore old attribute if changed
			loop l1		
			
			
			pop cx
			pop di
			pop es
			pop ax
			ret
			
changeBar:	mov ah,0xB0
			jmp barChanged
;------------------------------------------------------------------------------------
;loop to create a delay in the execution 
delay: 
			push cx
			mov cx, 0xffff
delayloop1:	loop delayloop1

			mov cx, 0xffff
delayloop2:	loop delayloop2

			mov cx, 0xffff
delayloop3:	loop delayloop3
			
			pop cx
			ret
			
;------------------------------------------------------------------------------------
;function to print a string by reciving attributes text and cordinates 
printStr:
			push bp
			mov bp, sp
			push es
			push ax
			push cx
			push si
			push di

			push ds
			pop es 					; es = ds

			mov di, [bp+4] 			; point di to string... es:di -->"Hello World"

			mov cx, 0xffff 			; load maximum number in cx
			xor al, al 				; load a zero in al
	
			repne scasb 			; find zero in the string
			
			mov ax, 0xffff 			; load maximum number in ax
			sub ax, cx 				; find change in cx
			dec ax 					; exclude null from length ; ffff - cx - 1 = 11 (strlen)
	
			jz exit 				; no printing if string is empty
			mov cx, ax 				; load string length in cx
		
			mov ax, 0xb800
			mov es, ax 				; point es to video base

			mov al, 80 				; load al with columns per row
			mul byte [bp+8]			; multiply with y position
			add ax, [bp+10] 		; add x position
			shl ax, 1 				; turn into byte offset
			mov di,ax 				; point di to required location
		
			mov si, [bp+4] 			; point si to string
			mov ah, [bp+6] 			; load attribute in ah

			cld 					; auto increment mode
.nextchar: 	lodsb 					; load next char in al
			stosw 					; print char/attribute pair
			loop .nextchar 			; repeat for the whole string

exit:		pop di
			pop si
			pop cx
			pop ax
			pop es
			pop bp
			ret 8
;------------------------------------------------------------------------------------
;function that prints all the messages on the welcome prompt screen
welcome:
			push ax
			push cx
			push bx
			call clrscr
			
			mov cx,79
			mov bx,1
			
topborder:	push bx 			; push column
			mov ax, 0
			push ax				; push row
			mov ax, 0x8F		; white on black blinking 
			push ax 			; push attribute
			mov ax, border
			push ax 			; push address of message
			call printStr
			add bx,1
			loop topborder
			
			mov cx,79
			mov bx,1
bottomborder:	
			push bx 			; push column
			mov ax, 24
			push ax				; push row
			mov ax, 0x8F		; white on black blinking 
			push ax 			; push attribute
			mov ax, border
			push ax 			; push address of message
			call printStr
			add bx,1
			loop bottomborder
			
			
			mov ax, 27
			push ax 			; push column
			mov ax, 3
			push ax				; push row
			mov ax, 4			; red on black 
			push ax 			; push attribute
			mov ax, w1
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine 
			
			mov ax, 27
			push ax 			; push column
			mov ax, 4
			push ax				; push row
			mov ax, 0x89		; blue on black blinking
			push ax 			; push attribute
			mov ax, underlineW1
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine

			mov ax, 3
			push ax 			; push column
			mov ax, 9
			push ax				; push row
			mov ax, 13			; magenta
			push ax 			; push attribute
			mov ax, w2
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine 
			
			mov ax, 3
			push ax 			; push column
			mov ax, 10
			push ax				; push row
			mov ax, 13			; magenta
			push ax 			; push attribute
			mov ax, w3
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine 
			
			mov ax, 3
			push ax 			; push column
			mov ax, 11
			push ax				; push row
			mov ax, 13			; magenta
			push ax 			; push attribute
			mov ax, w4
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine 
			
			mov ax, 3
			push ax 			; push column
			mov ax, 12
			push ax				; push row
			mov ax, 13			; magenta
			push ax 			; push attribute
			mov ax, w5
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine 
			
			mov ax, 29
			push ax 			; push column
			mov ax, 17
			push ax				; push row
			mov ax, 14			; yellow
			push ax 			; push attribute
			mov ax, w6
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine 
			
			mov ax, 29
			push ax 			; push column
			mov ax, 18
			push ax				; push row
			mov ax, 14			; yellow
			push ax 			; push attribute
			mov ax, w7
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine 
			
			mov ax, 60
			push ax 			; push column
			mov ax, 22
			push ax				; push row
			mov ax, 11			; cyan
			push ax 			; push attribute
			mov ax, myName
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine 
			
			
			pop bx
			pop cx
			pop ax
			ret
;------------------------------------------------------------------------------------
;function to print thankyou message and exit 
clrExit: 	call clrscr
exitGame:
			
			mov ax, 22
			push ax 			; push column
			mov ax, 14
			push ax				; push row
			mov ax, 14			; yellow
			push ax 			; push attribute
			mov ax, e1
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine
			
			mov ax, 60
			push ax 			; push column
			mov ax, 21
			push ax				; push row
			mov ax, 11			; cyan
			push ax 			; push attribute
			mov ax, myName
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine 
			
			mov sp,0xfffe
			
			mov ax, 0x4c00 		; terminate program
			int 0x21
;------------------------------------------------------------------------------------
;function to print message of loosing game 
lostExit: 
			call clrscr
			
			call printLives
			call printScore
			
			mov ax, 30
			push ax 			; push column
			mov ax, 10
			push ax				; push row
			mov ax, 12			; red
			push ax 			; push attribute
			mov ax, lost
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine
			
			jmp unhookexit
			
;------------------------------------------------------------------------------------
;function to print massage of winning game
winExit:
			call clrscr
			
			call printLives
			call printScore
			
			mov ax, 30
			push ax 			; push column
			mov ax, 10
			push ax				; push row
			mov ax, 12			; red
			push ax 			; push attribute
			mov ax, win
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine
			
			jmp unhookexit
;------------------------------------------------------------------------------------
;function to print message of quitting is esc is pressed after game has been started
quitExit:	
			call clrscr	
			
			call printLives
			call printScore
			
			mov ax, 35
			push ax 			; push column
			mov ax, 10
			push ax				; push row
			mov ax, 12			; red
			push ax 			; push attribute
			mov ax, quit
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine
			
			jmp unhookexit
;------------------------------------------------------------------------------------
;function to unhook the keyboard inturrpt 
unHook:
			push ax
			push bx
			push es
			
			xor ax,ax
			mov es,ax
		
			mov ax, [oldisr]								; read old offset in ax
			mov bx, [oldisr+2]								; read old segment in bx
			
			cli												; disable interrupts
			mov [es:9*4], ax								; restore old offset from ax
			mov [es:9*4+2], bx								; restore old segment from bx
			sti												; enable interrupts
			
			pop es
			pop bx
			pop ax
			ret
;------------------------------------------------------------------------------------
;function to call unhooking function and then exit function			
unhookexit:
			call unHook
			jmp exitGame
;------------------------------------------------------------------------------------
;function to print the start of the game by calling functions of ball, bar, bricks, score and lives printing
startGame: 	
			call clrscr
			
			call printLives
			call printScore
			
			push dx
			
			push 2				;row [bp+6]
			push 0x48			;color attribute
			call print_blocks
			
			push 4				;row
			push 0x68
			call print_blocks
			
			push 6				;row
			push 0x58	
			call print_blocks
			
			push 8				;row
			push 0xA8
			call print_blocks
			
			pop dx
			
			call print_ball
		
			call print_bar
			
			ret
;------------------------------------------------------------------------------------
;function to erase the brick from screen that was hit
;cordinates are not current but next
;remove brick and make the spaces on its side normal so ball can move into them
clrBrick:
			
			push ax
			push es
			push di
			push bx
			push cx
			
			mov ax, 0xb800 					
			mov es, ax 	

			mov ax, 80					; load al with columns per row
			mul word [ballY]			; 80 x r
			add ax, word[ballX]			; ((80xr)+c)
			shl ax, 1					; byte no (((80xr)+c)x2)
			mov di, ax
			
			cmp word[es:di],0x0020
			jne oneBrick				
			add word[score],1			;center of two bricks was hit so add extra score 
			call brickSound
			call delay
			
oneBrick:	call brickSound
			call delay
			mov ax, 0x0720				;normal attribute of clear screen
			
			mov word [es:di], ax 		;delete word that was hit
			
			mov bx,word[ballX]			;move postion that was hit to bx

checkLeft:	sub bx,1					;checking left
			cmp bx,0					;checking for screen left edge
			jb checkRight
			
			mov ax, 80					; load al with columns per row
			mul word [ballY]			; 80 x r
			add ax, bx					; ((80xr)+c)
			shl ax, 1					; byte no (((80xr)+c)x2)
			mov di, ax
			
			mov ax, 0x0720				;normal attribute of clear screen
			
			cmp word [es:di], 0x0020 	;check if brick side
			je delRight
			cmp word[es:di],0x0720		;check if brick has ended
			je checkRight
			mov word [es:di],ax			;if not end then still the brick that was hit so delete
			jmp checkLeft

delRight:	mov word[es:di],ax			;was brick side so clear and then strat checking right 

checkRight:								;left side of brick deleted now delete right
			mov bx,word[ballX]			;load the point that was hit in bx again
rightLoop:	add bx,1					;go right
			cmp bx,79					;check screen right edge
			ja blockDeled
			
			mov ax, 80					; load al with columns per row
			mul word [ballY]			; 80 x r
			add ax, bx					; ((80xr)+c)
			shl ax, 1					; byte no (((80xr)+c)x2)
			mov di, ax
			
			mov ax, 0x0720				;normal attribute of clear screen
			
			cmp word [es:di], 0x0020 	;check if brick side
			je delRet
			cmp word[es:di],0x0720		;check if brick has ended
			je blockDeled
			mov word [es:di],ax			;if not end then we're still inside brick that was hit so delete word
			jmp rightLoop
			
		
delRet:		mov word[es:di],ax			;edge of brick is reached so clear it and return
blockDeled:	pop cx						;block completely deleted so return 
			pop bx
			pop di
			pop es
			pop ax
			ret
			
			
;------------------------------------------------------------------------------------
;function of erasing the bar on the screen so a new one can be printed later
clrBar:		
			push ax
			push es
			push di
			push cx
			
			mov ax,0xb800
			mov es,ax
			
			mov al, 80
			mov cl, 23				;row
			mul cl
			add ax, word[barPos]	;column
			shl ax,1
			mov di,ax
			
			mov ah,	0x07			;black
			mov al, 0x20			;space
			
			mov cx,20
			
.l1:		mov word [es:di], ax	
			add di, 2									
			loop .l1		
			
			
			pop cx
			pop di
			pop es
			pop ax
			ret
			
;------------------------------------------------------------------------------------
;function that is hooked with keyboard inturrput that according to key codes moves the bar or exits
moveBar:
			
			push ax
		
			in al, 0x60
			
			cmp al,0x01				;esc
			je quitExit
			
			cmp al, 0x4D			
			je rightarrow
			
			cmp al, 0x4B		
			je leftarrow
			
			jmp nomatch
			
rightarrow:
			cmp word[barPos],60
			je nomatch
			cmp word[ballY],23
			je nomatch
			call clrBar
			add word[barPos],1
			call print_bar
			
			jmp nomatch
			
leftarrow:
			cmp word[barPos],0
			je nomatch
			cmp word[ballY],23
			je nomatch
			
			call clrBar
			sub word[barPos],1
			call print_bar
			
			jmp nomatch
			
nomatch:	

			pop ax

			jmp far [cs:oldisr] 	; call the original ISR
			
			ret
;------------------------------------------------------------------------------------
;function to calculate the next coordinates of ball
nextXY:		
			cmp byte[boolX],0
			je addX
			sub word[ballX],1
			jmp changeY

addX:		add word[ballX],1

changeY:	cmp byte[boolY],0
			je addY
			sub word[ballY],1
			jmp changedXY

addY:		add word[ballY],1

changedXY: 	ret 	
;------------------------------------------------------------------------------------
;prev coordinates of ball
prevXY:	
			cmp byte[boolX],0
			je subX
			add word[ballX],1
			jmp ChangeY

subX:		sub word[ballX],1

ChangeY:	cmp byte[boolY],0
			je subY
			add word[ballY],1
			jmp ChangedXY

subY:		sub word[ballY],1

ChangedXY: ret
;------------------------------------------------------------------------------------
;function that tells wetaher the next cordinates calculated are inside are bounds and ball can move onto them
;set ax to 1 if ball can move at coordinates else 0
canmove:	
			push es
			push di
			
			mov ax, 0xb800
			mov es, ax					; point es to video base
			
			mov ax, 80				; load al with columns per row
			mul word [ballY]		; 80 x r
			add ax, word[ballX]		; ((80xr)+c)
			shl ax, 1				; byte no (((80xr)+c)x2)
			mov di, ax
			
			
			mov ah, 0x07			;black
			mov al, 0x20			;space
			
			cmp word [es:di], ax 	; is next ball position black space
			je yes					; then ball can move there 
			mov ax,0				;ax=0 ball cannot move
			pop di
			pop es
			ret
			
			
yes: 		mov ax,1
			
			pop di
			pop es
			ret
;------------------------------------------------------------------------------------
;function to erase the ball so it can be reprinted at next position 
clrBall:
			push ax
			push es
			push di
			
			mov ax, 0xb800 					
			mov es, ax 	

			mov ax, 80				; load al with columns per row
			mul word [ballY]		; 80 x r
			add ax, word[ballX]		; ((80xr)+c)
			shl ax, 1				; byte no (((80xr)+c)x2)
			mov di, ax
			
			
			mov ah, 0x07			;black
			mov al, 0x20			;space
			
			mov word [es:di], ax 
			
			pop di
			pop es
			pop ax
			ret 
;------------------------------------------------------------------------------------
;function with infinite loop that keeps moving the ball around the screen by calling appropriate functions
moveBall:	

			cmp word[ballX],79
			je rightHit
			
			cmp word[ballX],0
			je leftHit
			
			cmp word[ballY],23
			je bottomHit
			
			cmp word[ballY],1	;row 0 is for scores and lives
			je topHit
			
goToNext2:	
			cmp word[ballY],1	;checking here again 
			je topHit

goToNext3:
			call nextXY			; get next postion of ball
			
			call canmove		; check if ball can move to this next position
			
			cmp ax,0
			je barHit			;cant move there because coloured block that is either brick or bar
			
			call prevXY			;get curreent position of ball back

goToNext:	
			call delay
			call delay
			call clrBall		; erase ball from there
			
			call nextXY			; get next postion of ball
			
			call print_ball		; print ball at new postion
			
			jmp moveBall

topHit:		call topBounce
			call hitSound
			jmp goToNext3
	
barHit:		
			cmp word[ballY],23	; check if hit was at brick or bar
			jne brickHit		; bar is at 23Y so if next pos is not at 23Y  then it was brick hit
			call hitSound
			call prevXY			;get current ball postion
			call barBounce
			cmp ax,1			;was mid hit called
			je moveBall			;ball has already been moved so go to top
			jmp goToNext		;normal procedure after direction change
			
			
brickHit:						
			call clrBrick
			call prevXY			; get curr position of ball
			call topBounce
			add word[score],1
			call printScore
			mov ax,word[totalBricks]
			cmp word[score],ax
			je winExit
			jmp goToNext2		
			
rightHit:	call hitSound
			call rightBounce
			jmp goToNext2		;ball can have only one type of hit so dont check for other types just move the ball 
			

leftHit:	call hitSound
			call leftBounce
			jmp goToNext2
			

bottomHit:	
			call bottomBounce
			sub byte[lives],1
			call printLives
			call lifeSound
			cmp byte[lives],'0'
			je lostExit
			call delay
			call delay
			call delay
			call delay
			call delay
			call delay
			jmp goToNext

;-----------------------------------------------------------------------------------
;bounce functions change direction of movement according to place of bounce 
;these functions chnage the values in boolX and boolY 	
;-----------------------------------------------------------------------------------
;mid of bar= pos+9 and bar pos+10
;left of bar= <pos+9
;right of bar= >pos+10
;move 1 to ax if bounce was mid bounce and 0 if not
barBounce:
			
			mov ax,word[barPos]
			add ax,9				;pos+9
			cmp word[ballX],ax
			je midHit
			add ax,1				;pos+10
			cmp word[ballX],ax
			je midHit
			cmp word[ballX],ax		;>pos+10
			ja barRight
			jmp barLeft
			
			
midHit:								;movememt here is in 90 degree angle
			sub word[ballY],1		;move up by one row and repeat until either brick or top is reached
			cmp word[ballY],1		;check if top edge is reached
			je end2
			call canmove			;check if there is no block
			cmp ax,0
			je end1
			add word[ballY],1		;go to where ball is currently
			call delay
			call delay
			call clrBall			;delete ball
			sub word[ballY],1		;move up by one row
			call print_ball			;print new ball
			jmp midHit

barRight:	mov byte[boolX],0		;go right
			mov byte[boolY],1		;go up
			mov ax,0
			ret
			
barLeft:	mov byte[boolX],1		;go left
			mov byte[boolY],1		;go up
			mov ax,0
			ret

end1:		
			call clrBrick	
			add word[score],1
			call printScore

end2:		add word[ballY],1		;current position of ball
			mov ax,1				;mid hit was called
			ret
			
;-----------------------------------------------------------------------------------
;ball was moving down (y+1) (y increases downwards as first row is 0 and last is 24)
bottomBounce:
				push dx
				
				mov byte[boolY],1		; y was 0 hence bottomBounce was called so invert the direction 

				mov byte[boolX],0
				call myRand				;returns random number in dx
				shr dx,1				; 1 or 0 randomly moved to CF
				adc byte[boolX],0		;add value of CF to boolX to set direction of X in bounce
				
				pop dx
				ret

;-----------------------------------------------------------------------------------
;ball was moving up (y-1)
topBounce:
				push dx
				
				mov byte[boolY],0		; y was 1 hence topBounce was called so invert the direction 

				mov byte[boolX],0
				call myRand				;returns random number in dx
				shr dx,1				; 1 or 0 randomly moved to CF
				adc byte[boolX],0		;add value of CF to boolX to set direction of X in bounce
				
				pop dx
				ret
;------------------------------------------------------------------------------------
;ball was moving left (x-1)
leftBounce:
				push dx
				
				mov byte[boolX],0		; x was 1 hence leftBounce was called so invert the direction 

				mov byte[boolY],0
				call rand				;returns random number in dx
				shr dx,1				; 1 or 0 randomly moved to CF
				adc byte[boolY],0		;add value of CF to boolY to set direction of Y in bounce
				
				pop dx
				ret
			
;-----------------------------------------------------------------------------------
;ball was moving right {x+1)
rightBounce:
				push dx
				
				mov byte[boolX],1		; x was 0 hence leftBounce was called so invert the direction 

				mov byte[boolY],0
				call rand				;returns random number in dx
				shr dx,1				; 1 or 0 randomly moved to CF
				adc byte[boolY],0		;add value of CF to boolY to set direction of Y in bounce
				
				pop dx
				ret
;------------------------------------------------------------------------------------
;this is a pseudo random function
;function to return a fabonaicii number
;returns rand number in dx
myRand:
				push di 
				 
				mov di,word[fabIndex]
				mov dx,word[fab+di]
				
				call indexinc
				
				pop di
				ret
;------------------------------------------------------------------------------------
;goes to next index for the fab array 
indexinc: 
			cmp word[fabIndex],38
			je zero
			add word[fabIndex],2
			ret
			
zero:		mov word[fabIndex],0
			ret
;------------------------------------------------------------------------------------
;function to hook the keyboard inturrpt
hook:
			push ax
			push es

			xor ax, ax
			mov es, ax										; point es to IVT base
			
			mov ax, [es:9*4]
			mov [oldisr], ax								; save offset of old routine
			mov ax, [es:9*4+2]
			mov [oldisr+2], ax								; save segment of old routine
			
			cli												; disable interrupts
			mov word [es:9*4], moveBar						; store offset at n*4
			mov [es:9*4+2], cs								; store segment at n*4+2
			sti												; enable interrupts
			
			pop es
			pop ax
			ret
;------------------------------------------------------------------------------------
;function to print the life count and text at the top of the screen
printLives:
			push ax
			push es
			push di
			
			mov ax, 71
			push ax 			; push column
			mov ax, 0
			push ax				; push row
			mov ax, 13			; magenta
			push ax 			; push attribute
			mov ax, livesText
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine
			
			
			mov ax, 0xb800 					
			mov es, ax 	

			mov ax, 0				; 80 x r
			add ax, 78				; ((80xr)+c)
			shl ax, 1				; byte no (((80xr)+c)x2)
			mov di, ax
			
			
			mov ah, 0x0F			;white
			mov al, byte[lives]		
			
			mov word [es:di], ax 
			
			pop di
			pop es
			pop ax
			ret 
;------------------------------------------------------------------------------------
;function that print decimal eqquivalent of hex data on screen where cordinates and number are sent in parameters 
printnum: 	
			push bp
			mov bp, sp
			push es
			push ax
			push bx
			push cx
			push dx
			push di
			
			mov ax, 0xb800
			mov es, ax 					; point es to video base
			mov ax, [bp+4]				; load number in ax
			mov bx, 10 					; use base 10 for division
			mov cx, 0 					; initialize count of digits
			
nextdigit: 	mov dx, 0 					; zero upper half of dividend
			div bx 						; divide by 10
			add dl, 0x30 				; convert digit into ascii value
			push dx 					; save ascii value on stack
			inc cx 						; increment count of values
			cmp ax, 0 					; is the quotient zero
			jnz nextdigit 				; if no divide it again
			mov di, 18	 				; point di to 9th column
			
nextpos: 	pop dx 						; remove a digit from the stack
			mov dh, 0x0F 				; use normal attribute
			mov [es:di], dx 			; print char on screen
			add di, 2 					; move to next screen location
			loop nextpos 				; repeat for all digits on stack
			
			pop di
			pop dx
			pop cx
			pop bx
			pop ax 
			pop es
			pop bp
			ret 2
;------------------------------------------------------------------------------------
;function to print the score count and the text
printScore:	
			push ax
			
			mov ax, 2
			push ax 			; push column
			mov ax, 0
			push ax				; push row
			mov ax, 13			; magenta
			push ax 			; push attribute
			mov ax, scoreText
			push ax 			; push address of message
			call printStr 		; call the printstr subroutine
			
			push word[score]
			call printnum
			
			pop ax
			ret
;----------------------------------------------------------------------------------
;function to call sound when a life is lost ie. ball hits bottom
lifeSound:
			push ax
			
			mov al, 0b6h
			out 43h, al

			;load the counter 2 value for h2
			mov ax, 9664
			out 42h, al
			mov al, ah
			out 42h, al

			;turn the speaker on
			in al, 61h
			mov ah,al
			or al, 3h
			out 61h, al
			call delay
			mov al, ah
			out 61h, al
			
			;load the counter 2 value for h2
			mov ax, 9664
			out 42h, al
			mov al, ah
			out 42h, al

			;turn the speaker on
			in al, 61h
			mov ah,al
			or al, 3h
			out 61h, al
			call delay
			mov al, ah
			out 61h, al
			
			pop ax
			ret
;-----------------------------------------------------------------------------------
;function to call sound when a brick is hit by the ball
brickSound:
			push ax
			
			mov al, 0b6h
			out 43h, al

			;load the counter 2 value for h3
			mov ax, 4832
			out 42h, al
			mov al, ah
			out 42h, al

			;turn the speaker on
			in al, 61h
			mov ah,al
			or al, 3h
			out 61h, al
			call delay
			mov al, ah
			out 61h, al
			
			pop ax
			ret
;------------------------------------------------------------------------------------
;function to call sound when the walls or the bar is hit
hitSound:
			push ax
			
			mov al, 0b6h
			out 43h, al
			
			;load the counter 2 value for e5
			mov ax, 1810
			out 42h, al
			mov al, ah
			out 42h, al
	
			;turn the speaker on
			in al, 61h
			mov ah,al
			or al, 3h
			out 61h, al
			call delay
			mov al, ah
			out 61h, al
			
			pop ax
			ret
;------------------------------------------------------------------------------------
;the main driver of the program that calls all the functions when program execution starts
start:
			call welcome
			
			mov ah,0
			int 0x16
			
			cmp ax, 0x011B		;esc
			je clrExit
			
			
			call startGame
			
			call print_ball
		
			call print_bar
			
			call hook
			
			call delay
			call delay
			call moveBall		;infinite loop 

			mov ax, 0x4c00 	
			int 0x21 