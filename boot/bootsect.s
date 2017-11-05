!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!
SYSSIZE = 0x3000
!
!	bootsect.s		(C) 1991 Linus Torvalds
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

SETUPLEN = 4				! nr of setup-sectors !!setup程序占用的扇区个数
BOOTSEG  = 0x07c0			! original address of boot-sector	!!计算机启动的时候BIOS将bootsect加载到的内存区域
INITSEG  = 0x9000			! we move boot here - out of the way !!我们将bootsect程序要移动到的内存其实地址
SETUPSEG = 0x9020			! setup starts here					!!setup程序开始的地方，简单计算下，9020H是段地址，转化成物理地址就是90200H也就是90000H+200H(512)
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536). !!系统加载到的内存其实地址
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading			!!系统结束的内存地址

! ROOT_DEV:	0x000 - same type of floppy as boot.
!		0x301 - first partition on first drive etc
!!start部分作用，将bootsect部分代码的起始地址从内存的BOOTSEG处移动到INITEG处，并初始化DS,ES,CS等段寄存器。

ROOT_DEV = 0x306

entry start
start:
	mov	ax,#BOOTSEG
	mov	ds,ax
	mov	ax,#INITSEG
	mov	es,ax
	mov	cx,#256
	sub	si,si
	sub	di,di
	rep			!!重复执行下面的一条指令cx次
	movw		!!将ds:si部分的一个字(16字节)移动到es:di处，并且执行后修改si与di
				!!在这之前的start操作都是为了上面的rep movw做准备，初始化ds寄存器，初始化es寄存器
	jmpi	go,INITSEG	!!设置CS寄存器为的值为INITSEG,IP寄存器为go的值
	
!!完成跳转代码继续执行，现在在逻辑上代码虽然是继续向下执行了，但在物理上我们的代码的执行是换到了另一个地方的。
go:	mov	ax,cs		!cs的值为INITSEG(0x9000),语句执行后ax也为INITSEG
	mov	ds,ax		!ds的值为INITSEG
	mov	es,ax		!es的值为INITSEG
! put stack at 0x9ff00.
	mov	ss,ax		!ss的值为INITSEG
	mov	sp,#0xFF00		! arbitrary value >>512

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.

load_setup:
	mov	dx,#0x0000		! drive 0, head 0	磁头号0,驱动器号0
	mov	cx,#0x0002		! sector 2, track 0	磁道号低8位,起始扇区号
	mov	bx,#0x0200		! address = 512, in INITSEG,将磁盘数据读到es:bx处，也就是90000+512=90512处
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors,ah=02代表读,al=4代表要读入4个扇区,也就是要读入2,3,4,5这四个扇区
	int	0x13			! read it,调用13号中断读取硬盘
	jnc	ok_load_setup		! ok - continue，如果上面的int 13没有出错，就跳转到ok_load_setup
	mov	dx,#0x0000
	mov	ax,#0x0000		! reset the diskette
	int	0x13
	j	load_setup

ok_load_setup:

! Get disk drive parameters, specifically nr of sectors/track

	mov	dl,#0x00		!读取驱动器0的参数
	mov	ax,#0x0800		! AH=8 is get drive parameters
	int	0x13
	mov	ch,#0x00
	seg cs				!设置段超越
	mov	sectors,cx		!将cx的值移动到cs:sectors
	mov	ax,#INITSEG		
	mov	es,ax			!因为int 0x13是会修改es的，现在修改回来

! Print some inane message

	mov	ah,#0x03		! read cursor pos
	xor	bh,bh
	int	0x10			!如果不读取当前鼠标的位置，下面输出的东西不会在接下来的地方显示出来，而是在屏幕的最上面显示
	
	mov	cx,#24
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg1
	mov	ax,#0x1301		! write string, move cursor
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000)

	mov	ax,#SYSSEG
	mov	es,ax		! segment of 0x010000
	call	read_it
	call	kill_motor

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.

	seg cs
	mov	ax,root_dev
	cmp	ax,#0
	jne	root_defined
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:

	jmpi	0,SETUPSEG

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUPLEN	! sectors read of current track
head:	.word 0			! current head
track:	.word 0			! current track

read_it:
	mov ax,es			
	test ax,#0x0fff      !0x0fff与ax的值0x1000做比较
die:	jne die			! es must be at 64kB boundary
	xor bx,bx		! bx is starting address within segment
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read
	ret
ok1_read:
	seg cs
	mov ax,sectors
	sub ax,sread
	mov cx,ax
	shl cx,#9
	add cx,bx
	jnc ok2_read
	je ok2_read
	xor ax,ax
	sub ax,bx
	shr ax,#9
ok2_read:
	call read_track
	mov cx,ax
	add ax,sread
	seg cs
	cmp ax,sectors
	jne ok3_read
	mov ax,#1
	sub ax,head
	jne ok4_read
	inc track
ok4_read:
	mov head,ax
	xor ax,ax
ok3_read:
	mov sread,ax
	shl cx,#9
	add bx,cx
	jnc rp_read
	mov ax,es
	add ax,#0x1000
	mov es,ax
	xor bx,bx
	jmp rp_read

read_track:
	push ax
	push bx
	push cx
	push dx
	mov dx,track
	mov cx,sread
	inc cx
	mov ch,dl
	mov dx,head
	mov dh,dl
	mov dl,#0
	and dx,#0x0100
	mov ah,#2
	int 0x13
	jc bad_rt
	pop dx
	pop cx
	pop bx
	pop ax
	ret
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

/*
 * This procedure turns off the floppy drive motor, so
 * that we enter the kernel in a known state, and
 * don't have to worry about it later.
 */
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "Loading system ..."
	.byte 13,10,13,10

.org 508
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:
