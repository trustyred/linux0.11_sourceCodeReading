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

	
!!该部分的作用是将setup部分的代码从磁盘中读取到内存中	
! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.

load_setup:
	mov	dx,#0x0000		! drive 0, head 0	磁头号0,驱动器号0
	mov	cx,#0x0002		! sector 2, track 0	磁道号低8位,起始扇区号
	mov	bx,#0x0200		! address = 512, in INITSEG,将磁盘数据读到es:bx处，也就是90000+512=90512处
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors,ah=02代表读,al=4代表要读入4个扇区,也就是要读入2,3,4,5这四个扇区
	int	0x13			! read it,调用13号中断读取硬盘
	jnc	ok_load_setup		!! ok - continue，如果上面的int 13没有出错，就跳转到ok_load_setup
	mov	dx,#0x0000
	mov	ax,#0x0000		! reset the diskette
	int	0x13
	j	load_setup

ok_load_setup:

! Get disk drive parameters, specifically nr of sectors/track

	mov	dl,#0x00		!!读取驱动器0的参数
	mov	ax,#0x0800		!! AH=8 is get drive parameters，该功能的返回参数，CH=最大磁道数低8位，CL(0:5)=每磁道最大扇区数，CL(6:7)=最大磁道数高两位，DH=最大磁头数
	int	0x13
	mov	ch,#0x00
	seg cs				!!设置段超越，如果不设置那么mov sector,cx的语义是将cx的值移动到ds:sector处，但此时的ds与cs的值是完全一样的为什么还要使用段超越呢
						!!我的理解是，我们使用的sectors是在代码最后定义好的，也就是说无论如何这个数据是一定在代码段下的，这样使用段超越是一种最稳妥的行为
	mov	sectors,cx		!!将cx的值移动到cs:sectors，这里linus默认将cl就当做了每磁道的扇区数，而没有管(6:7)位的磁道号高两位，是因为目前的磁道数都还没有达到256，故这两位可能默认为0了
	
	mov	ax,#INITSEG		
	mov	es,ax			!!因为int 0x13是会修改es的，现在修改回来

! Print some inane message

	mov	ah,#0x03		! read cursor pos
	xor	bh,bh
	int	0x10			!!如果不读取当前鼠标的位置，下面输出的东西不会在接下来的地方显示出来，而是在屏幕的最上面显示
	
	mov	cx,#24
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg1
	mov	ax,#0x1301		! write string, move cursor
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000)
!!到现在为止，bootsect与setup已经加载到内存中了，开始加载系统
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
	test ax,#0x0fff      !!0x0fff与ax的值0x1000按位相与结果为0
die:	jne die			! es must be at 64kB boundary
	xor bx,bx		! bx is starting address within segment
rp_read:
	mov ax,es			
	cmp ax,#ENDSEG		! have we loaded all yet? !!判断es是否已经到达ENDSEG，如果不是进入ok1_read
	jb ok1_read
	ret
ok1_read:
	seg cs				!!仍然是设置段超越，原因与上面相同
	mov ax,sectors		!!获得扇区个数
	sub ax,sread		!!用磁道所有扇区个数-读取的扇区个数=未读扇区个数
	mov cx,ax			!!cx存储未读扇区个数
	shl cx,#9			!!左移9位的意思就是乘512
	add cx,bx			!!这里的bx初始为0，但是在完成一次int 13h,ax=02h的时候，bx的数值就是es段的偏移地址，也就是读缓冲区的起始地址
	jnc ok2_read		!!如果将要读取的数据没有超过64kb，那么就跳转到ok2_read
	je ok2_read
	xor ax,ax
	sub ax,bx
	shr ax,#9
ok2_read:
	call read_track		!!读扇区
	mov cx,ax			!!此次操作应该读取的扇区数,这里的ax值是ok1_read中计算出来的，也就是(sectors-sread)
	add ax,sread		!!这个值是当前磁道上已经读取的扇区数，也就是sectors(即每磁道的扇区数，在不出错的情况下，现在已经将第一个磁道的所有扇区读取完毕)
	seg cs				!!继续设置段超越，因为需要对代码中存储的变量进行写操作
	cmp ax,sectors		!!计算ax-sectors的结果，将影响各种标志寄存器的值
	jne ok3_read		!!如果上面的相减结果不为0说明，还没有读完当前磁道的所有扇区，那么执行ok3_read
	mov ax,#1			!!接下来读磁道另一磁头上的数据，如果已经完成，则去读下一磁道
	sub ax,head			!!判断当前磁头
	jne ok4_read		!!如果是0磁头，则再去读1磁头上的扇区数据
	inc track			!!否则读下一磁道
ok4_read:
	mov head,ax
	xor ax,ax
ok3_read:
	mov sread,ax		!!保存ax，也就是已读扇区数
	shl cx,#9			!!计算上一次读取数据的字节数
	add bx,cx			!!将当前的段内偏移加上再读一次磁盘会发生的偏移
	jnc rp_read			!!如果上面的值小于64kb也就是一个段仍可以容纳，那么就调用rp_read继续读，否则就要调整当前段，为下一次读做准备
	mov ax,es			
	add ax,#0x1000		
	mov es,ax			!!修改段基质，跳到下一个段，因为本段已经无法满足继续的读操作了
	xor bx,bx			!!清空偏移量
	jmp rp_read			!!继续读

read_track:
	push ax
	push bx
	push cx
	push dx
	mov dx,track	!!获得当前的磁道号
	mov cx,sread	!!获取已经读了的扇区数
	inc cx			!!当前扇区+1代表要读的扇区
	mov ch,dl		!!ch代表当前磁道号的低8位
	mov dx,head		!!获取当前的磁头号
	mov dh,dl		!!dh=磁头号，这里是因为dx存储的是磁道磁头号，但是
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
