!
!	setup.s		(C) 1991 Linus Torvalds
!
! setup.s is responsible for getting the system data from the BIOS,
! and putting them into the appropriate places in system memory.
! both setup.s and system has been loaded by the bootblock.
!
! This code asks the bios for memory/disk/other parameters, and
! puts them in a "safe" place: 0x90000-0x901FF, ie where the
! boot-block used to be. It is then up to the protected mode
! system to read them from there before the area is overwritten
! for buffer-blocks.
!

! NOTE! These had better be the same as in bootsect.s!

INITSEG  = 0x9000	! we move boot here - out of the way
SYSSEG   = 0x1000	! system loaded at 0x10000 (65536).
SETUPSEG = 0x9020	! this is the current segment

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

entry start
start:

! ok, the read went well so we get current cursor position and save it for
! posterity.

	mov	ax,#INITSEG	! this is done in bootsect already, but...
	mov	ds,ax
	mov	ah,#0x03	! read cursor pos
	xor	bh,bh
	int	0x10		! save it in known place, con_init fetches	10中断的功能，读取当前光标的位置
	mov	[0],dx		! it from 0x90000.

! Get memory size (extended mem, kB)

	mov	ah,#0x88	!ah=88h是读取扩展内存大小的共呢个
	int	0x15		
	mov	[2],ax		!输出是ax，储存的是扩展内存的大小，以kb为单位

! Get video-card data:

	mov	ah,#0x0f
	int	0x10		!得到目前的显示模式
	mov	[4],bx		! bh = display page
	mov	[6],ax		! al = video mode, ah = window width

! check for EGA/VGA and some config parameters

	mov	ah,#0x12	!
	mov	bl,#0x10
	int	0x10
	mov	[8],ax
	mov	[10],bx
	mov	[12],cx

! Get hd0 data

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x41]	!!根据代码的这个部分了解中断向量表的结构，一个中断所占用的内存空间是4个字节，中断号*4就是这个跟中断表项的内存地址
					!!lds 的作用就是lds reg,mem 讲mem指向的地址高位放DS，低位放reg中
	mov	ax,#INITSEG	!!中断号*4就是这个中断向量的首地址
	mov	es,ax
	mov	di,#0x0080
	mov	cx,#0x10	!!传送0x10字节
					!!到这里ds:si=[中断描述符0x41的内存地址] es:di=0x9000:0x0080
	rep
	movsb

! Get hd1 data		!!原理同上

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x46]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	rep
	movsb

! Check that there IS a hd1 :-)

	mov	ax,#0x01500 !!ah=0x15代表读取设备类型功能号
	mov	dl,#0x81	!!输入 dl=0x81其中，0x80指第一块硬盘，0x81指第二块硬盘
	int	0x13		!!输出ah=类型码；00-没有这个盘,CF置位;01-是软盘;02-是软驱;03-是硬盘
	jc	no_disk1	
	cmp	ah,#3
	je	is_disk1
no_disk1:
	mov	ax,#INITSEG	
	mov	es,ax		
	mov	di,#0x0090	!!设置es:di=0x9000:0x0090
	mov	cx,#0x10
	mov	ax,#0x00	
	rep
	stosb			!!将al(也就是0)中的值按位复制到0x9000:0x0090-0x00A0处
is_disk1:	

! now we want to move to protected mode ...

	cli			! no interrupts allowed !		!!不可中断

! first we move the system to it's rightful place

	mov	ax,#0x0000
	cld			! 'direction'=0, movs moves forward
!!移动system模块，在bootsect.s中将系统加载到了0x10000开始的位置，这里将系统平移到0x00000位置
do_move:
	mov	es,ax		! destination segment
	add	ax,#0x1000	!!这里的0x1000(因这里是个段地址)就代表每次移动的字节数0x8000*2=0x10000
	cmp	ax,#0x9000
	jz	end_move
	mov	ds,ax		! source segment
	sub	di,di
	sub	si,si		!!到这里已经将ds:si=0x1000:0x0000  es:di=0x0000:0x0000设置成功
	mov 	cx,#0x8000	!!移动0x8000个字2^16个字节64KB
	rep
	movsw
	jmp	do_move

! then we load the segment descriptors

end_move:
	mov	ax,#SETUPSEG	! right, forgot this at first. didn't work :-)	!这是setup.s程序放置的段地址
	mov	ds,ax	
	lidt	idt_48		! load idt with 0,0	!!将idt_48的六个字节加载到idtr寄存器
	lgdt	gdt_48		! load gdt with whatever appropriate	!!将gdt_48的六个字节加载到gdtr寄存器中

! that was painless, now we enable A20

	call	empty_8042
	mov	al,#0xD1		! command write
	out	#0x64,al
	call	empty_8042
	mov	al,#0xDF		! A20 on
	out	#0x60,al
	call	empty_8042

! well, that went ok, I hope. Now we have to reprogram the interrupts :-(
! we put them right after the intel-reserved hardware interrupts, at
! int 0x20-0x2F. There they won't mess up anything. Sadly IBM really
! messed this up with the original PC, and they haven't been able to
! rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
! which is used for the internal hardware interrupts as well. We just
! have to reprogram the 8259's, and it isn't fun.

	mov	al,#0x11		! initialization sequence
	out	#0x20,al		! send it to 8259A-1
	.word	0x00eb,0x00eb		! jmp $+2, jmp $+2
	out	#0xA0,al		! and to 8259A-2
	.word	0x00eb,0x00eb
	mov	al,#0x20		! start of hardware int's (0x20)
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x28		! start of hardware int's 2 (0x28)
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x04		! 8259-1 is master
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x02		! 8259-2 is slave
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x01		! 8086 mode for both
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0xFF		! mask off all interrupts for now
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al

! well, that certainly wasn't fun :-(. Hopefully it works, and we don't
! need no steenking BIOS anyway (except for the initial loading :-).
! The BIOS-routine wants lots of unnecessary data, and it's less
! "interesting" anyway. This is how REAL programmers do it.
!
! Well, now's the time to actually move into protected mode. To make
! things as simple as possible, we do no register set-up or anything,
! we let the gnu-compiled 32-bit programs do that. We just jump to
! absolute address 0x00000, in 32-bit protected mode.

	mov	ax,#0x0001	! protected mode (PE) bit	
	lmsw	ax		! This is it!		!!置处理器状态字。但是只有操作数的低4位被存入CR0,
										!!也就是说只会影响PE,MP,EM,TS,其他位不会被影响，现在就进入了保护模式
	jmpi	0,8		! jmp offset 0 of segment 8 (cs)

! This routine checks that the keyboard command queue is empty
! No timeout is used - if this hangs there is something wrong with
! the machine, and we probably couldn't proceed anyway.
empty_8042:
	.word	0x00eb,0x00eb
	in	al,#0x64	! 8042 status port
	test	al,#2		! is input buffer full?
	jnz	empty_8042	! yes - loop
	ret

gdt:
	.word	0,0,0,0		! dummy

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9A00		! code read/exec
	.word	0x00C0		! granularity=4096, 386

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9200		! data read/write
	.word	0x00C0		! granularity=4096, 386

idt_48:
	.word	0			! idt limit=0
	.word	0,0			! idt base=0L

gdt_48:
	.word	0x800		! gdt limit=2048, 256 GDT entries
	.word	512+gdt,0x9	! gdt base = 0X9xxxx
	
.text
endtext:
.data
enddata:
.bss
endbss:
