/*
 *  linux/boot/head.s
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 *  head.s contains the 32-bit startup code.
 *
 * NOTE!!! Startup happens at absolute address 0x00000000, which is also where
 * the page directory will exist. The startup code will be overwritten by
 * the page directory.
 */
.text
.globl _idt,_gdt,_pg_dir,_tmp_floppy_area
_pg_dir:
startup_32:
	movl $0x10,%eax		/*在setup.s最后只是把cs寄存器设置为了保护模式下的寄存器值，在这里设置其他寄存器*/
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	mov %ax,%gs
	lss _stack_start,%esp	/*设置系统堆栈*/
	call setup_idt		/*初始化idt中断描述符表*/
	call setup_gdt		/*初始化gdt全局描述符表*/
	movl $0x10,%eax		# reload all the segment registers
	mov %ax,%ds		# after changing gdt. CS was already
	mov %ax,%es		# reloaded in 'setup_gdt'
	mov %ax,%fs
	mov %ax,%gs
	lss _stack_start,%esp
	
/**这里用来测试A20地址线是否开启了，向0x000000地址复制一个值，然后与0x100000比较，如果一直相同就代表是回环状态，操作系统会陷入

死机*/	
	xorl %eax,%eax
1:	incl %eax		# check that A20 really IS enabled
	movl %eax,0x000000	# loop forever if it isn't
	cmpl %eax,0x100000
	je 1b
/*
 * NOTE! 486 should set bit 16, to check for write-protect in supervisor
 * mode. Then it would be unnecessary with the "verify_area()"-calls.
 * 486 users probably want to set the NE (#5) bit also, so as to use
 * int 16 for math errors.
 */
/*
下面为了检测数学协处理器是否存在，我们不多做介绍，重点集中在after_page_table 
*/
 
	movl %cr0,%eax		# check math chip
	andl $0x80000011,%eax	# Save PG,PE,ET
/* "orl $0x10020,%eax" here for 486 might be good */
	orl $2,%eax		# set MP
	movl %eax,%cr0
	call check_x87
	jmp after_page_tables

/*
 * We depend on ET to be correct. This checks for 287/387.
 */
check_x87:
	fninit
	fstsw %ax
	cmpb $0,%al
	je 1f			/* no coprocessor: have to set bits */
	movl %cr0,%eax
	xorl $6,%eax		/* reset MP, set EM */
	movl %eax,%cr0
	ret
.align 2
1:	.byte 0xDB,0xE4		/* fsetpm for 287, ignored by 387 */
	ret

/*
 *  setup_idt
 *
 *  sets up a idt with 256 entries pointing to
 *  ignore_int, interrupt gates. It then loads
 *  idt. Everything that wants to install itself
 *  in the idt-table may do so themselves. Interrupts
 *  are enabled elsewhere, when we can be relatively
 *  sure everything is ok. This routine will be over-
 *  written by the page tables.
 */
 
/*
*	理解setup_idt需要理解两个概念，分别是中断描述符表和门描述符（可以理解为中断描述符表的表项）
*	在程序的后面有定义一个中断描述符表的数据结构，即"_idt .fill 256,8,0"这里定义了256个项，每个项8个字节，并用
*	0填充，也就是我们的中断描述符表初始的样子，setup_idt运行后，这256项都会填写上有意义的值。下面介绍门描述符
*   门描述符就是_idt的表项，它的作用是寻找这个中断所对应的功能，其实也就是一中寻址方式，比在bootsect.s中简陋的
*	中断号*4的寻址方式复杂了很多。其中0-7字节中，0-1字节是低16位偏移地址，6-7字节是高16位偏移地址，2-3字节是
*	段选择符，4-5字节是各种标志位不详细介绍，总之通过这8字节的结构我们就可以找到相应中断对应的功能，比如当我们
*	调用0x80中断的时候，对应的就是255个表项中第80位的中断功能，通过中断描述表的第80个表项就能寻址到这个功能。
* 
*/ 
setup_idt:
	lea ignore_int,%edx		/*此时的edx放置的为ignore_int的偏移地址   */
	movl $0x00080000,%eax	/*eax=0x00080000*/
	movw %dx,%ax		/* selector = 0x0008 = cs   eax=0x0008:dx(ignore_int低16位)*/
	movw $0x8E00,%dx	/* interrupt gate - dpl=0, present   edx=ignore_int(高16位):0x8E00  */

	lea _idt,%edi		/*把_idt的地址加载到edi中*/
	mov $256,%ecx
rp_sidt:
	movl %eax,(%edi)		
	movl %edx,4(%edi)		/*以上两条指令将一个门描述符设置好，(0-15)ignore_int的低16位,(16-31)0x00080000也就是段选择符
																(32-47)是各种标志位,(48-63)ignore的高16位*/
	addl $8,%edi
	dec %ecx
	jne rp_sidt
	lidt idt_descr
	ret

/*
 *  setup_gdt
 *
 *  This routines sets up a new gdt and loads it.
 *  Only two entries are currently built, the same
 *  ones that were built in init.s. The routine
 *  is VERY complicated at two whole lines, so this
 *  rather long comment is certainly needed :-).
 *  This routine will beoverwritten by the page tables.
 */
 
/*
*	将在代码中设置好的GDT入口加载到gdtr寄存器中，这里我们仔细看一下gdt_descr的结构
*gdt_descr:
*	.word 256*8-1		# so does gdt (not that that's any
*	.long _gdt		# magic number, but it works for me :^)
*
*	.align 3		#代表下面代码必须以2^3次方对其
*在gdt_descr结构中又提到了_gdt结构我们继续看下这个结构
*_gdt:	
*	.quad 0x0000000000000000	/* NULL descriptor */
*	.quad 0x00c09a0000000fff	/* 16Mb */
*	.quad 0x00c0920000000fff	/* 16Mb */
*	.quad 0x0000000000000000	/* TEMPORARY - don't use */
*	.fill 252,8,0			/* space for LDT's and TSS's etc */
*gdt_descr其实就是cpu中GDTR寄存器的结构32bit+16bit，其中32bit是gdt全局描述符表的基地址，16bit是gdt表长的界限
*_gdt就是全局描述符表,总共又256个项，每个项长8个字节
*/ 
setup_gdt:
	lgdt gdt_descr
	ret

/*
 * I put the kernel page tables right after the page directory,
 * using 4 of them to span 16 Mb of physical memory. People with
 * more than 16MB will have to expand this.
 */
.org 0x1000
pg0:

.org 0x2000
pg1:

.org 0x3000
pg2:

.org 0x4000
pg3:

.org 0x5000
/*
 * tmp_floppy_area is used by the floppy-driver when DMA cannot
 * reach to a buffer-block. It needs to be aligned, so that it isn't
 * on a 64kB border.
 */
_tmp_floppy_area:
	.fill 1024,1,0

after_page_tables:
	pushl $0		# These are the parameters to main :-)
	pushl $0
	pushl $0
	pushl $L6		# return address for main, if it decides to.
	pushl $_main
	jmp setup_paging
L6:
	jmp L6			# main should never return here, but
				# just in case, we know what happens.

/* This is the default interrupt "handler" :-) */
int_msg:
	.asciz "Unknown interrupt\n\r"
.align 2
ignore_int:
	pushl %eax
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	pushl $int_msg
	call _printk
	popl %eax
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret


/*
 * Setup_paging
 *
 * This routine sets up paging by setting the page bit
 * in cr0. The page tables are set up, identity-mapping
 * the first 16MB. The pager assumes that no illegal
 * addresses are produced (ie >4Mb on a 4Mb machine).
 *
 * NOTE! Although all physical memory should be identity
 * mapped by this routine, only the kernel page functions
 * use the >1Mb addresses directly. All "normal" functions
 * use just the lower 1Mb, or the local data space, which
 * will be mapped to some other place - mm keeps track of
 * that.
 *
 * For those with more memory than 16 Mb - tough luck. I've
 * not got it, why should you :-) The source is here. Change
 * it. (Seriously - it shouldn't be too difficult. Mostly
 * change some constants etc. I left it at 16Mb, as my machine
 * even cannot be extended past that (ok, but it was cheap :-)
 * I've tried to show which constants to change by having
 * some kind of marker at them (search for "16Mb"), but I
 * won't guarantee that's all :-( )
 */
.align 2
setup_paging:
	movl $1024*5,%ecx		/* 5 pages - pg_dir+4 page tables */
	xorl %eax,%eax
	xorl %edi,%edi			/* pg_dir is at 0x000 */
	cld;rep;stosl
	movl $pg0+7,_pg_dir		/* set present bit/user r/w */
	movl $pg1+7,_pg_dir+4		/*  --------- " " --------- */
	movl $pg2+7,_pg_dir+8		/*  --------- " " --------- */
	movl $pg3+7,_pg_dir+12		/*  --------- " " --------- */
	movl $pg3+4092,%edi
	movl $0xfff007,%eax		/*  16Mb - 4096 + 7 (r/w user,p) */
	std
1:	stosl			/* fill pages backwards - more efficient :-) */
	subl $0x1000,%eax
	jge 1b
	xorl %eax,%eax		/* pg_dir is at 0x0000 */
	movl %eax,%cr3		/* cr3 - page directory start */
	movl %cr0,%eax
	orl $0x80000000,%eax
	movl %eax,%cr0		/* set paging (PG) bit */
	ret			/* this also flushes prefetch-queue */

.align 2
.word 0
idt_descr:
	.word 256*8-1		# idt contains 256 entries
	.long _idt
.align 2
.word 0
gdt_descr:
	.word 256*8-1		# so does gdt (not that that's any
	.long _gdt		# magic number, but it works for me :^)

	.align 3
_idt:	.fill 256,8,0		# idt is uninitialized

_gdt:	.quad 0x0000000000000000	/* NULL descriptor */
	.quad 0x00c09a0000000fff	/* 16Mb */
	.quad 0x00c0920000000fff	/* 16Mb */
	.quad 0x0000000000000000	/* TEMPORARY - don't use */
	.fill 252,8,0			/* space for LDT's and TSS's etc */
