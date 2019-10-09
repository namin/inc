(add-tests-with-string-output-noboot "test compiler"
   [(let ()
      (begin
        (let ()
          (load "self.scm")
          (load "reader.scm")
          (load "compiler.scm")
          (all-conversions ''#(foo)))
        'ok))
    => "ok\n"]
   [(let ()
      (begin
        (let ()
          (load "self.scm")
          (load "reader.scm")
          (load "compiler.scm")
          (all-conversions '(list 1 2 3)))
        'ok))
   => "ok\n"]
    [(let ()
     (load "self.scm")
     (load "reader.scm")
     (load "compiler.scm")
     (emit-program '(let () (fx+ 2 2)))
     'ok)
   => "  .text\n  .globl scheme_entry\n  .type scheme_entry, @function\nscheme_entry:\n  mov 4(%esp), %ecx\n  mov %ebx, 4(%ecx)\n  mov %esi, 16(%ecx)\n  mov %edi, 20(%ecx)\n  mov %ebp, 24(%ecx)\n  mov %esp, 28(%ecx)\n  mov %ecx, %esi\n  mov 12(%esp), %ebp\n  mov 8(%esp), %esp\n  mov $0, %edi\n  call L_scheme_entry\n  mov %esi, %ecx\n  mov 4(%ecx), %ebx\n  mov 16(%ecx), %esi\n  mov 20(%ecx), %edi\n  mov 24(%ecx), %ebp\n  mov 28(%ecx), %esp\n  ret\n  .text\n  .globl L_scheme_entry\n  .type L_scheme_entry, @function\nL_scheme_entry:\n  mov $8, %eax\n  mov %eax, -4(%esp)\n  mov $8, %eax\n  add -4(%esp), %eax\n  ret\nok\n"]
  [(let ()
     (load "self.scm")
     (load "reader.scm")
     (load "compiler.scm")
     (emit-program 2)
     'ok)
   => "  .text\n  .globl scheme_entry\n  .type scheme_entry, @function\nscheme_entry:\n  mov 4(%esp), %ecx\n  mov %ebx, 4(%ecx)\n  mov %esi, 16(%ecx)\n  mov %edi, 20(%ecx)\n  mov %ebp, 24(%ecx)\n  mov %esp, 28(%ecx)\n  mov %ecx, %esi\n  mov 12(%esp), %ebp\n  mov 8(%esp), %esp\n  mov $0, %edi\n  call L_scheme_entry\n  mov %esi, %ecx\n  mov 4(%ecx), %ebx\n  mov 16(%ecx), %esi\n  mov 20(%ecx), %edi\n  mov 24(%ecx), %ebp\n  mov 28(%ecx), %esp\n  ret\n  .text\n  .globl L_scheme_entry\n  .type L_scheme_entry, @function\nL_scheme_entry:\n  mov $8, %eax\n  ret\nok\n"
   ]
  )
